﻿using System;
using System.Text;
using System.Linq;
using System.Collections.Generic;

using Gloon;
using Gloon.Symbols;
using Gloon.Syntax;
using Gloon.Compiler;

namespace GloonREPL
{
  internal sealed class Program
  {
    internal static void Main(string[] args)
    {
      var CST = false;
      var variables = new Dictionary<VariableSymbol, object>();
      var textBuilder = new StringBuilder();
      Compilation previous = null;

      while (true)
      {
        Console.ForegroundColor = ConsoleColor.Green;
        Console.Write(textBuilder.Length == 0 ? "» " : "· ");
        Console.ForegroundColor = ConsoleColor.White;
        var input = Console.ReadLine();
        var isBlank = string.IsNullOrEmpty(input);
        if (isBlank && textBuilder.Length == 0)
          break;
        if (input.StartsWith('#') && textBuilder.Length == 0)
        {
          if (string.IsNullOrEmpty(input)) return;
          switch (input)
          {
            case "#cls":
              Console.Clear();
              break;
            case "#quit":
              return;
            case "#cst":
              CST = !CST;
              Console.WriteLine($"{(CST ? "Showing" : "Hiding")} Concrete Syntax Tree");
              break;
            case "#clm":
              variables.Clear();
              previous = null;
              Console.WriteLine($"Memory clear");
              break;
            case "#vm":
            case "#viewmemory":
              foreach (var key in variables.Keys)
                Console.WriteLine($"  {key.Name}: {variables[key]}");
              break;
            default:
              Console.WriteLine("Invallid command");
              break;
          }
        }
        else
        {
          textBuilder.AppendLine(input);
          var text = textBuilder.ToString();
          var syntaxTree = SyntaxTree.Parse(text);
          if (!isBlank && syntaxTree.Diagnostics.Any())
            continue;
          var compilation =
            previous == null
            ? new Compilation(syntaxTree)
            : previous.ContinueWith(syntaxTree);
          if (CST) syntaxTree.RootNode.WriteTo(Console.Out);
          var result = compilation.Evaluate(variables);
          if (result.Diagnostics.Any())
          {
            result.Diagnostics.ToList().ForEach(diag =>
            {
              var lineIndex = syntaxTree.Text.GetLineIndex(diag.Span.Start);
              var line = syntaxTree.Text.Lines[lineIndex];
              var lineNumber = lineIndex + 1;
              var character = diag.Span.Start - line.Span.Start + 1;
              Console.WriteLine();
              Console.ForegroundColor = ConsoleColor.Red;
              Console.WriteLine($"({ lineNumber}, { character}): " + diag);
              Console.ForegroundColor = ConsoleColor.DarkGray;
              Console.Write(" line -> " + text[line.Start..diag.Span.Start].TrimStart());
              Console.ForegroundColor = ConsoleColor.Red;
              Console.Write(text[diag.Span.Start..diag.Span.End]);
              Console.ForegroundColor = ConsoleColor.DarkGray;
              Console.WriteLine(text[diag.Span.End..line.End]);
            });
            Console.WriteLine();
          }
          else
          {
            Console.ForegroundColor = ConsoleColor.Magenta;
            Console.WriteLine(result.Value);
            previous = compilation;
          }
          textBuilder.Clear();
        }
      }
    }
  }
}