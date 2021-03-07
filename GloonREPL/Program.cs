using System;
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
      while (true)
      {
        Console.ForegroundColor = ConsoleColor.Cyan;
        Console.Write("> ");
        Console.ForegroundColor = ConsoleColor.White;
        var line = Console.ReadLine();
        if (string.IsNullOrEmpty(line))
          return;
        if (line.StartsWith('#'))
        {
          switch (line)
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
              Console.WriteLine($"Memory clear");
              break;
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
          var syntaxTree = Parsing.Parse(line);
          var compilation = new Compilation(syntaxTree);
          if (CST) Utils.printCST(syntaxTree.ToExpression());
          var result = compilation.Evaluate(variables);
          if (result.Diagnostics.Any())
          {
            Console.WriteLine();
            result.Diagnostics.ToList().ForEach(diag =>
            {
              Console.ForegroundColor = ConsoleColor.Red;
              Console.WriteLine(diag);
              Console.ForegroundColor = ConsoleColor.DarkGray;
              Console.Write(" -> " + line[..diag.Span.Start]);
              Console.ForegroundColor = ConsoleColor.Red;
              Console.Write(line[diag.Span.Start..diag.Span.End]);
              Console.ForegroundColor = ConsoleColor.DarkGray;
              Console.WriteLine(line[diag.Span.End..]);
              Console.WriteLine();
            });
          }
          else
          {
            Console.ForegroundColor = ConsoleColor.Gray;
            Console.WriteLine(result.Value);
          }
        }
      }
    }
  }
}