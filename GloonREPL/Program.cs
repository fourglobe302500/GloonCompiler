using System;
using System.Linq;

using Gloon;
using Gloon.Compiler;
using Gloon.Syntax;

namespace GloonREPL
{
  internal sealed class Program
  {
    internal static void Main(string[] args)
    {
      var CST = false;
      var BST = false;
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
            case "#CST":
              CST = !CST;
              Console.WriteLine($"{(CST ? "Showing" : "Hiding")} Concrete Syntax Tree");
              break;
            case "#BST":
              BST = !BST;
              Console.WriteLine($"{(BST ? "Showing" : "Hiding")} Bound Syntax Tree");
              break;
            default:
              Console.WriteLine("Invallid command");
              break;
          }
        }
        else
        {
          var syntaxTree = Parser.Parse(line);
          var compilation = new Compilation(syntaxTree);
          if (CST) Utils.printCST(syntaxTree.ToExpression());
          var result = compilation.Evaluate();
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