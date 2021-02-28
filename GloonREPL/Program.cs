using Gloon;
using Gloon.Compiler.Binding;
using Gloon.Compiler.Syntax;
using Gloon.Evaluation;

using System;
using System.Linq;

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
        else
        {
          var (BoundSyntaxTree, Diagnostics, ConcreteSyntaxTree) = Binder.Bind(Parser.Parse(Lexer.Lex(line)));
          Console.ForegroundColor = ConsoleColor.Gray;
          if (CST) Utils.PrintCST(Types.SyntaxNode.NewCST(ConcreteSyntaxTree));
          if (BST) Utils.PrintBST(BoundTypes.BoundNode.NewExpression(BoundSyntaxTree));
          if (Diagnostics.Any())
          {
            Console.ForegroundColor = ConsoleColor.Red;
            Diagnostics.ToList().ForEach(diag => Console.WriteLine(diag));
          }
          else
          {
            Console.ForegroundColor = ConsoleColor.Magenta;
            Console.WriteLine(Evaluator.Evaluate(BoundSyntaxTree));
          }
        }
      }
    }
  }
}