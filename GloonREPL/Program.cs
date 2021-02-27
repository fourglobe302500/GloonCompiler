using Gloon;
using Gloon.Compiler.Binding;
using Gloon.Compiler.Syntax;

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
                    if (BST) Utils.PrintBST(BoundSyntaxTree);
                    if (Diagnostics.Any())
                    {
                        Console.ForegroundColor = ConsoleColor.Red;
                        Diagnostics.ToList().ForEach(diag => Console.WriteLine(diag));
                    }
                    else
                    {
                        Console.ForegroundColor = ConsoleColor.Magenta;
                        Console.WriteLine(Evaluate(BoundSyntaxTree));
                    }
                }
            }
        }

        internal static int Evaluate(BoundTypes.BoundNode node) => node switch
        {
            BoundTypes.BoundNode.Expression e => e.Item switch
            {
                BoundTypes.BoundExpression.BinaryExpression b => b.@operator.Tag switch
                {
                    BoundTypes.BinaryOperatorKind.Tags.Addition => Evaluate(BoundTypes.BoundNode.NewExpression(b.left)) + Evaluate(BoundTypes.BoundNode.NewExpression(b.right)),
                    BoundTypes.BinaryOperatorKind.Tags.Subtraction => Evaluate(BoundTypes.BoundNode.NewExpression(b.left)) - Evaluate(BoundTypes.BoundNode.NewExpression(b.right)),
                    BoundTypes.BinaryOperatorKind.Tags.Multiplication => Evaluate(BoundTypes.BoundNode.NewExpression(b.left)) * Evaluate(BoundTypes.BoundNode.NewExpression(b.right)),
                    BoundTypes.BinaryOperatorKind.Tags.Division => Evaluate(BoundTypes.BoundNode.NewExpression(b.left)) / Evaluate(BoundTypes.BoundNode.NewExpression(b.right)),
                    BoundTypes.BinaryOperatorKind.Tags.Power => (int)Math.Pow(Evaluate(BoundTypes.BoundNode.NewExpression(b.left)), Evaluate(BoundTypes.BoundNode.NewExpression(b.right))),
                    BoundTypes.BinaryOperatorKind.Tags.Modulos => Evaluate(BoundTypes.BoundNode.NewExpression(b.left)) % Evaluate(BoundTypes.BoundNode.NewExpression(b.right)),
                    _ => throw new Exception("GLOON::EVALUATION::EVALUATE Unexpected Operator."),
                },
                BoundTypes.BoundExpression.UnaryExpression u => u.@operator.Tag switch
                {
                    BoundTypes.UnaryOperatorKind.Tags.Identity => Evaluate(BoundTypes.BoundNode.NewExpression(u.operand)),
                    BoundTypes.UnaryOperatorKind.Tags.Negation => -Evaluate(BoundTypes.BoundNode.NewExpression(u.operand)),
                    _ => throw new Exception("GLOON::EVALUATION::EVALUATE Unexpected Operator."),
                },
                BoundTypes.BoundExpression.LiteralExpression l => (int)l.Value,
                _ => throw new Exception("GLOON::EVALUATION::EVALUATE Unexpected Node."),
            },
            _ => 0,
        };
    }
}