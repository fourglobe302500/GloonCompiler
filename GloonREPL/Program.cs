using Gloon;
using Gloon.Compiler.Syntax;

using System;
using System.Linq;

namespace GloonREPL
{
    class Program
    {
        static void Main(string[] args)
        {
            var CST = false;
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
                        default:
                            Console.WriteLine("Invallid command");
                            break;
                    }
                else
                {
                    var (tokens, diagnostics) = Lexer.Lex(line);
                    var tree = Parser.Parse(tokens, diagnostics);
                    Console.ForegroundColor = ConsoleColor.Gray;
                    if (CST) Utils.prettyPrint("", true, true, Types.SyntaxNode.NewCST(tree));
                    if (diagnostics.Length > 0)
                    {
                        Console.ForegroundColor = ConsoleColor.Red;
                        diagnostics.ToList().ForEach(diag => Console.WriteLine(diag));
                    }
                    else
                    {
                        Console.ForegroundColor = ConsoleColor.Magenta;
                        Console.WriteLine(Evaluate(Types.SyntaxNode.NewCST(tree)));
                    }
                }
            }
        }

        static object Evaluate(Types.SyntaxNode node) => node switch
        {
            Types.SyntaxNode.CST t => Evaluate(Types.SyntaxNode.NewExpression(t.Item.Root)),
            Types.SyntaxNode.Expression e => e.Item switch
            {
                Types.ExpressionSyntax.BinaryExpression b => b.Operator.Kind.Tag switch
                {
                    Types.TokenKind.Tags.PlusToken => (int)Evaluate(Types.SyntaxNode.NewExpression(b.Left)) + (int)Evaluate(Types.SyntaxNode.NewExpression(b.Right)),
                    Types.TokenKind.Tags.MinusToken => (int)Evaluate(Types.SyntaxNode.NewExpression(b.Left)) - (int)Evaluate(Types.SyntaxNode.NewExpression(b.Right)),
                    Types.TokenKind.Tags.StarToken => (int)Evaluate(Types.SyntaxNode.NewExpression(b.Left)) * (int)Evaluate(Types.SyntaxNode.NewExpression(b.Right)),
                    Types.TokenKind.Tags.SlashToken => (int)Evaluate(Types.SyntaxNode.NewExpression(b.Left)) / (int)Evaluate(Types.SyntaxNode.NewExpression(b.Right)),
                    Types.TokenKind.Tags.PowerToken => Math.Pow((int)Evaluate(Types.SyntaxNode.NewExpression(b.Left)), (int)Evaluate(Types.SyntaxNode.NewExpression(b.Right))),
                    Types.TokenKind.Tags.ModulosToken => (int)Evaluate(Types.SyntaxNode.NewExpression(b.Left)) % (int)Evaluate(Types.SyntaxNode.NewExpression(b.Right)),
                    _ => throw new Exception("GLOON::EVALUATION::EVALUATE Unexpected Operator.")
                },
                Types.ExpressionSyntax.UnaryExpression u => u.Operator.Kind.Tag switch
                {
                    Types.TokenKind.Tags.PlusToken => (int)Evaluate(Types.SyntaxNode.NewExpression(u.Operand)),
                    Types.TokenKind.Tags.MinusToken => -(int)Evaluate(Types.SyntaxNode.NewExpression(u.Operand)),
                    _ => throw new Exception("GLOON::EVALUATION::EVALUATE Unexpected Operator.")
                },
                Types.ExpressionSyntax.LiteralExpression l => Evaluate(Types.SyntaxNode.NewToken(l.LiteralToken)),
                Types.ExpressionSyntax.ParenthesysExpression p => Evaluate(Types.SyntaxNode.NewExpression(p.Expr)),
                Types.ExpressionSyntax.ErrorExpression err => throw new Exception(err.Error.Text),
                _ => throw new Exception("GLOON::EVALUATION::EVALUATE Unexpected Expression.")
            },
            Types.SyntaxNode.Token t => t.Item.Value,
            _ => throw new Exception("GLOON::EVALUATION::EVALUATE Unexpected Node."),
        };
    }
}