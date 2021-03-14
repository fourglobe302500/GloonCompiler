﻿namespace Gloon.Syntax

open Gloon.Text
open System
open System.IO

type TokenKind =
  | NumberLiteralToken    of int
  | BooleanLiteralToken   of bool
  | Identifier            of string
  | WhiteSpaceToken       of string
  | InvallidToken         of string
  | EndOfFileToken
  | IncrementToken
  | PlusToken
  | DecrementToken
  | MinusToken
  | PowerToken
  | StarToken
  | RootToken
  | SlashToken
  | PercentToken
  | BangToken
  | BangEqualsToken
  | DoubleEqualsToken
  | EqualsToken
  | LessThanEqualsToken
  | LessThanToken
  | GreaterThanEqualsToken
  | GreaterThanToken
  | DoubleAmpersandToken
  | DoublePipeToken
  | OpenParenToken
  | CloseParenToken

type Token =
  {
    Position: int
    Text: string
    Kind: TokenKind
    Value: obj
  }

  interface IReportable with
    member t.GetSpan () = t.Span
    member t.GetKind () = t.Kind.ToString()
    member t.GetText () = t.Text

  member t.Span = TextSpan(t.Position, t.Text.Length)

  override this.ToString () = this.Kind.ToString()

type ExpressionSyntax =
  | ParenthesysExpression of OpenParen: Token * Expr: ExpressionSyntax * CloseParen: Token
  | LiteralExpression     of LiteralToken: Token
  | IdentifierExpression  of IdentifierToken: Token
  | AssignmentExpression  of IdentifierToken: Token * EqualToken: Token * Expr: ExpressionSyntax
  | BinaryExpression      of Left: ExpressionSyntax * Operator: Token * Right: ExpressionSyntax
  | UnaryExpression       of Operator: Token * Operand: ExpressionSyntax
  | ErrorExpression       of Error: Token

  member e.Span =
    match e with
    | LiteralExpression                      n  -> n.Span
    | IdentifierExpression                   i  -> i.Span
    | AssignmentExpression         (i, _, expr) -> i.Span + expr.Span
    | ParenthesysExpression        (op, _, cp)  -> op.Span + cp.Span
    | BinaryExpression        (left, _, right)  -> left.Span + right.Span
    | UnaryExpression      (operator, operand)  -> operator.Span + operand.Span
    | ErrorExpression                        e  -> e.Span

  member this.Children = this |> function
    | LiteralExpression                      n  -> [Token n]
    | IdentifierExpression                   i  -> [Token i]
    | AssignmentExpression         (i, e, expr) -> [Token i; Token e; Expression expr]
    | ParenthesysExpression        (op, e, cp)  -> [Token op; Expression e; Token cp]
    | BinaryExpression (left, operator, right)  -> [Expression left; Token operator; Expression right]
    | UnaryExpression      (operator, operand)  -> [Token operator; Expression operand]
    | ErrorExpression                        e  -> [Token e]

  override this.ToString () = this |> function
    | LiteralExpression      _ -> "Literal Expression"
    | IdentifierExpression   _ -> "Identifier Expression"
    | AssignmentExpression   _ -> "Assigment Expression"
    | ParenthesysExpression  _ -> "Parenthesised Expression"
    | BinaryExpression       _ -> "Binary Expression"
    | UnaryExpression        _ -> "Unary Expression"
    | ErrorExpression        _ -> "Error Expression"

and CompilationUnit (root: ExpressionSyntax, endOfFileToken: Token) =
  let root = root
  let endOfFileToken = endOfFileToken

  member _.Root = root
  member _.EndOfFileToken = endOfFileToken
  member _.Span = root.Span
  member _.Children = [Expression root; Token endOfFileToken]

and SyntaxNode =
  | Expression      of ExpressionSyntax
  | Token           of Token
  | CompilationUnit of CompilationUnit

  member n.Span =
    match n with
    | Expression e -> e.Span
    | Token t -> t.Span
    | CompilationUnit t -> t.Span

  member this.Children = this |> function
    | Expression      e -> e.Children
    | Token           _ -> []
    | CompilationUnit t -> t.Children

  override this.ToString () = this |> function
    | Expression      e -> e.ToString ()
    | Token           t -> t.ToString ()
    | CompilationUnit _ -> "Compilation Unit"

  member node.WriteTo writer = node.PrettyPrint(writer, "", true, true)

  member private node.PrettyPrint (writer: TextWriter, indent, first, last) =
    let isToConsole = writer = Console.Out
    if isToConsole then Console.ForegroundColor <- ConsoleColor.DarkGray
    writer.Write("{0}{1}", indent, (if first then "" else if last then "└── " else "├── "))
    if isToConsole then
      Console.ForegroundColor <-
        match node with
        | Expression _ -> ConsoleColor.Cyan
        | Token _ -> ConsoleColor.Blue
        | CompilationUnit _ -> ConsoleColor.Yellow
    writer.WriteLine(node)
    let lastNode = node.Children |> List.tryLast
    node.Children |>
    List.iter (fun n -> n.PrettyPrint(writer, indent + (if not last then "│   " else if first then "" else "    "), false, (n = lastNode.Value)))