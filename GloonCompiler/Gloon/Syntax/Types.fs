namespace Gloon.Syntax

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

[<CustomEquality>]
[<CustomComparison>]
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
    | AssignmentExpression         (i, e, expr) -> i.Span + expr.Span
    | ParenthesysExpression        (op, e, cp)  -> op.Span + cp.Span
    | BinaryExpression (left, operator, right)  -> left.Span + right.Span
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

  member t.Match o =
    match t, o with
    | (ParenthesysExpression (o1,p1,c1)), (ParenthesysExpression (o2,p2,c2)) -> o1 = o2 && p1.Match(p2) && c1 = c2
    | (LiteralExpression l1), (LiteralExpression l2) -> l1 = l2
    | (IdentifierExpression i1), (IdentifierExpression i2) -> i1 = i2
    | (AssignmentExpression (a1,eq1,ex1)), (AssignmentExpression (a2,eq2,ex2)) -> a1 = a2 && eq1 = eq2 && ex1.Match(ex2)
    | (BinaryExpression (l1,o1,r1)), (BinaryExpression (l2,o2,r2)) -> l1.Match(l2) && o1 = o2 && r1.Match(r2)
    | (UnaryExpression (o1,e1)), (UnaryExpression (o2,e2)) -> o1 = o2 && e1.Match(e2)
    | (ErrorExpression e1), (ErrorExpression e2) -> e1 = e2
    | _ -> false

  override x.Equals (y) =
    match x, y with
    | (ParenthesysExpression _), (:? ExpressionSyntax as p) -> match p with | ParenthesysExpression _ -> true | _ -> false
    | (LiteralExpression _), (:? ExpressionSyntax as l) -> match l with | LiteralExpression _ -> true | _ -> false
    | (IdentifierExpression _), (:? ExpressionSyntax as i) -> match i with | IdentifierExpression _ -> true | _ -> false
    | (AssignmentExpression _), (:? ExpressionSyntax as a) -> match a with | AssignmentExpression _ -> true | _ -> false
    | (BinaryExpression _), (:? ExpressionSyntax as b) -> match b with | BinaryExpression _ -> true | _ -> false
    | (UnaryExpression _), (:? ExpressionSyntax as u) -> match u with | UnaryExpression _ -> true | _ -> false
    | (ErrorExpression _), (:? ExpressionSyntax as e) -> match e with | ErrorExpression _ -> true | _ -> false
    | _ -> false

  override x.GetHashCode () = 0

  interface IComparable<ExpressionSyntax> with
    member this.CompareTo(other: ExpressionSyntax): int = 0

and SyntaxNode =
  | Expression    of ExpressionSyntax
  | Token         of Token
  | CST           of CST

  member n.Span =
    match n with
    | Expression e -> e.Span
    | Token t -> t.Span
    | CST t -> t.Span

  member this.Children = this |> function
    | Expression   e -> e.Children
    | Token        _ -> []
    | CST          t -> t.Children

  override this.ToString () = this |> function
    | Expression   e -> e.ToString ()
    | Token        t -> t.ToString ()
    | CST          _ -> "Concrete Syntax Tree"

  member t.Match o =
    match t, o with
    | Expression e1, Expression e2 -> e1.Match e2
    | t, o -> t = o

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
        | CST _ -> ConsoleColor.Yellow
    writer.WriteLine(node)
    let lastNode = node.Children |> List.tryLast
    node.Children |>
    List.iter (fun n -> n.PrettyPrint(writer, indent + (if not last then "│   " else if first then "" else "    "), false, n.Match(lastNode.Value)))

and CST (text: SourceText, root: ExpressionSyntax, endOfFileToken: Token, diagnostics: DiagnosticsBag) =
  let text = text
  let root = root
  let endOfFileToken = endOfFileToken
  let diagnostics = diagnostics

  member _.Text = text
  member _.Root = root
  member _.Span = root.Span
  member _.EndOfFileToken = endOfFileToken
  member _.Diagnostics = diagnostics.Diagnostics
  member _.Children = [Expression root; Token endOfFileToken]
  member this.ToExpression () =
      SyntaxNode.CST this