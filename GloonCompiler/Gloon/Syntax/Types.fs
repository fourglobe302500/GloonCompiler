namespace Gloon.Syntax

open Gloon.Text
open System

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
    member t.GetSpan () =
      TextSpan(t.Position, t.Text.Length)
    member t.GetKind () = t.Kind.ToString()
    member t.GetText () = t.Text

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

  member this.Children = this |> function
    | Expression   e -> e.Children
    | Token        _ -> []
    | CST          t -> t.Children

  override this.ToString () = this |> function
    | Expression   e -> e.ToString ()
    | Token        t -> t.ToString ()
    | CST          _ -> "Concrete Syntax Tree"

and CST (root: ExpressionSyntax, endOfFileToken: Token, diagnostics: DiagnosticsBag) =
  let root = root
  let endOfFileToken = endOfFileToken
  let diagnostics = diagnostics

  member _.Root = root
  member _.EndOfFileToken = endOfFileToken
  member _.Diagnostics = diagnostics.Diagnostics
  member _.Children = [Expression root; Token endOfFileToken]
  member this.ToExpression () =
      SyntaxNode.CST this