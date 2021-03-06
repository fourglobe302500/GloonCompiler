namespace Gloon.Syntax

open Gloon.Text

type SyntaxNode =
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

and Token =
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

and TokenKind =
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

and ExpressionSyntax =
  | ParenthesysExpression of OpenParen: Token * Expr: ExpressionSyntax * CloseParen: Token
  | LiteralExpression     of LiteralToken: Token
  | IdentifierExpression  of IdentifierToken: Token
  | AssignmentExpression   of IdentifierToken: Token * EqualToken: Token * Expr: ExpressionSyntax
  | BinaryExpression      of Left: ExpressionSyntax * Operator: Token * Right: ExpressionSyntax
  | UnaryExpression       of Operator: Token * Operand: ExpressionSyntax
  | ErrorExpression       of Error: Token

  member this.Children = this |> function
    | LiteralExpression                      n -> [Token n]
    | IdentifierExpression                   i -> [Token i]
    | AssignmentExpression         (i, e, expr) -> [Token i; Token e; Expression expr]
    | ParenthesysExpression        (op, e, cp) -> [Token op; Expression e; Token cp]
    | BinaryExpression (left, operator, right) -> [Expression left; Token operator; Expression right]
    | UnaryExpression      (operator, operand) -> [Token operator; Expression operand]
    | ErrorExpression                        e -> [Token e]

  override this.ToString () = this |> function
    | LiteralExpression      _ -> "Literal Expression"
    | IdentifierExpression   _ -> "Identifier Expression"
    | AssignmentExpression    _ -> "Assigment Expression"
    | ParenthesysExpression  _ -> "Parenthesised Expression"
    | BinaryExpression       _ -> "Binary Expression"
    | UnaryExpression        _ -> "Unary Expression"
    | ErrorExpression        _ -> "Error Expression"