namespace Gloon.Compiler.Syntax

module Types =

  let private third (_,_,c) = c

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

  and CST (root: ExpressionSyntax, endOfFileToken: Token, diagnostics: string list) =
    let root = root
    let endOfFileToken = endOfFileToken
    let diagnostics = diagnostics

    member _.Root = root
    member _.EndOfFileToken = endOfFileToken
    member _.Diagnostics = diagnostics
    member _.Children = [Expression root; SyntaxNode.Token endOfFileToken]

  and Token =
    {
      Position: int
      Text: string
      Kind: TokenKind
      Value: obj
    } 

    override this.ToString () = this.Kind.ToString()

  and TokenKind =
    | NumberLiteralToken    of int
    | BooleanLiteralToken   of bool
    | Identifier            of string
    | WhiteSpaceToken       of string
    | EndOfFileToken
    | InvallidToken
    | IncrementToken
    | PlusToken
    | DecrementToken
    | MinusToken
    | PowerToken
    | StarToken
    | RootToken
    | SlashToken
    | ModulosToken
    | BangToken
    | DoubleAmpersandToken
    | DoublePipeToken
    | OpenParenToken
    | CloseParenToken

  and ExpressionSyntax =
    | ParenthesysExpression of OpenParen: Token       * Expr: ExpressionSyntax    * CloseParen: Token
    | LiteralExpression     of LiteralToken: Token
    | IdentifierExpression  of IdentifierToken: Token
    | BinaryExpression      of Left: ExpressionSyntax * Operator: Token           * Right: ExpressionSyntax
    | UnaryExpression       of Operator: Token        * Operand: ExpressionSyntax
    | ErrorExpression       of Error: Token

    member this.Children = this |> function
      | LiteralExpression                      n -> [SyntaxNode.Token n]
      | IdentifierExpression                   i -> [SyntaxNode.Token i]
      | ParenthesysExpression        (op, e, cp) -> [SyntaxNode.Token op; Expression e; SyntaxNode.Token cp]
      | BinaryExpression (left, operator, right) -> [Expression left; SyntaxNode.Token operator; Expression right]
      | UnaryExpression      (operator, operand) -> [SyntaxNode.Token operator; Expression operand]
      | ErrorExpression                        e -> [SyntaxNode.Token e]

    override this.ToString () = this |> function
      | LiteralExpression      _ -> "NumberExpression"
      | IdentifierExpression   _ -> "IdentifierExpression"
      | ParenthesysExpression  _ -> "ParenthesisedExpression"
      | BinaryExpression       _ -> "BinaryExpression"
      | UnaryExpression        _ -> "UnaryExpression"
      | ErrorExpression        _ -> "ErrorExpression"
