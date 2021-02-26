namespace Gloon.Compiler.Syntax

module Types =

    let private third (_,_,c) = c

    type SyntaxNode =
        | Expression    of ExpressionSyntax
        | Token         of Token
        | CST           of CST

        member this.getChildren () = this |> function
            | SyntaxNode.Expression   e -> e.getChildren()
            | SyntaxNode.Token        _ -> []
            | SyntaxNode.CST          t -> t.getChildren()

        override this.ToString () = this |> function
            | SyntaxNode.Expression   e -> e.ToString ()
            | SyntaxNode.Token        t -> t.ToString ()
            | SyntaxNode.CST          _ -> "AbstractSyntaxTree"

    and CST (root: ExpressionSyntax, endOfFileToken: Token, diagnostics: string list) =
        member val root = root
        member val endOfFileToken = endOfFileToken
        member val diagnostics = diagnostics

        member _.Root = root
        member _.EndOfFileToken = endOfFileToken
        member _.Diagnostics = diagnostics
        member a.getChildren () = [SyntaxNode.Expression root; SyntaxNode.Token endOfFileToken]

    and Token (position_: int, text_: string, kind_: TokenKind, value: obj) =
        let position = position_
        let text = text_
        let kind = kind_
        let mutable value = value

        override _.ToString () = kind.ToString()
        member _.Position = position
        member _.Text = text
        member _.Kind = kind
        member _.Value = value

    and TokenKind =
        | NumberLiteralToken    of int
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
        | OpenParenToken
        | CloseParenToken

    and ExpressionSyntax =
        | ParenthesysExpression of OpenParen: Token       * Expr: ExpressionSyntax    * CloseParen: Token
        | LiteralExpression     of LiteralToken: Token
        | BinaryExpression      of Left: ExpressionSyntax * Operator: Token           * Right: ExpressionSyntax
        | UnaryExpression       of Operator: Token        * Operand: ExpressionSyntax
        | ErrorExpression       of Error: Token

        member this.getChildren () = this |> function
            | ExpressionSyntax.LiteralExpression                      n -> [SyntaxNode.Token n                                                                   ]
            | ExpressionSyntax.ParenthesysExpression        (op, e, cp) -> [SyntaxNode.Token op;        SyntaxNode.Expression e;      SyntaxNode.Token cp        ]
            | ExpressionSyntax.BinaryExpression (left, operator, right) -> [SyntaxNode.Expression left; SyntaxNode.Token operator;    SyntaxNode.Expression right]
            | ExpressionSyntax.UnaryExpression      (operator, operand) -> [SyntaxNode.Token operator;  SyntaxNode.Expression operand                            ]
            | ExpressionSyntax.ErrorExpression                        e -> [SyntaxNode.Token e                                                                   ]

        override this.ToString () = this |> function
            | ExpressionSyntax.LiteralExpression      _ -> "NumberExpression"
            | ExpressionSyntax.ParenthesysExpression  _ -> "ParenthesisedExpression"
            | ExpressionSyntax.BinaryExpression       _ -> "BinaryExpression"
            | ExpressionSyntax.UnaryExpression        _ -> "UnaryExpression"
            | ExpressionSyntax.ErrorExpression        _ -> "ErrorExpression"