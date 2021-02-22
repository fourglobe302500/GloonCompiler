namespace Gloon

module Types =

    let third (_,_,c) = c

    type io = Int of int | Obj of obj

    type Node =
        | Expression    of Expression
        | Token         of Token
        | CST           of CST

        member this.getChildren () = this |> function
            | Node.Expression   e -> e.getChildren()
            | Node.Token        _ -> []
            | Node.CST          t -> t.getChildren()

        override this.ToString () = this |> function
            | Node.Expression   e -> e.ToString ()
            | Node.Token        t -> t.ToString ()
            | Node.CST          _ -> "AbstractSyntaxTree"

    and CST (root: Expression, endOfFileToken: Token, diagnostics: string list) =
        let root = root
        let endOfFileToken = endOfFileToken
        let diagnostics = diagnostics

        member _.Root = root
        member _.EndOfFileToken = endOfFileToken
        member _.Diagnostics = diagnostics
        member a.getChildren () = [Node.Expression root; Node.Token endOfFileToken]

    and Token (position_: int, text_: string, kind_: TokenKind, value: io) =
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

    and Expression =
        | ParenthesysExpression of OpenParen: Token   * Expr: Expression   * CloseParen: Token
        | LiteralExpression     of NumberToken: Token
        | BinaryExpression      of Left: Expression   * Operator: Token    * Right: Expression
        | UnaryExpression       of Operator: Token   * Operand: Expression
        | ErrorExpression       of Error: Token

        member this.getChildren () = this |> function
            | Expression.LiteralExpression                      n -> [Node.Token n                                                       ]
            | Expression.ParenthesysExpression        (op, e, cp) -> [Node.Token op;        Node.Expression e;      Node.Token cp        ]
            | Expression.BinaryExpression (left, operator, right) -> [Node.Expression left; Node.Token operator;    Node.Expression right]
            | Expression.UnaryExpression      (operator, operand) -> [Node.Token operator;  Node.Expression operand                      ]
            | Expression.ErrorExpression                        e -> [Node.Token e                                                       ]

        override this.ToString () = this |> function
            | Expression.LiteralExpression      _ -> "NumberExpression"
            | Expression.ParenthesysExpression  _ -> "ParenthesisedExpression"
            | Expression.BinaryExpression       _ -> "BinaryExpression"
            | Expression.UnaryExpression        _ -> "UnaryExpression"
            | Expression.ErrorExpression        _ -> "ErrorExpression"