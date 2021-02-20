module Gloon.Compiler.Types

let third (_,_,c) = c

type Node =
    | Expression    of Expression
    | Token         of Token

    member this.getChildren () =
        match this with
        | Expression e -> e.getChildren()
        | Token t -> []

    override this.ToString () =
        match this with
        | Expression e -> e.ToString ()
        | Token t -> t.ToString ()

and Token (_position: int, _text: string, _kind: TokenKind) =
    let position = _position
    let text = _text
    let kind = _kind

    override _.ToString () = kind.ToString()
    member _.Position = position
    member _.Text = text
    member _.Kind = kind

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
    | StartToken
    | RootToken
    | SlashToken
    | ModulosToken
    | OpenParenToken
    | CloseParenToken

and Expression =
    | NumberExpression  of NumberToken: Token
    | BinaryExpression  of Left: Expression * Operator: Token * Right: Expression
    | UnaryExpression   of Operator: Token * Operand: Expression
    | ErrorExpression   of Error: Token

    member this.getChildren () =
        match this with
        | NumberExpression n -> [Node.Token n]
        | BinaryExpression (left, operator, right) -> [Node.Expression left; Node.Token operator; Node.Expression right]
        | UnaryExpression  (operator, operand) -> [Node.Token operator; Node.Expression operand]
        | ErrorExpression  e -> [Node.Token e]

    override this.ToString () =
        match this with
        | NumberExpression n -> "NumberExpression"
        | BinaryExpression (left, operator, right) -> "BinaryExpression"
        | UnaryExpression  (operator, operand) -> "UnaryExpression"
        | ErrorExpression  e -> "ErrorExpression"