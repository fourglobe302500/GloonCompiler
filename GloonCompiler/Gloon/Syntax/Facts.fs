namespace Gloon.Syntax

module Facts =
  open Gloon.Syntax

  type TokenKind with
    member this.BinaryOperatorPrecedence = this |> function
      | PowerToken ->             (7, true )
      | PercentToken ->           (7, true )
      | StarToken ->              (6, false)
      | SlashToken ->             (6, false)
      | MinusToken ->             (5, false)
      | PlusToken ->              (5, false)
      | LessThanEqualsToken ->    (4, false)
      | LessThanToken ->          (4, false)
      | GreaterThanEqualsToken -> (4, false)
      | GreaterThanToken ->       (4, false)
      | DoubleEqualsToken ->      (3, false)
      | BangEqualsToken ->        (3, false)
      | DoubleAmpersandToken ->   (2, false)
      | DoublePipeToken ->        (1, false)
      | _ ->                      (0, false)

    member this.UnaryOperatorPrecedence = this |> function
      | PlusToken ->  1
      | MinusToken -> 1
      | BangToken ->  1
      | _ ->          0

    member t.Text = t |> function
      | NumberLiteralToken n -> n.ToString()
      | BooleanLiteralToken true -> "true"
      | BooleanLiteralToken false -> "false"
      | Identifier i -> i
      | WhiteSpaceToken w -> w
      | InvallidToken i -> i
      | EndOfFileToken -> "\0"
      | IncrementToken -> "++"
      | PlusToken -> "+"
      | DecrementToken -> "--"
      | MinusToken -> "-"
      | PowerToken -> "**"
      | StarToken -> "*"
      | RootToken -> "//"
      | SlashToken -> "/"
      | PercentToken -> "%"
      | BangToken -> "!"
      | BangEqualsToken -> "!="
      | DoubleEqualsToken -> "=="
      | EqualsToken -> "="
      | LessThanEqualsToken -> "<="
      | LessThanToken -> "<"
      | GreaterThanEqualsToken -> ">="
      | GreaterThanToken -> ">"
      | DoubleAmpersandToken -> "&&"
      | DoublePipeToken -> "||"
      | OpenParenToken -> "("
      | CloseParenToken -> ")"