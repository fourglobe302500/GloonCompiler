namespace Gloon.Syntax

module Facts =
  open Gloon.Syntax

  type TokenKind with
    member this.BinaryOperatorPrecedence = this |> function
      | PowerToken ->             7
      | PercentToken ->           7
      | StarToken ->              6
      | SlashToken ->             6
      | MinusToken ->             5
      | PlusToken ->              5
      | LessThanEqualsToken ->    4
      | LessThanToken ->          4
      | GreaterThanEqualsToken -> 4
      | GreaterThanToken ->       4
      | DoubleEqualsToken ->      3
      | BangEqualsToken ->        3
      | DoubleAmpersandToken ->   2
      | DoublePipeToken ->        1
      | _ ->                      0

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
      | EndOfFileToken -> "\000"
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

    static member GetAll () = seq {
      NumberLiteralToken 0
      BooleanLiteralToken false
      Identifier "a"
      WhiteSpaceToken " "
      InvallidToken "$"
      EndOfFileToken
      PlusToken
      IncrementToken
      MinusToken
      DecrementToken
      StarToken
      PowerToken
      SlashToken
      RootToken
      PercentToken
      BangToken
      BangEqualsToken
      EqualsToken
      DoubleEqualsToken
      GreaterThanEqualsToken
      GreaterThanToken
      LessThanEqualsToken
      LessThanToken
      DoubleAmpersandToken
      DoublePipeToken
      OpenParenToken
      CloseParenToken
    }
