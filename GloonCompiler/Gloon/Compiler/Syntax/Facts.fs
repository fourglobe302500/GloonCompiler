namespace Gloon.Compiler.Syntax

module Facts =
  open Gloon.Compiler.Syntax.Types

  type TokenKind with
    member this.BinaryOperatorPrecedence = this |> function
      | PowerToken ->             (5, true )
      | PercentToken ->           (5, true )
      | StarToken ->              (4, false)
      | SlashToken ->             (4, false)
      | MinusToken ->             (3, false)
      | PlusToken ->              (3, false)
      | DoubleAmpersandToken ->   (2, false)
      | DoublePipeToken ->        (1, false)
      | _ ->                      (0, false)

    member this.UnaryOperatorPrecedence = this |> function
      | PlusToken ->  1
      | MinusToken -> 1
      | BangToken ->  1
      | _ ->          0

