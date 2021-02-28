namespace Gloon.Compiler.Syntax

module Facts =
    open Gloon.Compiler.Syntax.Types

    type TokenKind with
        member this.BinaryOperatorPrecedence = this |> function
            | TokenKind.PowerToken -> (3, true)
            | TokenKind.ModulosToken -> (3, true)
            | TokenKind.StarToken -> (2, false)
            | TokenKind.SlashToken -> (2, false)
            | TokenKind.MinusToken -> (1, false)
            | TokenKind.PlusToken -> (1, false)
            | _ -> (0, false)

        member this.UnaryOperatorPrecedence = this |> function
            | TokenKind.PlusToken -> 1
            | TokenKind.MinusToken -> 1
            | _ -> 0