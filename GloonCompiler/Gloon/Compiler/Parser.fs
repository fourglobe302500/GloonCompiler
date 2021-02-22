namespace Gloon.Compiler

module Parser =

    open Gloon.Types

    let Parser (tokens_: Token list, diagnostics_: string list) =
        let tokens = tokens_ |> Seq.filter (fun t ->
            match t.Kind with
            | TokenKind.WhiteSpaceToken _ -> false
            | _ -> true) |> List.ofSeq
        let diagnostics = ResizeArray(diagnostics_)
        let mutable position = 0

        let peek x =
            if position + x >= tokens.Length
            then tokens.[tokens.Length - 1]
            else tokens.[position + x]
        let inline current () = peek 0
        let inline lookAhead () = peek 1

        let Next () =
            let current = current ()
            position <- position + 1
            current

        let inline Match (kind: TokenKind) =
            match (current ()).Kind with
            | k when k = kind -> Next()
            | _ ->
                diagnostics.Add($"GLOON::COMPILER::PARSER Unnexpected Token <{(current()).Kind}> expexted <{kind}> at: {(current ()).Position}.")
                Token ((Next ()).Position,"",kind, (Obj null))

        let rec ParsePrimaryExpression () =
            match (current ()).Kind with
            | TokenKind.NumberLiteralToken n -> Expression.LiteralExpression (Match (TokenKind.NumberLiteralToken n))
            | TokenKind.OpenParenToken -> Expression.ParenthesysExpression (Next (), ParseTerm (), Match TokenKind.CloseParenToken)
            | _ ->
                diagnostics.Add($"GLOON::COMPILER::PARSER Invallid Token <{(current ()).Kind}> at: {(current ()).Position}.")
                Expression.ErrorExpression (Next())

        and ParseExponential () =
            let mutable left = ParsePrimaryExpression ()
            while (current ()).Kind = TokenKind.PowerToken || (current ()).Kind = TokenKind.ModulosToken do
                left <- Expression.BinaryExpression (left, Next (), ParsePrimaryExpression ())
            left

        and ParseFactor () =
            let mutable left = ParseExponential ()
            while (current ()).Kind = TokenKind.StarToken || (current ()).Kind = TokenKind.SlashToken do
                left <- Expression.BinaryExpression (left, Next (), ParseExponential ())
            left

        and ParseTerm () =
            let mutable left = ParseFactor ()
            while (current ()).Kind = TokenKind.PlusToken || (current ()).Kind = TokenKind.MinusToken do
                left <- Expression.BinaryExpression (left, Next (), ParseFactor ())
            left

        AST (ParseTerm(), Match(TokenKind.EndOfFileToken), diagnostics.ToArray() |> Array.toList)