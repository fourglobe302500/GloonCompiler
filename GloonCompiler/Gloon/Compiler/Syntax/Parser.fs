namespace Gloon.Compiler.Syntax

module Parser =

    open Gloon.Types
    open Gloon.Compiler.Syntax.Facts

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
            | TokenKind.OpenParenToken -> Expression.ParenthesysExpression (Next (), ParseExpression 0, Match TokenKind.CloseParenToken)
            | _ ->
                diagnostics.Add($"GLOON::COMPILER::PARSER Invallid Token <{(current ()).Kind}> at: {(current ()).Position}.")
                Expression.ErrorExpression (Next())

        and ParseExpression parentPrecedence =
            let mutable left = ParsePrimaryExpression ()
            let mutable Break = false
            while not Break do
                let (precedence, right) = (current ()).Kind.GetBinaryOperatorPrecedence
                if precedence = 0 || (if right then precedence < parentPrecedence else precedence <= parentPrecedence)
                then Break <- true
                else left <- Expression.BinaryExpression (left, Next (), ParseExpression precedence)
            left

        AST (ParseExpression 0, Match(TokenKind.EndOfFileToken), diagnostics.ToArray() |> Array.toList)