namespace Gloon.Compiler

module Lexer =

    open System
    open Gloon.Types

    let consume f current next constructor =
        while f (current ()) do
            next ()
        constructor ()

    let Lexer (s: string) =
        let mutable position = 0
        let diagnostics = ResizeArray ()

        let peek x =
            if position + x >= s.Length
            then char 0
            else s.[position + x]
        let inline current () = peek 0
        let inline lookAhead () = peek 1

        let inline text start = s.[start..position - 1]
        let inline newToken (start, tokenKind, value) = Token (start, text start, tokenKind, value)

        let inline move x = position <- position + x; position - x
        let inline next () = move 1 |> ignore

        let NextToken () : Token =
            let start = position
            match current () with
            | _ when position >= s.Length -> (move 1, TokenKind.EndOfFileToken, Obj null) |> newToken
            | n when Char.IsNumber n ->
                consume Char.IsNumber current next (fun () ->
                    let res = ref 0
                    if not (Int32.TryParse (text start, res))
                    then
                        diagnostics.Add ($"GLOON::COMPILER::LEXER Invalid Int32 '{text start}' at: {position}")
                        (start, NumberLiteralToken 0, Obj null) |> newToken
                    else (start, TokenKind.NumberLiteralToken res.Value, Int res.Value) |> newToken)
            | w when Char.IsWhiteSpace w ->
                consume Char.IsWhiteSpace current next (fun () ->
                    (start, TokenKind.WhiteSpaceToken (text start), Obj null) |> newToken)
            | '+' when lookAhead () = '+' -> (move 2, TokenKind.IncrementToken, Obj null) |> newToken
            | '+' -> (move 1, TokenKind.PlusToken, Obj null) |> newToken
            | '-' when lookAhead () = '-' -> (move 2, TokenKind.DecrementToken, Obj null) |> newToken
            | '-' -> (move 1, TokenKind.MinusToken, Obj null) |> newToken
            | '*' when lookAhead () = '*' -> (move 2, TokenKind.PowerToken, Obj null) |> newToken
            | '*' -> (move 1, TokenKind.StarToken, Obj null) |> newToken
            | '/' when lookAhead () = '/' -> (move 2, TokenKind.RootToken, Obj null) |> newToken
            | '/' -> (move 1, TokenKind.SlashToken, Obj null) |> newToken
            | '(' -> (move 1, TokenKind.OpenParenToken, Obj null) |> newToken
            | ')' -> (move 1, TokenKind.CloseParenToken, Obj null) |> newToken
            | '%' -> (move 1, TokenKind.ModulosToken, Obj null) |> newToken
            | l when Char.IsLetter l ->
                consume Char.IsLetter current next (fun () ->
                    (start, TokenKind.Identifier (text start), Obj null) |> newToken)
            | _ ->
                next()
                diagnostics.Add($"GLOON::COMPILER::LEXER Invalid Token '{text start}' at: {position - 1}.")
                (start, TokenKind.InvallidToken, Obj null) |> newToken
        let mutable Break = false
        let tokens = ResizeArray ()
        while not Break do
            let token = NextToken()
            tokens.Add (token)
            if token.Kind = TokenKind.EndOfFileToken
            then Break <- true
        (tokens.ToArray() |> Array.toList,diagnostics.ToArray() |> Array.toList)