module Gloon.Compiler.Lexer

open System
open Gloon.Compiler.Types

let consume f current next constructor =
    while f (current ()) do
        next ()
    constructor ()

let Lexer (s: string) =
    let mutable position = 0
    let peek x =
        if position + x >= s.Length
        then char 0
        else s.[position + x]
    let inline current () = peek 0
    let inline lookAhead () = peek 1
    let text start = s.[start..position - 1]
    let newToken start tokenKind = Token (start, text start, tokenKind)
    let move x = position <- position + x; position - x
    let next () =
        position <- position + 1
    let nextToken () : Token =
        let start = position
        match current () with
        | _ when position >= s.Length -> newToken (move 1) TokenKind.EndOfFileToken
        | n when Char.IsNumber n ->
            consume Char.IsNumber current next (fun () ->
                let res = ref 0
                Int32.TryParse (text start, res) |> ignore
                newToken start (TokenKind.NumberLiteralToken res.Value))
        | w when Char.IsWhiteSpace w ->
            consume Char.IsWhiteSpace current next (fun () ->
                newToken start (TokenKind.WhiteSpaceToken (text start)))
        | '+' when lookAhead () = '+' -> newToken (move 2) TokenKind.IncrementToken
        | '+' -> newToken (move 1) TokenKind.PlusToken
        | '-' when lookAhead () = '-' -> newToken (move 2) TokenKind.DecrementToken
        | '-' -> newToken (move 1) TokenKind.MinusToken
        | '*' when lookAhead () = '*' -> newToken (move 2) TokenKind.PowerToken
        | '*' -> newToken (move 1) TokenKind.StartToken
        | '/' when lookAhead () = '/' -> newToken (move 2) TokenKind.RootToken
        | '/' -> newToken (move 1) TokenKind.SlashToken
        | '(' -> newToken (move 1) TokenKind.OpenParenToken
        | ')' -> newToken (move 1) TokenKind.CloseParenToken
        | '%' -> newToken (move 1) TokenKind.ModulosToken
        | l when Char.IsLetter l ->
            consume Char.IsLetter current next (fun () ->
                newToken start (TokenKind.Identifier (text start)))
        | _ ->
            next()
            newToken start TokenKind.InvallidToken
    let mutable Break = false
    let mutable tokens : Token list = []
    while not Break do
        let token = nextToken()
        if token.Kind <> TokenKind.EndOfFileToken
        then tokens <- tokens @ [token]
        else ((Break <- true)|>ignore)
    tokens