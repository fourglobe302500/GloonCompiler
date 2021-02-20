module Gloon.Compiler.Parser

open Gloon.Compiler.Types

let Parser (tokens_: Token list) =
    let tokens = tokens_ |> Seq.filter (fun t ->
        match t.Kind with
        | TokenKind.WhiteSpaceToken w -> false
        | TokenKind.EndOfFileToken -> false
        | _ -> true) |> List.ofSeq
    let mutable position = 0
    let peek x = if position + x >= tokens.Length then tokens.[tokens.Length - 1] else tokens.[position + x]
    let inline current () = peek 0
    let inline lookAhead () = peek 1
    let nextToken () =
        let current = current ()
        position <- position + 1
        current
    let matchToken (kind: TokenKind) =
        match (current ()).Kind with
        | k when k = kind -> nextToken()
        | _ -> Token ((current ()).Position,"",kind)
    let rec parse () =
        let mutable left = parsePrimaryExpression ()
        while (current ()).Kind = TokenKind.PlusToken || (current ()).Kind = TokenKind.MinusToken do
            left <- Expression.BinaryExpression (left,nextToken (), parsePrimaryExpression ())
        left
    and parsePrimaryExpression () =
        match (current ()).Kind with
        | TokenKind.NumberLiteralToken n -> Expression.NumberExpression (matchToken (TokenKind.NumberLiteralToken n))
        | _ -> Expression.ErrorExpression (matchToken (TokenKind.InvallidToken))
    parse ()