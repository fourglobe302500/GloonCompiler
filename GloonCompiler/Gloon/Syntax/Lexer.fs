﻿namespace Gloon.Syntax

module internal Lexer =

  open System
  open Gloon.Syntax
  open Gloon.Text

  let private consume f current next constructor =
    while f (current ()) do
      next ()
    constructor ()

  let Lex (s: string) =
    let mutable position = 0
    let diagnostics = DiagnosticsBag ("GLOON::SYNTAX::LEXER")

    let peek x =
      if position + x >= s.Length
      then char 0
      else s.[position + x]
    let inline current () = peek 0
    let inline lookAhead () = peek 1

    let inline text start = s.[start..position - 1]
    let inline get amount = if position + amount >= s.Length then s.[position..s.Length-1] else s.[position..position+amount]

    let inline move x = position <- position + x; position - x
    let inline next () = move 1 |> ignore

    let getKeywordKind = function
    | "true" -> BooleanLiteralToken true
    | "false" -> BooleanLiteralToken false
    | a -> Identifier a

    let getKeywordValue str : obj = str |> function
      | "true" ->  true :> obj
      | "false" -> false :> obj
      | a -> a :> obj

    let nextToken () : Token =
      let start = position
      match current () with
      | _ when position >= s.Length -> {Position = move 1; Text = text start; Kind = EndOfFileToken; Value = null}
      | n when Char.IsNumber n ->
        consume Char.IsNumber current next (fun () ->
          let res = ref 0
          if not (Int32.TryParse (text start, res))
          then
            diagnostics.ReportInvallidNumber start (position - start) (text start)
            {Position = start; Text = text start; Kind = NumberLiteralToken 0; Value = null}
          else {Position = start; Text = text start; Kind = NumberLiteralToken res.Value; Value = res.Value})
      | w when Char.IsWhiteSpace w ->
        consume Char.IsWhiteSpace current next (fun () ->
          {Position = start; Text = text start; Kind = WhiteSpaceToken (text start); Value = null})
      | '+' when lookAhead () = '+' -> {Position = move 2; Text = text start; Kind = IncrementToken; Value = null}
      | '+' -> {Position = move 1; Text = text start; Kind = PlusToken; Value = null}
      | '-' when lookAhead () = '-' -> {Position = move 2; Text = text start; Kind = DecrementToken; Value = null}
      | '-' -> {Position = move 1; Text = text start; Kind = MinusToken; Value = null}
      | '*' when lookAhead () = '*' -> {Position = move 2; Text = text start; Kind = PowerToken; Value = null}
      | '*' -> {Position = move 1; Text = text start; Kind = StarToken; Value = null}
      | '/' when lookAhead () = '/' -> {Position = move 2; Text = text start; Kind = RootToken; Value = null}
      | '/' -> {Position = move 1; Text = text start; Kind = SlashToken; Value = null}
      | '(' -> {Position = move 1; Text = text start; Kind = OpenParenToken; Value = null}
      | ')' -> {Position = move 1; Text = text start; Kind = CloseParenToken; Value = null}
      | '%' -> {Position = move 1; Text = text start; Kind = PercentToken; Value = null}
      | '&' when lookAhead () = '&' -> {Position = move 2; Text = text start; Kind = DoubleAmpersandToken; Value = null}
      | '|' when lookAhead () = '|' -> {Position = move 2; Text = text start; Kind = DoublePipeToken; Value = null}
      | '=' when lookAhead () = '=' -> {Position = move 2; Text = text start; Kind = DoubleEqualsToken; Value = null}
      | '=' -> {Position = move 1; Text = text start; Kind = EqualsToken; Value = null}
      | '!' when lookAhead () = '=' -> {Position = move 2; Text = text start; Kind = BangEqualsToken; Value = null}
      | '!' -> {Position = move 1; Text = text start; Kind = BangToken; Value = null}
      | '<' when lookAhead () = '=' -> {Position = move 2; Text = text start; Kind = LessThanEqualsToken; Value = null}
      | '<' -> {Position = move 1; Text = text start; Kind = LessThanToken; Value = null}
      | '>' when lookAhead () = '=' -> {Position = move 2; Text = text start; Kind = GreaterThanEqualsToken; Value = null}
      | '>' -> {Position = move 1; Text = text start; Kind = GreaterThanToken; Value = null}
      | l when Char.IsLetter l ->
        consume Char.IsLetter current next (fun () ->
          {Position = start; Text = text start; Kind = getKeywordKind (text start); Value = getKeywordValue (text start)})
      | _ ->
        diagnostics.ReportInvallidCharacter start (current ())
        {Position = move 1; Text = (text start); Kind = InvallidToken (text start); Value = null}
    let mutable Break = false
    let tokens = ResizeArray ()
    while not Break do
      let token = nextToken()
      tokens.Add (token)
      if token.Kind = EndOfFileToken
      then Break <- true
    tokens |> Seq.toArray , diagnostics