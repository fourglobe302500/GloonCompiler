module Gloon.Syntax.Parsing

open Gloon.Text
open Gloon.Syntax
open Gloon.Syntax.Lexer
open Gloon.Syntax.Parser

let Parse (text: SourceText) =
  Parse text

let ParseString (text: string) =
  let src = SourceText.From(text)
  Parse(src)

let Lex text =
  fst (Lex text)

let LexString text =
  let src = SourceText.From(text)
  Lex src