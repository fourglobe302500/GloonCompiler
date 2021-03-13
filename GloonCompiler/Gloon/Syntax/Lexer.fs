namespace Gloon.Syntax

module internal Lexer =

  open System
  open Gloon.Text
  open Gloon.Syntax
  open Gloon.Syntax.Facts

  let Lex (text: SourceText) =
    let mutable position = 0
    let diagnostics = DiagnosticsBag ("GLOON::SYNTAX::LEXER")

    let peek x =
      if position + x >= text.Length
      then char 0
      else text.[position + x]
    let inline current () = peek 0
    let inline lookAhead () = peek 1

    let inline get amount = if position + amount >= text.Length then text.[position..text.Length-1] else text.[position..position+amount]
    let inline GetText start = text.[start..position - 1]

    let inline move x = position <- position + x
    let inline next () = move 1

    let consume f validator =
      while f (current ()) do
        next ()
      validator ()

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
      let mutable kind = EndOfFileToken
      let mutable value = null
      match current () with
      | _ when position >= text.Length -> next ()
      | '+' when lookAhead () = '+' ->  move 2; kind <- IncrementToken
      | '+' ->                          move 1; kind <- PlusToken
      | '-' when lookAhead () = '-' ->  move 2; kind <- DecrementToken
      | '-' ->                          move 1; kind <- MinusToken
      | '*' when lookAhead () = '*' ->  move 2; kind <- PowerToken
      | '*' ->                          move 1; kind <- StarToken
      | '/' when lookAhead () = '/' ->  move 2; kind <- RootToken
      | '/' ->                          move 1; kind <- SlashToken
      | '%' ->                          move 1; kind <- PercentToken
      | '&' when lookAhead () = '&' ->  move 2; kind <- DoubleAmpersandToken
      | '|' when lookAhead () = '|' ->  move 2; kind <- DoublePipeToken
      | '=' when lookAhead () = '=' ->  move 2; kind <- DoubleEqualsToken
      | '=' ->                          move 1; kind <- EqualsToken
      | '!' when lookAhead () = '=' ->  move 2; kind <- BangEqualsToken
      | '!' ->                          move 1; kind <- BangToken
      | '<' when lookAhead () = '=' ->  move 2; kind <- LessThanEqualsToken
      | '<' ->                          move 1; kind <- LessThanToken
      | '>' when lookAhead () = '=' ->  move 2; kind <- GreaterThanEqualsToken
      | '>' ->                          move 1; kind <- GreaterThanToken
      | '(' ->                          move 1; kind <- OpenParenToken
      | ')' ->                          move 1; kind <- CloseParenToken
      | n when Char.IsNumber n -> consume Char.IsNumber <| fun () ->
        let res = ref 0
        if not (Int32.TryParse (GetText start, res))
        then diagnostics.ReportInvallidNumber start (position - start) (GetText start); kind <- NumberLiteralToken 0
        else kind <- NumberLiteralToken res.Value; value <- res.Value :> obj
      | l when Char.IsLetter l -> consume Char.IsLetter <| fun () -> kind <- getKeywordKind (GetText start); value <- getKeywordValue (GetText start)
      | w when Char.IsWhiteSpace w -> consume Char.IsWhiteSpace <| fun () -> kind <- WhiteSpaceToken (GetText start)
      | _ -> diagnostics.ReportInvallidCharacter start (current ()); move 1; kind <- InvallidToken (GetText start)
      { Position = start; Text = kind.Text; Kind = kind; Value = value}
    let mutable Break = false
    let tokens = ResizeArray ()
    while not Break do
      let token = nextToken()
      tokens.Add (token)
      if token.Kind = EndOfFileToken
      then Break <- true
    tokens |> Seq.toArray , diagnostics