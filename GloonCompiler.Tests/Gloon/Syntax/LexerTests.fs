namespace Gloon.Tests.Syntax

module Lexer =
  open Gloon.Syntax
  open Gloon.Syntax.Facts

  open Xunit
  open System.Linq
  open System.Collections.Generic

  let private GetTokens () = [
      (TokenKind.NumberLiteralToken 0, "0")
      (TokenKind.NumberLiteralToken 10, "10")
      (TokenKind.Identifier "var", "var")
      (TokenKind.Identifier "abc", "abc")
      (TokenKind.Identifier "a", "a")
      (TokenKind.BooleanLiteralToken true, "true")
      (TokenKind.BooleanLiteralToken false, "false")
      (TokenKind.IncrementToken, "++")
      (TokenKind.PlusToken, "+")
      (TokenKind.DecrementToken, "--")
      (TokenKind.MinusToken, "-")
      (TokenKind.PowerToken, "**")
      (TokenKind.StarToken, "*")
      (TokenKind.RootToken, "//")
      (TokenKind.SlashToken, "/")
      (TokenKind.PercentToken, "%")
      (TokenKind.BangToken, "!")
      (TokenKind.BangEqualsToken, "!=")
      (TokenKind.DoubleEqualsToken, "==")
      (TokenKind.EqualsToken, "=")
      (TokenKind.LessThanEqualsToken, "<=")
      (TokenKind.LessThanToken, "<")
      (TokenKind.GreaterThanEqualsToken, ">=")
      (TokenKind.GreaterThanToken, ">")
      (TokenKind.DoubleAmpersandToken, "&&")
      (TokenKind.DoublePipeToken, "||")
      (TokenKind.OpenParenToken, "(")
      (TokenKind.CloseParenToken, ")")
    ]

  let private GetWhiteSpaceTokens () = [
    (TokenKind.WhiteSpaceToken " "," ")
    (TokenKind.WhiteSpaceToken "  ","  ")
    (TokenKind.WhiteSpaceToken "\r","\r")
    (TokenKind.WhiteSpaceToken "\n","\n")
    (TokenKind.WhiteSpaceToken "\r\n","\r\n")
  ]

  let private GetTokensData () = seq {for (kind, text) in (GetTokens ()) @ (GetWhiteSpaceTokens ()) -> [|kind :> obj; text :> obj|]}

  [<TheoryAttribute>]
  [<MemberData(nameof GetTokensData)>]
  let ``Lexes Token`` (kind, text: string) =
    let tokens = SyntaxTree.Lex text
    Assert.Collection(tokens,
      System.Action<Token>(fun token ->
        Assert.Equal(kind, token.Kind)
        Assert.Equal(text, token.Text)),
      System.Action<Token>(fun token -> Assert.Equal(TokenKind.EndOfFileToken, token.Kind)))

  let private (|IsKeyword|_|) kind =
    if kind.ToString().EndsWith("Keyword") then Some kind else None

  let private RequiresSpacing = function
  | NumberLiteralToken _,NumberLiteralToken _ -> true
  | Identifier _, Identifier _ -> true
  | BooleanLiteralToken _, Identifier _ -> true
  | Identifier _, BooleanLiteralToken _ -> true
  | BooleanLiteralToken _, BooleanLiteralToken _ -> true
  | IsKeyword _, Identifier _ -> true
  | IsKeyword _, BooleanLiteralToken _ -> true
  | Identifier _, IsKeyword _ -> true
  | BooleanLiteralToken _, IsKeyword _ -> true
  | IsKeyword _, IsKeyword _ -> true
  | PlusToken, PlusToken -> true
  | PlusToken, IncrementToken -> true
  | MinusToken, MinusToken -> true
  | MinusToken, DecrementToken -> true
  | StarToken, StarToken -> true
  | StarToken, PowerToken -> true
  | SlashToken, SlashToken -> true
  | SlashToken, RootToken -> true
  | EqualsToken, EqualsToken -> true
  | EqualsToken, DoubleEqualsToken -> true
  | BangToken, EqualsToken -> true
  | BangToken, DoubleEqualsToken -> true
  | GreaterThanToken, EqualsToken -> true
  | GreaterThanToken, DoubleEqualsToken -> true
  | LessThanToken, EqualsToken -> true
  | LessThanToken, DoubleEqualsToken -> true
  | _, _ -> false

  let private GetTokenPairs () = seq {
    for (t1kind, t1text) in GetTokens () do
      for (t2kind, t2text) in GetTokens () -> t1kind, t1text, t2kind, t2text
  }

  let private GetTokenPairsData () =
    seq {
      for (t1kind, t1text, t2kind, t2text) in GetTokenPairs() do
        if not (RequiresSpacing (t1kind, t2kind))
        then yield [|t1kind :> obj; t1text :> obj; t2kind :> obj; t2text :> obj|]}

  [<Theory>]
  [<MemberData(nameof GetTokenPairsData)>]
  let ``Lexes Token Pairs`` (t1kind, t1text: string, t2kind, t2text) =
    let tokens = SyntaxTree.Lex (t1text + t2text)
    Assert.Collection(tokens,
      System.Action<Token>(fun token ->
        Assert.Equal(t1kind, token.Kind)
        Assert.Equal(t1text, token.Text)),
      System.Action<Token>(fun token ->
        Assert.Equal(t2kind, token.Kind)
        Assert.Equal(t2text, token.Text)),
      System.Action<Token>(fun token -> Assert.Equal(TokenKind.EndOfFileToken, token.Kind)))

  let private GetTokenPairsWithWhiteSpace () = seq {
    for (t1kind, t1text) in GetTokens () do
      for (t2kind, t2text) in GetTokens () do
        if RequiresSpacing(t1kind, t2kind) then
          for (wskind, wstext) in GetWhiteSpaceTokens () ->
            t1kind, t1text, wskind, wstext, t2kind, t2text
  }

  let private GetTokenPairsWithWhiteSpaceData () = seq {
    for (t1kind, t1text, wskind, wstext, t2kind, t2text) in GetTokenPairsWithWhiteSpace() -> [|
      t1kind :> obj; t1text :> obj; wskind :> obj; wstext :> obj; t2kind :> obj; t2text :> obj|]}

  [<Theory>]
  [<MemberData(nameof GetTokenPairsWithWhiteSpaceData)>]
  let ``Lexes Token Pairs With White Space`` (t1kind, t1text, wskind, wstext, t2kind, t2text: string) =
    let tokens = SyntaxTree.Lex (t1text + wstext + t2text)
    Assert.Collection(tokens,
      System.Action<Token>(fun token ->
        Assert.Equal(t1kind, token.Kind)
        Assert.Equal(t1text, token.Text)),
      System.Action<Token>(fun token ->
        Assert.Equal(wskind, token.Kind)
        Assert.Equal(wstext, token.Text)),
      System.Action<Token>(fun token ->
        Assert.Equal(t2kind, token.Kind)
        Assert.Equal(t2text, token.Text)),
      System.Action<Token>(fun token -> Assert.Equal(TokenKind.EndOfFileToken, token.Kind)))

  [<Fact>]
  let ``Lexer Test All Tokens`` () =
    let tokenKinds = TokenKind.GetAll()
    let testedTokens = GetTokens().Concat(GetWhiteSpaceTokens()) |> Seq.map (fun (k, t) -> k)
    let untestedTokens = new SortedSet<TokenKind>(tokenKinds)
    untestedTokens.ExceptWith(testedTokens)
    untestedTokens.Remove(EndOfFileToken) |> ignore
    untestedTokens.Remove(InvallidToken "$") |> ignore
    Assert.Empty(untestedTokens)