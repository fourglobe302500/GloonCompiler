namespace Gloon.Tests.Syntax

module Facts =
  open Gloon.Syntax
  open Gloon.Syntax.Facts
  open Gloon.Syntax.Parsing

  open Xunit
  open System

  let GetSyntaxKindData () = TokenKind.GetAll() |> Seq.map (fun k -> [|k :> obj|])

  [<Theory>]
  [<MemberData(nameof(GetSyntaxKindData))>]
  let ``Facts Get Text Round Trips`` (kind: TokenKind) =
    let text = kind.Text
    if (text = null || kind = EndOfFileToken) then ()
    else
      let tokens = Lex(text)
      Assert.Collection(tokens,
        Action<Token>(fun t ->
          Assert.Equal(kind, t.Kind)
          Assert.Equal(text, t.Text)),
        Action<Token>(fun t ->
          Assert.Equal(TokenKind.EndOfFileToken, t.Kind)))