namespace Gloon.Tests.Text

module SourceTextTests =
  open Gloon.Text

  open Xunit

  [<Theory>]
  [<InlineData(".", 1)>]
  [<InlineData(".\r\n", 2)>]
  [<InlineData(".\r\n\r\n", 3)>]
  let ``Source Text Includes Last Line`` (text, expectedLineCount) =
    let src = SourceText.From text
    Assert.Equal(expectedLineCount, src.Lines.Length)