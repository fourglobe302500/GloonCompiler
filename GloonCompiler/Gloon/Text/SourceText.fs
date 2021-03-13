namespace Gloon.Text

open System.Collections.Immutable

[<Sealed>]
type SourceText private (text: string) as src =
  let text = text
  let lines = SourceText.ParseLines src text

  member _.Lines = lines
  member _.Length = text.Length
  member _.Item (index) = text.[index]
  member _.GetSlice (start, finish) =
    let start = defaultArg start 0
    let finish = defaultArg finish text.Length
    text.[start..finish]

  static member private ParseLines src text =
    let result = ImmutableArray.CreateBuilder()

    let GetLineBreakWidth (text: string) position =
      let c = text.[position]
      let l = if position + 1 >= text.Length then '\000' else text.[position + 1]
      if c = '\r' && l = '\n' then 2
      elif c = '\r' || c = '\n' then 1
      else 0

    let mutable position = 0
    let mutable lineStart = 0

    let inline AddLine lineBreakWidth =
      let line = TextLine(src, lineStart, position - lineStart, position - lineStart + lineBreakWidth)
      result.Add(line)

    while position < text.Length do
      let lineBreakWidth = GetLineBreakWidth text position
      if lineBreakWidth = 0 then position <- position + 1
      else
        AddLine lineBreakWidth
        position <- position + lineBreakWidth
        lineStart <- position

    if position > lineStart then AddLine 0

    result.ToImmutable()

  static member From text = SourceText(text)

  member _.GetLineIndex position =
    let mutable lower = 0
    let mutable upper = lines.Length - 1
    while lower < upper do
      let index = lower + (upper - lower) / 2
      let start = lines.[index].Start
      if position = start then upper <- index; lower <- index
      elif position < start then upper <- index - 1
      else lower <- index + 1
    upper

  override _.ToString () = text

  member _.ToString (start, length) = text.Substring(start, length)

  member _.ToString (span: TextSpan) = text.Substring(span.Start, span.Length)

and public TextLine (text: SourceText, start: int, length: int, lengthIncludingLineBeak: int) =
  let text = text
  let start = start
  let length = length
  let lengthIncludingLineBeak = lengthIncludingLineBeak

  member _.Text = text
  member _.Start = start
  member _.Length = length
  member _.End = start + length
  member _.LengthIncludingLineBeak = lengthIncludingLineBeak
  member _.Span = TextSpan(start, length)
  member _.SpanIncludingLineBreak = TextSpan(start, lengthIncludingLineBeak)

  override t.ToString () = text.ToString(t.Span)