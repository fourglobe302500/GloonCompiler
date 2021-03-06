namespace Gloon.Text

[<Sealed>]
type Diagnostic (span: TextSpan, message: string) =
  let span = span
  let message = message

  member _.Span = span
  member _.Message = message

  override d.ToString () =
    d.Message