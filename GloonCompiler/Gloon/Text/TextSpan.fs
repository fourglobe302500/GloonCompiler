namespace Gloon.Text

[<StructAttribute>]
type TextSpan =
  val Start: int
  val Length: int
  new (start, lenght) = {Start = start; Length = lenght}
  member ts.End = ts.Start + ts.Length
  static member FromBounds (start, _end) = TextSpan(start, _end - start)
  static member (+) (x: TextSpan, y: TextSpan) = TextSpan.FromBounds(x.Start, y.End)