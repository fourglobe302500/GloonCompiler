namespace Gloon.Text

[<StructAttribute>]
type TextSpan =
  val Start: int
  val Length: int
  new (start, lenght) = {Start = start; Length = lenght}
  member ts.End = ts.Start + ts.Length
  static member (+) (x: TextSpan, y: TextSpan) =
    TextSpan(x.Start, y.End - x.Start)