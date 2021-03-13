namespace Gloon.Text

[<StructAttribute>]
type TextSpan =
  val Start: int
  val Lenght: int
  new (start, lenght) = {Start = start; Lenght = lenght}
  member ts.End = ts.Start + ts.Lenght
  static member (+) (x: TextSpan, y: TextSpan) =
    TextSpan(x.Start, y.End - x.Start)