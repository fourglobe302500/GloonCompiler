namespace Gloon.Text

[<StructAttribute>]
type TextSpan =
  val Start: int
  val Lenght: int
  new (start, lenght) = {Start = start; Lenght = lenght}
  member ts.End = ts.Start + ts.Lenght