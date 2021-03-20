namespace Gloon.Tests.Evaluation

open Gloon.Text

open System
open System.IO
open System.Text
open System.Collections.Generic
open System.Collections.Immutable

[<Sealed>]
type internal AnnotatedText (text: string, spans: ImmutableArray<TextSpan>) =
  member _.Text = text
  member _.Spans = spans

  static member Parse text =
    let text = AnnotatedText.Unindent text
    let textBuilder = StringBuilder()
    let spanBuilder = ImmutableArray.CreateBuilder()
    let starts = Stack()

    let mutable position = 0

    text
    |> String.iter (
      fun c ->
        match c with
        | '[' -> starts.Push(position)
        | ']' ->
          if (starts.Count = 0) then raise (new ArgumentException("Too many ']' in input text", nameof(text)))
          spanBuilder.Add(TextSpan.FromBounds(starts.Pop(), position))
        | c ->
          textBuilder.Append(c) |> ignore
          position <- position + 1
    )

    if (starts.Count <> 0) then raise (new ArgumentException("Missing ']' in input text", nameof(text)))

    AnnotatedText(textBuilder.ToString(), spanBuilder.ToImmutable())

  static member private Unindent text =
    let lines = ResizeArray()
    using (new StringReader(text)) (fun stringReader ->
      let mutable line = ""
      while line <> null do
        lines.Add(line)
        line <- stringReader.ReadLine()
    )
    let minIndent =
      lines
      |> Seq.filter (fun line -> line.Trim().Length > 0)
      |> Seq.fold (fun mi (l: string) -> min (l.Length - l.TrimStart().Length)  mi) (Int32.MaxValue)
    lines
    |> Seq.filter (fun l -> l.Trim().Length > 0)
    |> Seq.map (fun l -> l.Substring(minIndent))
    |> fun arr -> String.Join(Environment.NewLine, arr)