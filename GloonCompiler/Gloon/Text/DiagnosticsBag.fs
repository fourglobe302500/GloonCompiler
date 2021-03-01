namespace Gloon.Text

open System.Collections.Generic
open System.Collections

[<Interface>]
type IReportable =
  abstract GetSpan: unit -> TextSpan
  abstract GetKind: unit -> string
  abstract GetText: unit -> string

[<Sealed>]
type DiagnosticsBag (tag: string, bag: IEnumerable<Diagnostic>) =
  let tag = tag
  let diagnostics = ResizeArray (bag)

  interface IEnumerable<Diagnostic> with
    member _.GetEnumerator () = (diagnostics :> seq<Diagnostic>).GetEnumerator ()
  interface IEnumerable with
    member _.GetEnumerator () = (diagnostics:> IEnumerable<Diagnostic>).GetEnumerator () :> IEnumerator

  member b.AddRange (bag: DiagnosticsBag) =
    bag.Diagnostics |> List.iter (fun diag -> b.Report diag.Span diag.Message)

  new (tag) = DiagnosticsBag(tag, [])

  member private _.Tag = tag
  member private b.Report span message =
    diagnostics.Add(Diagnostic(span, $"ERROR::{b.Tag} {message}."))

  member b.ReportInvallidNumber start lenght value = b.Report (TextSpan (start, lenght)) $"Invallid Number Format '{value}'"
  member b.ReportInvallidCharacter start (token: char) = b.Report (TextSpan (start, 1)) $"Invalid Token '{token.ToString()}'"

  member b.ReportUnexpectedKind type_ (token: IReportable) kind =
    b.Report (token.GetSpan()) $"Unexpected {type_} <{token.GetKind()}> expexted <{kind}>"

  member b.ReportInvallidKind kind (token: IReportable) =
    b.Report (token.GetSpan()) $"Invallid {kind} <{token.GetKind()}>"

  member b.ReportUnaryNotDefined (token: IReportable) type_ =
    b.Report (token.GetSpan()) $"Unary operator '{token.GetText()}' is not defined for type <{type_}>"

  member b.ReportBinaryNotDefined (token: IReportable) leftType rightType =
    b.Report (token.GetSpan()) $"Binary operator '{token.GetText()}' is nor defined for types <{leftType}> and <{rightType}>"

  member _.Diagnostics : Diagnostic list = diagnostics |> Seq.toList