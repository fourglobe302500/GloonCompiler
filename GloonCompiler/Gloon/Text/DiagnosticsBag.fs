namespace Gloon.Text

open System.Collections.Generic
open System.Collections
open Gloon.Symbols

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

  member b.AddRange (diags: seq<Diagnostic>) =
    diags |> Seq.iter (fun diag -> b.Report diag.Span diag.Message)

  member b.Concat (bag: DiagnosticsBag) =
    b.AddRange bag
    b
  member b.Concat (diags: seq<Diagnostic>) =
    b.AddRange diags
    b

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
    b.Report (token.GetSpan()) $"Binary operator '{token.GetText()}' is not defined for types <{leftType}> and <{rightType}>"

  member b.ReportUndefinedVariable (token: IReportable) =
    b.Report (token.GetSpan()) $"Variable {token.GetText()} is not defined"

  member b.ReportVariableAlreadyDeclared (token: IReportable) =
    b.Report (token.GetSpan()) $"Variable {token.GetText()} is already declared"

  member b.ReportCannotConvert span fromType toType =
    b.Report span $"Cannot convert type <{fromType}> to type <{toType}>"

  member b.ReportNestedDeclaration span variable =
    b.Report span $"Cannot declare '{variable}' because value is a declaration statement"

  member b.ReportVariableIsReadOnly (token: IReportable) (variable: VariableSymbol) =
    b.Report (token.GetSpan()) $"Variable '{variable.Name}' is read only and cannot be changed"

  member _.Diagnostics : Diagnostic list = diagnostics |> Seq.toList