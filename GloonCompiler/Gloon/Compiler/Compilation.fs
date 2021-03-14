namespace Gloon.Compiler

open Gloon.Symbols
open Gloon.Text
open Gloon.Syntax
open Gloon.Binding
open Gloon.Evaluation.Evaluator

open System.Linq
open System.Collections.Generic
open System.Collections.Immutable
open System.Threading

type EvaluationResult =
  {
    Diagnostics: ImmutableArray<Diagnostic>
    Value: obj
  }

[<Sealed>]
type Compilation private (previous: Compilation option, tree: SyntaxTree) =
  let tree = tree
  let previous = previous
  let mutable globalScope = None
  member _.Tree = tree
  member internal _.GlobalScope =
    match globalScope with
    | None ->
      let scope = Binder.BindGlobalScope (match previous with | Some comp -> Some comp.GlobalScope | None -> None) (tree.Root)
      Interlocked.CompareExchange(&globalScope, Some scope, None) |> ignore
      scope
    | Some scope -> scope

  new (tree) = Compilation(None, tree)

  member t.ContinueWith syntaxTree =
    Compilation(Some t, syntaxTree)

  member c.Evaluate (variables: Dictionary<VariableSymbol, obj>) =
    let statement = c.GlobalScope.Statement
    let diagnostics = c.GlobalScope.Diagnostics.Concat(tree.Diagnostics).ToImmutableArray()
    if diagnostics.Length > 0
    then {Diagnostics = diagnostics; Value = null}
    else {Diagnostics = ImmutableArray.Empty; Value = Evaluate statement variables}