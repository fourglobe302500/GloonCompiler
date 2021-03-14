namespace Gloon.Compiler

open System.Collections.Generic
open Gloon.Symbols
open Gloon.Text
open Gloon.Syntax
open Gloon.Binding.Binder
open Gloon.Evaluation.Evaluator
open System.Collections.Immutable

type EvaluationResult =
  {
    Diagnostics: ImmutableArray<Diagnostic>
    Value: obj
  }

[<Sealed>]
type Compilation (tree: SyntaxTree) =
  let tree = tree
  member _.Tree = tree
  member c.Evaluate (variables: Dictionary<VariableSymbol, obj>) =
    let (expression,diagnostics,_) = bind c.Tree variables
    if diagnostics.Length > 0
    then {Diagnostics = diagnostics.ToImmutableArray(); Value = null}
    else {Diagnostics = ImmutableArray.Empty; Value = Evaluate expression variables}