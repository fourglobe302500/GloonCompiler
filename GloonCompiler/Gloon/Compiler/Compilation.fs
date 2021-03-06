namespace Gloon.Compiler

open System.Collections.Generic
open Gloon.Text
open Gloon.Syntax
open Gloon.Binding.Binder
open Gloon.Evaluation.Evaluator

type EvaluationResult =
  {
    Diagnostics: seq<Diagnostic>
    Value: obj
  }

[<SealedAttribute>]
type Compilation (tree: CST) =
  let tree = tree
  member _.Tree = tree
  member c.Evaluate (variables: Dictionary<string, obj>) =
    let (expression,diagnostics,_) = bind c.Tree variables
    if diagnostics.Length > 0
    then {Diagnostics = diagnostics; Value = null}
    else {Diagnostics = diagnostics; Value = Evaluate expression variables}