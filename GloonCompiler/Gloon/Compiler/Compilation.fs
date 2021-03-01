namespace Gloon.Compiler

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
  member c.Evaluate () =
    let (expression,diagnostics,_) = bind c.Tree
    if diagnostics.Length > 0
    then {Diagnostics = diagnostics; Value = null}
    else {Diagnostics = diagnostics; Value = expression |> evaluate}