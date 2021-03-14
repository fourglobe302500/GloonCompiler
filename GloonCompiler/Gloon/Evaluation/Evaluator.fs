namespace Gloon.Evaluation

module internal Evaluator =
  open System
  open System.Collections.Generic
  open Gloon.Symbols
  open Gloon.Binding.BoundTypes

  let rec internal Evaluate (node: BoundExpression) (variables: Dictionary<VariableSymbol, obj>) =
    let rec evaluate =
      function
      | LiteralExpression l -> l
      | UnaryExpression (o, e) -> EvaluateUnaryExpression (o) (evaluate e)
      | BinaryExpression (left,o,right) -> EvaluateBinaryExpression(evaluate left, o,evaluate right)
      | VariableExpression i -> variables.GetValueOrDefault(i)
      | AssignmentExpression (i, e) -> EvaluateAssigmentExpression (i, evaluate e)
      | _ -> raise (Exception "GLOON::EVALUATION::EVALUATOR Invaliid Expression")

    and EvaluateUnaryExpression o = function
    | :? int as op ->
      match o.Kind with
      | Identity -> upcast op
      | Negation -> upcast -op
      //| _ -> raise (Exception "GLOON::EVALUATION::EVALUATOR Invallid Unary Operation")
    | :? bool as op ->
      match o.Kind with
      | Negation -> upcast not op
      | _ -> raise (Exception "GLOON::EVALUATION::EVALUATOR Invallid Unary Operation")
    | _ -> null

    and EvaluateBinaryExpression (left, o, right) =
      match (left, right) with
      | (:? int as l), (:? int as r) ->
        match o.Kind with
        | Addition -> upcast (l + r)
        | Subtraction -> upcast (l - r)
        | Multiplication -> upcast (l * r)
        | Division -> upcast int (Math.Round (float (l / r)))
        | Modulos -> upcast (l % r)
        | Power -> upcast int (Math.Round ((float l) ** (float r)))
        | GreaterThan -> upcast (l > r)
        | GreaterThanOrEquals -> upcast (l >= r)
        | LesserThan -> upcast (l < r)
        | LesserThanOrEquals -> upcast (l <= r)
        | Equals -> upcast (l = r)
        | NotEquals -> upcast (l <> r)
        | _ -> raise (Exception "GLOON::EVALUATION::EVALUATOR Invallid Binary Operation")
      | (:? bool as l), (:? bool as r) ->
        match o.Kind with
        | LogicalAnd -> upcast (l && r)
        | LogicalOr -> upcast (l || r)
        | Equals -> upcast (l = r)
        | NotEquals -> upcast (l <> r)
        | _ -> raise (Exception "GLOON::EVALUATION::EVALUATOR Invallid Binary Operation")
      | null, null -> raise (Exception "GLOON::EVALUATION::EVALUATOR Cannot cast left and right value null to any know type")
      | null, _ -> raise (Exception "GLOON::EVALUATION::EVALUATOR Cannot cast left value null to any know type")
      | _, null -> raise (Exception "GLOON::EVALUATION::EVALUATOR Cannot cast right value null to any know type")
      | _ -> null

    and EvaluateAssigmentExpression (i, e) =
      variables.[i] <- e
      e

    evaluate node