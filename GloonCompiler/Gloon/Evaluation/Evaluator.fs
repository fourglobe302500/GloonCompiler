namespace Gloon.Evaluation

module Evaluator =
  open System
  open Gloon.Compiler.Binding.BoundTypes
  
  let rec internal evaluate (node: BoundExpression) = 
    match node with
    | LiteralExpression l -> l
    | BinaryExpression (left,o,right) -> 
      match (evaluate left, evaluate right) with
      | (:? int as l), (:? int as r) -> 
        match o.Kind with
        | Addition -> upcast (l + r)
        | Subtraction -> upcast (l - r)
        | Multiplication -> upcast (l * r)
        | Division -> upcast Math.Round (float (l / r))
        | Modulos -> upcast (l % r)
        | Power -> upcast Math.Round ((float l) ** (float r))
        | _ -> raise (Exception "GLOON::EVALUATION::EVALUATOR Invallid Binary Operation")
      | (:? bool as l), (:? bool as r) ->
        match o.Kind with
        | LogicalAnd -> upcast (l && r)
        | LogicalOr -> upcast (l || r)
        | _ -> raise (Exception "GLOON::EVALUATION::EVALUATOR Invallid Binary Operation")
      | _ -> null
    | UnaryExpression (o, e) -> 
      match evaluate e with
      | :? int as op ->
        match o.Kind with
        | Identity -> upcast op
        | Negation -> upcast -op
        | _ -> raise (Exception "GLOON::EVALUATION::EVALUATOR Invallid Unary Operation")
      | :? bool as op ->
        match o.Kind with
        | Negation -> upcast not op
        | _ -> raise (Exception "GLOON::EVALUATION::EVALUATOR Invallid Unary Operation")
      | _ -> null
    | _ -> raise (Exception "GLOON::EVALUATION::EVALUATOR Invaliid Expression")
