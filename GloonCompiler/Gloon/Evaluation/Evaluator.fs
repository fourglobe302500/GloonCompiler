namespace Gloon.Evaluation

module Evaluator =
  open System
  open Gloon.Compiler.Binding.BoundTypes
  
  let rec Evaluate (node: BoundExpression) = 
    match node with
    | LiteralExpression l -> l
    | BinaryExpression (left,o,right) -> 
      match (Evaluate left, Evaluate right) with
      | (:? int as l), (:? int as r) -> 
        match o with
        | Addition -> upcast (l + r)
        | Subtraction -> upcast (l - r)
        | Multiplication -> upcast (l * r)
        | Division -> upcast Math.Round (float (l / r))
        | Modulos -> upcast (l % r)
        | Power -> upcast Math.Round ((float l) ** (float r))
        | _ -> raise (Exception "GLOON::EVALUATION::EVALUATOR Invallid Binary Operation")
      | _ -> null
    | UnaryExpression (o, e) -> 
      match Evaluate e with
      | :? int as op ->
        match o with
        | Identity -> upcast op
        | Negation -> upcast -op
        | _ -> raise (Exception "GLOON::EVALUATION::EVALUATOR Invallid Unary Operation")
      | _ -> null
    | _ -> raise (Exception "GLOON::EVALUATION::EVALUATOR Invaliid Expression")
