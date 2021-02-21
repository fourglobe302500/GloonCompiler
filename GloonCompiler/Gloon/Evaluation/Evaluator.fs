module Gloon.Evaluation.Evaluator

open Gloon.Types

let rec Evaluate (root: Expression) =
    root |> function
    | Expression.NumberExpression n ->
        n.Value |> function
        | io.Int v -> v
        | _ -> 0
    | Expression.ParenthesysExpression (_,e,_) -> Evaluate e
    | Expression.BinaryExpression (l, o, r) ->
        o.Kind |> function
        | TokenKind.PlusToken -> (Evaluate l) + (Evaluate r)
        | TokenKind.MinusToken -> (Evaluate l) - (Evaluate r)
        | TokenKind.StarToken -> (Evaluate l) * (Evaluate r)
        | TokenKind.SlashToken -> (Evaluate l) / (Evaluate r)
        | TokenKind.ModulosToken -> (Evaluate l) % (Evaluate r)
        | TokenKind.PowerToken -> pown (Evaluate l) (Evaluate r)
        | _ -> 0
    | Expression.UnaryExpression (op, e) -> Evaluate e
    | _ -> 0