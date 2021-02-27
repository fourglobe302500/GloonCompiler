namespace Gloon.Compiler.Binding

module BoundTypes =
    open System
    open Gloon.Compiler.Syntax.Types

    type BoundNode =
        | Expression of BoundExpression
        | UnaryOperator of UnaryOperatorKind
        | BinaryOperator of BinaryOperatorKind

        member n.Children = n |> function
            | BoundNode.Expression e -> e.Children
            | _ -> []

        override n.ToString () = n |> function
            | BoundNode.Expression _ -> "Bound Expression"
            | BoundNode.UnaryOperator u -> $"Unary Operator {u}"
            | BoundNode.BinaryOperator b -> $"Binary Operator {b}"

    and UnaryOperatorKind =
        | Identity
        | Negation
        | Invallid

        member _.Children = []

    and BinaryOperatorKind =
        | Addition
        | Subtraction
        | Multiplication
        | Division
        | Modulos
        | Power
        | Invallid

        member _.Children = []

    and BoundExpression =
        | LiteralExpression of Value: Object
        | UnaryExpression of operator: UnaryOperatorKind * operand: BoundExpression
        | BinaryExpression of left: BoundExpression * operator: BinaryOperatorKind * right: BoundExpression
        | ErrorExpression of error: string

        member e.Type = e |> function
            | BoundExpression.LiteralExpression v -> v.GetType()
            | BoundExpression.UnaryExpression (_,op) -> op.Type
            | BoundExpression.BinaryExpression (l,_,_) -> l.Type
            | BoundExpression.ErrorExpression _ -> ("").GetType()

        member e.Children = e |> function
            | BoundExpression.UnaryExpression (op, e) -> [BoundNode.UnaryOperator op; BoundNode.Expression e]
            | BoundExpression.BinaryExpression (l, o, r) -> [BoundNode.Expression l; BoundNode.BinaryOperator o; BoundNode.Expression r]
            | _ -> []

        override e.ToString () = e |> function
            | BoundExpression.UnaryExpression _ -> "Unary Expression"
            | BoundExpression.BinaryExpression _ -> "Binary Expression"
            | BoundExpression.LiteralExpression l -> $"Literal Expression {l}"
            | BoundExpression.ErrorExpression e -> $"Error Expression {e}"