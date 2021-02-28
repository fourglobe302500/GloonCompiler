namespace Gloon.Compiler.Binding

module BoundTypes =
  open System

  type BoundNode =
    | Expression of BoundExpression
    | UnaryOperator of UnaryOperatorKind
    | BinaryOperator of BinaryOperatorKind

    member n.Children = n |> function
      | Expression e -> e.Children
      | _ -> []

    override n.ToString () = n |> function
      | Expression _ -> "Bound Expression"
      | UnaryOperator u -> $"Unary Operator {u}"
      | BinaryOperator b -> $"Binary Operator {b}"

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
    | LogicalAnd
    | LogicalOr
    | Invallid

    member _.Children = []

  and BoundExpression =
    | LiteralExpression of Value: Object
    | UnaryExpression of operator: UnaryOperatorKind * operand: BoundExpression
    | BinaryExpression of left: BoundExpression * operator: BinaryOperatorKind * right: BoundExpression
    | ErrorExpression of error: string

    member e.Type = e |> function
      | LiteralExpression v -> v.GetType()
      | UnaryExpression (_,op) -> op.Type
      | BinaryExpression (l,_,_) -> l.Type
      | ErrorExpression _ -> ("").GetType()

    member e.Children = e |> function
      | UnaryExpression (op, e) -> [UnaryOperator op; Expression e]
      | BinaryExpression (l, o, r) -> [Expression l; BinaryOperator o; Expression r]
      | _ -> []

    override e.ToString () = e |> function
      | UnaryExpression _ -> "Unary Expression"
      | BinaryExpression _ -> "Binary Expression"
      | LiteralExpression l -> $"Literal Expression {l}"
      | ErrorExpression e -> $"Error Expression {e}"



