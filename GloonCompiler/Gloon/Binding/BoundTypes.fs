namespace Gloon.Binding

module BoundTypes =
  open System
  open Gloon.Syntax

  type internal BoundNode =
    | Expression of BoundExpression
    | UnaryOperator of UnaryOperator
    | BinaryOperator of BinaryOperator

    member n.Children = n |> function
      | Expression e -> e.Children
      | _ -> []

    override n.ToString () = n |> function
      | Expression _ -> "Bound Expression"
      | UnaryOperator u -> $"Unary Operator {u}"
      | BinaryOperator b -> $"Binary Operator {b}"

  and internal UnaryOperator private (token: TokenKind, kind: UnaryOperatorKind, operandType: Type, returnType: Type) =
    let token = token
    let kind = kind
    let operandType = operandType
    let returnType = returnType

    private new (token, kind, operandType) = UnaryOperator(token, kind, operandType, operandType)

    member _.Token = token
    member _.Kind = kind
    member _.OperandType = operandType
    member _.Type = returnType

    static member private _operators = [
      UnaryOperator (PlusToken, Identity, typeof<int>)
      UnaryOperator (MinusToken, Negation, typeof<int>)

      UnaryOperator (BangToken, Negation, typeof<bool>)
    ]

    static member public Bind (token, opType) =
      UnaryOperator._operators
      |> List.tryFind (fun op -> op.Token = token && op.OperandType = opType)

  and internal UnaryOperatorKind =
    | Identity
    | Negation

    member _.Children = []

  and internal BinaryOperator private (token: TokenKind, kind: BinaryOperatorKind, leftType: Type, rightType: Type, returnType: Type) =
    let token = token
    let kind = kind
    let leftType = leftType
    let rightType = rightType
    let returnType = returnType

    private new (token, kind, leftType, returnType) = BinaryOperator(token, kind, leftType, leftType, returnType)
    private new (token, kind, leftType) = BinaryOperator(token, kind, leftType, leftType, leftType)

    member _.Token = token
    member _.Kind = kind
    member _.LeftType = leftType
    member _.RightType = rightType
    member _.Type = returnType

    static member private _operators: BinaryOperator list = [
      BinaryOperator (PlusToken, Addition, typeof<int>)
      BinaryOperator (MinusToken, Subtraction, typeof<int>)
      BinaryOperator (StarToken, Multiplication, typeof<int>)
      BinaryOperator (SlashToken, Division, typeof<int>)
      BinaryOperator (PercentToken, Modulos, typeof<int>)
      BinaryOperator (PowerToken, Power, typeof<int>)

      BinaryOperator (BangEqualsToken, NotEquals, typeof<int>, typeof<bool>)
      BinaryOperator (DoubleEqualsToken, Equals, typeof<int>, typeof<bool>)
      BinaryOperator (LessThanEqualsToken, LesserThanOrEquals, typeof<int>, typeof<bool>)
      BinaryOperator (LessThanToken, LesserThan, typeof<int>, typeof<bool>)
      BinaryOperator (GreaterThanEqualsToken, GreaterThanOrEquals, typeof<int>, typeof<bool>)
      BinaryOperator (GreaterThanToken, GreaterThan, typeof<int>, typeof<bool>)

      BinaryOperator (BangEqualsToken, NotEquals, typeof<bool>)
      BinaryOperator (DoubleEqualsToken, Equals, typeof<bool>)
      BinaryOperator (DoubleAmpersandToken, LogicalAnd, typeof<bool>)
      BinaryOperator (DoublePipeToken, LogicalOr, typeof<bool>)
    ]

    static member public Bind (token, leftType, rightType) =
      BinaryOperator._operators
      |> List.tryFind (fun op -> op.Token = token && op.LeftType = leftType && op.RightType = rightType)

  and internal BinaryOperatorKind =
    | Addition
    | Subtraction
    | Multiplication
    | Division
    | Modulos
    | Power
    | NotEquals
    | Equals
    | GreaterThan
    | GreaterThanOrEquals
    | LesserThan
    | LesserThanOrEquals
    | LogicalAnd
    | LogicalOr

    member _.Children = []

  and internal BoundExpression =
    | LiteralExpression of Value: Object
    | VariableExpression of Identifier: string * Type: Type
    | AssignmentExpression of Identifier: string * Expr: BoundExpression
    | UnaryExpression of operator: UnaryOperator * operand: BoundExpression
    | BinaryExpression of left: BoundExpression * operator: BinaryOperator * right: BoundExpression
    | ErrorExpression of error: string

    member e.Type = e |> function
      | LiteralExpression v -> v.GetType()
      | VariableExpression (_, t) -> t
      | AssignmentExpression (_, e) -> e.Type
      | UnaryExpression (op,_) -> op.Type
      | BinaryExpression (_,op,_) -> op.Type
      | ErrorExpression _ -> ("").GetType()

    member e.Children = e |> function
      | UnaryExpression (op, e) -> [BoundNode.UnaryOperator op; Expression e]
      | BinaryExpression (l, o, r) -> [Expression l; BoundNode.BinaryOperator o; Expression r]
      | _ -> []

    override e.ToString () = e |> function
      | LiteralExpression l -> $"Literal Expression '{l}'"
      | VariableExpression (n,_) -> $"Variable Expression '{n}'"
      | AssignmentExpression (i, _) -> $"Assigment Expression '{i}'"
      | UnaryExpression _ -> "Unary Expression"
      | BinaryExpression _ -> "Binary Expression"
      | ErrorExpression e -> $"Error Expression '{e}'"