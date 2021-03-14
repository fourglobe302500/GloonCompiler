namespace Gloon.Binding

open System
open System.Collections.Generic
open System.Collections.Immutable

open Gloon.Text
open Gloon.Symbols
open Gloon.Syntax

type internal UnaryOperatorKind =
  | Identity
  | Negation

[<Sealed>]
type internal UnaryOperator private (token: TokenKind, kind: UnaryOperatorKind, operandType: Type, returnType: Type) =
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

type internal BinaryOperatorKind =
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

[<Sealed>]
type internal BinaryOperator private (token: TokenKind, kind: BinaryOperatorKind, leftType: Type, rightType: Type, returnType: Type) =
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

type internal BoundExpression =
  | LiteralExpression of Value: Object
  | VariableExpression of Identifier: VariableSymbol
  | AssignmentExpression of Identifier: VariableSymbol * Expr: BoundExpression
  | UnaryExpression of operator: UnaryOperator * operand: BoundExpression
  | BinaryExpression of left: BoundExpression * operator: BinaryOperator * right: BoundExpression
  | ErrorExpression of error: string

  member e.Type = e |> function
    | LiteralExpression l -> l.GetType()
    | VariableExpression v -> v.Type
    | AssignmentExpression (_, e) -> e.Type
    | UnaryExpression (op,_) -> op.Type
    | BinaryExpression (_,op,_) -> op.Type
    | ErrorExpression _ -> null

type internal BoundStatement =
  | ExpressionStatement of BoundExpression
  | BlockStatement of ImmutableArray<BoundStatement>

type internal BoundNode =
  | Statement of BoundStatement
  | Expression of BoundExpression

[<Sealed>]
type internal BoundScope (parent: BoundScope option) =
  let variables = new Dictionary<string, VariableSymbol>()

  member _.Parent = parent

  member _.TryDeclare (variable: VariableSymbol) =
    if variables.ContainsKey(variable.Name) then false
    else
      variables.Add(variable.Name, variable)
      true

  member _.TryLookup (name, variable: byref<_>) =
    if variables.TryGetValue(name, &variable) then true
    else
      match parent with
      | Some parent -> parent.TryLookup(name, &variable)
      | None -> false

  member _.GetDeclaradVariables () = variables.Values.ToImmutableArray()

[<Sealed>]
type internal BoundGlobalScope (previous: BoundGlobalScope option, diagnostics: ImmutableArray<Diagnostic>, variables: ImmutableArray<VariableSymbol>, statement: BoundStatement) =
  member _.Previous = previous
  member _.Diagnostics = diagnostics
  member _.Variables = variables
  member _.Statement = statement