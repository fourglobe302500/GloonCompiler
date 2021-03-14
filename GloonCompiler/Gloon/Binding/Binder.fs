namespace Gloon.Binding

open System.Linq
open System.Collections.Generic
open System.Collections.Immutable

open Gloon.Symbols
open Gloon.Text
open Gloon.Syntax
open Gloon.Binding

type internal Binder (parent: BoundScope option) =
  let diagnostics = DiagnosticsBag ("GLOON::BINDING::BINDER")
  let scope = BoundScope(parent)

  member _.Diagnostics = diagnostics
  member private _.scope = scope

  static member BindGlobalScope previous (syntax: CompilationUnit) =
    let parent = Binder.CreateParentScope(previous)
    let binder = Binder(parent)
    let expression = binder.BindExpression(syntax.Root)
    let variables = binder.scope.GetDeclaradVariables()
    let mutable diagnostics = binder.Diagnostics.ToImmutableArray()
    BoundGlobalScope(previous, diagnostics, variables, expression)

  static member private CreateParentScope (previous: BoundGlobalScope option) =
    let mutable previous = previous
    let stack = new Stack<BoundGlobalScope>()
    while previous <> None do
      match previous with
      | Some prev ->
        stack.Push(prev)
        previous <- prev.Previous
      | None -> previous <- None

    let mutable current = None

    while stack.Count > 0 do
      let globalScope = stack.Pop()
      let scope = BoundScope(current)
      for v in globalScope.Variables do
        scope.TryDeclare(v) |> ignore
      current <- Some scope

    current

  member b.BindExpression = function
    | ExpressionSyntax.ParenthesysExpression (_,e,_) -> b.BindExpression (e)
    | ExpressionSyntax.LiteralExpression l -> b.BindLiteralExpression (l)
    | ExpressionSyntax.IdentifierExpression i -> b.BindNameExpression (i)
    | ExpressionSyntax.AssignmentExpression (i, _, e) -> b.BindAssigmentExpression (i, e)
    | ExpressionSyntax.UnaryExpression (o, e) -> b.BindUnaryExpression (o, e)
    | ExpressionSyntax.BinaryExpression (l,o,r) -> b.BindBinaryExpression (l, o, r)
    | ExpressionSyntax.ErrorExpression e -> b.BindErrorExpression (e)

  member b.BindLiteralExpression syntax =
    syntax.Value |> LiteralExpression

  member b.BindNameExpression i =
    let mutable variable = VariableSymbol()
    if not (scope.TryLookup(i.Text, &variable)) then
      b.Diagnostics.ReportUndefinedVariable i
      ErrorExpression i.Text
    else
      VariableExpression variable

  member b.BindAssigmentExpression (i, e) =
    let boundExpr = b.BindExpression e
    let mutable variable = VariableSymbol(i.Text, boundExpr.Type)
    if not (scope.TryLookup(i.Text, &variable)) then
      variable <- VariableSymbol(i.Text, boundExpr.Type)
      scope.TryDeclare(variable) |> ignore
    match boundExpr with
    | ErrorExpression e ->
      ErrorExpression e
    | _ ->
      if boundExpr.Type <> variable.Type then
        diagnostics.ReportCannotConvert e.Span boundExpr.Type variable.Type
        ErrorExpression i.Text
      else
        AssignmentExpression (variable, boundExpr)

  member b.BindUnaryExpression (op, e) =
    let boundOperand = b.BindExpression e
    match boundOperand with
    | ErrorExpression e ->
      ErrorExpression e
    | _ ->
      let boundOperator = UnaryOperator.Bind (op.Kind, boundOperand.Type)
      if boundOperator.IsNone then
        diagnostics.ReportUnaryNotDefined op boundOperand.Type
        ErrorExpression op.Text
      else
        UnaryExpression (boundOperator.Value, boundOperand)

  member b.BindBinaryExpression (l, o, r) =
    let boundLeft = b.BindExpression l
    let boundRight = b.BindExpression r
    match boundLeft, boundRight with
    | (ErrorExpression e), _ | _, (ErrorExpression e) -> ErrorExpression e
    | _ ->
      let boundOperator = BinaryOperator.Bind(o.Kind, boundLeft.Type, boundRight.Type)
      if boundOperator.IsNone then
        diagnostics.ReportBinaryNotDefined o boundLeft.Type boundRight.Type
        ErrorExpression o.Text
      else
        BinaryExpression (boundLeft, boundOperator.Value, boundRight)

  member b.BindErrorExpression e =
    ErrorExpression e.Text