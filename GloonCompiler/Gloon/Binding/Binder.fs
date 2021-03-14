namespace Gloon.Binding

module internal Binder =

  open System
  open System.Collections.Generic
  open System.Linq
  open Gloon.Symbols
  open Gloon.Text
  open Gloon.Syntax
  open Gloon.Binding.BoundTypes

  let internal bind (tree: SyntaxTree) (variables: Dictionary<VariableSymbol, obj>) =
    let diagnostics = DiagnosticsBag ("GLOON::BINDING::BINDER", tree.Diagnostics)

    let rec bindExpression = function
    | ExpressionSyntax.ParenthesysExpression (_,e,_) -> bindExpression (e)
    | ExpressionSyntax.LiteralExpression l -> bindLiteralExpression (l)
    | ExpressionSyntax.IdentifierExpression i -> bindNameExpression (i)
    | ExpressionSyntax.AssignmentExpression (i, _, e) -> bindAssigmentExpression (i, e)
    | ExpressionSyntax.UnaryExpression (o, e) -> bindUnaryExpression (o, e)
    | ExpressionSyntax.BinaryExpression (l,o,r) -> bindBinaryExpression (l, o, r)
    | ExpressionSyntax.ErrorExpression e -> bindErrorExpression (e)

    and bindLiteralExpression syntax : BoundExpression =
      syntax.Value |> LiteralExpression

    and bindNameExpression syntax : BoundExpression =
      let variable = variables.FirstOrDefault(fun v -> v.Key.Name = syntax.Text).Key
      if variable.Type = null then
        diagnostics.ReportUndefinedVariable syntax
        LiteralExpression 0
      else
        VariableExpression variable

    and bindAssigmentExpression (i, e) : BoundExpression =
      let boundExpr = bindExpression e

      let existingVariable = variables.Keys.FirstOrDefault(fun v -> v.Name = i.Text)
      if existingVariable.Type <> null then
        variables.Remove(existingVariable) |> ignore
      let variable = VariableSymbol(i.Text, boundExpr.Type)
      variables.[variable] <- null

      AssignmentExpression (variable, boundExpr)

    and bindUnaryExpression (op, e) : BoundExpression =
      let boundOperand = bindExpression e
      let boundOperator = UnaryOperator.Bind (op.Kind, boundOperand.Type)
      if boundOperator.IsNone then
        diagnostics.ReportUnaryNotDefined op boundOperand.Type
        boundOperand
      else
        UnaryExpression (boundOperator.Value, boundOperand)

    and bindBinaryExpression (l, o, r) : BoundExpression =
      let boundLeft = bindExpression l
      let boundRight = bindExpression r
      let boundOperator = BinaryOperator.Bind(o.Kind, boundLeft.Type, boundRight.Type)
      if boundOperator.IsNone then
        diagnostics.ReportBinaryNotDefined o boundLeft.Type boundRight.Type
        boundLeft
      else
        BinaryExpression (boundLeft, boundOperator.Value, boundRight)

    and bindErrorExpression e : BoundExpression =
      ErrorExpression e.Text

    bindExpression tree.Expression, diagnostics.Diagnostics, tree