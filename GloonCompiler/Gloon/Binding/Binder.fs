namespace Gloon.Binding

module internal Binder =

  open System
  open System.Collections.Generic
  open Gloon.Text
  open Gloon.Syntax
  open Gloon.Binding.BoundTypes

  let internal bind (cst: CST) (variables: Dictionary<string, obj>) =
    let diagnostics = DiagnosticsBag ("GLOON::BINDING::BINDER", cst.Diagnostics)

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
      let mutable value = null
      if not (variables.TryGetValue(syntax.Text, &value)) then
        diagnostics.ReportUndefinedVariable syntax
        LiteralExpression 0
      else
        let type_ = value.GetType()
        VariableExpression(syntax.Text, type_)

    and bindAssigmentExpression (i, e) : BoundExpression =
      let boundExpr = bindExpression e

      let defaultValue =
        if boundExpr.Type = typeof<int> then 0 :> obj
        elif boundExpr.Type = typeof<bool> then false :> obj
        else null

      if defaultValue = null then raise (new Exception $"Unsupported variable type: {boundExpr.Type}.")

      variables.[i.Text] <- defaultValue

      AssignmentExpression (i.Text, boundExpr)

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

    bindExpression cst.Root, diagnostics.Diagnostics, cst