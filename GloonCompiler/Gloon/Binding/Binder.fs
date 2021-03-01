namespace Gloon.Binding

module internal Binder =

  open Gloon.Text
  open Gloon.Syntax
  open Gloon.Binding.BoundTypes

  let internal bind (cst: CST) =
    let diagnostics = DiagnosticsBag ("GLOON::BINDING::BINDER", cst.Diagnostics)

    let rec bindExpression = function
    | ExpressionSyntax.LiteralExpression l -> bindLiteralExpression (l)
    | ExpressionSyntax.IdentifierExpression i -> bindErrorExpression (i)
    | ExpressionSyntax.UnaryExpression (op, e) -> bindUnaryExpression (op, e)
    | ExpressionSyntax.BinaryExpression (l,o,r) -> bindBinaryExpression (l, o, r)
    | ExpressionSyntax.ParenthesysExpression (_,e,_) -> bindExpression (e)
    | ExpressionSyntax.ErrorExpression e -> bindErrorExpression (e)

    and bindLiteralExpression syntax : BoundExpression =
      syntax.Value |> LiteralExpression

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