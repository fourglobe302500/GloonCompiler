namespace Gloon.Compiler.Binding

module Binder =

  open Gloon.Compiler.Syntax.Types
  open Gloon.Compiler.Binding.BoundTypes

  let internal bind (cst: CST) =
    let diagnostics = ResizeArray (cst.Diagnostics)

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
        diagnostics.Add($"Unary operator '{op.Text}' is not defined for type <{boundOperand.Type}>.")
        boundOperand
      else
        UnaryExpression (boundOperator.Value, boundOperand)

    and bindBinaryExpression (l, o, r) : BoundExpression =
      let boundLeft = bindExpression l
      let boundRight = bindExpression r
      let boundOperator = BinaryOperator.Bind(o.Kind, boundLeft.Type, boundRight.Type)
      if boundOperator.IsNone then
        diagnostics.Add($"Binary operator '{o.Text}' is nor defined for types <{boundLeft.Type}> and <{boundRight.Type}>.")
        boundLeft
      else
        BinaryExpression (boundLeft, boundOperator.Value, boundRight)

    and bindErrorExpression e : BoundExpression =
      ErrorExpression e.Text

    bindExpression cst.Root, diagnostics.ToArray (), cst


