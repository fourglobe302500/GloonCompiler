namespace Gloon.Compiler.Binding

module Binder =

  open Gloon.Compiler.Syntax.Types
  open Gloon.Compiler.Binding.BoundTypes

  let Bind (cst: CST) =
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
      let boundOperator = bindUnaryOperatorKind (op, boundOperand.Type)
      if boundOperator = UnaryOperatorKind.Invallid then
        diagnostics.Add($"Unary operator '{op.Text}' is not defined for type <{boundOperand.Type}>.")
        boundOperand
      else
        UnaryExpression (boundOperator, boundOperand)

    and bindBinaryExpression (l, o, r) : BoundExpression =
      let boundLeft = bindExpression l
      let boundRight = bindExpression r
      let boundOperator = bindBinaryOperatorKind (o, boundLeft.Type, boundRight.Type)
      if boundOperator = Invallid then
        diagnostics.Add($"Binary operator '{o.Text}' is nor defined for types <{boundLeft.Type}> and <{boundRight.Type}>.")
        boundLeft
      else
        BinaryExpression (boundLeft, boundOperator, boundRight)

    and bindErrorExpression e : BoundExpression =
      ErrorExpression e.Text

    and bindUnaryOperatorKind (o, t) : UnaryOperatorKind =
      if (t = typedefof<int>) then 
        match o.Kind with
        | PlusToken -> Identity
        | MinusToken -> Negation
        | _ -> UnaryOperatorKind.Invallid
      elif (t = typedefof<bool>) then   
        match o.Kind with
        | BangToken -> Negation
        | _ -> UnaryOperatorKind.Invallid
      else UnaryOperatorKind.Invallid

    and bindBinaryOperatorKind (o, lt, rt) : BinaryOperatorKind =
      if (lt = typedefof<int> && rt = typedefof<int>) then 
        match o.Kind with
        | PlusToken ->    Addition
        | MinusToken ->   Subtraction
        | StarToken ->    Multiplication
        | SlashToken ->   Division
        | ModulosToken -> Modulos
        | PowerToken ->   Power
        | _ ->            Invallid
      elif (lt = typedefof<bool> && rt = typedefof<bool>) then
        match o.Kind with
        | DoubleAmpersandToken -> LogicalAnd
        | DoublePipeToken ->      LogicalOr
        | _ ->                    Invallid
      else Invallid
    bindExpression cst.Root, diagnostics.ToArray (), cst


