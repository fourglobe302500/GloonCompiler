namespace Gloon.Compiler.Binding

module Binder =

    open Gloon.Compiler.Syntax.Types
    open Gloon.Compiler.Binding.BoundTypes

    let Bind (cst: CST) =
        let diagnostics = ResizeArray (cst.Diagnostics)

        let rec BindExpression = function
        | ExpressionSyntax.LiteralExpression l -> BindLiteralExpression (l)
        | ExpressionSyntax.UnaryExpression (op, e) -> BindUnaryExpression (op, e)
        | ExpressionSyntax.BinaryExpression (l,o,r) -> BindBinaryExpression (l, o, r)
        | ExpressionSyntax.ParenthesysExpression (_,e,_) -> BindExpression (e)
        | ExpressionSyntax.ErrorExpression e -> BindErrorExpression (e)

        and BindLiteralExpression syntax : BoundExpression =
            syntax.Value |> BoundExpression.LiteralExpression

        and BindUnaryExpression (op, e) : BoundExpression =
            let boundOperand = BindExpression e
            let boundOperator = BindUnaryOperatorKind (op, boundOperand.Type)
            if boundOperator = UnaryOperatorKind.Invallid
            then
                diagnostics.Add($"Unary operator '{op.Text}' is not defined for type <{boundOperand.Type}>.")
                boundOperand
            else
                BoundExpression.UnaryExpression (boundOperator, boundOperand)

        and BindBinaryExpression (l, o, r) =
            let boundLeft = BindExpression l
            let boundRight = BindExpression r
            let boundOperator = BindBinaryOperatorKind (o, boundLeft.Type, boundRight.Type)
            if boundOperator = BinaryOperatorKind.Invallid
            then
                diagnostics.Add($"Binary operator '{o.Text}' is nor defined for types <{boundLeft.Type}> and <{boundRight.Type}>.")
                boundLeft
            else
                BoundExpression.BinaryExpression (boundLeft, boundOperator, boundRight)

        and BindErrorExpression e =
            BoundExpression.ErrorExpression e.Text

        and BindUnaryOperatorKind (o, t) =
            if (t = typedefof<int>)
            then o.Kind |> function
                | TokenKind.PlusToken -> UnaryOperatorKind.Identity
                | TokenKind.MinusToken -> UnaryOperatorKind.Negation
                | _ -> raise (System.Exception $"GLOON::COMPILER::BINDING::BINDER Unexpected unary operator {o.Kind} at: {o.Position}.")
            else UnaryOperatorKind.Invallid

        and BindBinaryOperatorKind (o, lt, rt) =
            if (lt = typedefof<int> && rt = typedefof<int>)
            then o.Kind |> function
                | TokenKind.PlusToken -> BinaryOperatorKind.Addition
                | TokenKind.MinusToken -> BinaryOperatorKind.Subtraction
                | TokenKind.StarToken -> BinaryOperatorKind.Multiplication
                | TokenKind.SlashToken -> BinaryOperatorKind.Division
                | TokenKind.ModulosToken -> BinaryOperatorKind.Modulos
                | TokenKind.PowerToken -> BinaryOperatorKind.Power
                | _ -> raise (System.Exception $"GLOON::COMPILER::BINDING::BINDER Unexpected binary operator {o.Kind} at: {o.Position}.")
            else BinaryOperatorKind.Invallid
        BoundNode.Expression (BindExpression cst.Root), diagnostics.ToArray (), cst