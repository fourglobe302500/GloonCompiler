namespace Gloon.Tests.Syntax

module Parser =
  open Gloon.Syntax
  open Gloon.Syntax.Facts

  open Xunit

  let GetBinaryOperatorPairsData () = seq {
    for op1 in TokenKind.GetBinaryOperators() do
      for op2 in TokenKind.GetBinaryOperators() -> [| op1 :> obj; op2 :> obj |] }

  [<Theory>]
  [<MemberData(nameof(GetBinaryOperatorPairsData))>]
  let ``Binary Expression Honor Precedences`` (op1: TokenKind, op2: TokenKind) =
    let op1Precendece = op1.BinaryOperatorPrecedence
    let op2Precendece = op2.BinaryOperatorPrecedence
    let text = $"a {op1.Text} b {op2.Text} c"
    let syntaxTree = SyntaxTree.Parse text
    let aToken   = {Position = 0; Text = "a"; Kind = Identifier "a"; Value = "a"}
    let op1Token = {Position = 2; Text = op1.Text; Kind = op1; Value = null}
    let bToken   = {Position = 3 + op1.Text.Length; Text = "b"; Kind = Identifier "b"; Value = "b"}
    let op2Token = {Position = 5 + op1.Text.Length; Text = op2.Text; Kind = op2; Value = null}
    let cToken   = {Position = 6 + op1.Text.Length + op2.Text.Length; Text = "c"; Kind = Identifier "c"; Value = "c"}
    let aExpression = IdentifierExpression aToken
    let bExpression = IdentifierExpression bToken
    let cExpression = IdentifierExpression cToken
    if op1Precendece >= op2Precendece then
      let expr = BinaryExpression (BinaryExpression(aExpression, op1Token, bExpression), op2Token, cExpression)
      Assert.Equal(expr, syntaxTree.Expression)
    else
      let expr = BinaryExpression (aExpression, op1Token, BinaryExpression(bExpression, op2Token, cExpression))
      Assert.Equal(expr, syntaxTree.Expression)