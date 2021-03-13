namespace Gloon.Tests.Syntax

module Parser =
  open Gloon.Syntax
  open Gloon.Syntax.Parsing
  open Gloon.Syntax.Facts
  open Gloon.Tests.Syntax

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
    let expression = ParseString text
    let Default = (
      LiteralExpression {Position = 0; Text = "0"; Kind = NumberLiteralToken 0; Value = 0},
      {Position = 0; Text = "+"; Kind = PlusToken; Value = null},
      LiteralExpression {Position = 0; Text = "0"; Kind = NumberLiteralToken 0; Value = 0})
    if op1Precendece >= op2Precendece then
      use e = new AssertingEnumerator(SyntaxNode.Expression expression.Root)
      e.AssertNode (BinaryExpression Default)
      e.AssertNode (BinaryExpression Default)
      e.AssertNode (IdentifierExpression {Position = 0; Text = "a"; Kind = Identifier "a"; Value = null})
      e.AssertToken (Identifier "a") "a"
      e.AssertToken op1 op1.Text
      e.AssertNode (IdentifierExpression {Position = 0; Text = "b"; Kind = Identifier "b"; Value = null})
      e.AssertToken (Identifier "b") "b"
      e.AssertToken op2 op2.Text
      e.AssertNode (IdentifierExpression {Position = 0; Text = "c"; Kind = Identifier "c"; Value = null})
      e.AssertToken (Identifier "c") "c"
    else
      use e = new AssertingEnumerator(SyntaxNode.Expression expression.Root)
      e.AssertNode (BinaryExpression Default)
      e.AssertNode (IdentifierExpression {Position = 0; Text = "a"; Kind = Identifier "a"; Value = null})
      e.AssertToken (Identifier "a") "a"
      e.AssertToken op1 op1.Text
      e.AssertNode (BinaryExpression Default)
      e.AssertNode (IdentifierExpression {Position = 0; Text = "b"; Kind = Identifier "b"; Value = null})
      e.AssertToken (Identifier "b") "b"
      e.AssertToken op2 op2.Text
      e.AssertNode (IdentifierExpression {Position = 0; Text = "c"; Kind = Identifier "c"; Value = null})
      e.AssertToken (Identifier "c") "c"