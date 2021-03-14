namespace Gloon.Tests.Evaluation

module EvaluatorTests =
  open Gloon.Symbols
  open Gloon.Syntax
  open Gloon.Compiler

  open Xunit
  open System.Collections.Generic

  [<Theory>]
  [<InlineData("1", 1)>]
  [<InlineData("+1", 1)>]
  [<InlineData("-1", -1)>]
  [<InlineData("(10)", 10)>]
  [<InlineData("1 + 1", 2)>]
  [<InlineData("2 - 3", -1)>]
  [<InlineData("10 * 45", 450)>]
  [<InlineData("6 / 2", 3)>]
  [<InlineData("4 ** 2", 16)>]
  [<InlineData("50 % 9", 5)>]

  [<InlineData("1 == 2", false)>]
  [<InlineData("3 == 3", true)>]
  [<InlineData("1 != 2", true)>]
  [<InlineData("3 != 3", false)>]
  [<InlineData("1  < 2", true)>]
  [<InlineData("2  < 1", false)>]
  [<InlineData("2  < 2", false)>]
  [<InlineData("3 <= 4", true)>]
  [<InlineData("4 <= 3", false)>]
  [<InlineData("5 <= 5", true)>]
  [<InlineData("1  > 2", false)>]
  [<InlineData("2  > 1", true)>]
  [<InlineData("2  > 2", false)>]
  [<InlineData("3 >= 4", false)>]
  [<InlineData("4 >= 3", true)>]
  [<InlineData("5 >= 5", true)>]

  [<InlineData("true", true)>]
  [<InlineData("false", false)>]
  [<InlineData("!true", false)>]
  [<InlineData("!false", true)>]
  [<InlineData("false || false", false)>]
  [<InlineData("false || true" , true)>]
  [<InlineData("true  || false", true)>]
  [<InlineData("true  || true" , true)>]
  [<InlineData("false && false", false)>]
  [<InlineData("false && true" , false)>]
  [<InlineData("true  && false", false)>]
  [<InlineData("true  && true" , true)>]
  [<InlineData("false == false", true)>]
  [<InlineData("false == true" , false)>]
  [<InlineData("true  == false", false)>]
  [<InlineData("true  == true" , true)>]
  [<InlineData("false != false", false)>]
  [<InlineData("false != true" , true)>]
  [<InlineData("true  != false", true)>]
  [<InlineData("true  != true" , false)>]

  [<InlineData("(1 * 5 - 6) > 2 || 1 == 2", false)>]
  [<InlineData("{let a = 4  a ** a}", 256)>]

  let ``Evaluator Evaluates`` (expression: string, expectedValue) =
    let tree = SyntaxTree.Parse expression
    let compilation = Compilation tree
    let variables = new Dictionary<VariableSymbol, obj>()
    let result = compilation.Evaluate(variables)
    Assert.Empty(result.Diagnostics)
    Assert.Equal(expectedValue, result.Value)