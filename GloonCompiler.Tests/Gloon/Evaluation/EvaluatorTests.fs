namespace Gloon.Tests.Evaluation

module EvaluatorTests =
  open Gloon.Symbols
  open Gloon.Text
  open Gloon.Syntax
  open Gloon.Compiler

  open Xunit
  open System
  open System.Text.RegularExpressions
  open System.Collections.Generic

  let AssertValue (expression: string) expectedValue =
    let tree = SyntaxTree.Parse expression
    let compilation = Compilation tree
    let variables = new Dictionary<VariableSymbol, obj>()
    let result = compilation.Evaluate(variables)
    Assert.Empty(result.Diagnostics)
    Assert.Equal(expectedValue, result.Value)

  let private AssertDiagnostics text (expectedDiagnostics: string list) =
    let annotatedText = AnnotatedText.Parse(text)
    let syntaxTree = SyntaxTree.Parse(annotatedText.Text)
    let compilation = new Compilation(syntaxTree)
    let result = compilation.Evaluate(new Dictionary<_,_>())

    Assert.Equal(expectedDiagnostics.Length, result.Diagnostics.Length)

    if annotatedText.Spans.Length <> expectedDiagnostics.Length then
      raise (new Exception "ERROR::GLOON::TESTS::EVALUATION Must mark as many spans as there are diagnostics")

    Seq.iteri (
      fun i (actual: Diagnostic) ->
        let expectedMessage = expectedDiagnostics.[i]
        let actualMessage = Regex("^([A-Z:])* ").Replace(actual.Message, "")
        Assert.Equal(expectedMessage, actualMessage)

        let expectedSpan = annotatedText.Spans.[i]
        let actualSpan = actual.Span
        Assert.Equal(expectedSpan, actualSpan)
    ) result.Diagnostics

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
    AssertValue expression expectedValue

  [<Fact>]
  let ``Variable Declaration Reports Redeclaration`` () =
    AssertDiagnostics
      @"
        {
          let x = 10
          let y = 100
          {
            let x = 50
          }
          let [x] = 5
        }
      "
      ["Variable 'x' is already declared."]

  [<Fact>]
  let ``Name Reports Undefined`` () =
    AssertDiagnostics @"[x] * 20" ["Variable 'x' is not defined."]

  [<Fact>]
  let ``Assigment Reports Undefined`` () =
    AssertDiagnostics @"[x] = 20" ["Variable 'x' is not defined."]

  [<Fact>]
  let ``Assigment Reports Read-Only`` () =
    AssertDiagnostics
      @"
        {
          def x = 10
          x [=] 50
        }
      "
      ["Variable 'x' is read only and cannot be changed."]

  [<Fact>]
  let ``Assigment Reports Cannot Convert`` () =
    AssertDiagnostics
      @"
        {
          let x = 10
          x = [true]
        }
      "
      ["Cannot convert type <System.Boolean> to type <System.Int32>."]

  [<Fact>]
  let ``Unary Reports Undefined Operator`` () =
    AssertDiagnostics @"[+]true" ["Unary operator '+' is not defined for type <System.Boolean>."]

  [<Fact>]
  let ``Binary Reports Undefined Operator`` () =
    AssertDiagnostics @"1 [||] 2" ["Binary operator '||' is not defined for types <System.Int32> and <System.Int32>."]