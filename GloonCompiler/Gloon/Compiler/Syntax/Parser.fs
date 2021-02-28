namespace Gloon.Compiler.Syntax

module Parser =

  open Gloon.Compiler.Syntax.Types
  open Gloon.Compiler.Syntax.Facts

  let Parse struct(tokens_: Token list, diagnostics_: string list) =
    let tokens = tokens_ |> Seq.filter (fun t ->
      match t.Kind with
      | WhiteSpaceToken _ -> false
      | _ -> true) |> List.ofSeq
    let diagnostics = ResizeArray(diagnostics_)
    let mutable position = 0

    let peek x =
      if position + x >= tokens.Length
      then tokens.[tokens.Length - 1]
      else tokens.[position + x]
    let inline current () = peek 0
    let inline lookAhead () = peek 1

    let next () =
      let current = current ()
      position <- position + 1
      current

    let inline matchToken (kind: TokenKind) =
      match (current ()).Kind with
      | k when k = kind -> next()
      | _ ->
        diagnostics.Add($"GLOON::COMPILER::SYNTAX::PARSER Unnexpected Token <{(current()).Kind}> expexted <{kind}> at: {(current ()).Position}.")
        {next () with Text = ""; Kind = kind; Value = null}

    let rec parsePrimaryExpression () =
      match (current ()).Kind with
      | NumberLiteralToken n -> LiteralExpression (matchToken (NumberLiteralToken n))
      | OpenParenToken -> ParenthesysExpression (next (), parseBinaryExpression 0, matchToken CloseParenToken)
      | BooleanLiteralToken b -> LiteralExpression (matchToken (BooleanLiteralToken b))
      | Identifier i -> IdentifierExpression (matchToken (Identifier i))
      | _ ->
        diagnostics.Add($"GLOON::COMPILER::SYNTAX::PARSER Invallid Token <{(current ()).Kind}> at: {(current ()).Position}.")
        ErrorExpression (next())

    and parseUnaryPrefixExpression () =
      if (current ()).Kind.UnaryOperatorPrecedence > 0
      then UnaryExpression(next (), parseUnaryPrefixExpression ())
      else parsePrimaryExpression ()

    and parseBinaryExpression parentPrecedence =
      let mutable left = parseUnaryPrefixExpression ()
      let mutable Break = false
      while not Break do
        let (precedence, right) = (current ()).Kind.BinaryOperatorPrecedence
        if precedence = 0 || (if right then precedence < parentPrecedence else precedence <= parentPrecedence)
        then Break <- true
        else left <- BinaryExpression (left, next (), parseBinaryExpression precedence)
      left

    CST (parseBinaryExpression 0, matchToken(EndOfFileToken), diagnostics.ToArray() |> Array.toList)



