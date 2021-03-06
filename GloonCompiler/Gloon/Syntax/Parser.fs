namespace Gloon.Syntax

module Parser =

  open Gloon.Text
  open Gloon.Syntax
  open Gloon.Syntax.Facts
  open Gloon.Syntax.Lexer

  let Parse line =
    let (tokens_, diagnostics_) = Lex line
    let tokens = tokens_ |> Seq.filter (fun t ->
      match t.Kind with
      | WhiteSpaceToken _ -> false
      | _ -> true) |> List.ofSeq
    let diagnostics = DiagnosticsBag("GLOON::SYNTAX::PARSER", diagnostics_)
    let mutable position = 0

    let peek x =
      if position + x >= tokens.Length
      then tokens.[tokens.Length - 1]
      else tokens.[position + x]
    let inline current () = peek 0
    let inline lookAhead () = peek 1
    let inline currentKind () = (current ()).Kind

    let next () =
      let current = current ()
      position <- position + 1
      current

    let inline matchToken (kind: TokenKind) =
      match (current ()).Kind with
      | k when k = kind -> next()
      | _ ->
        diagnostics.ReportUnexpectedKind "Token" (current()) kind
        {next () with Text = ""; Kind = kind; Value = null}

    let rec parsePrimaryExpression () =
      match currentKind() with
      | NumberLiteralToken n -> LiteralExpression (matchToken (NumberLiteralToken n))
      | OpenParenToken -> ParenthesysExpression (next (), parseExpression (), matchToken CloseParenToken)
      | BooleanLiteralToken b -> LiteralExpression (matchToken (BooleanLiteralToken b))
      | Identifier i when (lookAhead ()).Kind = TokenKind.EqualsToken ->
        AssignmentExpression (matchToken (TokenKind.Identifier i), matchToken TokenKind.EqualsToken, parseExpression())
      | Identifier i -> IdentifierExpression (matchToken (Identifier i))
      | _ ->
        diagnostics.ReportInvallidKind "token" (current ())
        ErrorExpression (next())

    and parseUnaryPrefixExpression () =
      if currentKind().UnaryOperatorPrecedence > 0
      then UnaryExpression(next (), parseUnaryPrefixExpression ())
      else parsePrimaryExpression ()

    and parseBinaryExpression parentPrecedence =
      let mutable left = parseUnaryPrefixExpression ()
      let mutable Break = false
      while not Break do
        let (precedence, right) = currentKind().BinaryOperatorPrecedence
        if precedence = 0 || (if right then precedence < parentPrecedence else precedence <= parentPrecedence)
        then Break <- true
        else left <- BinaryExpression (left, next (), parseBinaryExpression precedence)
      left

    and parseExpression () = parseBinaryExpression 0

    CST (parseExpression (), matchToken(EndOfFileToken), diagnostics)