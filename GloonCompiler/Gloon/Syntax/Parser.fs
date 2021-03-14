namespace Gloon.Syntax

module internal Parser =

  open Gloon.Text
  open Gloon.Syntax
  open Gloon.Syntax.Facts
  open Gloon.Syntax.Lexer

  let Parse text =
    let (tokens_, diagnostics_) = Lex text
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

    let inline next () =
      let current = current ()
      position <- position + 1
      current

    let matchToken = function
    | k when k = currentKind() -> next()
    | kind ->
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
        let precedence = currentKind().BinaryOperatorPrecedence
        if precedence = 0 || precedence <= parentPrecedence
        then Break <- true
        else left <- BinaryExpression (left, next (), parseBinaryExpression precedence)
      left

    and parseExpression () = parseBinaryExpression 0

    parseExpression (), matchToken(EndOfFileToken), diagnostics

  let ParseCompilationUnit text =
    let (root, endOfFileToken, diagnostics) = Parse text
    (new CompilationUnit(root, endOfFileToken), diagnostics)