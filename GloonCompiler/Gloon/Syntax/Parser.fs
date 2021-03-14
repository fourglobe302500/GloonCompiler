namespace Gloon.Syntax

module internal Parser =

  open Gloon.Text
  open Gloon.Syntax
  open Gloon.Syntax.Facts
  open Gloon.Syntax.Lexer

  open System.Collections.Immutable

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
      if currentKind () <> InvallidToken (current().Text) then
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
        if currentKind () <> InvallidToken (current().Text) then
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

    let rec parseBlockStatement () =
      let builder = ImmutableArray.CreateBuilder()
      let openBrace = matchToken OpenCurlyBraceToken
      while currentKind () <> CloseCurlyBraceToken && currentKind () <> EndOfFileToken do
        builder.Add(parseStatement ())
      let closeBrace = matchToken CloseCurlyBraceToken
      BlockStatement (openBrace, builder.ToImmutable(), closeBrace)

    and parseDeclarationStatetement () =
      let isMutable = currentKind () = LetKeyword
      let declareToken =
        if isMutable then matchToken LetKeyword
        else matchToken DefKeyword
      let identifier = matchToken (Identifier (current().Text))
      let equalsToken = matchToken EqualsToken
      let statement = parseStatement ()
      DeclarationStatement(declareToken, identifier, equalsToken, statement)

    and parseStatement () =
      match currentKind () with
      | OpenCurlyBraceToken -> parseBlockStatement ()
      | LetKeyword | DefKeyword -> parseDeclarationStatetement ()
      | _ -> ExpressionStatement (parseExpression ())

    parseStatement (), matchToken(EndOfFileToken), diagnostics

  let parseCompilationUnit text =
    let (statement, endOfFileToken, diagnostics) = Parse text
    (new CompilationUnit(statement, endOfFileToken), diagnostics)