namespace Gloon.Syntax

open Gloon.Text
open Gloon.Syntax.Lexer
open Gloon.Syntax.Parser
open System.Collections.Immutable

type SyntaxTree private (text: SourceText, root: CompilationUnit, diagnostics: DiagnosticsBag) =
  let text = text
  let root = root
  let diagnostics = diagnostics

  private new (text) =
    let (root, diagnostics) = ParseCompilationUnit(text)
    SyntaxTree(text, root, diagnostics)

  member _.Text = text
  member _.Root = root
  member _.RootNode = CompilationUnit root
  member _.Expression = root.Root
  member _.Diagnostics = diagnostics.Diagnostics.ToImmutableArray()

  static member Parse text = SyntaxTree(text)
  static member Parse text =
    let src = SourceText.From text
    SyntaxTree.Parse src

  static member Lex text = fst (Lex text)
  static member Lex text =
    let src = SourceText.From text
    SyntaxTree.Lex src