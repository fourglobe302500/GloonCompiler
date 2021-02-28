module Gloon.Runner

open System

open Gloon.Compiler.Syntax.Types
open Gloon.Compiler.Syntax.Lexer
open Gloon.Compiler.Syntax.Parser
open Gloon.Compiler.Binding.BoundTypes
open Gloon.Compiler.Binding.Binder
open Gloon.Evaluation.Evaluator
open Gloon.Utils

let mutable CST = false
let mutable BST = false

let mutable Break = false

while not Break do
  Console.ForegroundColor <- ConsoleColor.Cyan
  Console.Write("> ")
  Console.ForegroundColor <- ConsoleColor.White
  let line = Console.ReadLine()
  if String.IsNullOrEmpty line then Break <- true
  if line.StartsWith '#' then
    match line with
    | "#cls" -> Console.Clear()
    | "#quit" -> Break <- true
    | "#CST" ->
      CST <- not CST
      printfn "%s Concrete Syntax Tree" (if CST then "Showing" else "Hiding")
    | "#BST" ->
      BST <- not BST
      printfn "%s Bound Syntax Tree" (if BST then "Showing" else "Hiding")
    | _ -> printfn "Invallid command"
  else
    let (BoundSyntaxTree, Diagnostics, ConcreteSyntaxTree) = Lex line |> Parse |> bind
    Console.ForegroundColor <- ConsoleColor.Gray
    if CST then printCST (SyntaxNode.CST ConcreteSyntaxTree)
    if BST then printBST (Expression BoundSyntaxTree)
    if Diagnostics.Length > 0 then
        Console.ForegroundColor <- ConsoleColor.Red
        Diagnostics |> Array.iter (printfn "%s")
    else
        Console.ForegroundColor <- ConsoleColor.Magenta
        evaluate BoundSyntaxTree |> printfn "%O"