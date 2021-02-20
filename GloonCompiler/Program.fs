open System
open Gloon.Compiler.Lexer
open Gloon.Compiler.Parser
open Gloon.Compiler.Types

let mutable Break = true

let rec prettyPrint indent first last (node: Node) =
    printfn "%s%s%O" indent (if first then "" else if last then "└──" else "├──") node
    let lastNode = node.getChildren() |> List.tryLast
    node.getChildren () |>
    List.iter (fun n -> prettyPrint (indent + (if not last then "│  " else if first then "" else "   ")) false (n = lastNode.Value) n)

while Break do
    Console.ForegroundColor <- ConsoleColor.Cyan
    printf "> "
    Console.ForegroundColor <- ConsoleColor.White
    Console.ReadLine()
    |> fun line ->
        if String.IsNullOrEmpty line
        then Break <- false
        else
            Lexer line |> Parser |> fun node -> Node.Expression node |> prettyPrint "" true true