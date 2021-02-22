open System
open Gloon.Compiler.Syntax.Lexer
open Gloon.Compiler.Syntax.Parser
open Gloon.Evaluation.Evaluator
open Gloon.Types

//https://connelhooley.uk/blog/2017/04/30/f-sharp-to-c-sharp

let mutable Break = false
let mutable AST = false

let rec prettyPrint indent first last (node: Node) =
    printfn "%s%s%O" indent (if first then "" else if last then "└── " else "├── ") node
    let lastNode = node.getChildren() |> List.tryLast
    node.getChildren () |>
    List.iter (fun n -> prettyPrint (indent + (if not last then "│   " else if first then "" else "    ")) false (n = lastNode.Value) n)

let Process line =
    line |> Lexer |> Parser |> fun (tree: AST) ->
            if AST then Node.AST tree |> prettyPrint "" true true
            if tree.Diagnostics.Length > 0
            then
                Console.ForegroundColor <- ConsoleColor.Red
                tree.Diagnostics |> List.iter (printfn "%s")
            else
                Console.ForegroundColor <- ConsoleColor.Gray
                tree.Root |> Evaluate |> printfn "%i"

let Sudo =
    function
    | "#cls" -> Console.Clear ()
    | "#quit" -> Break <- true
    | "#AST" ->
        AST <- not AST
        printfn "%s AST" (if AST then "Showing" else "Hiding")
    | _ -> printfn "invallid Command"

while not Break do
    Console.ForegroundColor <- ConsoleColor.Cyan
    printf "> "
    Console.ForegroundColor <- ConsoleColor.White
    Console.ReadLine()
    |> fun line ->
        if String.IsNullOrEmpty line
        then Break <- true
        elif line.StartsWith '#'
        then line |> Sudo
        else line |> Process 