namespace Gloon

module Utils =

    open Gloon.Compiler.Syntax.Types

    //https://connelhooley.uk/blog/2017/04/30/f-sharp-to-c-sharp

    let mutable Break = false
    let mutable CST = false

    let rec prettyPrint indent first last (node: SyntaxNode) =
        printfn "%s%s%O" indent (if first then "" else if last then "└── " else "├── ") node
        let lastNode = node.getChildren() |> List.tryLast
        node.getChildren () |>
        List.iter (fun n -> prettyPrint (indent + (if not last then "│   " else if first then "" else "    ")) false (n = lastNode.Value) n)