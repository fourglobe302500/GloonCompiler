namespace Gloon

module Utils =

    open Gloon.Compiler.Syntax.Types
    open Gloon.Compiler.Binding.BoundTypes

    //https://connelhooley.uk/blog/2017/04/30/f-sharp-to-c-sharp

    let rec private PrintCST' indent first last (node: SyntaxNode) =
        printfn "%s%s%O" indent (if first then "" else if last then "└── " else "├── ") node
        let lastNode = node.Children |> List.tryLast
        node.Children |>
        List.iter (fun n -> PrintCST' (indent + (if not last then "│   " else if first then "" else "    ")) false (n = lastNode.Value) n)

    let PrintCST node = PrintCST' "" true true node

    let rec PrintBST' indent first last (node: BoundNode) =
        printfn "%s%s%O" indent (if first then "" else if last then "└── " else "├── ") node
        let lastNode = node.Children |> List.tryLast
        node.Children |>
        List.iter (fun n -> PrintBST' (indent + (if not last then "│   " else if first then "" else "    ")) false (n = lastNode.Value) n)

    let PrintBST node = PrintBST' "" true true node