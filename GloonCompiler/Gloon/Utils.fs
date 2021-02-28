namespace Gloon

module Utils =

    open Gloon.Compiler.Syntax.Types
    open Gloon.Compiler.Binding.BoundTypes

    //https://connelhooley.uk/blog/2017/04/30/f-sharp-to-c-sharp

    let rec private printCST' indent first last (node: SyntaxNode) =
        printfn "%s%s%O" indent (if first then "" else if last then "└── " else "├── ") node
        let lastNode = node.Children |> List.tryLast
        node.Children |>
        List.iter (fun n -> printCST' (indent + (if not last then "│   " else if first then "" else "    ")) false (n = lastNode.Value) n)

    let printCST node = printCST' "" true true node

    let rec internal printBST' indent first last (node: BoundNode) =
        printfn "%s%s%O" indent (if first then "" else if last then "└── " else "├── ") node
        let lastNode = node.Children |> List.tryLast
        node.Children |>
        List.iter (fun n -> printBST' (indent + (if not last then "│   " else if first then "" else "    ")) false (n = lastNode.Value) n)

    let internal printBST node = printBST' "" true true node