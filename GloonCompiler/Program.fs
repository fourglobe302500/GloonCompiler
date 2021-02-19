open System

let mutable Break = true

while Break do
    Console.ForegroundColor <- ConsoleColor.Cyan
    printf "> "
    Console.ForegroundColor <- ConsoleColor.White
    Console.ReadLine()
    |> fun line ->
        if String.IsNullOrEmpty line
        then Break <- false
        else
            printf "%s" line