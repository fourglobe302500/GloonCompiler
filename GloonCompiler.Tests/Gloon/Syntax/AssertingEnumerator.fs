namespace Gloon.Tests.Syntax

open Gloon.Syntax

open System
open System.Linq
open System.Collections.Generic

open Xunit

type AssertingEnumerator (node: SyntaxNode) =
  let enumerator = AssertingEnumerator.Flatten node
  let mutable hasError = false

  static member private Flatten node :IEnumerator<SyntaxNode> =
    seq {
      let stack = new Stack<SyntaxNode>()
      stack.Push (node)
      while stack.Count > 0 do
        let n = stack.Pop()
        yield n
        for child in n.Children.Reverse() do
          stack.Push(child)
    } |> fun seq -> seq.GetEnumerator()

  member _.AssertToken kind text =
    try
      Assert.True(enumerator.MoveNext())
      match enumerator.Current with
      | Token t ->
        Assert.Equal(kind, t.Kind)
        Assert.Equal(text, t.Text)
      | _ -> Assert.False(true)
    with
    | a ->
      hasError <- true
      raise a

  member _.AssertNode kind =
    try
      Assert.True(enumerator.MoveNext())
      match enumerator.Current with
      | Expression e -> Assert.Equal(kind, e)
      | _ -> Assert.False(true)
    with
    | a ->
      hasError <- true
      raise a

  interface IDisposable with
    member this.Dispose(): unit =
      if not hasError then
        Assert.False(enumerator.MoveNext())
      enumerator.Dispose()