namespace Gloon.Symbols

open System

[<StructAttribute>]
type VariableSymbol =
  val Name: string
  val Type: Type
  internal new (name, type_) = {Name = name; Type = type_}