namespace Gloon.Symbols

open System

[<StructAttribute>]
type VariableSymbol =
  val Name: string
  val Type: Type
  val IsReadOnly: bool
  internal new (name, type_, isReadOnly) = {Name = name; Type = type_; IsReadOnly = isReadOnly}