module Tests

open System
open Xunit
open LetPoly.Program

let is<'T> (expected: 'T) (actual: 'T) =
  Assert.Equal(expected, actual)

let trim (s: string) =
  s.TrimStart().TrimEnd()

[<Fact>]
let testParseIntLit () =
  run """\_. 42""" |> trim |> is """\_. 42 : ? -> Nat"""
