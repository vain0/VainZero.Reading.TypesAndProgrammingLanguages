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

[<Fact>]
let testParseBoolLit () =
  run """true""" |> trim |> is """true : Bool"""
  run """\_. false""" |> trim |> is """\_. false : ? -> Bool"""

  run """\_. falseTrue""" |> trim |> is """\_. falseTrue : ? -> ?"""

[<Fact>]
let testInference () =
  let src = """\f. \g. f (g (g true (f true)) 1)"""
  let expected = """\f. \g. f (g (g true (f true)) 1) : (Bool -> Nat) -> (Bool -> Nat -> Bool) -> Nat"""
  src |> run |> trim |> is expected

[<Fact>]
let testGoal () =
  let src = """
    let headOr x xs =
      let aux x xs =
        case xs of
          | [] -> x
          | y :: _ -> y in
      aux x xs in
    do assert (isZero (headOr 0 [])) in
    do assert (headOr false (true :: [])) in
    0
  """
  let expected = """0 : Nat"""
  // src |> run |> trim |> is expected
  ()
