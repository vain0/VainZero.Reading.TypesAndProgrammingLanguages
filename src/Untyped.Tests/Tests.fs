module Tests

open System.Collections.Generic
open Xunit
open Untyped.Program

let is<'T> (expected: 'T) (actual: 'T) =
  Assert.Equal(expected, actual)

let trim (s: string) =
  s.TrimStart().TrimEnd()

[<Fact>]
let testParseId () =
  run """\x. x""" |> trim |> is """\x. x"""

[<Fact>]
let testParseCombinator () =
  run """\f. (\x. f (x x)) (\x. f (x x))"""
  |> trim
  |> is """\f. (\x. f (x x)) (\x. f (x x))"""

[<Fact>]
let testEval () =
  run """
    (\add. \two. \three. add two three)
      (\l. \r. \f. \x. r f (l f x))
      (\f. \x. f (f x))
      (\f. \x. f (f (f x)))
      (\x. (\_. x))
      (\x. x)
    """ |> trim |> is """\_. \_. \_. \_. \_. \x. x"""

[<Fact>]
let testDump () =
  run """(\ f . (\ x . (\ y . ((f x) y))))""" |> trim |> is """\f. \x. \y. f x y"""

[<Fact>]
let testIncompleteInput () =
  let src = """x/;
x;
x + x;
\f x y ((x y);"""
  let expected = """(1, 2)..(1, 3) ERROR
  Expected ';'
(1, 2)..(1, 3) ERROR
  Unexpected token
(1, 3)..(1, 4) ERROR
  Unexpected token
(3, 3)..(3, 4) ERROR
  Expected ';'
(3, 3)..(3, 4) ERROR
  Unexpected token
(4, 4)..(4, 5) ERROR
  Expected '.'
(4, 8)..(4, 9) ERROR
  Not closed by ')'"""

  src |> run |> trim |> is expected

[<Fact>]
let testLambda () =
  let src = """// Examples for testing

x;

\ x. x;
(\ x. x) (\ x. x x);
"""
  let expected = """x
\x. x
\x. x x"""
  src |> run |> trim |> is expected
