/// Lexical analysis.
/// Create a list of tokens from a text of single file.
module rec LetPoly.Tokenize

open LetPoly.Types

// -----------------------------------------------
// Helpers
// -----------------------------------------------

type ScanAcc = (TokenKind * TextRange) list

let charNull: char = char 0

let charIsSpace (c: char): bool =
  c = ' ' || c = '\t' || c = '\r' || c = '\n'

let charIsDigit (c: char): bool =
  '0' <= c && c <= '9'

let charIsAlpha (c: char): bool =
  ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')

let charIsIdent (c: char): bool =
  c = '_' || charIsDigit c || charIsAlpha c

/// `s.[i..].StartsWith(prefix)`
let strNthStartsWith (i: int) (prefix: string) (s: string): bool =
  /// `s.[si..].StartsWith(prefix.[pi..])`
  let rec go pi si =
    pi = prefix.Length || (
      si < s.Length
      && prefix.[pi] = s.[si]
      && go (pi + 1) (si + 1)
    )
  i + prefix.Length <= s.Length && go 0 i

// -----------------------------------------------
// Scanners
// -----------------------------------------------

let scanError (acc: ScanAcc, text: string, i: int) =
  let endIndex = i + 1
  (TokenKind.Error, (i, endIndex)) :: acc, text, endIndex

let scanSpace (acc: ScanAcc, text: string, i: int) =
  assert (text.[i] |> charIsSpace)
  let rec go i =
    if i < text.Length && text.[i] |> charIsSpace then
      go (i + 1)
    else
      i
  acc, text, go i

/// Skip over the current line.
let scanLine (acc: ScanAcc, text: string, i: int) =
  let rec go i =
    if i = text.Length then
      i
    else if text.[i] = '\n' then
      i + 1
    else
      go (i + 1)
  acc, text, go i

let scanIdent (acc: ScanAcc, text: string, i: int) =
  assert (text.[i] |> charIsIdent && text.[i] |> charIsDigit |> not)
  let rec go i =
    if i < text.Length && text.[i] |> charIsIdent then
      go (i + 1)
    else
      i
  let endIndex = go i
  (TokenKind.Ident, (i, endIndex)) :: acc, text, endIndex

let scanAll (text: string) =
  let rec go (acc, text, i) =
    let t = acc, text, i
    let follow prefix = text |> strNthStartsWith i prefix

    if i >= text.Length then
      text, acc |> List.rev
    else if text.[i] |> charIsSpace then
      t |> scanSpace |> go
    else if follow "//" then
      t |> scanLine |> go
    else if follow "(" then
      ((TokenKind.ParenL, (i, i + 1)) :: acc, text, i + 1) |> go
    else if follow ")" then
      ((TokenKind.ParenR, (i, i + 1)) :: acc, text, i + 1) |> go
    else if follow "." then
      ((TokenKind.Dot, (i, i + 1)) :: acc, text, i + 1) |> go
    else if follow "\\" then
      ((TokenKind.Lambda, (i, i + 1)) :: acc, text, i + 1) |> go
    else if follow ";" then
      ((TokenKind.Semi, (i, i + 1)) :: acc, text, i + 1) |> go
    else if text.[i] |> charIsIdent then
      t |> scanIdent |> go
    else
      t |> scanError |> go
  go ([], text, 0)

let tokenize (text: string): string * Token list =
  text |> scanAll
