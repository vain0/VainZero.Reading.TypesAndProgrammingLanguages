module rec LetPoly.Syntax

open LetPoly.Types
open LetPoly.Helpers
open LetPoly.Tokenize
open LetPoly.Parse

// -----------------------------------------------
// Syntax transformation
// -----------------------------------------------

let synCollectErrors (syn: Syn): Syn * SynError list =
  let rec go (syn, acc) =
    match syn with
    | Syn.Error (msg, range, children) ->
      let acc = SynError (msg, range) :: acc
      match children with
      | None ->
        Syn.Missing, acc
      | Some syn ->
        // Replace the error with child.
        go (syn, acc)

    | Syn.Missing
    | Syn.Token _ ->
      syn, acc

    | Syn.Node (synId, kind, children) ->
      let children, acc = (children, acc) |> listMapWithState go
      Syn.Node (synId, kind, children), acc

  let syn, acc = go (syn, [])
  let errors = acc |> List.rev
  syn, errors

let synWithIds (syn: Syn, serial: int): Syn * Serial =
  let rec go (syn, serial) =
    match syn with
    | Syn.Error (_, _, None)
    | Syn.Missing
    | Syn.Token _ ->
      syn, serial

    | Syn.Error (msg, range, Some child) ->
      let child, serial = go (child, serial)
      Syn.Error (msg, range, Some child), serial

    | Syn.Node (_, kind, children) ->
      let synId, serial = serialNext serial
      let children, serial = (children, serial) |> listMapWithState go
      Syn.Node (synId, kind, children), serial

  go (syn, serial)

let synBuild (text: string, serial: Serial) =
  let text, tokens = tokenize text
  let text, syn = parse (text, tokens)
  let syn, errors = synCollectErrors syn
  let syn, serial = synWithIds (syn, serial)
  text, syn, errors, serial
