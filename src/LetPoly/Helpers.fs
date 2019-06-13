module rec LetPoly.Helpers

open LetPoly.Types

let optionListCollect xs =
  let rec go acc xs =
    match xs with
    | [] ->
      acc |> List.rev |> Some
    | None :: _ ->
      None
    | Some x :: xs ->
      go (x :: acc) xs

  go [] xs

/// `listMapWithState f (xs, state) = (ys, state)` where
/// `f: (xs, state) -> (y, state)`
let listMapWithState f (xs, state) =
  let rec go acc state xs =
    match xs with
    | [] ->
      List.rev acc, state

    | x :: xs ->
      let y, state = f (x, state)
      let acc = y :: acc
      go acc state xs

  go [] state xs

let strSlice start endIndex (s: string) =
  if start >= endIndex then
    ""
  else
    s.[start..endIndex - 1]

let serialNext (serial: Serial): int * Serial =
  serial + 1, serial + 1

let noId: Serial = 0

let noDbi: DeBruijnIndex = 0

let noCtxLen: ContextLength = 0

let noRange: TextRange = 0, 0
