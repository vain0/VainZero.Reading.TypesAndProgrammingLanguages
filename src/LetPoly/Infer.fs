module rec LetPoly.Infer

open LetPoly.Types
open LetPoly.Helpers

let intEq (l: int) (r: int) = l = r

let assocFind eq key assoc =
  let rec go assoc =
    match assoc with
    | [] ->
      None

    | (theKey, value) :: _ when eq theKey key ->
      Some value

    | _ :: assoc ->
      go assoc

  go assoc

let tyFreshMeta serial =
  let i, serial = serialNext serial
  Ty.Meta i, serial

let typeContextFind dbi ctx =
  ctx |> List.item dbi

// (term, state) -> (ty, state) where
// state = (serial, constraints)
let inferTerm term =
  let rec go ctx (term, (serial, acc)) =
    match term with
    | Term.BoolLit _ ->
      Ty.Bool, (serial, acc)

    | Term.IntLit _ ->
      Ty.Nat, (serial, acc)

    | Term.Var (_, name, dbi, _) ->
      let ty = ctx |> typeContextFind dbi
      eprintfn "%s : %A" name ty
      ty, (serial, acc)

    | Term.Abs (_, _, _, body) ->
      let sTy, serial = tyFreshMeta serial
      let tTy, (serial, acc) = go (sTy :: ctx) (body, (serial, acc))
      let calTy = Ty.Fun (sTy, tTy)
      eprintfn "%A : %A" term calTy
      calTy, (serial, acc)

    | Term.App (_, cal, arg) ->
      let calTy, (serial, acc) = go ctx (cal, (serial, acc))
      let argTy, (serial, acc) = go ctx (arg, (serial, acc))
      let appTy, serial = tyFreshMeta serial
      let acc = (calTy, Ty.Fun (argTy, appTy)) :: acc
      eprintfn "%A : %A ~ %A" term appTy (Ty.Fun (argTy, appTy))
      appTy, (serial, acc)

  let globalTypeCtx =
    [
      // is_zero
      Ty.Fun (Ty.Nat, Ty.Bool)
    ]

  go globalTypeCtx term

let tySubst meta newTy ty =
  let rec go ty =
    match ty with
    | Ty.Meta theMeta when meta = theMeta ->
      newTy

    | Ty.Any
    | Ty.Bool
    | Ty.Nat
    | Ty.Meta _ ->
      ty

    | Ty.Fun (sTy, tTy) ->
      let sTy = go sTy
      let tTy = go tTy
      Ty.Fun (sTy, tTy)

  go ty

let constraintsSubst meta newTy constraints =
  constraints |> List.map (fun (sTy, tTy) -> tySubst meta newTy sTy, tySubst meta newTy tTy)

let tyMetaOccursIn ty meta =
  let rec go ty =
    match ty with
    | Ty.Any
    | Ty.Bool
    | Ty.Nat ->
      false

    | Ty.Meta theMeta ->
      theMeta = meta

    | Ty.Fun (sTy, tTy) ->
      go sTy || go tTy

  go ty

let unify _ constraints =
  let rec go substs constraints =
    match constraints with
    | [] ->
      List.rev substs

    | (Ty.Meta l, Ty.Meta r) :: constraints when l = r ->
      go substs constraints

    | (lTy, Ty.Meta meta) :: _ when meta |> tyMetaOccursIn lTy ->
      failwith "infinite type"

    | (lTy, Ty.Meta meta) :: constraints ->
      let constraints = constraintsSubst meta lTy constraints
      go ((meta, lTy) :: substs) constraints

    | (Ty.Meta meta, rTy) :: _ when meta |> tyMetaOccursIn rTy ->
      failwith "infinite type"

    | (Ty.Meta meta, rTy) :: constraints ->
      let constraints = constraintsSubst meta rTy constraints
      go ((meta, rTy) :: substs) constraints

    | (Ty.Nat, Ty.Nat) :: constraints
    | (Ty.Bool, Ty.Bool) :: constraints ->
      go substs constraints

    | (Ty.Fun (lsTy, ltTy), Ty.Fun (rsTy, rtTy)) :: constraints ->
      go substs ((lsTy, rsTy) :: (ltTy, rtTy) :: constraints)

    | (lTy, rTy) :: _ ->
      failwithf "Type error %A %A" lTy rTy

  go [] constraints

let tyApplySubst substs ty =
  let rec go ty substs =
    match substs with
    | [] ->
      ty

    | (meta, newTy) :: substs ->
      go (tySubst meta newTy ty) substs

  go ty substs
