(** Copyright 2025-2025, Unwale *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type expr =
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Shl of expr * int
  | Shr of expr * int
  | Int of int
  | Variable of string

let fold_constants expr =
  let rec helper = function
    | Add (Int x, Int y) -> Int (x + y)
    | Sub (Int x, Int y) -> Int (x - y)
    | Mul (Int x, Int y) -> Int (x * y)
    | Div (Int x, Int y) -> Int (x / y)
    | Add (left, right) -> (
        let folded_left = helper left in
        let folded_right = helper right in
        match (folded_left, folded_right) with
        | Int n1, Int n2 -> Int (n1 + n2)
        | Int n1, Add (Int n2, any) -> Add (Int (n1 + n2), any)
        | Add (Int n1, any), Int n2 -> Add (Int (n1 + n2), any)
        | Add (Int n1, any1), Add (Int n2, any2) ->
            Add (Int (n1 + n2), Add (any1, any2))
        | _ -> Add (folded_left, folded_right))
    | Mul (left, right) -> (
        let folded_left = helper left in
        let folded_right = helper right in
        match (folded_left, folded_right) with
        | Int n1, Int n2 -> Int (n1 * n2)
        | Int n1, Mul (Int n2, any) -> Mul (Int (n1 * n2), any)
        | Mul (Int n1, any), Int n2 -> Mul (Int (n1 * n2), any)
        | Mul (Int n1, any1), Mul (Int n2, any2) ->
            Mul (Int (n1 * n2), Mul (any1, any2))
        | _ -> Mul (folded_left, folded_right))
    (* TODO: убрать, когда деление на константу будет заменено на умножение*)
    | Div (left, right) -> (
        let folded_left = helper left in
        let folded_right = helper right in
        match (folded_left, folded_right) with
        | Int n1, Int n2 when n2 <> 0 -> Int (n1 / n2)
        | _ -> Div (folded_left, folded_right))
    | other -> other
  in
  helper expr

let rec sink_constants expr =
  match expr with
  | Add (left, right) -> (
      let sorted_left = sink_constants left in
      let sorted_right = sink_constants right in
      match (sorted_left, sorted_right) with
      | _, Int _ -> Add (sorted_right, sorted_left)
      | _ -> Add (sorted_left, sorted_right))
  | Mul (left, right) -> (
      let sorted_left = sink_constants left in
      let sorted_right = sink_constants right in
      match (sorted_left, sorted_right) with
      | _, Int _ -> Mul (sorted_right, sorted_left)
      | _ -> Mul (sorted_left, sorted_right))
  | Sub (left, Int x) -> Add (Int (-x), sink_constants left)
  | Sub (left, right) -> Sub (sink_constants left, sink_constants right)
  (*TODO: заменить деление на константу на умножение*)
  | Div (left, right) -> Div (sink_constants left, sink_constants right)
  | _ -> expr

let remove_neutral_operations = function
  | Add (Int 0, x) -> x
  | Sub (x, Int 0) -> x
  | Mul (Int 1, x) -> x
  | Mul (Int 0, _) -> Int 0
  | Div (x, Int 1) -> x
  | Div (Int 0, _) -> Int 0
  | other -> other

let apply_until_stable f x =
  let rec helper x =
    let y = f x in
    if x = y then x else helper y
  in
  helper x

let canonicalize expr =
  apply_until_stable
    (fun x ->
      x |> sink_constants |> fold_constants |> remove_neutral_operations)
    expr

let strength_reduce expr =
  let is_power_of_two = function
    | n when n <= 0 -> false
    | n -> n land (n - 1) = 0
  in
  let log2 n = int_of_float (log (float_of_int n) /. log 2.0) in
  let rec helper = function
    | Add (x, y) -> Add (helper x, helper y)
    | Sub (x, y) -> Sub (helper x, helper y)
    | Mul (Int y, x) when is_power_of_two y ->
        let shift = log2 y in
        Shl (helper x, shift)
    | Mul (x, y) -> Mul (helper x, helper y)
    | Div (x, Int y) when is_power_of_two y ->
        let shift = log2 y in
        Shr (helper x, shift)
    | Div (x, y) -> Div (helper x, helper y)
    | other -> other
  in
  helper expr
