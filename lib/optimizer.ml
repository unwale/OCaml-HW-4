(** Copyright 2025-2025, Unwale *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type expr =
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Int of int
  | Variable of string

let fold_constants expr =
  let rec helper = function
    | Add (Int x, Int y) -> Int (x + y)
    | Sub (Int x, Int y) -> Int (x - y)
    | Mul (Int x, Int y) -> Int (x * y)
    | Div (Int x, Int y) -> Int (x / y)
    | Add (x, y) -> Add (helper x, helper y)
    | Sub (x, y) -> Sub (helper x, helper y)
    | Mul (x, y) -> Mul (helper x, helper y)
    | Div (x, y) -> Div (helper x, helper y)
    | other -> other
  in
  helper expr
