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

val canonicalize : expr -> expr
val strength_reduce : expr -> expr
