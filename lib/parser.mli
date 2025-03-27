(** Copyright 2025-2025, Unwale *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type expr =
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Int of int
  | Variable of string

type program_data = {
  args_names : string list;
  function_body : expr;
  args : int list;
}

type 'a parser_result = Failed | Parsed of 'a * char list

val parse : char list -> program_data parser_result
