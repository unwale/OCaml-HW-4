open Hw4.Optimizer
open Alcotest

let rec pp_expr fmt = function
  | Add (x, y) -> Format.fprintf fmt "(%a + %a)" pp_expr x pp_expr y
  | Sub (x, y) -> Format.fprintf fmt "(%a - %a)" pp_expr x pp_expr y
  | Mul (x, y) -> Format.fprintf fmt "(%a * %a)" pp_expr x pp_expr y
  | Div (x, y) -> Format.fprintf fmt "(%a / %a)" pp_expr x pp_expr y
  | Int n -> Format.fprintf fmt "%d" n
  | Variable v -> Format.fprintf fmt "%s" v

let expr_testable = testable pp_expr ( = )

let canonicalization =
  [
    ( "addition of constants",
      `Quick,
      fun () ->
        let expr = Add (Int 1, Int 2) in
        let optimized = canonicalize expr in
        Alcotest.check expr_testable "optimized" (Int 3) optimized );
    ( "subtraction of constants",
      `Quick,
      fun () ->
        let expr = Sub (Int 5, Int 2) in
        let optimized = canonicalize expr in
        Alcotest.check expr_testable "optimized" (Int 3) optimized );
    ( "multiplication of constants",
      `Quick,
      fun () ->
        let expr = Mul (Int 3, Int 4) in
        let optimized = canonicalize expr in
        Alcotest.check expr_testable "optimized" (Int 12) optimized );
    ( "division of constants",
      `Quick,
      fun () ->
        let expr = Div (Int 8, Int 2) in
        let optimized = canonicalize expr in
        Alcotest.check expr_testable "optimized" (Int 4) optimized );
    ( "constant sinking with variables",
      `Quick,
      fun () ->
        let expr = Add (Variable "x", Int 2) in
        let optimized = canonicalize expr in
        Alcotest.check expr_testable "optimized"
          (Add (Int 2, Variable "x"))
          optimized );
    ( "complex expression",
      `Quick,
      fun () ->
        let expr = Add (Mul (Int 2, Int 3), Sub (Int 5, Int 2)) in
        let optimized = canonicalize expr in
        Alcotest.check expr_testable "optimized" (Int 9) optimized );
    ( "nested addition",
      `Quick,
      fun () ->
        let expr = Add (Mul (Int 2, Int 3), Sub (Variable "x", Int 2)) in
        let optimized = canonicalize expr in
        Alcotest.check expr_testable "optimized"
          (Add (Int 4, Variable "x"))
          optimized );
    ( "nested multiplication",
      `Quick,
      fun () ->
        let expr = Mul (Mul (Int 2, Int 3), Mul (Variable "a", Int 4)) in
        let optimized = canonicalize expr in
        Alcotest.check expr_testable "optimized"
          (Mul (Int 24, Variable "a"))
          optimized );
    ( "nested with 2 variables",
      `Quick,
      fun () ->
        let expr = Mul (Mul (Variable "a", Int 3), Mul (Variable "b", Int 4)) in
        let optimized = canonicalize expr in
        Alcotest.check expr_testable "optimized"
          (Mul (Int 12, Mul (Variable "a", Variable "b")))
          optimized );
  ]

let () =
  Alcotest.run "Optimizer tests" [ ("canonicalization", canonicalization) ]
