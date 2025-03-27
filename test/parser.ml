open Alcotest
open Hw4.Parser

let pp_program_data fmt data =
  let args_names = String.concat ", " data.args_names in
  let args = String.concat ", " (List.map string_of_int data.args) in
  Fmt.pf fmt "{ args_names = [%s]; args = [%s] }" args_names args

let program_data = testable pp_program_data ( = )

let test_parser name input expected =
  match parse input with
  | Failed -> Alcotest.fail "Parsing failed"
  | Parsed (data, _) -> Alcotest.check program_data name expected data

let test_cases =
  [
    ( "test_case_1",
      List.of_seq
        (String.to_seq "let f x y z = 0 \nlet main = print_int (f 2 3 4)"),
      {
        args_names = [ "x"; "y"; "z" ];
        function_body = Int 0;
        args = [ 2; 3; 4 ];
      } );
    ( "test_case_2",
      List.of_seq
        (String.to_seq "let f a b = a + b \nlet main = print_int (f 5 10)"),
      {
        args_names = [ "a"; "b" ];
        function_body = Add (Variable "a", Variable "b");
        args = [ 5; 10 ];
      } );
    ( "test_case_3",
      List.of_seq
        (String.to_seq
           "let f x y z = x * y - z \nlet main = print_int (f 7 8 9)"),
      {
        args_names = [ "x"; "y"; "z" ];
        function_body = Sub (Mul (Variable "x", Variable "y"), Variable "z");
        args = [ 7; 8; 9 ];
      } );
  ]
  |> List.map (fun (name, input, expected) ->
         (name, `Quick, fun () -> test_parser name input expected))

let () = Alcotest.run "Parser Tests" [ ("parser", test_cases) ]
