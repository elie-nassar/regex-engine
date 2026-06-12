open Regex_lib.Ast

let test_parse input =
  try
    let ast = parse input in
    let result = string_of_regex ast in
    Printf.printf "Input: \"%s\" -> Output: \"%s\"\n" input result
  with ParseError msg ->
    Printf.printf "Input: \"%s\" -> Error: %s\n" input msg

let () =
  Printf.printf "Testing parse method:\n\n";
  test_parse "";
  test_parse "a";
  test_parse "ab";
  test_parse "a|b";
  test_parse "a*";
  test_parse "a*b";
  test_parse "(a|b)*";
  test_parse "a|b*";
  test_parse "(a|b)c";
  test_parse "a(b|c)d";
  Printf.printf "\nAll tests completed!\n"