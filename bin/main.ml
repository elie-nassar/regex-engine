open Regex_lib.Ast
open Regex_lib.Dfa

(* let test_parse input =
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
  Printf.printf "\nAll tests completed!\n" *)

let test_match pattern input = 
  try
    let ast = parse pattern in
    let dfa = regex_to_dfa ast in
    let result = dfa_match dfa input in
    Printf.printf "Pattern: \"%s\", Input: \"%s\" -> Match: %b\n" pattern input result
  with ParseError msg ->
    Printf.printf "Pattern: \"%s\" -> Error: %s\n" pattern msg

let () =
  Printf.printf "Testing regex matching method:\n\n";
  test_match "a" "a";
  test_match "a" "b";
  test_match "ab" "ab";
  test_match "ab" "abc";
  test_match "a|b" "a";
  test_match "a|b" "b";
  test_match "a|b" "c";
  test_match "a*" "";
  test_match "a*" "a";
  test_match "a*" "aa";
  test_match "a*b" "b";
  test_match "a*b" "ab";
  test_match "a*b" "aab";
  test_match "(a|b)*" "";
  test_match "(a|b)*" "ab";
  test_match "(a|b)*" "aba";
  test_match "a|b*" "a";
  test_match "a|b*" "b";
  test_match "a|b*" "bb";
  test_match "(a|b)c" "ac";
  test_match "(a|b)c" "bc";
  test_match "(a|b)c" "c";
  test_match "a(b|c)d" "abd";
  test_match "a(b|c)d" "acd";
  test_match "a(b|c)d" "ad";
  Printf.printf "\nAll tests completed!\n"