type regex =
  | Epsilon
  | Char of char
  | Concat of regex * regex
  | Union of regex * regex
  | Star of regex

type token =
  | T_Char of char
  | T_Union
  | T_Star
  | T_LParen
  | T_RParen

let lex expression =
  let rec loop i acc =
    if i < 0 then acc
    else
      let tok = match expression.[i] with
        | '|' -> T_Union
        | '*' -> T_Star
        | '(' -> T_LParen
        | ')' -> T_RParen
        | c   -> T_Char c
      in
      loop (i - 1) (tok :: acc)
  in
  loop (String.length expression - 1) []

exception ParseError of string

let rec parse_atom tokens =
  match tokens with
  | T_Char c :: rest -> (Char c, rest)
  | T_LParen :: rest ->
      let (expr, remaining) = parse_expr rest in
      (match remaining with
       | T_RParen :: next -> (expr, next)
       | _ -> raise (ParseError "Expected closing parenthesis ')'"))
  | _ -> raise (ParseError "Unexpected token or empty expression")

and parse_factor tokens =
  let (atom, rest) = parse_atom tokens in
  match rest with
  | T_Star :: next -> (Star atom, next)
  | _ -> (atom, rest)

and parse_term tokens =
  let (factor, rest) = parse_factor tokens in
  match rest with
  | T_Char _ :: _ | T_LParen :: _ ->
      let (next_term, remaining) = parse_term rest in
      (Concat (factor, next_term), remaining)
  | _ -> (factor, rest)

and parse_expr tokens =
  let (term, rest) = parse_term tokens in
  match rest with
  | T_Union :: next ->
      let (next_expr, remaining) = parse_expr next in
      (Union (term, next_expr), remaining)
  | _ -> (term, rest)

let parse exression =
  if exression = "" then Epsilon
  else
    let tokens = lex exression in
    let (ast, remaining) = parse_expr tokens in
    match remaining with
    | [] -> ast
    | _ -> raise (ParseError "Trailing unparsed tokens")

let rec string_of_regex regex = match regex with
    | Epsilon -> ""
    | Char char -> String.make 1 char
    | Concat (regex1,regex2) -> "(" ^ string_of_regex regex1 ^ string_of_regex regex2 ^ ")"
    | Union (regex1,regex2) -> "(" ^ string_of_regex regex1 ^ "|" ^ string_of_regex regex2 ^ ")"
    | Star regex -> "(" ^ string_of_regex regex ^ ")*"

let print_regex regex = 
  Printf.printf "%s\n" regex