open Ast

type state = int
type 'a transition = state * 'a option * state

type nfa = {
  states : state list;
  alphabet : char list;
  transitions : char transition list;
  start : state;
  accepts : state list;
}

let next_state = ref 0

let fresh_state () =
  let s = !next_state in
  incr next_state;
  s

let reset_state_counter () =
  next_state := 0

let get_alphabet regex =
  let rec collect acc = function
    | Epsilon -> acc
    | Char c -> if List.mem c acc then acc else acc @ [c]
    | Concat (r1, r2) -> collect (collect acc r1) r2
    | Union (r1, r2) -> collect (collect acc r1) r2
    | Star r -> collect acc r
  in
  List.sort Char.compare (collect [] regex)

let nfa_epsilon () =
  let s = fresh_state () in
  {
    states = [s];
    alphabet = [];
    transitions = [];
    start = s;
    accepts = [s];
  }

let nfa_char c =
  let s1 = fresh_state () in
  let s2 = fresh_state () in
  {
    states = [s1; s2];
    alphabet = [c];
    transitions = [(s1, Some c, s2)];
    start = s1;
    accepts = [s2];
  }

let nfa_concat n1 n2 =
  let epsilon_trans = 
    List.map (fun accept -> (accept, None, n2.start)) n1.accepts
  in
  {
    states = n1.states @ n2.states;
    alphabet = List.sort_uniq Char.compare (n1.alphabet @ n2.alphabet);
    transitions = n1.transitions @ epsilon_trans @ n2.transitions;
    start = n1.start;
    accepts = n2.accepts;
  }

let nfa_union n1 n2 =
  let s_new = fresh_state () in
  let s_accept = fresh_state () in
  let epsilon_trans = 
    [(s_new, None, n1.start); (s_new, None, n2.start)] @
    (List.map (fun s -> (s, None, s_accept)) (n1.accepts @ n2.accepts))
  in
  {
    states = s_new :: n1.states @ n2.states @ [s_accept];
    alphabet = List.sort_uniq Char.compare (n1.alphabet @ n2.alphabet);
    transitions = epsilon_trans @ n1.transitions @ n2.transitions;
    start = s_new;
    accepts = [s_accept];
  }

let nfa_star n =
  let s_new = fresh_state () in
  let s_accept = fresh_state () in
  let epsilon_trans =
    [(s_new, None, n.start); (s_new, None, s_accept)] @
    (List.map (fun s -> (s, None, n.start)) n.accepts) @
    [(List.hd n.accepts, None, s_accept)]
  in
  {
    states = s_new :: n.states @ [s_accept];
    alphabet = n.alphabet;
    transitions = epsilon_trans @ n.transitions;
    start = s_new;
    accepts = [s_accept];
  }

let regex_to_nfa regex =
  let rec build = function
    | Epsilon -> nfa_epsilon ()
    | Char c -> nfa_char c
    | Concat (r1, r2) -> nfa_concat (build r1) (build r2)
    | Union (r1, r2) -> nfa_union (build r1) (build r2)
    | Star r -> nfa_star (build r)
  in
  build regex

let epsilon_closure nfa states =
  let rec closure visited queue =
    match queue with
    | [] -> visited
    | s :: rest ->
        if List.mem s visited then
          closure visited rest
        else
          let next_states =
            List.filter_map
              (fun (from, sym, to_) ->
                if from = s && sym = None then Some to_ else None)
              nfa.transitions
          in
          closure (visited @ [s]) (rest @ next_states)
  in
  let rec dedup = function
    | [] -> []
    | x :: xs -> if List.mem x xs then dedup xs else x :: dedup xs
  in
  dedup (closure [] states)

let move nfa states c =
  List.filter_map
    (fun (from, sym, to_) ->
      if List.mem from states && sym = Some c then Some to_ else None)
    nfa.transitions
