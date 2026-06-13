open Nfa
open Ast

type dfa = {
  dfa_states: state list;
  dfa_alphabet: char list;
  dfa_transitions: (state * char * state) list;
  dfa_start: state;
  dfa_accepts: state list;
}

let nfa_to_dfa nfa =
  reset_state_counter ();
  
  let state_map = Hashtbl.create 100 in
  let new_states = ref [] in
  let new_transitions = ref [] in
  let state_counter = ref 0 in
  
  let get_or_create_state set =
    let key = String.concat "," (List.map string_of_int (List.sort Int.compare set)) in
    try
      Hashtbl.find state_map key
    with Not_found ->
      let new_st = !state_counter in
      incr state_counter;
      Hashtbl.add state_map key new_st;
      new_states := !new_states @ [new_st];
      new_st
  in
  
  let start_set = epsilon_closure nfa [nfa.start] in
  let start_dfa = get_or_create_state start_set in
  
  let rec process_queue queue visited =
    match queue with
    | [] -> ()
    | nfa_set :: rest ->
        if List.mem nfa_set visited then
          process_queue rest visited
        else
          let dfa_state = get_or_create_state nfa_set in
          List.iter
            (fun c ->
              let new_nfa_set =
                let moved = move nfa nfa_set c in
                epsilon_closure nfa moved
              in
              if new_nfa_set <> [] then (
                let new_dfa_state = get_or_create_state new_nfa_set in
                new_transitions := !new_transitions @ [(dfa_state, c, new_dfa_state)];
                process_queue (rest @ [new_nfa_set]) (visited @ [nfa_set])
              ) else
                process_queue (rest @ [new_nfa_set]) (visited @ [nfa_set])
            )
            nfa.alphabet;
          process_queue rest (visited @ [nfa_set])
  in
  
  process_queue [start_set] [];
  
  let dfa_accepts =
    Hashtbl.fold
      (fun key dfa_state acc ->
        let nfa_set = 
          String.split_on_char ',' key
          |> List.filter (fun s -> s <> "")
          |> List.map int_of_string
        in
        if List.exists (fun s -> List.mem s nfa.accepts) nfa_set then
          acc @ [dfa_state]
        else
          acc)
      state_map []
  in
  
  {
    dfa_states = !new_states;
    dfa_alphabet = nfa.alphabet;
    dfa_transitions = !new_transitions;
    dfa_start = start_dfa;
    dfa_accepts = dfa_accepts;
  }
 
let regex_to_dfa regex =
  reset_state_counter ();
  let nfa = regex_to_nfa regex in
  nfa_to_dfa nfa
 
let dfa_match dfa input =
  let rec process state idx =
    if idx >= String.length input then
      List.mem state dfa.dfa_accepts
    else
      let c = input.[idx] in
      try
        let (_, _, next_state) =
          List.find
            (fun (from, sym, _) -> from = state && sym = c)
            dfa.dfa_transitions
        in
        process next_state (idx + 1)
      with Not_found ->
        false
  in
  process dfa.dfa_start 0

let regex_match pattern input = 
  let dfa = regex_to_dfa (parse pattern) in
  dfa_match dfa input