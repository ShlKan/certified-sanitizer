open Automata_lib
open Rel

let int_to_nat i =
  if i < 0 then failwith "int_to_nat: negative integer"
  else Automata_lib.nat_of_integer (Z.of_int i)

let pair_int_to_Z (i1, i2) = (Z.of_int i1, Z.of_int i2)

let transition_to_nat (q, ls, c, q') =
  let ls' =
    match ls with [] -> None | _ -> Some (List.map pair_int_to_Z ls)
  in
  (int_to_nat q, ((ls', Z.of_int c), int_to_nat q'))

let nft_construct (states, transitions, init, accepts, ofun) =
  let n_states = List.map int_to_nat states in
  let n_transitions = List.map transition_to_nat transitions in
  let n_init = List.map int_to_nat init in
  let n_accepts = List.map int_to_nat accepts in
  Automata_lib.rs_nft_construct_interval
    (nFA_states_nat, linorder_nat)
    (equal_Z, linorder_Z) linorder_Z linorder_Z_list
    (n_states, (n_transitions, (n_init, (n_accepts, ofun))))

let nft_product nft =
  Automata_lib.rs_product_transducer
    (nFA_states_nat, linorder_nat)
    (equal_Z, linorder_Z) linorder_Z
    (equal_Z, linorder_Z_list)
    nft

let nfa_normal nfa =
  Automata_lib.rs_nfa_normal
    (nFA_states_nat, linorder_nat)
    (equal_Z, linorder_Z_list)
    nfa

let fmap ff e =
  match e with
  | [] -> None
  | (a, b) :: l ->
      let l = ff (Some a) in
      let r = ff (Some b) in
      if l = None then None else l

let nfa_construct (q, d, i, f) =
  Automata_lib.rs_nfa_construct_interval
    (nFA_states_nat, linorder_nat)
    (equal_Z, linorder_Z)
    ( List.map z_to_int q
    , ( List.map
          (fun (a, l, c) ->
            ( z_to_int a
            , ( List.map (fun (l, r) -> (Z.of_int l, Z.of_int r)) l
              , z_to_int c ) ) )
          d
      , (List.map z_to_int i, List.map z_to_int f) ) )

let nfa_destruct nfa =
  Automata_lib.rs_nfa_destruct
    (nFA_states_nat, linorder_nat)
    (equal_Z, linorder_Z_list)
    nfa

let nfa_construct_reachable nfa =
  Automata_lib.rs_nfa_construct_reachable
    (nFA_states_nat, linorder_nat)
    (equal_Z, linorder_Z) nfa

let nfa_elim nfa =
  Automata_lib.rs_nfa_elim
    ( Automata_lib.equal_prod equal_nat equal_nat
    , nFA_states_natnat
    , linorder_natnat )
    (equal_Z, linorder_Z_list)
    nfa

let rec print_list l =
  match l with
  | [] -> Format.printf "\n"
  | a :: rl ->
      Format.printf "%d" (Z.to_int (Automata_lib.integer_of_nat a)) ;
      Format.printf "; " ;
      print_list rl

let rec print_pair l =
  match l with
  | [] -> print_string "\n"
  | (a1, a2) :: rl ->
      print_string "(" ;
      print_int (Z.to_int (Automata_lib.integer_of_nat a1)) ;
      print_string ", " ;
      print_int (Z.to_int (Automata_lib.integer_of_nat a2)) ;
      print_string "); " ;
      print_pair rl

let rec print_rc l =
  match l with
  | [] -> print_string ""
  | (a, l) :: rl ->
      print_int (Z.to_int (Automata_lib.integer_of_nat a)) ;
      print_pair l ;
      print_string "; " ;
      print_rc rl

let rec print_tran l =
  match l with
  | [] -> Format.printf "\n"
  | (a, (l, c)) :: rl ->
      Format.printf "(" ;
      Format.printf "%d" (Z.to_int (Automata_lib.integer_of_nat a)) ;
      Format.printf ", " ;
      List.iter
        (fun (l, r) ->
          Format.printf "[%a, %a]"
            (Format.pp_print_list
               ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
               Format.pp_print_int )
            (List.map Z.to_int l)
            (Format.pp_print_list
               ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
               Format.pp_print_int )
            (List.map Z.to_int r) )
        l ;
      Format.printf ", " ;
      Format.printf "%d" (Z.to_int (Automata_lib.integer_of_nat c)) ;
      Format.printf ")\n" ;
      print_tran rl

let rec print_tran_str l =
  match l with
  | [] -> Format.printf "\n"
  | (a, (l, c)) :: rl ->
      Format.printf "(" ;
      Format.printf "%d" (Z.to_int (Automata_lib.integer_of_nat a)) ;
      Format.printf ", " ;
      List.iter (fun (l, r) -> Format.printf "[%s, %s]" l r) l ;
      Format.printf ", " ;
      Format.printf "%d" (Z.to_int (Automata_lib.integer_of_nat c)) ;
      Format.printf ")" ;
      print_tran_str rl

let print_auto a =
  match a with
  | s, (d, (i, f)) ->
      Format.printf "States:" ;
      print_list s ;
      Format.printf "Initial states:" ;
      print_list i ;
      Format.printf "Transitions:" ;
      print_tran d ;
      Format.printf "Accepting states:" ;
      print_list f

let print_auto_str a =
  match a with
  | s, (d, (i, f)) ->
      Format.printf "States:\n" ;
      print_list s ;
      Format.printf "Initial states:\n" ;
      print_list i ;
      Format.printf "Transitions:\n" ;
      print_tran_str d ;
      Format.printf "Accepting states:\n" ;
      print_list f

let fe f l =
  match l with
  | [] -> false
  | e :: _l' -> if f (Some (fst e)) = None then true else false

let string_to_automaton s =
  let len = String.length s in
  if len = 0 then
    (* Empty string automaton: single state that is both initial and
       accepting *)
    ([0], [], [0], [0])
  else
    let states = List.init (len + 1) (fun i -> i) in
    let rec build_transitions i acc =
      if i >= len then acc
      else
        let char_code = Char.code (String.get s i) in
        let transition = (i, [(char_code, char_code)], i + 1) in
        build_transitions (i + 1) (transition :: acc)
    in
    let transitions = List.rev (build_transitions 0 []) in
    let initial_states = [0] in
    let accepting_states = [len] in
    (states, transitions, initial_states, accepting_states)
