open Transducer
open ITransducer

let auto =
  nfa_construct_reachable
    (nfa_construct
       ( [0; 1; 2]
       , [(0, [(1, 1); (2, 2)], 1); (1, [(2, 3)], 2); (2, [], 3)]
       , [0]
       , [2] ) )

let nft_example () =
  let states = [0; 1; 2] in
  let transitions =
    [(0, [(1, 1); (2, 2)], 1, 1); (1, [(2, 3)], 2, 2); (2, [], 3, 0)]
  in
  let init = [0] in
  let accepts = [2] in
  let ofun = fun _ _ -> None in
  nft_construct (states, transitions, init, accepts, ofun)

let output_nfa =
  nfa_normal (nfa_elim (nft_product (nft_example ()) auto fmap fe))

let () = print_auto (nfa_destruct output_nfa)

let test_string_to_automaton () =
  (* Test multi-character string *)
  let states, transitions, initial, accepting =
    string_to_automaton "!<>abc"
  in
  Printf.printf "Multi-character 'abc' automaton:\n" ;
  Printf.printf "  States: %s\n"
    (String.concat "; " (List.map string_of_int states)) ;
  Printf.printf "  Transitions: %s\n"
    (String.concat "; "
       (List.map
          (fun (q, ls, q') ->
            Printf.sprintf "(%d, [%s], %d)" q
              (String.concat "; "
                 (List.map (fun (l, r) -> Printf.sprintf "(%d,%d)" l r) ls) )
              q' )
          transitions ) ) ;
  Printf.printf "  Initial: %s\n"
    (String.concat "; " (List.map string_of_int initial)) ;
  Printf.printf "  Accepting: %s\n"
    (String.concat "; " (List.map string_of_int accepting))

let () = test_string_to_automaton ()
