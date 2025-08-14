open Transducer
open ITransducer
open Attackmodel (* Ensure Attackmodel.ml exists and is compiled *)

let translate_automata_to_int (q, (t, (i, f))) =
  let nat_to_int n = Z.to_int (Automata_lib.Automata_lib.integer_of_nat n) in
  let z_pair_to_int_pair (z1, z2) = (Z.to_int z1, Z.to_int z2) in
  let states = List.map nat_to_int q in
  let transitions =
    List.map
      (fun (q_from, (labels, q_to)) ->
        let from_state = nat_to_int q_from in
        let to_state = nat_to_int q_to in
        let int_labels = List.map z_pair_to_int_pair labels in
        (from_state, int_labels, to_state) )
      t
  in
  let init_states = List.map nat_to_int i in
  let accept_states = List.map nat_to_int f in
  (states, transitions, init_states, accept_states)

(* By default we use any input string from Unicode *)
let santizer_check (* a list of transducers of sanitizer *) santizers
    (* an NFA contains unsafe strings. *) attach_model =
  List.fold_left
    (fun acc santizer ->
      let nfa = nft_product_str santizer acc fmap fe in
      let elim_nfa = nfa_elim_str nfa in
      let norm_nfa = nfa_normal_str elim_nfa in
      let destruct_nfa = nfa_destruct_str norm_nfa in
      let tran_nfa = transform_nfa_string_to_char destruct_nfa in
      nfa_construct_reachable
        (nfa_construct (translate_automata_to_int tran_nfa)) )
    universial_nfa santizers

let nft = nft_product_str
