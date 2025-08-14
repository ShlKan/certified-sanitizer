open Sanitizer
open Sanitizercheck
open Hescapestring
open Attackmodel
open Addslash

(* open Addslash *)

(* Main function to construct the sanitizer NFA and print it *)

(* Function to compute an accepting path in an NFA *)
let _compute_accepting_path
    (_states, (transitions, (initial_states, accepting_states))) =
  let rec bfs queue visited =
    match queue with
    | [] -> None (* No accepting path found *)
    | (current_state, path) :: rest ->
        if List.mem current_state visited then bfs rest visited
        else
          let new_visited = current_state :: visited in
          (* Check if we reached an accepting state *)
          if List.mem current_state accepting_states then
            Some (List.rev path)
          else
            (* Find all possible transitions from current state *)
            let possible_moves =
              List.filter (fun (q1, _) -> q1 = current_state) transitions
            in
            let new_queue_items =
              List.fold_left
                (fun acc (q1, (labels, q2)) ->
                  (q2, (q1, (labels, q2)) :: path) :: acc )
                [] possible_moves
            in
            bfs (rest @ new_queue_items) new_visited
  in
  (* Start BFS from all initial states *)
  let initial_queue = List.map (fun state -> (state, [])) initial_states in
  bfs initial_queue []

(* let main filename = (* Read file content as string *) let ic = open_in
   filename in let content = really_input_string ic (in_channel_length ic) in
   (* Create a map for ASCII characters *) close_in ic ; (* Convert string to
   NFA *) let string_nfa = nfa_construct_reachable (nfa_construct
   (string_to_automaton content)) in (* Compute product of NFA and
   nft_sanitizer *) let _ = nft_product (nft_sanitizer ()) string_nfa fmap fe
   in ()

   (* Generate random ASCII characters *) let generate_random_ascii size =
   Random.self_init () ; let buffer = Buffer.create size in for _ = 1 to size
   do let ascii_code = Random.int 128 in (* ASCII range 0-127 *)
   Buffer.add_char buffer (Char.chr ascii_code) done ; Buffer.contents buffer

   (* Write string to file *) let write_to_file filename content = let oc =
   open_out filename in output_string oc content ; close_out oc

   let () = let argc = Array.length Sys.argv in if argc < 2 then
   Format.printf "Usage: %s <filename> | --input-gen --size=<n>
   --file=<name>\n" Sys.argv.(0) else if Sys.argv.(1) = "--input-gen" then (
   let size = ref 10 in let filename = ref "output.txt" in for i = 2 to argc
   - 1 do let arg = Sys.argv.(i) in if String.length arg > 7 && String.sub
   arg 0 7 = "--size=" then size := int_of_string (String.sub arg 7
   (String.length arg - 7)) else if String.length arg > 7 && String.sub arg 0
   7 = "--file=" then filename := String.sub arg 7 (String.length arg - 7)
   done ; let content = generate_random_ascii !size in write_to_file
   !filename content ; Format.printf "Generated %d random ASCII characters
   and saved to %s\n" !size !filename ) else main Sys.argv.(1)

   let test_transform () = (* Create a complex test NFA with multiple
   string-based transitions *) let test_nfa = let q1 = int_to_nat 0 in let q2
   = int_to_nat 1 in let q3 = int_to_nat 2 in let q4 = int_to_nat 3 in let
   states = [q1; q2; q3; q4] in let transitions = [ (q1, ([(["hello"],
   ["hello"])], q2)); (q1, ([(["world"], ["world"])], q3)); (q2, ([(["test"],
   ["test"])], q4)); (q3, ([(["abc"], ["abc"])], q4)) ] in let initial = [q1]
   in let final = [q4] in (states, (transitions, (initial, final))) in
   Format.printf "Original NFA with string transitions:\n" ;
   print_auto_str_label test_nfa ; Format.printf "\nTransformed NFA with
   character transitions:\n" ; let transformed = transform_nfa_string_to_char
   test_nfa in print_auto_str transformed ; Format.printf "\n" *)

(* let main () = let string_nfa = nfa_construct_reachable (nfa_construct
   ([0], [(0, [(0x21, 0x21)], 0)], [0], [0])) in let sanitizer_nfa =
   nft_sanitizer () in let product_nfa = nft_product_str sanitizer_nfa
   string_nfa fmap fe in let nfa = nfa_destruct_str (nfa_normal_str
   (nfa_elim_str product_nfa)) in print_auto_str_label nfa ; let nfa_ =
   transform_nfa_string_to_char nfa in print_auto_str nfa_*)

let main () =
  santizer_check [nft_addslashes (); nft_escapeString ()] attack_html_attr

let () = main ()
