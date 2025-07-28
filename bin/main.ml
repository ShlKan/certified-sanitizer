open Transducer
open ITransducer

let output_fun idx =
  match Z.to_int idx with
  (* case for episilon *)
  | 1 -> fun _ -> None
  | 2 -> (
      fun input ->
        match input with
        | None -> None
        | Some x ->
            let str = Z.to_string x in
            let ascii_list =
              List.map
                (fun c -> Z.of_int (Char.code c))
                (List.of_seq (String.to_seq str))
            in
            Some
              [ ( [Z.of_int 0x26; Z.of_int 0x23] @ ascii_list
                , [Z.of_int 0x26; Z.of_int 0x23] @ ascii_list ) ] )
  (* case for Id function *)
  | _ -> (
      fun input ->
        match input with None -> None | Some x -> Some [([x], [x])] )

let nft_sanitizer () =
  let states = [0; 1; 2] in
  let transitions =
    [ ( 0
      , [ (0x61, 0x7A)
        ; (0x5F, 0x5F)
        ; (0x41, 0x5A)
        ; (0x30, 0x39)
        ; (0x2E, 0x2E)
        ; (0x2D, 0x2D)
        ; (0x2C, 0x2C)
        ; (0x20, 0x20) ]
      , 0
      , 1 )
    ; ( 0
      , [ (0x00, 0x1F)
        ; (0x21, 0x2B)
        ; (0x2F, 0x2F)
        ; (0x3A, 0x40)
        ; (0x5B, 0x5E)
        ; (0x60, 0x60)
        ; (0x7B, 0x11FFFF) ]
      , 2
      , 2 )
    ; (1, [], 1, 0)
    ; (2, [], 1, 0) ]
  in
  let init = [0] in
  let accepts = [0; 1; 2] in
  nft_construct (states, transitions, init, accepts, output_fun)

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

let main filename =
  (* Read file content as string *)
  let ic = open_in filename in
  let content = really_input_string ic (in_channel_length ic) in
  (* Create a map for ASCII characters *)
  close_in ic ;
  (* Convert string to NFA *)
  let string_nfa =
    nfa_construct_reachable (nfa_construct (string_to_automaton content))
  in
  (* Compute product of NFA and nft_sanitizer *)
  let _ = nft_product (nft_sanitizer ()) string_nfa fmap fe in
  ()

(* Generate random ASCII characters *)
let generate_random_ascii size =
  Random.self_init () ;
  let buffer = Buffer.create size in
  for _ = 1 to size do
    let ascii_code = Random.int 128 in
    (* ASCII range 0-127 *)
    Buffer.add_char buffer (Char.chr ascii_code)
  done ;
  Buffer.contents buffer

(* Write string to file *)
let write_to_file filename content =
  let oc = open_out filename in
  output_string oc content ; close_out oc

let () =
  let argc = Array.length Sys.argv in
  if argc < 2 then
    Format.printf
      "Usage: %s <filename> | --input-gen --size=<n> --file=<name>\n"
      Sys.argv.(0)
  else if Sys.argv.(1) = "--input-gen" then (
    let size = ref 10 in
    let filename = ref "output.txt" in
    for i = 2 to argc - 1 do
      let arg = Sys.argv.(i) in
      if String.length arg > 7 && String.sub arg 0 7 = "--size=" then
        size := int_of_string (String.sub arg 7 (String.length arg - 7))
      else if String.length arg > 7 && String.sub arg 0 7 = "--file=" then
        filename := String.sub arg 7 (String.length arg - 7)
    done ;
    let content = generate_random_ascii !size in
    write_to_file !filename content ;
    Format.printf "Generated %d random ASCII characters and saved to %s\n"
      !size !filename )
  else main Sys.argv.(1)
