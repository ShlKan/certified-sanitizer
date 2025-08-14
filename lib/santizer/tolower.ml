(* ToLower transducer: Converts uppercase letters to lowercase

   Equivalent to JavaScript String.prototype.toLowerCase() - Converts A-Z
   (0x41-0x5A) to a-z (0x61-0x7A) - Leaves all other characters unchanged *)

open Transducer
open ITransducer

let output_fun idx =
  match Z.to_int idx with
  (* case for uppercase letter conversion to lowercase *)
  | 2 -> (
      fun input ->
        match input with
        | None -> None
        | Some x ->
            let char_code = Z.to_int x in
            let lower_code = char_code + 32 in
            (* Convert A-Z to a-z *)
            let lower_char = Char.chr lower_code in
            let str = String.make 1 lower_char in
            Some [([str], [str])] )
  (* case for identity function - all other characters *)
  | _ -> (
      fun input ->
        match input with
        | None -> None
        | Some x ->
            let str = Z.to_string x in
            Some [([str], [str])] )

let nft_tolower () =
  let states = [0] in
  let transitions =
    [ (* Uppercase letters A-Z -> lowercase conversion *)
      (0, [(0x41, 0x5A)], 2, 0) (* A-Z (0x41-0x5A) -> convert to a-z *)
    ; (* All other characters - identity function *)
      ( 0
      , [(0x00, 0x40) (* 0x00-0x40 (before 'A') *); (0x5B, 0x11FFFF)]
        (* 0x5B and above (after 'Z') *)
      , 0
      , 0 ) ]
  in
  let init = [0] in
  let accepts = [0] in
  nft_construct_str (states, transitions, init, accepts, output_fun)
