(* ToUpper transducer: Converts lowercase letters to uppercase
   
   Equivalent to JavaScript String.prototype.toUpperCase()
   - Converts a-z (0x61-0x7A) to A-Z (0x41-0x5A)  
   - Leaves all other characters unchanged
*)

open Transducer
open ITransducer

let output_fun idx =
  match Z.to_int idx with
  (* case for lowercase letter conversion to uppercase *)
  | 2 -> (
      fun input ->
        match input with 
        | None -> None 
        | Some x -> 
            let char_code = Z.to_int x in
            let upper_code = char_code - 32 in (* Convert a-z to A-Z *)
            let upper_char = Char.chr upper_code in
            let str = String.make 1 upper_char in
            Some [([str], [str])] )
  (* case for identity function - all other characters *)
  | _ -> (
      fun input ->
        match input with 
        | None -> None 
        | Some x -> 
            let str = Z.to_string x in
            Some [([str], [str])] )

let nft_toupper () =
  let states = [0] in
  let transitions =
    [ (* Lowercase letters a-z -> uppercase conversion *)
      (0, [(0x61, 0x7A)], 2, 0)  (* a-z (0x61-0x7A) -> convert to A-Z *)
    ; (* All other characters - identity function *)
      ( 0
      , [ (0x00, 0x60)      (* 0x00-0x60 (before 'a') *)
        ; (0x7B, 0x11FFFF) ] (* 0x7B and above (after 'z') *)
      , 0
      , 0 ) ]
  in
  let init = [0] in
  let accepts = [0] in
  nft_construct_str (states, transitions, init, accepts, output_fun)