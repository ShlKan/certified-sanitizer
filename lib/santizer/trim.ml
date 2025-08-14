(* Trim transducer: Removes leading and trailing whitespace

   Equivalent to JavaScript String.prototype.trim() - Removes whitespace
   characters from beginning and end of string - Whitespace includes: space,
   tab, newline, carriage return, form feed, vertical tab - Uses state
   machine to track whether we're in leading, middle, or trailing
   whitespace *)

open Transducer
open ITransducer

let output_fun idx =
  match Z.to_int idx with
  (* case for leading whitespace -> remove (None) *)
  | 1 -> fun _ -> None
  (* case for trailing whitespace -> remove (None) *)
  | 2 -> fun _ -> None
  (* case for non-whitespace character -> output normally *)
  | 3 -> (
      fun input ->
        match input with
        | None -> None
        | Some x ->
            let str = Z.to_string x in
            Some [([str], [str])] )
  (* case for whitespace in middle -> output normally *)
  | 4 -> (
      fun input ->
        match input with
        | None -> None
        | Some x ->
            let str = Z.to_string x in
            Some [([str], [str])] )
  (* case for identity function - fallback *)
  | _ -> (
      fun input ->
        match input with
        | None -> None
        | Some x ->
            let str = Z.to_string x in
            Some [([str], [str])] )

let nft_trim () =
  let states = [0; 1; 2] in
  let transitions =
    [ (* State 0: Initial state (leading whitespace phase) *)
      (* Leading whitespace -> stay in state 0, remove *)
      ( 0
      , [(0x09, 0x09); (0x0A, 0x0A); (0x0C, 0x0C); (0x0D, 0x0D); (0x20, 0x20)]
      , 1
      , 0 )
    ; (* Non-whitespace -> go to state 1 (middle content), output *)
      ( 0
      , [ (0x00, 0x08) (* 0x00-0x08 *)
        ; (0x0B, 0x0B) (* 0x0B *)
        ; (0x0E, 0x1F) (* 0x0E-0x1F (excluding whitespace) *)
        ; (0x21, 0x11FFFF) ]
        (* 0x21 and above (non-space) *)
      , 3
      , 1 )
    ; (* State 1: Middle content phase *)
      (* Non-whitespace -> stay in state 1, output *)
      ( 1
      , [ (0x00, 0x08) (* 0x00-0x08 *)
        ; (0x0B, 0x0B) (* 0x0B *)
        ; (0x0E, 0x1F) (* 0x0E-0x1F (excluding whitespace) *)
        ; (0x21, 0x11FFFF) ]
        (* 0x21 and above (non-space) *)
      , 3
      , 1 )
    ; (* Whitespace in middle -> go to state 2 (potential trailing),
         buffer *)
      ( 1
      , [(0x09, 0x09); (0x0A, 0x0A); (0x0C, 0x0C); (0x0D, 0x0D); (0x20, 0x20)]
      , 4
      , 2 )
    ; (* State 2: Potential trailing whitespace phase *)
      (* More whitespace -> stay in state 2, buffer *)
      ( 2
      , [(0x09, 0x09); (0x0A, 0x0A); (0x0C, 0x0C); (0x0D, 0x0D); (0x20, 0x20)]
      , 4
      , 2 )
    ; (* Non-whitespace -> back to state 1, output (including buffered
         whitespace) *)
      ( 2
      , [ (0x00, 0x08) (* 0x00-0x08 *)
        ; (0x0B, 0x0B) (* 0x0B *)
        ; (0x0E, 0x1F) (* 0x0E-0x1F (excluding whitespace) *)
        ; (0x21, 0x11FFFF) ]
        (* 0x21 and above (non-space) *)
      , 3
      , 1 ) ]
  in
  let init = [0] in
  let accepts = [0; 1; 2] in
  (* Accept ending in any state, but trailing whitespace in state 2 is
     trimmed *)
  nft_construct_str (states, transitions, init, accepts, output_fun)
