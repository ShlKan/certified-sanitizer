open Transducer
open ITransducer

(* escapeString — single-state NFT (only self-loops on state 0) *)

(* Classes:
   1 = identity
   2 = double quote -> escaped quote
   3 = single quote -> escaped apostrophe  
   4 = backslash    -> escaped backslash
   5 = 0x08         -> backslash-b
   7 = 0x09         -> backslash-t
   8 = 0x0A         -> backslash-n
   9 = 0x0B         -> backslash-x0B      (not backslash-v)
  10 = 0x0C         -> backslash-f
  11 = 0x0D         -> backslash-r
  12 = hex group    -> backslash-xHH      (0x00..0x07, 0x0E..0x1F, 0x7F..0xFF)
*)

let escape_output_fun (klass : Z.t) (cp : Z.t option) =
  let one_char_of_cp () =
    String.make 1 (Char.chr (Z.to_int (Option.get cp)))
  in
  match Z.to_int klass with
  | 1 ->
      let c = one_char_of_cp () in
      Some [ ([c], [c]) ]                 (* identity *)
  | 2 -> Some [ (["\\\""], ["\\\""]) ]    (* U+0022 -> escaped quote *)
  | 3 -> Some [ (["\\'"], ["\\'"]) ]      (* U+0027 -> escaped apostrophe *)
  | 4 -> Some [ (["\\\\"], ["\\\\"]) ]    (* U+005C -> escaped backslash *)
  | 5 -> Some [ (["\\b"], ["\\b"]) ]      (* U+0008 -> backspace *)
  | 7 -> Some [ (["\\t"], ["\\t"]) ]      (* U+0009 -> tab *)
  | 8 -> Some [ (["\\n"], ["\\n"]) ]      (* U+000A -> newline *)
  | 9 -> Some [ (["\\x0B"], ["\\x0B"]) ]  (* U+000B -> hex 0B *)
  | 10 -> Some [ (["\\f"], ["\\f"]) ]     (* U+000C -> form feed *)
  | 11 -> Some [ (["\\r"], ["\\r"]) ]     (* U+000D -> carriage return *)
  | 12 ->
      let code = Z.to_int (Option.get cp) in
      let esc = Printf.sprintf "\\x%02X" code in
      Some [ ([esc], [esc]) ]             (* generic hex escape *)
  | _ -> failwith "unknown class"

let nft_escapeString () =
  (* Single state 0, start=accept *)
  let states = [0] in
  let transitions =
    [
      (* ---- special singletons ---- *)
      (0, [ (0x22, 0x22) ], 2, 0)        (* double quote -> class 2 *)
    ; (0, [ (0x27, 0x27) ], 3, 0)        (* single quote -> class 3 *)
    ; (0, [ (0x5C, 0x5C) ], 4, 0)        (* backslash -> class 4 *)
    ; (0, [ (0x08, 0x08) ], 5, 0)        (* 0x08 -> backslash-b *)
    ; (0, [ (0x09, 0x09) ], 7, 0)        (* 0x09 -> backslash-t *)
    ; (0, [ (0x0A, 0x0A) ], 8, 0)        (* 0x0A -> backslash-n *)
    ; (0, [ (0x0B, 0x0B) ], 9, 0)        (* 0x0B -> backslash-x0B *)
    ; (0, [ (0x0C, 0x0C) ], 10, 0)       (* 0x0C -> backslash-f *)
    ; (0, [ (0x0D, 0x0D) ], 11, 0)       (* 0x0D -> backslash-r *)
    ; (* hex group *)
      (0, [ (0x00, 0x07); (0x0E, 0x1F); (0x7F, 0xFF) ], 12, 0)
    ; (* identity everywhere else *)
      ( 0
      , [ (0x20, 0x21)
        ; (0x23, 0x26)
        ; (0x28, 0x5B)
        ; (0x5D, 0x7E)
        ; (0x0100, 0x10FFFF) ]
      , 1
      , 0 )
    ]
  in
  let init = [0] in
  let accepts = [0] in
  nft_construct_str (states, transitions, init, accepts, escape_output_fun)
