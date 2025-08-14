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
            let escaped_str = "&#" ^ str in
            Some [([escaped_str], [escaped_str])] )
  (* case for Id function *)
  | _ -> (
      fun input ->
        match input with
        | None -> None
        | Some x ->
            let str = Z.to_string x in
            Some [([str], [str])] )

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
      , 0 )
    ; ( 0
      , [ (0x00, 0x1F)
        ; (0x21, 0x2B)
        ; (0x2F, 0x2F)
        ; (0x3A, 0x40)
        ; (0x5B, 0x5E)
        ; (0x60, 0x60)
        ; (0x7B, 0x11FFFF) ]
      , 2
      , 0 ) ]
  in
  let init = [0] in
  let accepts = [0] in
  nft_construct_str (states, transitions, init, accepts, output_fun)
