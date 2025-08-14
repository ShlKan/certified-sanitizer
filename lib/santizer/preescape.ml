open Transducer
open ITransducer

let output_fun idx =
  match Z.to_int idx with
  (* case for epsilon *)
  | 1 -> fun _ -> None
  (* case for line breaks (CR LF) -> space *)
  | 2 -> fun _ -> Some [([" "], [" "])]
  (* case for control characters (0x00-0x08, 0x0B, 0x0C, 0x0E-0x1F) -> remove *)
  | 3 -> fun _ -> None
  (* case for identity function - all other characters *)
  | _ -> (
      fun input ->
        match input with 
        | None -> None 
        | Some x -> 
            let str = Z.to_string x in
            Some [([str], [str])] )

let nft_preescape () =
  let states = [0; 1; 2; 3] in
  let transitions =
    [ (* Line break characters -> space *)
      (0, [(0x0A, 0x0A)], 2, 0)  (* LF (line feed) -> space *)
    ; (0, [(0x0D, 0x0D)], 2, 0)  (* CR (carriage return) -> space *)
    ; (* Control characters to remove (ASCII < 32 except tab, LF, CR) *)
      (0, [(0x00, 0x08)], 3, 0)  (* 0x00-0x08 -> remove *)
    ; (0, [(0x0B, 0x0B)], 3, 0)  (* 0x0B (vertical tab) -> remove *)
    ; (0, [(0x0C, 0x0C)], 3, 0)  (* 0x0C (form feed) -> remove *)
    ; (0, [(0x0E, 0x1F)], 3, 0)  (* 0x0E-0x1F -> remove *)
    ; (* All other characters - identity function *)
      ( 0
      , [ (0x09, 0x09)      (* tab (0x09) - keep *)
        ; (0x20, 0x11FFFF) ] (* space and above - keep *)
      , 0
      , 0 ) ]
  in
  let init = [0] in
  let accepts = [0] in
  nft_construct_str (states, transitions, init, accepts, output_fun)