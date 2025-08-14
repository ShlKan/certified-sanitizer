open Transducer
open ITransducer
(* addslashes NFT *)

(* Optional: sample output function; adapt its signature to your
   nft_construct. *)
let addslashes_output_fun (klass : Z.t) (cp : Z.t option) =
  match Z.to_int klass with
  | 1 ->
      Some
        [ ( [String.make 1 (Char.chr (Z.to_int (Option.get cp)))]
          , [String.make 1 (Char.chr (Z.to_int (Option.get cp)))] ) ]
      (* identity *)
  | 2 -> Some [(["\\\""], ["\\\""])] (* U+0022 ->\ *)
  | 3 -> Some [(["\\'"], ["\\'"])] (* U+0027 -> \' *)
  | 4 -> Some [(["\\\\"], ["\\\\"])] (* U+005C -> \\ *)
  | 5 -> Some [(["\\ "], ["\\ "])] (* U+0000 -> backslash + space *)
  | 6 -> None
  | _ -> failwith "unknown class"

let nft_addslashes () =
  let states = [0; 1; 2; 3; 4; 5] in
  let transitions =
    [ (* Special cases *)
      (0, [(0x22, 0x22)], 2, 2)
    ; (* '"' -> class 2 *)
      (0, [(0x27, 0x27)], 3, 3)
    ; (* ''' -> class 3 *)
      (0, [(0x5C, 0x5C)], 4, 4)
    ; (* '\' -> class 4 *)
      (0, [(0x00, 0x00)], 5, 5)
    ; (* NUL -> class 5 *)

      (* Identity everywhere else *)
      (0, [(0x01, 0x21); (0x23, 0x26); (0x28, 0x5B); (0x5D, 0x11FFFF)], 1, 1)
    ; (* Epsilon “emit and return” steps for each class *)
      (1, [], 6, 0)
    ; (2, [], 6, 0)
    ; (3, [], 6, 0)
    ; (4, [], 6, 0)
    ; (5, [], 6, 0) ]
  in
  let init = [0] in
  let accepts = [0; 1; 2; 3; 4; 5] in
  nft_construct_str
    (states, transitions, init, accepts, addslashes_output_fun)
