open Transducer
open ITransducer

let output_fun idx =
  match Z.to_int idx with
  (* case for epsilon *)
  | 1 -> fun _ -> None
  (* case for & -> &amp; *)
  | 2 -> fun _ -> Some [(["&amp;"], ["&amp;"])]
  (* case for < -> &lt; *)
  | 3 -> fun _ -> Some [(["&lt;"], ["&lt;"])]
  (* case for > -> &gt; *)
  | 4 -> fun _ -> Some [(["&gt;"], ["&gt;"])]
  (* case for quote -> &quot; *)
  | 5 -> fun _ -> Some [(["&quot;"], ["&quot;"])]
  (* case for apostrophe -> &#39; *)
  | 6 -> fun _ -> Some [(["&#39;"], ["&#39;"])]
  (* case for identity function - all other characters *)
  | _ -> (
      fun input ->
        match input with
        | None -> None
        | Some x ->
            let str = Z.to_string x in
            Some [([str], [str])] )

let nft_gahtmlescape () =
  let states = [0; 1; 2; 3; 4; 5; 6] in
  let transitions =
    [ (* Special character transitions *)
      (0, [(0x26, 0x26)], 2, 0) (* & (0x26) -> state 2 *)
    ; (0, [(0x3C, 0x3C)], 3, 0) (* < (0x3C) -> state 3 *)
    ; (0, [(0x3E, 0x3E)], 4, 0) (* > (0x3E) -> state 4 *)
    ; (0, [(0x22, 0x22)], 5, 0) (* quote (0x22) -> state 5 *)
    ; (0, [(0x27, 0x27)], 6, 0) (* apostrophe (0x27) -> state 6 *)
    ; (* All other characters - identity function *)
      ( 0
      , [ (0x00, 0x21) (* 0x00-0x21 (excluding 0x22 which is quote) *)
        ; (0x23, 0x25) (* 0x23-0x25 (excluding 0x26 which is &) *)
        ; (0x28, 0x3B) (* 0x28-0x3B (excluding 0x3C which is <) *)
        ; (0x3D, 0x3D) (* 0x3D (=) *)
        ; (0x3F, 0x11FFFF) ]
        (* 0x3F and above (excluding 0x3E which is >) *)
      , 0
      , 0 ) ]
  in
  let init = [0] in
  let accepts = [0] in
  nft_construct_str (states, transitions, init, accepts, output_fun)
