(* JavaScript xmlEsc function implementation:

   function xmlEsc(value) { return String(value) // Strip invalid XML control
   characters (per XML 1.0) .replace(/[\x00-\x08\x0B\x0C\x0E-\x1F]/g, '') //
   Escape XML special chars .replace(/&/g, '&amp;') .replace(/</g, '&lt;')
   .replace(/>/g, '&gt;') .replace(/quote/g, '&quot;')
   .replace(/apostrophe/g, '&apos;'); }

   This function implements XML 1.0 escaping by: 1. Removing invalid control
   characters (except tab 0x09, LF 0x0A, CR 0x0D) 2. Escaping the five XML
   special characters with proper entities *)

open Transducer
open ITransducer

let output_fun idx =
  match Z.to_int idx with
  (* case for identity/normal processing *)
  | 0 -> (
      fun input ->
        match input with
        | None -> None
        | Some x ->
            let str = Z.to_string x in
            Some [([str], [str])] )
  (* case for epsilon *)
  | 1 -> fun _ -> None
  (* case for control characters -> remove (None) *)
  | 2 -> fun _ -> None
  (* case for & -> &amp; *)
  | 3 -> fun _ -> Some [(["&amp;"], ["&amp;"])]
  (* case for < -> &lt; *)
  | 4 -> fun _ -> Some [(["&lt;"], ["&lt;"])]
  (* case for > -> &gt; *)
  | 5 -> fun _ -> Some [(["&gt;"], ["&gt;"])]
  (* case for double quote -> &quot; *)
  | 6 -> fun _ -> Some [(["&quot;"], ["&quot;"])]
  (* case for single quote -> &apos; *)
  | 7 -> fun _ -> Some [(["&apos;"], ["&apos;"])]
  (* case for identity function - all other characters *)
  | _ -> (
      fun input ->
        match input with
        | None -> None
        | Some x ->
            let str = Z.to_string x in
            Some [([str], [str])] )

let nft_gaxmlesc () =
  let states = [0] in
  let transitions =
    [ (* Invalid XML control characters to remove (0x00-0x08, 0x0B, 0x0C, 0x0E-0x1F) *)
      (* Note: XML 1.0 allows tab (0x09), LF (0x0A), and CR (0x0D) *)
      (0, [(0x00, 0x08)], 2, 0) (* 0x00-0x08 -> remove *)
    ; (0, [(0x0B, 0x0B)], 2, 0) (* 0x0B (vertical tab) -> remove *)
    ; (0, [(0x0C, 0x0C)], 2, 0) (* 0x0C (form feed) -> remove *)
    ; (0, [(0x0E, 0x1F)], 2, 0) (* 0x0E-0x1F -> remove *)
    ; (* XML special character transitions *)
      (0, [(0x26, 0x26)], 3, 0) (* & (0x26) -> &amp; *)
    ; (0, [(0x3C, 0x3C)], 4, 0) (* < (0x3C) -> &lt; *)
    ; (0, [(0x3E, 0x3E)], 5, 0) (* > (0x3E) -> &gt; *)
    ; (0, [(0x22, 0x22)], 6, 0) (* double quote (0x22) -> &quot; *)
    ; (0, [(0x27, 0x27)], 7, 0) (* single quote (0x27) -> &apos; *)
    ; (* All other characters - identity function *)
      ( 0
      , [ (0x09, 0x09) (* tab (0x09) - allowed in XML *)
        ; (0x0A, 0x0A) (* newline (0x0A) - allowed in XML *)
        ; (0x0D, 0x0D) (* carriage return (0x0D) - allowed in XML *)
        ; (0x20, 0x21) (* space to ! *)
        ; (0x23, 0x25) (* # to % (excluding &) *)
        ; (0x28, 0x3B) (* ( to ; (excluding <) *)
        ; (0x3D, 0x3D) (* = (excluding >) *)
        ; (0x3F, 0x11FFFF) ]
        (* ? and above (excluding quotes, etc.) *)
      , 0
      , 0 ) ]
  in
  let init = [0] in
  let accepts = [0] in
  nft_construct_str (states, transitions, init, accepts, output_fun)
