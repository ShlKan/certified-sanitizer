(* JavaScript owaspHtmlEncode function implementation:
   
   function owaspHtmlEncode(input) {
     return String(input)
       // Normalize + strip risky control chars (except tab/newline if you want them)
       .replace(/[\x00-\x08\x0B\x0C\x0E-\x1F\x7F]/g, '')
       // Encode the six special characters per OWASP
       .replace(/&/g, '&amp;')
       .replace(/</g, '&lt;')
       .replace(/>/g, '&gt;')
       .replace(/quote/g, '&quot;')
       .replace(/apostrophe/g, '&#x27;')   // or &#39;
       .replace(/\//g, '&#x2F;'); // optional but recommended
   }

   Example:
   const unsafe = Hello script tag ampersand quote you quote all quote forward slash test;
   const safe = owaspHtmlEncode(unsafe);
   Output: Hello &lt;/script&gt; &amp; &quot;you&quot; &#x27;all&#x27; &#x2F; test
*)

open Transducer
open ITransducer

let output_fun idx =
  match Z.to_int idx with
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
  (* case for quote -> &quot; *)
  | 6 -> fun _ -> Some [(["&quot;"], ["&quot;"])]
  (* case for apostrophe -> &#x27; *)
  | 7 -> fun _ -> Some [(["&#x27;"], ["&#x27;"])]
  (* case for forward slash -> &#x2F; *)
  | 8 -> fun _ -> Some [(["&#x2F;"], ["&#x2F;"])]
  (* case for identity function - all other characters *)
  | _ -> (
      fun input ->
        match input with 
        | None -> None 
        | Some x -> 
            let str = Z.to_string x in
            Some [([str], [str])] )

let nft_owasphtmlencode () =
  let states = [0] in
  let transitions =
    [ (* Control characters to remove (0x00-0x08, 0x0B, 0x0C, 0x0E-0x1F, 0x7F) *)
      (0, [(0x00, 0x08)], 2, 0)  (* 0x00-0x08 -> remove *)
    ; (0, [(0x0B, 0x0B)], 2, 0)  (* 0x0B (vertical tab) -> remove *)
    ; (0, [(0x0C, 0x0C)], 2, 0)  (* 0x0C (form feed) -> remove *)
    ; (0, [(0x0E, 0x1F)], 2, 0)  (* 0x0E-0x1F -> remove *)
    ; (0, [(0x7F, 0x7F)], 2, 0)  (* 0x7F (DEL) -> remove *)
    ; (* OWASP special character transitions *)
      (0, [(0x26, 0x26)], 3, 0)  (* & (0x26) -> &amp; *)
    ; (0, [(0x3C, 0x3C)], 4, 0)  (* < (0x3C) -> &lt; *)
    ; (0, [(0x3E, 0x3E)], 5, 0)  (* > (0x3E) -> &gt; *)
    ; (0, [(0x22, 0x22)], 6, 0)  (* quote (0x22) -> &quot; *)
    ; (0, [(0x27, 0x27)], 7, 0)  (* apostrophe (0x27) -> &#x27; *)
    ; (0, [(0x2F, 0x2F)], 8, 0)  (* forward slash (0x2F) -> &#x2F; *)
    ; (* All other characters - identity function *)
      ( 0
      , [ (0x09, 0x09)      (* tab (0x09) - keep *)
        ; (0x0A, 0x0A)      (* newline (0x0A) - keep *)
        ; (0x0D, 0x0D)      (* carriage return (0x0D) - keep *)
        ; (0x20, 0x21)      (* space to ! *)
        ; (0x23, 0x25)      (* # to % (excluding &) *)
        ; (0x28, 0x2E)      (* ( to . (excluding /) *)
        ; (0x30, 0x3B)      (* 0 to ; (excluding <) *)
        ; (0x3D, 0x3D)      (* = (excluding >) *)
        ; (0x3F, 0x11FFFF) ] (* ? and above (excluding quotes, etc.) *)
      , 0
      , 0 ) ]
  in
  let init = [0] in
  let accepts = [0] in
  nft_construct_str (states, transitions, init, accepts, output_fun)