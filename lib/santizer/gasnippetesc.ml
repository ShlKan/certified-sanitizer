(* JavaScript snippetEsc function implementation:
   
   function snippetEsc(str) {
     return String(str)
       .replace(/\\/g, '\\\\')   // Escape backslashes
       .replace(/'/g, "\\\'")     // Escape single quotes
       .replace(/"/g, '\\"')     // Escape double quotes
       .replace(/\r/g, '\\r')    // Escape carriage returns
       .replace(/\n/g, '\\n')    // Escape newlines
       .replace(/\u2028/g, '\\u2028') // Escape line separator
       .replace(/\u2029/g, '\\u2029'); // Escape paragraph separator
   }

   Example:
   const unsafe = He said Hi with newline and script alert XSS script;
   const safeForSnippet = snippetEsc(unsafe);
   console.log(safeForSnippet);
   // Output: He said backslash-quote Hi backslash-n script alert backslash-quote XSS backslash-quote script
*)

open Transducer
open ITransducer

let output_fun idx =
  match Z.to_int idx with
  (* case for epsilon *)
  | 1 -> fun _ -> None
  (* case for backslash -> \\\\ *)
  | 2 -> fun _ -> Some [(["\\\\"], ["\\\\"])]
  (* case for single quote -> backslash apostrophe *)
  | 3 -> fun _ -> Some [(["\\'"], ["\\'"])]
  (* case for double quote -> backslash quote *)
  | 4 -> fun _ -> Some [(["\\\""], ["\\\""])]
  (* case for carriage return -> \\r *)
  | 5 -> fun _ -> Some [([("\\r")], [("\\r")])]
  (* case for newline -> \\n *)
  | 6 -> fun _ -> Some [([("\\n")], [("\\n")])]
  (* case for line separator U+2028 -> \\u2028 *)
  | 7 -> fun _ -> Some [([("\\u2028")], [("\\u2028")])]
  (* case for paragraph separator U+2029 -> \\u2029 *)
  | 8 -> fun _ -> Some [([("\\u2029")], [("\\u2029")])]
  (* case for identity function - all other characters *)
  | _ -> (
      fun input ->
        match input with 
        | None -> None 
        | Some x -> 
            let str = Z.to_string x in
            Some [([str], [str])] )

let nft_gasnippetesc () =
  let states = [0; 1; 2; 3; 4; 5; 6; 7; 8] in
  let transitions =
    [ (* Special character transitions *)
      (0, [(0x5C, 0x5C)], 2, 0)  (* backslash (0x5C) -> state 2 *)
    ; (0, [(0x27, 0x27)], 3, 0)  (* single quote (0x27) -> state 3 *)
    ; (0, [(0x22, 0x22)], 4, 0)  (* double quote (0x22) -> state 4 *)
    ; (0, [(0x0D, 0x0D)], 5, 0)  (* carriage return (0x0D) -> state 5 *)
    ; (0, [(0x0A, 0x0A)], 6, 0)  (* newline (0x0A) -> state 6 *)
    ; (0, [(0x2028, 0x2028)], 7, 0)  (* line separator (U+2028) -> state 7 *)
    ; (0, [(0x2029, 0x2029)], 8, 0)  (* paragraph separator (U+2029) -> state 8 *)
    ; (* All other characters - identity function *)
      ( 0
      , [ (0x00, 0x09)      (* 0x00-0x09 (excluding 0x0A which is newline) *)
        ; (0x0B, 0x0C)      (* 0x0B-0x0C (excluding 0x0D which is CR) *)
        ; (0x0E, 0x21)      (* 0x0E-0x21 (excluding 0x22 which is quote) *)
        ; (0x23, 0x26)      (* 0x23-0x26 (excluding 0x27 which is apostrophe) *)
        ; (0x28, 0x5B)      (* 0x28-0x5B (excluding 0x5C which is backslash) *)
        ; (0x5D, 0x2027)    (* 0x5D-0x2027 (excluding U+2028) *)
        ; (0x202A, 0x11FFFF) ] (* 0x202A and above (excluding U+2029) *)
      , 0
      , 0 ) ]
  in
  let init = [0] in
  let accepts = [0] in
  nft_construct_str (states, transitions, init, accepts, output_fun)