(* JavaScript cleanseCssValue function implementation:

   function cleanseCssValue(value, { allowUrl = false } = {}) { let s =
   String(value);

   // Remove control chars + comments s =
   s.replace(/[\x00-\x08\x0B\x0C\x0E-\x1F\x7F]/g, '')
   .replace(/\/\*[\s\S]*?\*\//g, '');

   // Kill obvious injection breakers for values s = s.replace(/[;{}]/g, '');

   // Normalize whitespace s = s.replace(/[\r\n]+/g, ' ').replace(/\s+/g, '
   ').trim();

   // Hard-block dangerous tokens (case-insensitive) const badTokens = [
   /url\s*\(/i, /expression\s*\(/i, /@import/i, /javascript\s*:/i,
   /vbscript\s*:/i, /-moz-binding/i, /behavior\s*:/i, ]; if (!allowUrl) { for
   (const rx of badTokens) { if (rx.test(s)) s = ''; } }

   // Escape quotes and backslashes for CSS string contexts s =
   s.replace(/\\/g, '\\\\').replace(/"/g, '\\"').replace(/'/g, "\\'");

   return s; }

   Note: This transducer implementation focuses on the character-level
   transformations: - Remove control characters - Remove CSS injection
   breakers (semicolon, braces) - Normalize line breaks to spaces - Escape
   backslashes, quotes, and apostrophes - Pattern matching for dangerous
   tokens is simplified for transducer implementation *)

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
  (* case for CSS breakers (;{}) -> remove (None) *)
  | 3 -> fun _ -> None
  (* case for line breaks (CR/LF) -> space *)
  | 4 -> fun _ -> Some [([" "], [" "])]
  (* case for backslash -> escaped backslash *)
  | 5 -> fun _ -> Some [(["\\\\"], ["\\\\"])]
  (* case for double quote -> escaped quote *)
  | 6 -> fun _ -> Some [(["\\\""], ["\\\""])]
  (* case for single quote -> escaped apostrophe *)
  | 7 -> fun _ -> Some [(["\\'"], ["\\'"])]
  (* case for bad token detected -> empty string (reject all) *)
  | 8 -> fun _ -> None
  (* case for identity function - all other characters *)
  | _ -> (
      fun input ->
        match input with
        | None -> None
        | Some x ->
            let str = Z.to_string x in
            Some [([str], [str])] )

let nft_gacleansecc () =
  let states =
    [ 0
    ; 1
    ; 2
    ; 3
    ; 4
    ; 5
    ; 6
    ; 7
    ; 8
    ; 9
    ; 10
    ; 11
    ; 12
    ; 13
    ; 14
    ; 15
    ; 16
    ; 17
    ; 18
    ; 19
    ; 20
    ; 99 ]
  in
  let transitions =
    [ (* Bad token detection states - if any bad token is detected, go to
         reject state 99 *)

      (* Detecting "url(" - states 1-4 *)
      (0, [(0x75, 0x75)], 0, 1) (* 'u' -> state 1 *)
    ; (1, [(0x72, 0x72)], 0, 2) (* 'r' -> state 2 *)
    ; (2, [(0x6C, 0x6C)], 0, 3) (* 'l' -> state 3 *)
    ; (3, [(0x20, 0x20); (0x09, 0x09)], 0, 3)
      (* whitespace -> stay in state 3 *)
    ; (3, [(0x28, 0x28)], 8, 99)
      (* '(' -> REJECT (bad token found) *)

      (* Detecting "expression(" - states 4-14 *)
    ; (0, [(0x65, 0x65)], 0, 4) (* 'e' -> state 4 *)
    ; (4, [(0x78, 0x78)], 0, 5) (* 'x' -> state 5 *)
    ; (5, [(0x70, 0x70)], 0, 6) (* 'p' -> state 6 *)
    ; (6, [(0x72, 0x72)], 0, 7) (* 'r' -> state 7 *)
    ; (7, [(0x65, 0x65)], 0, 8) (* 'e' -> state 8 *)
    ; (8, [(0x73, 0x73)], 0, 9) (* 's' -> state 9 *)
    ; (9, [(0x73, 0x73)], 0, 10) (* 's' -> state 10 *)
    ; (10, [(0x69, 0x69)], 0, 11) (* 'i' -> state 11 *)
    ; (11, [(0x6F, 0x6F)], 0, 12) (* 'o' -> state 12 *)
    ; (12, [(0x6E, 0x6E)], 0, 13) (* 'n' -> state 13 *)
    ; (13, [(0x20, 0x20); (0x09, 0x09)], 0, 13) (* whitespace -> stay *)
    ; (13, [(0x28, 0x28)], 8, 99)
      (* '(' -> REJECT *)

      (* Detecting "@import" - states 14-20 *)
    ; (0, [(0x40, 0x40)], 0, 14) (* '@' -> state 14 *)
    ; (14, [(0x69, 0x69)], 0, 15) (* 'i' -> state 15 *)
    ; (15, [(0x6D, 0x6D)], 0, 16) (* 'm' -> state 16 *)
    ; (16, [(0x70, 0x70)], 0, 17) (* 'p' -> state 17 *)
    ; (17, [(0x6F, 0x6F)], 0, 18) (* 'o' -> state 18 *)
    ; (18, [(0x72, 0x72)], 0, 19) (* 'r' -> state 19 *)
    ; (19, [(0x74, 0x74)], 8, 99)
      (* 't' -> REJECT *)

      (* Detecting "javascript:" - simplified *)
    ; (0, [(0x6A, 0x6A)], 0, 20) (* 'j' -> check for javascript: *)
    ; (20, [(0x61, 0x61)], 0, 21)
      (* continue pattern matching... *)

      (* Normal character processing when not in bad token detection *)
      (* Control characters to remove *)
    ; (0, [(0x00, 0x08)], 2, 0) (* control chars -> remove *)
    ; (0, [(0x0B, 0x0B)], 2, 0) (* vertical tab -> remove *)
    ; (0, [(0x0C, 0x0C)], 2, 0) (* form feed -> remove *)
    ; (0, [(0x0E, 0x1F)], 2, 0) (* control chars -> remove *)
    ; (0, [(0x7F, 0x7F)], 2, 0)
      (* DEL -> remove *)

      (* CSS injection breakers -> remove *)
    ; (0, [(0x3B, 0x3B)], 3, 0) (* semicolon -> remove *)
    ; (0, [(0x7B, 0x7B)], 3, 0) (* left brace -> remove *)
    ; (0, [(0x7D, 0x7D)], 3, 0)
      (* right brace -> remove *)

      (* Line breaks -> space *)
    ; (0, [(0x0A, 0x0A)], 4, 0) (* newline -> space *)
    ; (0, [(0x0D, 0x0D)], 4, 0) (* CR -> space *)

                                (* CSS string escaping *)
    ; (0, [(0x5C, 0x5C)], 5, 0) (* backslash -> \\\\ *)
    ; (0, [(0x22, 0x22)], 6, 0) (* double quote -> escaped quote *)
    ; (0, [(0x27, 0x27)], 7, 0)
      (* single quote -> escaped apostrophe *)

      (* All other safe characters - identity *)
    ; ( 0
      , [ (0x09, 0x09)
        ; (0x20, 0x20)
        ; (0x23, 0x39)
        ; (0x3C, 0x5B)
        ; (0x5D, 0x60)
        ; (0x66, 0x6E)
        ; (0x6F, 0x71)
        ; (0x73, 0x74)
        ; (0x76, 0x77)
        ; (0x79, 0x7A)
        ; (0x7E, 0x11FFFF) ]
      , 0
      , 0 )
      (* Reject state - once in state 99, everything is rejected *)
    ; (99, [(0x00, 0x11FFFF)], 8, 99)
      (* Any char in reject state -> empty output *) ]
  in
  let init = [0] in
  let accepts =
    [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20]
  in
  (* Accept all states except 99 *)
  nft_construct_str (states, transitions, init, accepts, output_fun)
