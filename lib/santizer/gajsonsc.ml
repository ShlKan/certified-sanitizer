(* JavaScript jsonEsc function implementation:

   function jsonEsc(value) { return String(value) .replace(/\\/g, '\\\\') //
   Escape backslashes .replace(/quote/g, 'backslash-quote') // Escape double
   quotes .replace(/\u0008/g, 'backslash-b') // Backspace .replace(/\t/g,
   'backslash-t') // Tab .replace(/\n/g, 'backslash-n') // Newline
   .replace(/\f/g, 'backslash-f') // Form feed .replace(/\r/g, 'backslash-r')
   // Carriage return .replace(/\u2028/g, 'backslash-u2028') // Line
   separator .replace(/\u2029/g, 'backslash-u2029'); // Paragraph separator }

   This function escapes characters for safe JSON string representation,
   including the standard JSON escape sequences and Unicode line
   separators. *)

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
  (* case for backslash -> escaped backslash *)
  | 2 -> fun _ -> Some [(["\\\\"], ["\\\\"])]
  (* case for double quote -> escaped quote *)
  | 3 -> fun _ -> Some [(["\\\""], ["\\\""])]
  (* case for backspace -> JSON backspace *)
  | 4 -> fun _ -> Some [(["\\b"], ["\\b"])]
  (* case for tab -> JSON tab *)
  | 5 -> fun _ -> Some [(["\\t"], ["\\t"])]
  (* case for newline -> JSON newline *)
  | 6 -> fun _ -> Some [(["\\n"], ["\\n"])]
  (* case for form feed -> JSON form feed *)
  | 7 -> fun _ -> Some [(["\\f"], ["\\f"])]
  (* case for carriage return -> JSON CR *)
  | 8 -> fun _ -> Some [(["\\r"], ["\\r"])]
  (* case for line separator U+2028 -> JSON line sep *)
  | 9 -> fun _ -> Some [(["\\u2028"], ["\\u2028"])]
  (* case for paragraph separator U+2029 -> JSON para sep *)
  | 10 -> fun _ -> Some [(["\\u2029"], ["\\u2029"])]
  (* case for identity function - all other characters *)
  | _ -> (
      fun input ->
        match input with
        | None -> None
        | Some x ->
            let str = Z.to_string x in
            Some [([str], [str])] )

let nft_gajsonsc () =
  let states = [0] in
  let transitions =
    [ (* JSON special character transitions *)
      (0, [(0x5C, 0x5C)], 2, 0) (* backslash (0x5C) -> escaped backslash *)
    ; (0, [(0x22, 0x22)], 3, 0) (* double quote (0x22) -> escaped quote *)
    ; (0, [(0x08, 0x08)], 4, 0) (* backspace (0x08) -> JSON backspace *)
    ; (0, [(0x09, 0x09)], 5, 0) (* tab (0x09) -> JSON tab *)
    ; (0, [(0x0A, 0x0A)], 6, 0) (* newline (0x0A) -> JSON newline *)
    ; (0, [(0x0C, 0x0C)], 7, 0) (* form feed (0x0C) -> JSON form feed *)
    ; (0, [(0x0D, 0x0D)], 8, 0) (* carriage return (0x0D) -> JSON CR *)
    ; (0, [(0x2028, 0x2028)], 9, 0)
      (* line separator (U+2028) -> JSON line sep *)
    ; (0, [(0x2029, 0x2029)], 10, 0)
      (* paragraph separator (U+2029) -> JSON para sep *)
    ; (* All other characters - identity function *)
      ( 0
      , [ (0x00, 0x07) (* 0x00-0x07 (excluding 0x08 backspace) *)
        ; (0x0B, 0x0B) (* 0x0B (vertical tab) *)
        ; (0x0E, 0x21) (* 0x0E-0x21 (excluding 0x22 quote) *)
        ; (0x23, 0x5B) (* 0x23-0x5B (excluding 0x5C backslash) *)
        ; (0x5D, 0x2027) (* 0x5D-0x2027 (excluding U+2028) *)
        ; (0x202A, 0x11FFFF) ]
        (* 0x202A and above (excluding U+2029) *)
      , 0
      , 0 ) ]
  in
  let init = [0] in
  let accepts = [0] in
  nft_construct_str (states, transitions, init, accepts, output_fun)
