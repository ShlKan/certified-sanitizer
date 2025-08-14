open Transducer
open ITransducer

(* HTML (text-node) — unsafe if it contains characters that must be encoded:
   Σ* ( [&<>\doublequote'] | <\s*/\s*[sS][cC][rR][iI][pP][tT]\b ) Σ**)
let attack_html_text =
  nfa_construct_reachable
    (nfa_construct
       ( [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 13; 14]
       , [ (* Loop Σ* before *)
           (0, [(0x0000, 0x10FFFF)], 0)
         ; (* From Σ* to first symbol of each alternative *)
           (* [&<>] branch *)
           (0, [(0x26, 0x26)], 13)
         ; (* & *)
           (0, [(0x3C, 0x3C)], 13)
         ; (* < *)
           (0, [(0x3E, 0x3E)], 13)
         ; (* > *)

           (* [\doublequote'] branch *)
           (0, [(0x22, 0x22)], 13)
         ; (* \doublequot *)
           (0, [(0x27, 0x27)], 13)
         ; (* ' *)

           (* </\s*script\b branch *)
           (0, [(0x3C, 0x3C)], 3)
         ; (* '<' *)
           (3, [(0x2F, 0x2F)], 4)
         ; (* '/' *)

           (* \s* : loop on whitespace *)
           (4, [(0x09, 0x09)], 4)
         ; (* HT *)
           (4, [(0x0A, 0x0A)], 4)
         ; (* LF *)
           (4, [(0x0B, 0x0B)], 4)
         ; (* VT *)
           (4, [(0x0C, 0x0C)], 4)
         ; (* FF *)
           (4, [(0x0D, 0x0D)], 4)
         ; (* CR *)
           (4, [(0x20, 0x20)], 4)
         ; (* space *)

           (* from whitespace loop or directly after '/' to 's' *)
           (4, [(0x73, 0x73)], 5)
         ; (* 's' *)
           (3, [(0x73, 0x73)], 5)
         ; (5, [(0x63, 0x63)], 6)
         ; (* 'c' *)
           (6, [(0x72, 0x72)], 7)
         ; (* 'r' *)
           (7, [(0x69, 0x69)], 8)
         ; (* 'i' *)
           (8, [(0x70, 0x70)], 9)
         ; (* 'p' *)
           (9, [(0x74, 0x74)], 10)
         ; (* 't' *)

           (* word-boundary approximation: require NonWord after t *)
           (10, [(0x0000, 0x002F)], 13)
         ; (10, [(0x003A, 0x0040)], 13)
         ; (10, [(0x005B, 0x005E)], 13)
         ; (10, [(0x0060, 0x0060)], 13)
         ; (10, [(0x007B, 0x10FFFF)], 13)
         ; (* Σ* after middle group *)
           (13, [(0x0000, 0x10FFFF)], 14)
         ; (14, [(0x0000, 0x10FFFF)], 14) ]
       , [0]
       , [13; 14] ) )

(* HTML attribute value — unsafe if it breaks or escapes the attribute NFA
   (regex): Σ* ( [\doublequote'`<>&] | [\x00-\x08\x0B\x0C\x0E-\x1F\x7F] |
   [\r\n] ) Σ**)

let attack_html_attr =
  nfa_construct_reachable
    (nfa_construct
       ( (* states *) [0; 1]
       , (* transitions: (from_state, [ (lo,hi); ... ], to_state) *)
         [ (* Σ* before *)
           (0, [(0x0000, 0x10FFFF)], 0)
         ; (* Middle group: [\doublequote'`<>&] | [\x00-\x08 \x0B \x0C
              \x0E-\x1F \x7F] | [\r\n] *)
           ( 0
           , [ (* ["'`<>&] *) (0x22,0x22); (* " *)
               (0x27, 0x27)
             ; (* ' *)
               (0x60, 0x60)
             ; (* ` *)
               (0x3C, 0x3C)
             ; (* < *)
               (0x3E, 0x3E)
             ; (* > *)
               (0x26, 0x26)
             ; (* & *)

               (* [\x00-\x08 \x0B \x0C \x0E-\x1F \x7F] *)
               (0x0000, 0x0008)
             ; (0x000B, 0x000B)
             ; (0x000C, 0x000C)
             ; (0x000E, 0x001F)
             ; (0x007F, 0x007F)
             ; (* [\r\n] *)
               (0x000A, 0x000A)
             ; (* \n *)
               (0x000D, 0x000D) (* \r *) ]
           , 1 )
         ; (* Σ* after *)
           (1, [(0x0000, 0x10FFFF)], 1) ]
       , (* start *) [0]
       , (* accepting *) [1] ) )

(* JavaScript string literal (inside <script> or event handlers) Σ* (
   (?<!\\)["'] | \\(?!["'\\nrtbfux0-9]) | \r | \n | \u2028 | \u2029 ) Σ* *)

let attach_javascript =
  nfa_construct_reachable
    (nfa_construct
       ( (* states *)
         [0; 1; 2]
       , (* transitions: (from_state, [ (lo,hi); ... ], to_state) *)
         [ (* --- Scanning with memory of last backslash --- *)

           (* From state 0 (prev != '\'): on '\' go to state 1; on anything
              else stay in 0 *)
           (0, [(0x5C, 0x5C)], 1)
         ; (* '\' *)
           (0, [(0x0000, 0x005B); (0x005D, 0x10FFFF)], 0)
         ; (* Σ \ {'\'} *)

           (* From state 1 (prev == '\'): another '\' keeps us in 1 *)
           (1, [(0x5C, 0x5C)], 1)
         ; (* After a backslash, if next is a *valid* escape starter, just
              continue scanning: [\doublequote '\\nrtbfux0-9] -> go to state
              based on what we just consumed *)
           (1, [(0x22, 0x22)], 0)
         ; (* \doublequote *)
           (1, [(0x27, 0x27)], 0)
         ; (* ' *)
           (1, [(0x6E, 0x6E)], 0)
         ; (* n *)
           (1, [(0x72, 0x72)], 0)
         ; (* r *)
           (1, [(0x74, 0x74)], 0)
         ; (* t *)
           (1, [(0x62, 0x62)], 0)
         ; (* b *)
           (1, [(0x66, 0x66)], 0)
         ; (* f *)
           (1, [(0x75, 0x75)], 0)
         ; (* u *)
           (1, [(0x78, 0x78)], 0)
         ; (* x *)
           (1, [(0x30, 0x39)], 0)
         ; (* 0-9 *)
           ( 1
           , [ (0x0000, 0x0021)
             ; (0x0023, 0x0026)
             ; (0x0028, 0x002F)
             ; (0x003A, 0x005B)
             ; (0x005D, 0x0061)
             ; (0x0063, 0x0065)
             ; (0x0067, 0x006D)
             ; (0x006F, 0x0071)
             ; (0x0073, 0x0073)
             ; (0x0076, 0x0077)
             ; (0x0079, 0x10FFFF) ]
           , 2 )
         ; (* --- Branch: (?<!\\)[\doublequote'] — only from state 0 (prev !=
              '\') --- *)
           (0, [(0x22, 0x22)], 2)
         ; (* \doublequote *)
           (0, [(0x27, 0x27)], 2)
         ; (* ' *)

           (* --- Branches: \r | \n | \u2028 | \u2029 (from anywhere while
              scanning) --- *)
           (0, [(0x000D, 0x000D)], 2)
         ; (* \r *)
           (0, [(0x000A, 0x000A)], 2)
         ; (* \n *)
           (0, [(0x2028, 0x2028)], 2)
         ; (* U+2028 *)
           (0, [(0x2029, 0x2029)], 2)
         ; (* U+2029 *)
           (1, [(0x000D, 0x000D)], 2)
         ; (* \r after '\' *)
           (1, [(0x000A, 0x000A)], 2)
         ; (* \n after '\' *)
           (1, [(0x2028, 0x2028)], 2)
         ; (1, [(0x2029, 0x2029)], 2)
         ; (* --- Trailing Σ* --- *)
           (2, [(0x0000, 0x10FFFF)], 2) ]
       , (* start *) [0]
       , (* accepting *) [2] ) )

(* xml Σ* ( \doublequote | \\(?![\doublequote\\/bfnrtu] ) | [\x00-\x1F] |
   \u2028 | \u2029 ) Σ**)
let attack_xml =
  nfa_construct_reachable
    (nfa_construct
       ( (* states *) [0; 10; 1]
       , (* transitions: (from_state, [ (lo,hi); ... ], to_state) *)
         [ (* Σ* before: scan anywhere *)
           (0, [(0x0000, 0x10FFFF)], 0)
         ; (* Branch: a literal double quote *)
           (0, [(0x22, 0x22)], 1)
         ; (* Branch: control characters [\x00-\x1F] *)
           (0, [(0x0000, 0x001F)], 1)
         ; (* Branch: explicit Unicode line separators *)
           (0, [(0x2028, 0x2028)], 1)
         ; (0, [(0x2029, 0x2029)], 1)
         ; (* Branch: backslash with negative lookahead
              \\(?![\\doublequote\/bfnrtu]) *)
           (0, [(0x5C, 0x5C)], 10)
         ; (* '\' *)

           (* If the char after '\' is an allowed escape starter, do NOT
              match; resume scanning *)
           (10, [(0x22, 0x22)], 0)
         ; (* \doublequote *)
           (10, [(0x2F, 0x2F)], 0)
         ; (* / *)
           (10, [(0x5C, 0x5C)], 0)
         ; (* \ *)
           (10, [(0x62, 0x62)], 0)
         ; (* b *)
           (10, [(0x66, 0x66)], 0)
         ; (* f *)
           (10, [(0x6E, 0x6E)], 0)
         ; (* n *)
           (10, [(0x72, 0x72)], 0)
         ; (* r *)
           (10, [(0x74, 0x74)], 0)
         ; (* t *)
           (10, [(0x75, 0x75)], 0)
         ; (* u *)
           ( 10
           , [ (0x0000, 0x0021)
             ; (* up to '!' *)
               (0x0023, 0x002E)
             ; (* skip '\doublequote' and '/' *)
               (0x0030, 0x005B)
             ; (* '0'..'[' (skip '\') *)
               (0x005D, 0x0061)
             ; (* ']'..'a' (skip 'b') *)
               (0x0063, 0x0065)
             ; (* 'c'..'e' (skip 'f') *)
               (0x0067, 0x006D)
             ; (* 'g'..'m' (skip 'n') *)
               (0x006F, 0x0071)
             ; (* 'o'..'q' (skip 'r') *)
               (0x0073, 0x0073)
             ; (* 's' (skip 't') *)
               (0x0076, 0x10FFFF) (* 'v'..end (skip 'u') *) ]
           , 1 )
         ; (* Σ* after *)
           (1, [(0x0000, 0x10FFFF)], 1) ]
       , (* start state *) [0]
       , (* accepting states *) [1] ) )

(* XML attack: Σ* ( [&<>] | [\doublequote'] | [\x00-\x08\x0B\x0C\x0E-\x1F] )
   Σ* *)
let attack_xml =
  nfa_construct_reachable
    (nfa_construct
       ( (* states *) [0; 10; 1]
       , (* transitions: (from_state, [ (lo,hi); ... ], to_state) *)
         [ (* Σ* before: scan anywhere *)
           (0, [(0x0000, 0x10FFFF)], 0)
         ; (* Branch: a literal double quote *)
           (0, [(0x22, 0x22)], 1)
         ; (* Branch: control characters [\x00-\x1F] *)
           (0, [(0x0000, 0x001F)], 1)
         ; (* Branch: explicit Unicode line separators *)
           (0, [(0x2028, 0x2028)], 1)
         ; (0, [(0x2029, 0x2029)], 1)
         ; (0, [(0x5C, 0x5C)], 10)
         ; (* '\' *)

           (* If the char after '\' is an allowed escape starter, do NOT
              match; resume scanning *)
           (10, [(0x22, 0x22)], 0)
         ; (* \doublequote *)
           (10, [(0x2F, 0x2F)], 0)
         ; (* / *)
           (10, [(0x5C, 0x5C)], 0)
         ; (* \ *)
           (10, [(0x62, 0x62)], 0)
         ; (* b *)
           (10, [(0x66, 0x66)], 0)
         ; (* f *)
           (10, [(0x6E, 0x6E)], 0)
         ; (* n *)
           (10, [(0x72, 0x72)], 0)
         ; (* r *)
           (10, [(0x74, 0x74)], 0)
         ; (* t *)
           (10, [(0x75, 0x75)], 0)
         ; (* u *)

           (* Otherwise (not in [\doublequote\\/bfnrtu]), we MATCH the
              \\(?!...) branch *)
           ( 10
           , [ (0x0000, 0x0021)
             ; (* up to '!' *)
               (0x0023, 0x002E)
             ; (* skip '\doublequote' and '/' *)
               (0x0030, 0x005B)
             ; (* '0'..'[' (skip '\') *)
               (0x005D, 0x0061)
             ; (* ']'..'a' (skip 'b') *)
               (0x0063, 0x0065)
             ; (* 'c'..'e' (skip 'f') *)
               (0x0067, 0x006D)
             ; (* 'g'..'m' (skip 'n') *)
               (0x006F, 0x0071)
             ; (* 'o'..'q' (skip 'r') *)
               (0x0073, 0x0073)
             ; (* 's' (skip 't') *)
               (0x0076, 0x10FFFF) (* 'v'..end (skip 'u') *) ]
           , 1 )
         ; (* Σ* after *)
           (1, [(0x0000, 0x10FFFF)], 1) ]
       , (* start state *) [0]
       , (* accepting states *) [1] ) )

(* CSS attack: Σ* ( / \* [\s\S]*? \*/ # comment | ; | \{ | \} # declaration
   breakers | [eE][xX][pP][rR][eE][sS][sS][iI][oO][nN]\s*\( |
   @\s*[iI][mM][pP][oO][rR][tT] | [bB][eE][hH][aA][vV][iI][oO][rR]\s*: |
   -\s*[mM][oO][zZ]-\s*[bB][iI][nN][dD][iI][nN][gG] | (?!allowUrl)
   [uU][rR][lL]\s*\( # treat url( as unsafe unless explicitly allowed ) Σ* *)

let attack_css =
  nfa_construct_reachable
    (nfa_construct
       ( (* states *)
         [ 0
         ; (* start / Σ* before *)
           10
         ; 11
         ; 12
         ; 13
         ; (* single-character breakers *)
           (* (no extra states needed; go straight to 900) *)
           (* expression\( *)
           20
         ; 21
         ; 22
         ; 23
         ; 24
         ; 25
         ; 26
         ; 27
         ; 28
         ; 29
         ; 30
         ; (* '@' \s* 'import' *)
           40
         ; 41
         ; 42
         ; 43
         ; 44
         ; 45
         ; 46
         ; (* 'behavior' \s* ':' *)
           50
         ; 51
         ; 52
         ; 53
         ; 54
         ; 55
         ; 56
         ; 57
         ; 60
         ; 61
         ; 62
         ; 63
         ; 64
         ; 65
         ; 66
         ; 67
         ; 68
         ; 69
         ; 70
         ; 71
         ; (* 'url' \s* '(' *)
           80
         ; 81
         ; 82
         ; 83
         ; (* accepting *)
           900 ]
       , (* transitions: (from_state, [ (lo,hi); ... ], to_state) *)
         [ (* Σ* before *)
           (0, [(0x0000, 0x10FFFF)], 0)
         ; (0, [(0x2F, 0x2F)], 10)
         ; (* '/' *)
           (10, [(0x2A, 0x2A)], 11)
         ; (* '*' *)
           (* inside comment, generic char loop: *)
           (11, [(0x2A, 0x2A)], 12)
         ; (* possible end-seq start '*' *)
           (11, [(0x0000, 0x2A); (0x2B, 0x10FFFF)], 11)
         ; (* any not '*' -> stay *)
           (* seen one-or-more '*'s: *)
           (12, [(0x2A, 0x2A)], 12)
         ; (* absorb repeated '*' *)
           (12, [(0x2F, 0x2F)], 900)
         ; (* '*/' => accept *)
           (12, [(0x0000, 0x2E); (0x30, 0x10FFFF)], 11)
         ; (* anything else -> back to inside *)

           (* ---------- Declaration breakers ---------- *)
           (0, [(0x3B, 0x3B)], 900)
         ; (* ';' *)
           (0, [(0x7B, 0x7B)], 900)
         ; (* '{' *)
           (0, [(0x7D, 0x7D)], 900)
         ; (* '}' *)

           (* ---------- 'expression' \s* '(' ---------- *)
           (0, [(0x65, 0x65); (0x45, 0x45)], 20)
         ; (* e/E *)
           (20, [(0x78, 0x78); (0x58, 0x58)], 21)
         ; (* x/X *)
           (21, [(0x70, 0x70); (0x50, 0x50)], 22)
         ; (* p/P *)
           (22, [(0x72, 0x72); (0x52, 0x52)], 23)
         ; (* r/R *)
           (23, [(0x65, 0x65); (0x45, 0x45)], 24)
         ; (* e/E *)
           (24, [(0x73, 0x73); (0x53, 0x53)], 25)
         ; (* s/S *)
           (25, [(0x73, 0x73); (0x53, 0x53)], 26)
         ; (* s/S *)
           (26, [(0x69, 0x69); (0x49, 0x49)], 27)
         ; (* i/I *)
           (27, [(0x6F, 0x6F); (0x4F, 0x4F)], 28)
         ; (* o/O *)
           (28, [(0x6E, 0x6E); (0x4E, 0x4E)], 29)
         ; (* n/N *)
           (* \s* then '(' *)
           ( 29
           , [ (0x09, 0x09)
             ; (0x0A, 0x0A)
             ; (0x0B, 0x0B)
             ; (0x0C, 0x0C)
             ; (0x0D, 0x0D)
             ; (0x20, 0x20) ]
           , 29 )
         ; (29, [(0x28, 0x28)], 900)
         ; (* '(' *)

           (* ---------- '@' \s* 'import' ---------- *)
           (0, [(0x40, 0x40)], 40)
         ; (* '@' *)
           ( 40
           , [ (0x09, 0x09)
             ; (0x0A, 0x0A)
             ; (0x0B, 0x0B)
             ; (0x0C, 0x0C)
             ; (0x0D, 0x0D)
             ; (0x20, 0x20) ]
           , 40 )
         ; (40, [(0x69, 0x69); (0x49, 0x49)], 41)
         ; (* i/I *)
           (41, [(0x6D, 0x6D); (0x4D, 0x4D)], 42)
         ; (* m/M *)
           (42, [(0x70, 0x70); (0x50, 0x50)], 43)
         ; (* p/P *)
           (43, [(0x6F, 0x6F); (0x4F, 0x4F)], 44)
         ; (* o/O *)
           (44, [(0x72, 0x72); (0x52, 0x52)], 45)
         ; (* r/R *)
           (45, [(0x74, 0x74); (0x54, 0x54)], 900)
         ; (* t/T -> accept *)

           (* ---------- 'behavior' \s* ':' ---------- *)
           (0, [(0x62, 0x62); (0x42, 0x42)], 50)
         ; (* b/B *)
           (50, [(0x65, 0x65); (0x45, 0x45)], 51)
         ; (* e/E *)
           (51, [(0x68, 0x68); (0x48, 0x48)], 52)
         ; (* h/H *)
           (52, [(0x61, 0x61); (0x41, 0x41)], 53)
         ; (* a/A *)
           (53, [(0x76, 0x76); (0x56, 0x56)], 54)
         ; (* v/V *)
           (54, [(0x69, 0x69); (0x49, 0x49)], 55)
         ; (* i/I *)
           (55, [(0x6F, 0x6F); (0x4F, 0x4F)], 56)
         ; (* o/O *)
           (56, [(0x72, 0x72); (0x52, 0x52)], 57)
         ; (* r/R *)
           (* \s* then ':' *)
           ( 57
           , [ (0x09, 0x09)
             ; (0x0A, 0x0A)
             ; (0x0B, 0x0B)
             ; (0x0C, 0x0C)
             ; (0x0D, 0x0D)
             ; (0x20, 0x20) ]
           , 57 )
         ; (57, [(0x3A, 0x3A)], 900)
         ; (* ':' *)

           (* ---------- '-'\s*'moz'-\s*'binding' ---------- *)
           (0, [(0x2D, 0x2D)], 60)
         ; (* '-' *)
           ( 60
           , [ (0x09, 0x09)
             ; (0x0A, 0x0A)
             ; (0x0B, 0x0B)
             ; (0x0C, 0x0C)
             ; (0x0D, 0x0D)
             ; (0x20, 0x20) ]
           , 60 )
         ; (60, [(0x6D, 0x6D); (0x4D, 0x4D)], 61)
         ; (* m/M *)
           (61, [(0x6F, 0x6F); (0x4F, 0x4F)], 62)
         ; (* o/O *)
           (62, [(0x7A, 0x7A); (0x5A, 0x5A)], 63)
         ; (* z/Z *)
           (63, [(0x2D, 0x2D)], 64)
         ; (* '-' *)
           ( 64
           , [ (0x09, 0x09)
             ; (0x0A, 0x0A)
             ; (0x0B, 0x0B)
             ; (0x0C, 0x0C)
             ; (0x0D, 0x0D)
             ; (0x20, 0x20) ]
           , 64 )
         ; (64, [(0x62, 0x62); (0x42, 0x42)], 65)
         ; (* b/B *)
           (65, [(0x69, 0x69); (0x49, 0x49)], 66)
         ; (* i/I *)
           (66, [(0x6E, 0x6E); (0x4E, 0x4E)], 67)
         ; (* n/N *)
           (67, [(0x64, 0x64); (0x44, 0x44)], 68)
         ; (* d/D *)
           (68, [(0x69, 0x69); (0x49, 0x49)], 69)
         ; (* i/I *)
           (69, [(0x6E, 0x6E); (0x4E, 0x4E)], 70)
         ; (* n/N *)
           (70, [(0x67, 0x67); (0x47, 0x47)], 71)
         ; (* g/G *)
           (71, [(0x0000, 0x10FFFF)], 900)
         ; (* after full keyword -> accept (optionally could require a
              boundary) *)
           (0, [(0x75, 0x75); (0x55, 0x55)], 80)
         ; (* u/U *)
           (80, [(0x72, 0x72); (0x52, 0x52)], 81)
         ; (* r/R *)
           (81, [(0x6C, 0x6C); (0x4C, 0x4C)], 82)
         ; (* l/L *)
           ( 82
           , [ (0x09, 0x09)
             ; (0x0A, 0x0A)
             ; (0x0B, 0x0B)
             ; (0x0C, 0x0C)
             ; (0x0D, 0x0D)
             ; (0x20, 0x20) ]
           , 82 )
         ; (82, [(0x28, 0x28)], 900)
         ; (* '(' *)

           (* ---------- Σ* after ---------- *)
           (900, [(0x0000, 0x10FFFF)], 900) ]
       , (* start *) [0]
       , (* accepting *) [900] ) )

let universial_nfa =
  (* Universal automaton for Σ* over Unicode: - Single accepting state 0 -
     Self-loop on all code points 0x0000..0x10FFFF - Accepts every string
     (including empty) *)
  nfa_construct_reachable
    (nfa_construct
       ( (* states *) [0]
       , (* transitions *) [(0, [(0x0000, 0x10FFFF)], 0)]
       , (* start *) [0]
       , (* accepting *) [0] ) )
