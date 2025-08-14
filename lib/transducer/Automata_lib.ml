module Uint : sig
  type t = int

  val dflt_size : Z.t

  val less : t -> t -> bool

  val less_eq : t -> t -> bool

  val shiftl : t -> Z.t -> t

  val shiftr : t -> Z.t -> t

  val shiftr_signed : t -> Z.t -> t

  val test_bit : t -> Z.t -> bool

  val int_mask : int

  val int32_mask : int32

  val int64_mask : int64
end = struct
  type t = int

  let dflt_size = Z.of_int Sys.int_size

  (* negative numbers have their highest bit set, so they are greater than
     positive ones *)
  let less x y = if x < 0 then y < 0 && x < y else y < 0 || x < y

  let less_eq x y = if x < 0 then y < 0 && x <= y else y < 0 || x <= y

  let shiftl x n = x lsl Z.to_int n

  let shiftr x n = x lsr Z.to_int n

  let shiftr_signed x n = x asr Z.to_int n

  let test_bit x n = x land (1 lsl Z.to_int n) <> 0

  let int_mask = if Sys.int_size < 32 then lnot 0 else 0xFFFFFFFF

  let int32_mask =
    if Sys.int_size < 32 then
      Int32.pred (Int32.shift_left Int32.one Sys.int_size)
    else Int32.of_string "0xFFFFFFFF"

  let int64_mask =
    if Sys.int_size < 64 then
      Int64.pred (Int64.shift_left Int64.one Sys.int_size)
    else Int64.of_string "0xFFFFFFFFFFFFFFFF"
end

(*struct Uint*)

module Uint32 : sig
  val less : int32 -> int32 -> bool

  val less_eq : int32 -> int32 -> bool

  val shiftl : int32 -> Z.t -> int32

  val shiftr : int32 -> Z.t -> int32

  val shiftr_signed : int32 -> Z.t -> int32

  val test_bit : int32 -> Z.t -> bool
end = struct
  (* negative numbers have their highest bit set, so they are greater than
     positive ones *)
  let less x y =
    if Int32.compare x Int32.zero < 0 then
      Int32.compare y Int32.zero < 0 && Int32.compare x y < 0
    else Int32.compare y Int32.zero < 0 || Int32.compare x y < 0

  let less_eq x y =
    if Int32.compare x Int32.zero < 0 then
      Int32.compare y Int32.zero < 0 && Int32.compare x y <= 0
    else Int32.compare y Int32.zero < 0 || Int32.compare x y <= 0

  let shiftl x n = Int32.shift_left x (Z.to_int n)

  let shiftr x n = Int32.shift_right_logical x (Z.to_int n)

  let shiftr_signed x n = Int32.shift_right x (Z.to_int n)

  let test_bit x n =
    Int32.compare
      (Int32.logand x (Int32.shift_left Int32.one (Z.to_int n)))
      Int32.zero
    <> 0
end

(*struct Uint32*)

module Automata_lib : sig
  type 'a equal = {equal: 'a -> 'a -> bool}

  type nat = Nat of Z.t

  type 'a nFA_states = {states_enumerate: nat -> 'a}

  type 'a ord = {less_eq: 'a -> 'a -> bool; less: 'a -> 'a -> bool}

  type 'a preorder = {ord_preorder: 'a ord}

  type 'a order = {preorder_order: 'a preorder}

  type 'a linorder = {order_linorder: 'a order}

  val equal_prod : 'a equal -> 'b equal -> ('a * 'b) equal

  val linorder_prod : 'a linorder -> 'b linorder -> ('a * 'b) linorder

  val linorder_list : 'a equal * 'a linorder -> 'a list linorder

  val integer_of_nat : nat -> Z.t

  val nat_of_integer : Z.t -> nat

  type ('b, 'a) rbt

  val rs_nfa_qsize :
       'a nFA_states * 'a linorder
    -> 'b linorder
    -> ('a, unit) rbt
       * ( ('a, (('b * 'b) list, ('a, unit) rbt) rbt) rbt
         * (('a, unit) rbt * ('a, unit) rbt) )
    -> nat

  val rs_nfa_union :
       'a nFA_states * 'a linorder
    -> 'b equal * 'b linorder
    -> ('a, unit) rbt
       * ( ('a, (('b * 'b) list, ('a, unit) rbt) rbt) rbt
         * (('a, unit) rbt * ('a, unit) rbt) )
    -> ('a, unit) rbt
       * ( ('a, (('b * 'b) list, ('a, unit) rbt) rbt) rbt
         * (('a, unit) rbt * ('a, unit) rbt) )
    -> ('a, unit) rbt
       * ( ('a, (('b * 'b) list, ('a, unit) rbt) rbt) rbt
         * (('a, unit) rbt * ('a, unit) rbt) )

  val prod_encode : nat * nat -> nat

  val rs_gen_S_from_list : 'a linorder -> 'a list -> ('a, unit) rbt

  val rs_gen_rm_from_list :
       'a linorder
    -> 'b nFA_states * 'b linorder
    -> 'c equal * 'c linorder
    -> ( 'a
       * ( ('b, unit) rbt
         * ( ('b, (('c * 'c) list, ('b, unit) rbt) rbt) rbt
           * (('b, unit) rbt * ('b, unit) rbt) ) ) )
       list
    -> ( 'a
       , ('b, unit) rbt
         * ( ('b, (('c * 'c) list, ('b, unit) rbt) rbt) rbt
           * (('b, unit) rbt * ('b, unit) rbt) ) )
       rbt

  val rs_gen_rc_from_list :
       'a linorder
    -> ('a * ('a * 'a) list) list
    -> ('a, ('a * 'a, unit) rbt) rbt

  val rs_S_to_list : 'a linorder -> ('a, unit) rbt -> 'a list

  val rs_rm_to_list :
       'a linorder
    -> 'b nFA_states * 'b linorder
    -> 'c equal * 'c linorder
    -> ( 'a
       , ('b, unit) rbt
         * ( ('b, (('c * 'c) list, ('b, unit) rbt) rbt) rbt
           * (('b, unit) rbt * ('b, unit) rbt) ) )
       rbt
    -> ( 'a
       * ( ('b, unit) rbt
         * ( ('b, (('c * 'c) list, ('b, unit) rbt) rbt) rbt
           * (('b, unit) rbt * ('b, unit) rbt) ) ) )
       list

  val rs_indegree :
       'a equal * 'a linorder
    -> ('a, unit) rbt
    -> ('a, ('a * 'a, unit) rbt) rbt
    -> bool

  val rs_nfa_rename :
       'a linorder
    -> 'b equal * 'b linorder
    -> 'c linorder
    -> ('a, unit) rbt
       * ( ('a, (('b * 'b) list, ('a, unit) rbt) rbt) rbt
         * (('a, unit) rbt * ('a, unit) rbt) )
    -> ('a -> 'c)
    -> ('c, unit) rbt
       * ( ('c, (('b * 'b) list, ('c, unit) rbt) rbt) rbt
         * (('c, unit) rbt * ('c, unit) rbt) )

  val rs_nfa_uniq :
       'a equal * 'a linorder
    -> 'd nFA_states * 'd linorder
    -> ('a -> 'a)
    -> ('a -> 'a)
    -> 'b
       * ('c * (('d, (('a * 'a) list, ('d, unit) rbt) rbt) rbt * ('e * 'f)))
    -> 'b
       * ('c * (('d, (('a * 'a) list, ('d, unit) rbt) rbt) rbt * ('e * 'f)))

  val rs_nfa_concate :
       'a nFA_states * 'a linorder
    -> 'b equal * 'b linorder
    -> ('a, unit) rbt
       * ( ('a, (('b * 'b) list, ('a, unit) rbt) rbt) rbt
         * (('a, unit) rbt * ('a, unit) rbt) )
    -> ('a, unit) rbt
       * ( ('a, (('b * 'b) list, ('a, unit) rbt) rbt) rbt
         * (('a, unit) rbt * ('a, unit) rbt) )
    -> ('a, unit) rbt
       * ( ('a, (('b * 'b) list, ('a, unit) rbt) rbt) rbt
         * (('a, unit) rbt * ('a, unit) rbt) )

  val rs_nfa_destruct :
       'a nFA_states * 'a linorder
    -> 'b equal * 'b linorder
    -> ('a, unit) rbt
       * ( ('a, (('b * 'b) list, ('a, unit) rbt) rbt) rbt
         * (('a, unit) rbt * ('a, unit) rbt) )
    -> 'a list * (('a * (('b * 'b) list * 'a)) list * ('a list * 'a list))

  val rs_nfa_bool_comb :
       'a nFA_states * 'a linorder
    -> 'b equal * 'b linorder
    -> (bool -> bool -> bool)
    -> ('a, unit) rbt
       * ( ('a, (('b * 'b) list, ('a, unit) rbt) rbt) rbt
         * (('a, unit) rbt * ('a, unit) rbt) )
    -> ('a, unit) rbt
       * ( ('a, (('b * 'b) list, ('a, unit) rbt) rbt) rbt
         * (('a, unit) rbt * ('a, unit) rbt) )
    -> ('a, unit) rbt
       * ( ('a, (('b * 'b) list, ('a, unit) rbt) rbt) rbt
         * (('a, unit) rbt * ('a, unit) rbt) )

  val rs_nfa_elim :
       'a equal * 'a nFA_states * 'a linorder
    -> 'b equal * 'b linorder
    -> ('a, unit) rbt
       * ( ('a, (('b * 'b) list, ('a, unit) rbt) rbt) rbt
         * (('a * 'a, unit) rbt * (('a, unit) rbt * ('a, unit) rbt)) )
    -> ('a, unit) rbt
       * ( ('a, (('b * 'b) list, ('a, unit) rbt) rbt) rbt
         * (('a, unit) rbt * ('a, unit) rbt) )

  val rs_nfa_complement :
       'a nFA_states * 'a linorder
    -> ('a, unit) rbt * ('b * ('c * ('d * ('a, unit) rbt)))
    -> ('a, unit) rbt * ('b * ('c * ('d * ('a, unit) rbt)))

  val rs_nfa_determinise :
       'a nFA_states * 'a linorder
    -> 'b equal * 'b linorder
    -> ('a, unit) rbt
       * ( ('b * 'b) list
         * ( ('a, (('b * 'b) list, ('a, unit) rbt) rbt) rbt
           * (('a, unit) rbt * ('a, unit) rbt) ) )
    -> ('a, unit) rbt
       * ( ('b * 'b) list
         * ( ('a, (('b * 'b) list, ('a, unit) rbt) rbt) rbt
           * (('a, unit) rbt * ('a, unit) rbt) ) )

  val rs_nfa_concate_rename :
       'a nFA_states * 'a linorder
    -> 'b equal * 'b linorder
    -> ('a -> 'a * 'a)
    -> ('a -> 'a * 'a)
    -> ('a, unit) rbt
       * ( ('a, (('b * 'b) list, ('a, unit) rbt) rbt) rbt
         * (('a, unit) rbt * ('a, unit) rbt) )
    -> ('a, unit) rbt
       * ( ('a, (('b * 'b) list, ('a, unit) rbt) rbt) rbt
         * (('a, unit) rbt * ('a, unit) rbt) )
    -> ('a, unit) rbt
       * ( ('a, (('b * 'b) list, ('a, unit) rbt) rbt) rbt
         * (('a, unit) rbt * ('a, unit) rbt) )

  val rs_nfa_normal :
       'a nFA_states * 'a linorder
    -> 'b equal * 'b linorder
    -> ('a * 'a, unit) rbt
       * ( ('a * 'a, (('b * 'b) list, ('a * 'a, unit) rbt) rbt) rbt
         * (('a * 'a, unit) rbt * ('a * 'a, unit) rbt) )
    -> ('a, unit) rbt
       * ( ('a, (('b * 'b) list, ('a, unit) rbt) rbt) rbt
         * (('a, unit) rbt * ('a, unit) rbt) )

  val rs_nft_destruct :
       'a nFA_states * 'a linorder
    -> 'c linorder
    -> 'd linorder
    -> ('a, unit) rbt
       * ( 'b
         * ( ('a, (('c * 'c) list option * 'd, ('a, unit) rbt) rbt) rbt
           * (('a, unit) rbt * (('a, unit) rbt * 'e)) ) )
    -> 'a list
       * ( ('a * ((('c * 'c) list option * 'd) * 'a)) list
         * ('a list * ('a list * 'e)) )

  val rs_nfae_destruct :
       'a nFA_states * 'a linorder
    -> 'b equal * 'b linorder
    -> ('a * 'a, unit) rbt
       * ( ('a * 'a, (('b * 'b) list, ('a * 'a, unit) rbt) rbt) rbt
         * ( (('a * 'a) * ('a * 'a), unit) rbt
           * (('a * 'a, unit) rbt * ('a * 'a, unit) rbt) ) )
    -> ('a * 'a) list
       * ( (('a * 'a) * (('b * 'b) list * ('a * 'a))) list
         * ((('a * 'a) * ('a * 'a)) list * (('a * 'a) list * ('a * 'a) list))
         )

  val rs_nfa_tran_complement :
       'a equal * 'a linorder
    -> 'b equal * 'b nFA_states * 'b linorder
    -> ('a -> 'a)
    -> ('a -> 'a)
    -> ('b, unit) rbt
       * ( ('a * 'a) list
         * (('b, (('a * 'a) list, ('b, unit) rbt) rbt) rbt * ('c * 'd)) )
    -> 'b
    -> ('b, unit) rbt
       * ( ('a * 'a) list
         * (('b, (('a * 'a) list, ('b, unit) rbt) rbt) rbt * ('c * 'd)) )

  val rs_nfa_construct_interval :
       'a nFA_states * 'a linorder
    -> 'b equal * 'b linorder
    -> 'a list * (('a * (('b * 'b) list * 'a)) list * ('a list * 'a list))
    -> ('a, unit) rbt
       * ( ('a, (('b * 'b) list, ('a, unit) rbt) rbt) rbt
         * (('a, unit) rbt * ('a, unit) rbt) )

  val rs_nfa_construct_reachable :
       'a nFA_states * 'a linorder
    -> 'b equal * 'b linorder
    -> ('a, unit) rbt
       * ( ('a, (('b * 'b) list, ('a, unit) rbt) rbt) rbt
         * (('a, unit) rbt * ('a, unit) rbt) )
    -> ('a, unit) rbt
       * ( ('a, (('b * 'b) list, ('a, unit) rbt) rbt) rbt
         * (('a, unit) rbt * ('a, unit) rbt) )

  val rs_product_transducer :
       'a nFA_states * 'a linorder
    -> 'b equal * 'b linorder
    -> 'c linorder
    -> 'e equal * 'e linorder
    -> ('a, unit) rbt
       * ( ('a, (('b * 'b) list option * 'c, ('a, unit) rbt) rbt) rbt
         * ( ('a, unit) rbt
           * (('a, unit) rbt * ('c -> 'd option -> ('e * 'e) list option)) )
         )
    -> ('a, unit) rbt
       * ( ('a, (('b * 'b) list, ('a, unit) rbt) rbt) rbt
         * (('a, unit) rbt * ('a, unit) rbt) )
    -> (   ('d option -> ('e * 'e) list option)
        -> ('b * 'b) list
        -> ('e * 'e) list option )
    -> (('d option -> ('e * 'e) list option) -> ('b * 'b) list -> bool)
    -> ('a * 'a, unit) rbt
       * ( ('a * 'a, (('e * 'e) list, ('a * 'a, unit) rbt) rbt) rbt
         * ( (('a * 'a) * ('a * 'a), unit) rbt
           * (('a * 'a, unit) rbt * ('a * 'a, unit) rbt) ) )

  val rs_nft_construct_interval :
       'a nFA_states * 'a linorder
    -> 'b equal * 'b linorder
    -> 'c linorder
    -> 'd linorder
    -> 'a list
       * ( ('a * ((('b * 'b) list option * 'c) * 'a)) list
         * ('a list * ('a list * ('c -> 'b option -> ('d * 'd) list option)))
         )
    -> ('a, unit) rbt
       * ( ('a, (('b * 'b) list option * 'c, ('a, unit) rbt) rbt) rbt
         * ( ('a, unit) rbt
           * (('a, unit) rbt * ('c -> 'b option -> ('d * 'd) list option)) )
         )
end = struct
  type nat = Nat of Z.t

  let rec integer_of_nat (Nat x) = x

  let rec less_eq_nat m n = Z.leq (integer_of_nat m) (integer_of_nat n)

  type 'a ord = {less_eq: 'a -> 'a -> bool; less: 'a -> 'a -> bool}

  let less_eq _A = _A.less_eq

  let less _A = _A.less

  let rec less_nat m n = Z.lt (integer_of_nat m) (integer_of_nat n)

  let ord_nat = ({less_eq= less_eq_nat; less= less_nat} : nat ord)

  type 'a preorder = {ord_preorder: 'a ord}

  type 'a order = {preorder_order: 'a preorder}

  let preorder_nat = ({ord_preorder= ord_nat} : nat preorder)

  let order_nat = ({preorder_order= preorder_nat} : nat order)

  type 'a linorder = {order_linorder: 'a order}

  let linorder_nat = ({order_linorder= order_nat} : nat linorder)

  type 'a equal = {equal: 'a -> 'a -> bool}

  let equal _A = _A.equal

  let rec eq _A a b = equal _A a b

  let rec equal_list _A x0 x1 =
    match (x0, x1) with
    | [], x21 :: x22 -> false
    | x21 :: x22, [] -> false
    | x21 :: x22, y21 :: y22 -> eq _A x21 y21 && equal_list _A x22 y22
    | [], [] -> true

  let rec rec_list f1 f2 x2 =
    match (f1, f2, x2) with
    | f1, f2, [] -> f1
    | f1, f2, x21 :: x22 -> f2 x21 x22 (rec_list f1 f2 x22)

  let rec less_eq_list (_A1, _A2) =
   fun x y ->
    rec_list
      (fun a -> match a with [] -> false | _ :: _ -> true)
      (fun x_0 _ res_0 a ->
        match a with
        | [] -> false
        | y_0 :: y_1 -> less _A2 x_0 y_0 || (eq _A1 x_0 y_0 && res_0 y_1) )
      x y
    || equal_list _A1 x y

  let rec less_list (_A1, _A2) =
    rec_list
      (fun a -> match a with [] -> false | _ :: _ -> true)
      (fun x_0 _ res_0 a ->
        match a with
        | [] -> false
        | y_0 :: y_1 -> less _A2 x_0 y_0 || (eq _A1 x_0 y_0 && res_0 y_1) )

  let rec ord_list (_A1, _A2) =
    ( {less_eq= less_eq_list (_A1, _A2); less= less_list (_A1, _A2)}
      : 'a list ord )

  let rec preorder_list (_A1, _A2) =
    ( {ord_preorder= ord_list (_A1, _A2.preorder_order.ord_preorder)}
      : 'a list preorder )

  let rec order_list (_A1, _A2) =
    ({preorder_order= preorder_list (_A1, _A2)} : 'a list order)

  let rec linorder_list (_A1, _A2) =
    ( {order_linorder= order_list (_A1, _A2.order_linorder)}
      : 'a list linorder )

  let rec less_eq_option _A xa0 x =
    match (xa0, x) with
    | Some x, Some y -> less_eq _A.ord_preorder x y
    | Some x, None -> false
    | None, x -> true

  let rec less_option _A x xa1 =
    match (x, xa1) with
    | Some x, Some y -> less _A.ord_preorder x y
    | None, Some x -> true
    | x, None -> false

  let rec ord_option _A =
    ({less_eq= less_eq_option _A; less= less_option _A} : 'a option ord)

  let rec preorder_option _A =
    ({ord_preorder= ord_option _A} : 'a option preorder)

  let rec order_option _A =
    ({preorder_order= preorder_option _A.preorder_order} : 'a option order)

  let rec linorder_option _A =
    ({order_linorder= order_option _A.order_linorder} : 'a option linorder)

  let rec equal_proda _A _B (x1, x2) (y1, y2) = eq _A x1 y1 && eq _B x2 y2

  let rec equal_prod _A _B = ({equal= equal_proda _A _B} : ('a * 'b) equal)

  let rec less_eq_prod _A _B (x1, y1) (x2, y2) =
    less _A x1 x2 || (less_eq _A x1 x2 && less_eq _B y1 y2)

  let rec less_prod _A _B (x1, y1) (x2, y2) =
    less _A x1 x2 || (less_eq _A x1 x2 && less _B y1 y2)

  let rec ord_prod _A _B =
    ({less_eq= less_eq_prod _A _B; less= less_prod _A _B} : ('a * 'b) ord)

  let rec preorder_prod _A _B =
    ( {ord_preorder= ord_prod _A.ord_preorder _B.ord_preorder}
      : ('a * 'b) preorder )

  let rec order_prod _A _B =
    ( {preorder_order= preorder_prod _A.preorder_order _B.preorder_order}
      : ('a * 'b) order )

  let rec linorder_prod _A _B =
    ( {order_linorder= order_prod _A.order_linorder _B.order_linorder}
      : ('a * 'b) linorder )

  let ord_integer = ({less_eq= Z.leq; less= Z.lt} : Z.t ord)

  type 'a nFA_states = {states_enumerate: nat -> 'a}

  let states_enumerate _A = _A.states_enumerate

  type num = One | Bit0 of num | Bit1 of num

  type color = R | B

  type ('a, 'b) rbta =
    | Empty
    | Branch of color * ('a, 'b) rbta * 'a * 'b * ('a, 'b) rbta

  type ('b, 'a) rbt = RBT of ('b, 'a) rbta

  let rec id x = (fun xa -> xa) x

  let rec plus_nat m n = Nat (Z.add (integer_of_nat m) (integer_of_nat n))

  let one_nat : nat = Nat (Z.of_int 1)

  let rec suc n = plus_nat n one_nat

  let rec comp f g = fun x -> f (g x)

  let rec null = function [] -> true | x :: xs -> false

  let rec empty _A = RBT Empty

  let rec foldl f a x2 =
    match (f, a, x2) with
    | f, a, [] -> a
    | f, a, x :: xs -> foldl f (f a x) xs

  let rec foldr f x1 =
    match (f, x1) with f, [] -> id | f, x :: xs -> comp (f x) (foldr f xs)

  let rec balance x0 s t x3 =
    match (x0, s, t, x3) with
    | Branch (R, a, w, x, b), s, t, Branch (R, c, y, z, d) ->
        Branch (R, Branch (B, a, w, x, b), s, t, Branch (B, c, y, z, d))
    | Branch (R, Branch (R, a, w, x, b), s, t, c), y, z, Empty ->
        Branch (R, Branch (B, a, w, x, b), s, t, Branch (B, c, y, z, Empty))
    | ( Branch (R, Branch (R, a, w, x, b), s, t, c)
      , y
      , z
      , Branch (B, va, vb, vc, vd) ) ->
        Branch
          ( R
          , Branch (B, a, w, x, b)
          , s
          , t
          , Branch (B, c, y, z, Branch (B, va, vb, vc, vd)) )
    | Branch (R, Empty, w, x, Branch (R, b, s, t, c)), y, z, Empty ->
        Branch
          (R, Branch (B, Empty, w, x, b), s, t, Branch (B, c, y, z, Empty))
    | ( Branch (R, Branch (B, va, vb, vc, vd), w, x, Branch (R, b, s, t, c))
      , y
      , z
      , Empty ) ->
        Branch
          ( R
          , Branch (B, Branch (B, va, vb, vc, vd), w, x, b)
          , s
          , t
          , Branch (B, c, y, z, Empty) )
    | ( Branch (R, Empty, w, x, Branch (R, b, s, t, c))
      , y
      , z
      , Branch (B, va, vb, vc, vd) ) ->
        Branch
          ( R
          , Branch (B, Empty, w, x, b)
          , s
          , t
          , Branch (B, c, y, z, Branch (B, va, vb, vc, vd)) )
    | ( Branch (R, Branch (B, ve, vf, vg, vh), w, x, Branch (R, b, s, t, c))
      , y
      , z
      , Branch (B, va, vb, vc, vd) ) ->
        Branch
          ( R
          , Branch (B, Branch (B, ve, vf, vg, vh), w, x, b)
          , s
          , t
          , Branch (B, c, y, z, Branch (B, va, vb, vc, vd)) )
    | Empty, w, x, Branch (R, b, s, t, Branch (R, c, y, z, d)) ->
        Branch (R, Branch (B, Empty, w, x, b), s, t, Branch (B, c, y, z, d))
    | ( Branch (B, va, vb, vc, vd)
      , w
      , x
      , Branch (R, b, s, t, Branch (R, c, y, z, d)) ) ->
        Branch
          ( R
          , Branch (B, Branch (B, va, vb, vc, vd), w, x, b)
          , s
          , t
          , Branch (B, c, y, z, d) )
    | Empty, w, x, Branch (R, Branch (R, b, s, t, c), y, z, Empty) ->
        Branch
          (R, Branch (B, Empty, w, x, b), s, t, Branch (B, c, y, z, Empty))
    | ( Empty
      , w
      , x
      , Branch (R, Branch (R, b, s, t, c), y, z, Branch (B, va, vb, vc, vd))
      ) ->
        Branch
          ( R
          , Branch (B, Empty, w, x, b)
          , s
          , t
          , Branch (B, c, y, z, Branch (B, va, vb, vc, vd)) )
    | ( Branch (B, va, vb, vc, vd)
      , w
      , x
      , Branch (R, Branch (R, b, s, t, c), y, z, Empty) ) ->
        Branch
          ( R
          , Branch (B, Branch (B, va, vb, vc, vd), w, x, b)
          , s
          , t
          , Branch (B, c, y, z, Empty) )
    | ( Branch (B, va, vb, vc, vd)
      , w
      , x
      , Branch (R, Branch (R, b, s, t, c), y, z, Branch (B, ve, vf, vg, vh))
      ) ->
        Branch
          ( R
          , Branch (B, Branch (B, va, vb, vc, vd), w, x, b)
          , s
          , t
          , Branch (B, c, y, z, Branch (B, ve, vf, vg, vh)) )
    | Empty, s, t, Empty -> Branch (B, Empty, s, t, Empty)
    | Empty, s, t, Branch (B, va, vb, vc, vd) ->
        Branch (B, Empty, s, t, Branch (B, va, vb, vc, vd))
    | Empty, s, t, Branch (v, Empty, vb, vc, Empty) ->
        Branch (B, Empty, s, t, Branch (v, Empty, vb, vc, Empty))
    | Empty, s, t, Branch (v, Branch (B, ve, vf, vg, vh), vb, vc, Empty) ->
        Branch
          ( B
          , Empty
          , s
          , t
          , Branch (v, Branch (B, ve, vf, vg, vh), vb, vc, Empty) )
    | Empty, s, t, Branch (v, Empty, vb, vc, Branch (B, vf, vg, vh, vi)) ->
        Branch
          ( B
          , Empty
          , s
          , t
          , Branch (v, Empty, vb, vc, Branch (B, vf, vg, vh, vi)) )
    | ( Empty
      , s
      , t
      , Branch
          (v, Branch (B, ve, vj, vk, vl), vb, vc, Branch (B, vf, vg, vh, vi))
      ) ->
        Branch
          ( B
          , Empty
          , s
          , t
          , Branch
              ( v
              , Branch (B, ve, vj, vk, vl)
              , vb
              , vc
              , Branch (B, vf, vg, vh, vi) ) )
    | Branch (B, va, vb, vc, vd), s, t, Empty ->
        Branch (B, Branch (B, va, vb, vc, vd), s, t, Empty)
    | Branch (B, va, vb, vc, vd), s, t, Branch (B, ve, vf, vg, vh) ->
        Branch
          (B, Branch (B, va, vb, vc, vd), s, t, Branch (B, ve, vf, vg, vh))
    | Branch (B, va, vb, vc, vd), s, t, Branch (v, Empty, vf, vg, Empty) ->
        Branch
          ( B
          , Branch (B, va, vb, vc, vd)
          , s
          , t
          , Branch (v, Empty, vf, vg, Empty) )
    | ( Branch (B, va, vb, vc, vd)
      , s
      , t
      , Branch (v, Branch (B, vi, vj, vk, vl), vf, vg, Empty) ) ->
        Branch
          ( B
          , Branch (B, va, vb, vc, vd)
          , s
          , t
          , Branch (v, Branch (B, vi, vj, vk, vl), vf, vg, Empty) )
    | ( Branch (B, va, vb, vc, vd)
      , s
      , t
      , Branch (v, Empty, vf, vg, Branch (B, vj, vk, vl, vm)) ) ->
        Branch
          ( B
          , Branch (B, va, vb, vc, vd)
          , s
          , t
          , Branch (v, Empty, vf, vg, Branch (B, vj, vk, vl, vm)) )
    | ( Branch (B, va, vb, vc, vd)
      , s
      , t
      , Branch
          (v, Branch (B, vi, vn, vo, vp), vf, vg, Branch (B, vj, vk, vl, vm))
      ) ->
        Branch
          ( B
          , Branch (B, va, vb, vc, vd)
          , s
          , t
          , Branch
              ( v
              , Branch (B, vi, vn, vo, vp)
              , vf
              , vg
              , Branch (B, vj, vk, vl, vm) ) )
    | Branch (v, Empty, vb, vc, Empty), s, t, Empty ->
        Branch (B, Branch (v, Empty, vb, vc, Empty), s, t, Empty)
    | Branch (v, Empty, vb, vc, Branch (B, ve, vf, vg, vh)), s, t, Empty ->
        Branch
          ( B
          , Branch (v, Empty, vb, vc, Branch (B, ve, vf, vg, vh))
          , s
          , t
          , Empty )
    | Branch (v, Branch (B, vf, vg, vh, vi), vb, vc, Empty), s, t, Empty ->
        Branch
          ( B
          , Branch (v, Branch (B, vf, vg, vh, vi), vb, vc, Empty)
          , s
          , t
          , Empty )
    | ( Branch
          (v, Branch (B, vf, vg, vh, vi), vb, vc, Branch (B, ve, vj, vk, vl))
      , s
      , t
      , Empty ) ->
        Branch
          ( B
          , Branch
              ( v
              , Branch (B, vf, vg, vh, vi)
              , vb
              , vc
              , Branch (B, ve, vj, vk, vl) )
          , s
          , t
          , Empty )
    | Branch (v, Empty, vf, vg, Empty), s, t, Branch (B, va, vb, vc, vd) ->
        Branch
          ( B
          , Branch (v, Empty, vf, vg, Empty)
          , s
          , t
          , Branch (B, va, vb, vc, vd) )
    | ( Branch (v, Empty, vf, vg, Branch (B, vi, vj, vk, vl))
      , s
      , t
      , Branch (B, va, vb, vc, vd) ) ->
        Branch
          ( B
          , Branch (v, Empty, vf, vg, Branch (B, vi, vj, vk, vl))
          , s
          , t
          , Branch (B, va, vb, vc, vd) )
    | ( Branch (v, Branch (B, vj, vk, vl, vm), vf, vg, Empty)
      , s
      , t
      , Branch (B, va, vb, vc, vd) ) ->
        Branch
          ( B
          , Branch (v, Branch (B, vj, vk, vl, vm), vf, vg, Empty)
          , s
          , t
          , Branch (B, va, vb, vc, vd) )
    | ( Branch
          (v, Branch (B, vj, vk, vl, vm), vf, vg, Branch (B, vi, vn, vo, vp))
      , s
      , t
      , Branch (B, va, vb, vc, vd) ) ->
        Branch
          ( B
          , Branch
              ( v
              , Branch (B, vj, vk, vl, vm)
              , vf
              , vg
              , Branch (B, vi, vn, vo, vp) )
          , s
          , t
          , Branch (B, va, vb, vc, vd) )

  let rec paint c x1 =
    match (c, x1) with
    | c, Empty -> Empty
    | c, Branch (uu, l, k, v, r) -> Branch (c, l, k, v, r)

  let rec balance_right a k x xa3 =
    match (a, k, x, xa3) with
    | a, k, x, Branch (R, b, s, y, c) ->
        Branch (R, a, k, x, Branch (B, b, s, y, c))
    | Branch (B, a, k, x, b), s, y, Empty ->
        balance (Branch (R, a, k, x, b)) s y Empty
    | Branch (B, a, k, x, b), s, y, Branch (B, va, vb, vc, vd) ->
        balance (Branch (R, a, k, x, b)) s y (Branch (B, va, vb, vc, vd))
    | Branch (R, a, k, x, Branch (B, b, s, y, c)), t, z, Empty ->
        Branch
          (R, balance (paint R a) k x b, s, y, Branch (B, c, t, z, Empty))
    | ( Branch (R, a, k, x, Branch (B, b, s, y, c))
      , t
      , z
      , Branch (B, va, vb, vc, vd) ) ->
        Branch
          ( R
          , balance (paint R a) k x b
          , s
          , y
          , Branch (B, c, t, z, Branch (B, va, vb, vc, vd)) )
    | Empty, k, x, Empty -> Empty
    | Branch (R, va, vb, vc, Empty), k, x, Empty -> Empty
    | Branch (R, va, vb, vc, Branch (R, ve, vf, vg, vh)), k, x, Empty ->
        Empty
    | Empty, k, x, Branch (B, va, vb, vc, vd) -> Empty
    | Branch (R, ve, vf, vg, Empty), k, x, Branch (B, va, vb, vc, vd) ->
        Empty
    | ( Branch (R, ve, vf, vg, Branch (R, vi, vj, vk, vl))
      , k
      , x
      , Branch (B, va, vb, vc, vd) ) ->
        Empty

  let rec balance_left x0 s y c =
    match (x0, s, y, c) with
    | Branch (R, a, k, x, b), s, y, c ->
        Branch (R, Branch (B, a, k, x, b), s, y, c)
    | Empty, k, x, Branch (B, a, s, y, b) ->
        balance Empty k x (Branch (R, a, s, y, b))
    | Branch (B, va, vb, vc, vd), k, x, Branch (B, a, s, y, b) ->
        balance (Branch (B, va, vb, vc, vd)) k x (Branch (R, a, s, y, b))
    | Empty, k, x, Branch (R, Branch (B, a, s, y, b), t, z, c) ->
        Branch
          (R, Branch (B, Empty, k, x, a), s, y, balance b t z (paint R c))
    | ( Branch (B, va, vb, vc, vd)
      , k
      , x
      , Branch (R, Branch (B, a, s, y, b), t, z, c) ) ->
        Branch
          ( R
          , Branch (B, Branch (B, va, vb, vc, vd), k, x, a)
          , s
          , y
          , balance b t z (paint R c) )
    | Empty, k, x, Empty -> Empty
    | Empty, k, x, Branch (R, Empty, vb, vc, vd) -> Empty
    | Empty, k, x, Branch (R, Branch (R, ve, vf, vg, vh), vb, vc, vd) ->
        Empty
    | Branch (B, va, vb, vc, vd), k, x, Empty -> Empty
    | Branch (B, va, vb, vc, vd), k, x, Branch (R, Empty, vf, vg, vh) ->
        Empty
    | ( Branch (B, va, vb, vc, vd)
      , k
      , x
      , Branch (R, Branch (R, vi, vj, vk, vl), vf, vg, vh) ) ->
        Empty

  let rec combine xa0 x =
    match (xa0, x) with
    | Empty, x -> x
    | Branch (v, va, vb, vc, vd), Empty -> Branch (v, va, vb, vc, vd)
    | Branch (R, a, k, x, b), Branch (R, c, s, y, d) -> (
      match combine b c with
      | Empty -> Branch (R, a, k, x, Branch (R, Empty, s, y, d))
      | Branch (R, b2, t, z, c2) ->
          Branch (R, Branch (R, a, k, x, b2), t, z, Branch (R, c2, s, y, d))
      | Branch (B, b2, t, z, c2) ->
          Branch (R, a, k, x, Branch (R, Branch (B, b2, t, z, c2), s, y, d))
      )
    | Branch (B, a, k, x, b), Branch (B, c, s, y, d) -> (
      match combine b c with
      | Empty -> balance_left a k x (Branch (B, Empty, s, y, d))
      | Branch (R, b2, t, z, c2) ->
          Branch (R, Branch (B, a, k, x, b2), t, z, Branch (B, c2, s, y, d))
      | Branch (B, b2, t, z, c2) ->
          balance_left a k x (Branch (B, Branch (B, b2, t, z, c2), s, y, d))
      )
    | Branch (B, va, vb, vc, vd), Branch (R, b, k, x, c) ->
        Branch (R, combine (Branch (B, va, vb, vc, vd)) b, k, x, c)
    | Branch (R, a, k, x, b), Branch (B, va, vb, vc, vd) ->
        Branch (R, a, k, x, combine b (Branch (B, va, vb, vc, vd)))

  let rec rbt_del _A x xa1 =
    match (x, xa1) with
    | x, Empty -> Empty
    | x, Branch (c, a, y, s, b) ->
        if less _A x y then rbt_del_from_left _A x a y s b
        else if less _A y x then rbt_del_from_right _A x a y s b
        else combine a b

  and rbt_del_from_left _A x xa1 y s b =
    match (x, xa1, y, s, b) with
    | x, Branch (B, lt, z, v, rt), y, s, b ->
        balance_left (rbt_del _A x (Branch (B, lt, z, v, rt))) y s b
    | x, Empty, y, s, b -> Branch (R, rbt_del _A x Empty, y, s, b)
    | x, Branch (R, va, vb, vc, vd), y, s, b ->
        Branch (R, rbt_del _A x (Branch (R, va, vb, vc, vd)), y, s, b)

  and rbt_del_from_right _A x a y s xa4 =
    match (x, a, y, s, xa4) with
    | x, a, y, s, Branch (B, lt, z, v, rt) ->
        balance_right a y s (rbt_del _A x (Branch (B, lt, z, v, rt)))
    | x, a, y, s, Empty -> Branch (R, a, y, s, rbt_del _A x Empty)
    | x, a, y, s, Branch (R, va, vb, vc, vd) ->
        Branch (R, a, y, s, rbt_del _A x (Branch (R, va, vb, vc, vd)))

  let rec rbt_delete _A k t = paint B (rbt_del _A k t)

  let rec impl_of _B (RBT x) = x

  let rec delete _A xb xc =
    RBT
      (rbt_delete _A.order_linorder.preorder_order.ord_preorder xb
         (impl_of _A xc) )

  let rec rbt_ins _A f k v x3 =
    match (f, k, v, x3) with
    | f, k, v, Empty -> Branch (R, Empty, k, v, Empty)
    | f, k, v, Branch (B, l, x, y, r) ->
        if less _A k x then balance (rbt_ins _A f k v l) x y r
        else if less _A x k then balance l x y (rbt_ins _A f k v r)
        else Branch (B, l, x, f k y v, r)
    | f, k, v, Branch (R, l, x, y, r) ->
        if less _A k x then Branch (R, rbt_ins _A f k v l, x, y, r)
        else if less _A x k then Branch (R, l, x, y, rbt_ins _A f k v r)
        else Branch (R, l, x, f k y v, r)

  let rec rbt_insert_with_key _A f k v t = paint B (rbt_ins _A f k v t)

  let rec rbt_insert _A = rbt_insert_with_key _A (fun _ _ nv -> nv)

  let rec insert _A xc xd xe =
    RBT
      (rbt_insert _A.order_linorder.preorder_order.ord_preorder xc xd
         (impl_of _A xe) )

  let rec rbt_lookup _A x0 k =
    match (x0, k) with
    | Empty, k -> None
    | Branch (uu, l, x, y, r), k ->
        if less _A k x then rbt_lookup _A l k
        else if less _A x k then rbt_lookup _A r k
        else Some y

  let rec lookup _A x =
    rbt_lookup _A.order_linorder.preorder_order.ord_preorder (impl_of _A x)

  let rec map f x1 =
    match (f, x1) with f, [] -> [] | f, x21 :: x22 -> f x21 :: map f x22

  let rec product x0 uu =
    match (x0, uu) with
    | [], uu -> []
    | x :: xs, ys -> map (fun a -> (x, a)) ys @ product xs ys

  let rec is_none = function Some x -> false | None -> true

  let rec worklist b f x2 =
    match (b, f, x2) with
    | b, f, (s, e :: wl) ->
        if b s then
          let sa, n = f s e in
          worklist b f (sa, n @ wl)
        else (s, e :: wl)
    | b, f, (s, []) -> (s, [])

  let rec ltsga_image imf f =
    imf f (fun _ -> true) (fun _ -> true) (fun _ -> true) (fun _ -> true)

  let rec the (Some x2) = x2

  let rec snd (x1, x2) = x2

  let rec fst (x1, x2) = x1

  let rec diffI _A suc pre ib is =
    if less _A.order_linorder.preorder_order.ord_preorder (snd ib) (fst ib)
    then []
    else if
      less _A.order_linorder.preorder_order.ord_preorder (snd is) (fst is)
    then [ib]
    else if
      less _A.order_linorder.preorder_order.ord_preorder (snd is) (fst ib)
    then [ib]
    else if
      less _A.order_linorder.preorder_order.ord_preorder (snd is) (snd ib)
    then
      if
        less_eq _A.order_linorder.preorder_order.ord_preorder (fst is)
          (fst ib)
      then [(suc (snd is), snd ib)]
      else [(fst ib, pre (fst is)); (suc (snd is), snd ib)]
    else if
      less _A.order_linorder.preorder_order.ord_preorder (snd ib) (fst is)
    then [ib]
    else if
      less_eq _A.order_linorder.preorder_order.ord_preorder (fst is) (fst ib)
    then []
    else [(fst ib, pre (fst is))]

  let rec nempI _A s = less_eq _A (fst s) (snd s)

  let rec apsnd f (x, y) = (x, f y)

  let rec diffIs_aux _A suc pre x2 i =
    match (suc, pre, x2, i) with
    | suc, pre, [], i -> []
    | suc, pre, a :: l, i -> diffI _A suc pre a i @ diffIs_aux _A suc pre l i

  let rec diffIs _A suc pre l1 x3 =
    match (suc, pre, l1, x3) with
    | suc, pre, l1, [] -> l1
    | suc, pre, l1, i :: l2 ->
        diffIs _A suc pre (diffIs_aux _A suc pre l1 i) l2

  let rec iterate_to_list it = it (fun _ -> true) (fun a b -> a :: b) []

  let rec ltsga_to_list it = comp iterate_to_list it

  let rec emptyIs _A l = null l

  let rec ltsga_iterator it =
    it (fun _ -> true) (fun _ -> true) (fun _ -> true) (fun _ -> true)

  let rec rm_iterateoi x0 c f sigma =
    match (x0, c, f, sigma) with
    | Empty, c, f, sigma -> sigma
    | Branch (col, l, k, v, r), c, f, sigma ->
        if c sigma then
          let sigmaa = rm_iterateoi l c f sigma in
          if c sigmaa then rm_iterateoi r c f (f (k, v) sigmaa) else sigmaa
        else sigma

  let rec nemptyIs _A l = not (null l)

  let rec iteratei_set_op_list_it_rs_ops _A s =
   fun c f -> rm_iterateoi (impl_of _A s) c (comp f fst)

  let rec iteratei_map_op_list_it_rm_ops _A s = rm_iterateoi (impl_of _A s)

  let rec map_iterator_key_filter p it =
   fun c f -> it c (fun x sigma -> if p (fst x) then f x sigma else sigma)

  let rec map_iterator_product it_a it_b =
   fun c f -> it_a c (fun a -> it_b (snd a) c (fun b -> f (fst a, b)))

  let rec set_iterator_filter p it =
   fun c f -> it c (fun x sigma -> if p x then f x sigma else sigma)

  let rec map_to_set_iterator m it = it m

  let rec ltsbm_filter_it it1 it2 it3 p_v1 p_e p_v2 p m1 =
    set_iterator_filter
      (fun (v, (e, va)) -> p_v2 va && p (v, (e, va)))
      (map_iterator_product
         (map_iterator_key_filter p_v1 (map_to_set_iterator m1 it1))
         (fun m2 ->
           map_iterator_product
             (map_iterator_key_filter p_e (map_to_set_iterator m2 it2))
             it3 ) )

  let rec ltsbm_it it1 it2 it3 = ltsga_iterator (ltsbm_filter_it it1 it2 it3)

  let rec rs_lts_it _A _B _C =
    ltsbm_it
      (iteratei_map_op_list_it_rm_ops _A)
      (iteratei_map_op_list_it_rm_ops _B)
      (iteratei_set_op_list_it_rs_ops _C)

  let rec divmod_integer k l =
    if Z.equal k Z.zero then (Z.zero, Z.zero)
    else if Z.lt Z.zero l then
      if Z.lt Z.zero k then
        (fun k l ->
          if Z.equal Z.zero l then (Z.zero, l)
          else Z.div_rem (Z.abs k) (Z.abs l) )
          k l
      else
        let r, s =
          (fun k l ->
            if Z.equal Z.zero l then (Z.zero, l)
            else Z.div_rem (Z.abs k) (Z.abs l) )
            k l
        in
        if Z.equal s Z.zero then (Z.neg r, Z.zero)
        else (Z.sub (Z.neg r) (Z.of_int 1), Z.sub l s)
    else if Z.equal l Z.zero then (Z.zero, k)
    else
      apsnd Z.neg
        ( if Z.lt k Z.zero then
            (fun k l ->
              if Z.equal Z.zero l then (Z.zero, l)
              else Z.div_rem (Z.abs k) (Z.abs l) )
              k l
          else
            let r, s =
              (fun k l ->
                if Z.equal Z.zero l then (Z.zero, l)
                else Z.div_rem (Z.abs k) (Z.abs l) )
                k l
            in
            if Z.equal s Z.zero then (Z.neg r, Z.zero)
            else (Z.sub (Z.neg r) (Z.of_int 1), Z.sub (Z.neg l) s) )

  let rec divide_integer k l = fst (divmod_integer k l)

  let rec divide_nat m n =
    Nat (divide_integer (integer_of_nat m) (integer_of_nat n))

  let rec times_nat m n = Nat (Z.mul (integer_of_nat m) (integer_of_nat n))

  let rec max _A a b = if less_eq _A a b then b else a

  let rec nat_of_integer k = Nat (max ord_integer Z.zero k)

  let rec triangle n =
    divide_nat (times_nat n (suc n)) (nat_of_integer (Z.of_int 2))

  let rec empty_rm_basic_ops _A = fun _ -> empty _A

  let rec ins_rm_basic_ops _A x s = insert _A x () s

  let rec g_sng_dflt_basic_oops_rm_basic_ops _A x =
    ins_rm_basic_ops _A x (empty_rm_basic_ops _A ())

  let rec g_sng_rm_basic_ops _A k v = insert _A k v (empty _A)

  let rec rs_lts_add _A _B =
   fun v w va l ->
    match lookup _A l v with
    | None ->
        insert _A v
          (g_sng_rm_basic_ops _B w
             (g_sng_dflt_basic_oops_rm_basic_ops _A va) )
          l
    | Some m2 -> (
      match lookup _B m2 w with
      | None ->
          insert _A v
            (insert _B w (g_sng_dflt_basic_oops_rm_basic_ops _A va) m2)
            l
      | Some s3 -> insert _A v (insert _B w (ins_rm_basic_ops _A va s3) m2) l
      )

  let rec whilea b c s = if b s then whilea b c (c s) else s

  let rec intersectI _A _B s1 s2 =
    ( (if less _A (fst s1) (fst s2) then fst s2 else fst s1)
    , if less _B (snd s1) (snd s2) then snd s1 else snd s2 )

  let rec intersectIs_aux _A a x1 =
    match (a, x1) with
    | a, [] -> []
    | a, b :: l ->
        if
          nempI _A.order_linorder.preorder_order.ord_preorder
            (intersectI _A.order_linorder.preorder_order.ord_preorder
               _A.order_linorder.preorder_order.ord_preorder a b )
        then
          intersectI _A.order_linorder.preorder_order.ord_preorder
            _A.order_linorder.preorder_order.ord_preorder a b
          :: intersectIs_aux _A a l
        else intersectIs_aux _A a l

  let rec intersectIs _A x0 l2 =
    match (x0, l2) with
    | [], l2 -> []
    | a :: l1, l2 -> intersectIs_aux _A a l2 @ intersectIs _A l1 l2

  let rec ltsga_image_filter e a it f p_v1 p_e p_v2 p l =
    it p_v1 p_e p_v2 p l
      (fun _ -> true)
      (fun vev la ->
        let v, (ea, va) = f vev in
        a v ea va la )
      e

  let rec set_iterator_union it_a it_b =
   fun c f sigma_0 -> it_b c f (it_a c f sigma_0)

  let rec tri_union_iterator it_1 it_2 it_3 =
   fun q ->
    set_iterator_union (it_1 q) (set_iterator_union (it_2 q) (it_3 q))

  let rec concat_impl_aux c_inter c_nempty const f it_1 it_2 it_3 i1a i2a i1
      f1 i2 fP1 fP2 =
   fun aA1 aA2 ->
    const f
      ( if c_nempty (c_inter (i1 aA1) (f1 aA1)) then i1a aA1 @ i2a aA2
        else i1a aA1 )
      (fP2 aA2)
      (tri_union_iterator (it_1 aA1) (it_2 aA2)
         (it_3 aA1 (fP1 aA1) (i2 aA2)) )

  let rec dnfa_iter _A _B _C =
   fun x c f ->
    iteratei_map_op_list_it_rm_ops _A x c (fun (q, aq) ->
        iteratei_map_op_list_it_rm_ops _B aq
          (fun _ -> true)
          (fun (a, mq) ->
            iteratei_set_op_list_it_rs_ops _C mq
              (fun _ -> true)
              (fun qa -> f (q, (a, qa))) ) )

  let rec rs_lts_empty _A _B = empty _A

  let rec rs_lts_filter_it _A _B _C =
    ltsbm_filter_it
      (iteratei_map_op_list_it_rm_ops _A)
      (iteratei_map_op_list_it_rm_ops _B)
      (iteratei_set_op_list_it_rs_ops _C)

  let rec rs_lts_image_filter _A _B _C _D =
    ltsga_image_filter (rs_lts_empty _C _D) (rs_lts_add _C _D)
      (rs_lts_filter_it _A _B _A)

  let rec rs_lts_image _A _B _C _D =
    ltsga_image (rs_lts_image_filter _A _B _C _D)

  let rec iteratei_bset_op_list_it_dflt_basic_oops_rm_basic_ops _A s =
   fun c f -> rm_iterateoi (impl_of _A s) c (comp f fst)

  let rec g_to_list_dflt_basic_oops_rm_basic_ops _A s =
    iteratei_bset_op_list_it_dflt_basic_oops_rm_basic_ops _A s
      (fun _ -> true)
      (fun a b -> a :: b)
      []

  let rec iteratei_bmap_op_list_it_rm_basic_ops _A s =
    rm_iterateoi (impl_of _A s)

  let rec g_to_list_rm_basic_ops _A m =
    iteratei_bmap_op_list_it_rm_basic_ops _A m
      (fun _ -> true)
      (fun a b -> a :: b)
      []

  let rec rs_lts_union _A _B =
   fun l1 l2 ->
    foldl
      (fun s t ->
        foldl
          (fun sa ta ->
            foldl
              (fun sb q ->
                match lookup _A sb (fst t) with
                | None ->
                    insert _A (fst t)
                      (g_sng_rm_basic_ops _B (fst ta)
                         (g_sng_dflt_basic_oops_rm_basic_ops _A q) )
                      sb
                | Some m2 -> (
                  match lookup _B m2 (fst ta) with
                  | None ->
                      insert _A (fst t)
                        (insert _B (fst ta)
                           (g_sng_dflt_basic_oops_rm_basic_ops _A q)
                           m2 )
                        sb
                  | Some s3 ->
                      insert _A (fst t)
                        (insert _B (fst ta) (ins_rm_basic_ops _A q s3) m2)
                        sb ) )
              sa
              (g_to_list_dflt_basic_oops_rm_basic_ops _A (snd ta)) )
          s
          (g_to_list_rm_basic_ops _B (snd t)) )
      l2
      (g_to_list_rm_basic_ops _A l1)

  let zero_nat : nat = Nat Z.zero

  let rec g_size_dflt_basic_oops_rm_basic_ops _A s =
    iteratei_bset_op_list_it_dflt_basic_oops_rm_basic_ops _A s
      (fun _ -> true)
      (fun _ -> suc)
      zero_nat

  let rec rs_nfa_qsize (_A1, _A2) _B n =
    g_size_dflt_basic_oops_rm_basic_ops _A2 (fst n)

  let rec g_union_dflt_basic_oops_rm_basic_ops _A s1 s2 =
    iteratei_bset_op_list_it_dflt_basic_oops_rm_basic_ops _A s1
      (fun _ -> true)
      (ins_rm_basic_ops _A) s2

  let rec rs_nfa_union (_A1, _A2) (_B1, _B2) n1 n2 =
    ( g_union_dflt_basic_oops_rm_basic_ops _A2 (fst n1) (fst n2)
    , ( rs_lts_union _A2
          (linorder_list (equal_prod _B1 _B1, linorder_prod _B2 _B2))
          (fst (snd n1))
          (fst (snd n2))
      , ( g_union_dflt_basic_oops_rm_basic_ops _A2
            (fst (snd (snd n1)))
            (fst (snd (snd n2)))
        , g_union_dflt_basic_oops_rm_basic_ops _A2
            (snd (snd (snd n1)))
            (snd (snd (snd n2))) ) ) )

  let rec prod_encode x =
    (fun (m, n) -> plus_nat (triangle (plus_nat m n)) m) x

  let rec rename_states_gen_impl im im2 (q, (d, (i, f))) =
   fun fa ->
    ( im fa q
    , ( im2 (fun qaq -> (fa (fst qaq), (fst (snd qaq), fa (snd (snd qaq))))) d
      , (im fa i, im fa f) ) )

  let rec rename_states_impl im im2 = rename_states_gen_impl im im2

  let rec rs_nfa_rename _A (_B1, _B2) _C =
    rename_states_impl
      (fun f s ->
        iteratei_set_op_list_it_rs_ops _A s
          (fun _ -> true)
          (fun b -> ins_rm_basic_ops _C (f b))
          (empty_rm_basic_ops _C ()) )
      (rs_lts_image _A
         (linorder_list (equal_prod _B1 _B1, linorder_prod _B2 _B2))
         _C
         (linorder_list (equal_prod _B1 _B1, linorder_prod _B2 _B2)) )

  let rec rs_split_trans_code (_A1, _A2) (_B1, _B2) f1 f2 ts =
    let x =
      dnfa_iter _B2
        (linorder_list (equal_prod _A1 _A1, linorder_prod _A2 _A2))
        _B2 ts
        (fun _ -> true)
        (fun x ->
          ins_rm_basic_ops
            (linorder_list (equal_prod _A1 _A1, linorder_prod _A2 _A2))
            (fst (snd x)) )
        (empty_rm_basic_ops
           (linorder_list (equal_prod _A1 _A1, linorder_prod _A2 _A2))
           () )
    in
    dnfa_iter _B2
      (linorder_list (equal_prod _A1 _A1, linorder_prod _A2 _A2))
      _B2 ts
      (fun _ -> true)
      (fun xa sigma ->
        let a =
          let xb =
            iteratei_set_op_list_it_rs_ops
              (linorder_list (equal_prod _A1 _A1, linorder_prod _A2 _A2))
              x
              (fun _ -> true)
              (fun xb sigmaa ->
                iteratei_set_op_list_it_rs_ops
                  (linorder_list
                     (equal_prod _A1 _A1, linorder_prod _A2 _A2) )
                  sigmaa
                  (fun _ -> true)
                  (fun xc sigmab ->
                    if nemptyIs _A2 (intersectIs _A2 xc xb) then
                      if
                        equal_list (equal_prod _A1 _A1) xc
                          (intersectIs _A2 xc xb)
                      then
                        ins_rm_basic_ops
                          (linorder_list
                             (equal_prod _A1 _A1, linorder_prod _A2 _A2) )
                          xc sigmab
                      else
                        ins_rm_basic_ops
                          (linorder_list
                             (equal_prod _A1 _A1, linorder_prod _A2 _A2) )
                          (intersectIs _A2 xc xb)
                          (ins_rm_basic_ops
                             (linorder_list
                                (equal_prod _A1 _A1, linorder_prod _A2 _A2) )
                             (diffIs _A2 f1 f2 xc (intersectIs _A2 xc xb))
                             sigmab )
                    else
                      ins_rm_basic_ops
                        (linorder_list
                           (equal_prod _A1 _A1, linorder_prod _A2 _A2) )
                        xc sigmab )
                  (empty_rm_basic_ops
                     (linorder_list
                        (equal_prod _A1 _A1, linorder_prod _A2 _A2) )
                     () ) )
              (ins_rm_basic_ops
                 (linorder_list (equal_prod _A1 _A1, linorder_prod _A2 _A2))
                 (fst (snd xa))
                 (empty_rm_basic_ops
                    (linorder_list
                       (equal_prod _A1 _A1, linorder_prod _A2 _A2) )
                    () ) )
          in
          iteratei_set_op_list_it_rs_ops
            (linorder_list (equal_prod _A1 _A1, linorder_prod _A2 _A2))
            xb
            (fun _ -> true)
            (fun xc ->
              rs_lts_add _B2
                (linorder_list (equal_prod _A1 _A1, linorder_prod _A2 _A2))
                (fst xa) xc
                (snd (snd xa)) )
            (rs_lts_empty _B2
               (linorder_list (equal_prod _A1 _A1, linorder_prod _A2 _A2)) )
        in
        rs_lts_union _B2
          (linorder_list (equal_prod _A1 _A1, linorder_prod _A2 _A2))
          sigma a )
      (rs_lts_empty _B2
         (linorder_list (equal_prod _A1 _A1, linorder_prod _A2 _A2)) )

  let rec rs_nfa_uniq (_A1, _A2) (_D1, _D2) f1 f2 (q, (a, (d, (i, f)))) =
    let qa, (aa, (da, (ia, fa))) = (q, (a, (d, (i, f)))) in
    (qa, (aa, (rs_split_trans_code (_A1, _A2) (_D1, _D2) f1 f2 da, (ia, fa))))

  let rec set_iterator_emp c f sigma_0 = sigma_0

  let rec rs_lts_succ_it _A _B =
   fun m1 v _ ->
    match lookup _A m1 v with
    | None -> set_iterator_emp
    | Some m2 ->
        map_iterator_product
          (iteratei_map_op_list_it_rm_ops _B m2)
          (iteratei_set_op_list_it_rs_ops _A)

  let rec rs_lts_to_list _A _B = ltsga_to_list (rs_lts_it _A _B _A)

  let rec g_isEmpty_dflt_basic_oops_rm_basic_ops _A s =
    iteratei_bset_op_list_it_dflt_basic_oops_rm_basic_ops _A s
      (fun c -> c)
      (fun _ _ -> false)
      true

  let rec ins_dj_rm_basic_ops _A x s = insert _A x () s

  let rec memb_rm_basic_ops _A x s = not (is_none (lookup _A s x))

  let rec g_inter_dflt_basic_oops_rm_basic_ops _A s1 s2 =
    iteratei_bset_op_list_it_dflt_basic_oops_rm_basic_ops _A s1
      (fun _ -> true)
      (fun x s ->
        if memb_rm_basic_ops _A x s2 then ins_dj_rm_basic_ops _A x s else s )
      (empty_rm_basic_ops _A ())

  let rec rs_lts_succ_label_it _A _B =
   fun m1 v ->
    match lookup _A m1 v with
    | None -> set_iterator_emp
    | Some m2 ->
        map_iterator_product
          (iteratei_map_op_list_it_rm_ops _B m2)
          (iteratei_set_op_list_it_rs_ops _A)

  let rec set_iterator_image_filter g it =
   fun c f ->
    it c (fun x sigma ->
        match g x with None -> sigma | Some xa -> f xa sigma )

  let rec rs_lts_connect_it _A _B _C =
   fun m1 sa si v ->
    match lookup _A m1 v with
    | None -> fun _ _ sigma_0 -> sigma_0
    | Some m2 ->
        map_iterator_product
          (set_iterator_image_filter
             (fun a ->
               if
                 not
                   (g_isEmpty_dflt_basic_oops_rm_basic_ops _A
                      (g_inter_dflt_basic_oops_rm_basic_ops _A (snd a) sa) )
               then Some a
               else None )
             (iteratei_map_op_list_it_rm_ops _B m2) )
          (fun _ -> iteratei_set_op_list_it_rs_ops _C si)

  let rec rs_nfa_concate (_A1, _A2) (_B1, _B2) (q1, (d1, (i1, f1)))
      (q2, (d2, (i2, f2))) =
    let a, b =
      foldl
        (fun (a, b) ->
          (let qm, n = a in
           fun is q ->
             ( (insert _A2 (id q) (states_enumerate _A1 n) qm, suc n)
             , ins_dj_rm_basic_ops _A2 (states_enumerate _A1 n) is ) )
            b )
        ((empty _A2, zero_nat), empty_rm_basic_ops _A2 ())
        ( if
            not
              (g_isEmpty_dflt_basic_oops_rm_basic_ops _A2
                 (g_inter_dflt_basic_oops_rm_basic_ops _A2 i1 f1) )
          then
            g_to_list_dflt_basic_oops_rm_basic_ops _A2 i1
            @ g_to_list_dflt_basic_oops_rm_basic_ops _A2 i2
          else g_to_list_dflt_basic_oops_rm_basic_ops _A2 i1 )
    in
    (let qm, n = a in
     fun is ->
       let aa, ba =
         worklist
           (fun _ -> true)
           (fun (aa, ba) ->
             (let qma, na = aa in
              fun (qs, (dd, (isa, fs))) q ->
                let r = the (lookup _A2 qma (id q)) in
                if memb_rm_basic_ops _A2 r qs then
                  (((qma, na), (qs, (dd, (isa, fs)))), [])
                else
                  let ab, bb =
                    set_iterator_union
                      (rs_lts_succ_label_it _A2
                         (linorder_list
                            (equal_prod _B1 _B1, linorder_prod _B2 _B2) )
                         d1 q )
                      (set_iterator_union
                         (rs_lts_succ_label_it _A2
                            (linorder_list
                               (equal_prod _B1 _B1, linorder_prod _B2 _B2) )
                            d2 q )
                         (rs_lts_connect_it _A2
                            (linorder_list
                               (equal_prod _B1 _B1, linorder_prod _B2 _B2) )
                            _A2 d1 f1 i2 q ) )
                      (fun _ -> true)
                      (fun (ab, qa) (bb, c) ->
                        (let qmb, nb = bb in
                         fun (dda, naa) ->
                           if nemptyIs _B2 ab then
                             let r_opt = lookup _A2 qmb (id qa) in
                             let bc, ca =
                               if is_none r_opt then
                                 let ra = states_enumerate _A1 nb in
                                 ((insert _A2 (id qa) ra qmb, suc nb), ra)
                               else ((qmb, nb), the r_opt)
                             in
                             (let qmc, nc = bc in
                              fun ra ->
                                ( (qmc, nc)
                                , ( rs_lts_add _A2
                                      (linorder_list
                                         ( equal_prod _B1 _B1
                                         , linorder_prod _B2 _B2 ) )
                                      r ab ra dda
                                  , qa :: naa ) ) )
                               ca
                           else ((qmb, nb), (dda, naa)) )
                          c )
                      ((qma, na), (dd, []))
                  in
                  (let qmb, nb = ab in
                   fun (dda, ac) ->
                     ( ( (qmb, nb)
                       , ( ins_dj_rm_basic_ops _A2 r qs
                         , ( dda
                           , ( isa
                             , if memb_rm_basic_ops _A2 q f2 then
                                 ins_dj_rm_basic_ops _A2 r fs
                               else fs ) ) ) )
                     , ac ) )
                    bb )
               ba )
           ( ( (qm, n)
             , ( empty_rm_basic_ops _A2 ()
               , ( rs_lts_empty _A2
                     (linorder_list
                        (equal_prod _B1 _B1, linorder_prod _B2 _B2) )
                 , (is, empty_rm_basic_ops _A2 ()) ) ) )
           , if
               not
                 (g_isEmpty_dflt_basic_oops_rm_basic_ops _A2
                    (g_inter_dflt_basic_oops_rm_basic_ops _A2 i1 f1) )
             then
               g_to_list_dflt_basic_oops_rm_basic_ops _A2 i1
               @ g_to_list_dflt_basic_oops_rm_basic_ops _A2 i2
             else g_to_list_dflt_basic_oops_rm_basic_ops _A2 i1 )
       in
       (let _, aaa = aa in
        fun _ -> aaa )
         ba )
      b

  let rec ltsga_to_collect_list to_list l = to_list l

  let rec rs_lts_succq_it _A _B =
   fun m1 v e ->
    match lookup _A m1 v with
    | None -> set_iterator_emp
    | Some m2 -> (
      match lookup _B m2 e with
      | None -> set_iterator_emp
      | Some a -> iteratei_set_op_list_it_rs_ops _A a )

  let rec rs_lts_to_collect_list _A _B =
    ltsga_to_collect_list (rs_lts_to_list _A _B)

  let rec rs_nfa_destruct (_A1, _A2) (_B1, _B2) (q, (d, (i, f))) =
    ( g_to_list_dflt_basic_oops_rm_basic_ops _A2 q
    , ( rs_lts_to_collect_list _A2
          (linorder_list (equal_prod _B1 _B1, linorder_prod _B2 _B2))
          d
      , ( g_to_list_dflt_basic_oops_rm_basic_ops _A2 i
        , g_to_list_dflt_basic_oops_rm_basic_ops _A2 f ) ) )

  let rec lookup_aux (_A1, _A2) v rc = lookup _A2 rc v

  let rec rs_nfa_bool_comb (_A1, _A2) (_B1, _B2) bc (q1, (d1, (i1, f1)))
      (q2, (d2, (i2, f2))) =
    let a, b =
      foldl
        (fun (a, b) ->
          (let qm, n = a in
           fun is q ->
             ( ( insert (linorder_prod _A2 _A2) (id q)
                   (states_enumerate _A1 n) qm
               , suc n )
             , ins_dj_rm_basic_ops _A2 (states_enumerate _A1 n) is ) )
            b )
        ((empty (linorder_prod _A2 _A2), zero_nat), empty_rm_basic_ops _A2 ())
        (product
           (g_to_list_dflt_basic_oops_rm_basic_ops _A2 i1)
           (g_to_list_dflt_basic_oops_rm_basic_ops _A2 i2) )
    in
    (let qm, n = a in
     fun is ->
       let aa, ba =
         worklist
           (fun _ -> true)
           (fun (aa, ba) ->
             (let qma, na = aa in
              fun (qs, (dd, (isa, fs))) q ->
                let r = the (lookup (linorder_prod _A2 _A2) qma (id q)) in
                if memb_rm_basic_ops _A2 r qs then
                  (((qma, na), (qs, (dd, (isa, fs)))), [])
                else
                  let ab, bb =
                    (let q1a, q2a = q in
                     fun c f ->
                       rs_lts_succ_label_it _A2
                         (linorder_list
                            (equal_prod _B1 _B1, linorder_prod _B2 _B2) )
                         d1 q1a c
                         (fun (a1, q1b) ->
                           rs_lts_succ_it _A2
                             (linorder_list
                                (equal_prod _B1 _B1, linorder_prod _B2 _B2) )
                             d2 q2a a1 c
                             (fun (a2, q2b) -> f ((a1, a2), (q1b, q2b))) ) )
                      (fun _ -> true)
                      (fun (ab, qa) (bb, c) ->
                        (let qmb, nb = bb in
                         fun (dda, naa) ->
                           if
                             nemptyIs _B2 (intersectIs _B2 (fst ab) (snd ab))
                           then
                             let r_opt =
                               lookup (linorder_prod _A2 _A2) qmb (id qa)
                             in
                             let bd, ca =
                               if is_none r_opt then
                                 let ra = states_enumerate _A1 nb in
                                 ( ( insert (linorder_prod _A2 _A2) (id qa)
                                       ra qmb
                                   , suc nb )
                                 , ra )
                               else ((qmb, nb), the r_opt)
                             in
                             (let qmc, nc = bd in
                              fun ra ->
                                ( (qmc, nc)
                                , ( rs_lts_add _A2
                                      (linorder_list
                                         ( equal_prod _B1 _B1
                                         , linorder_prod _B2 _B2 ) )
                                      r
                                      (intersectIs _B2 (fst ab) (snd ab))
                                      ra dda
                                  , qa :: naa ) ) )
                               ca
                           else ((qmb, nb), (dda, naa)) )
                          c )
                      ((qma, na), (dd, []))
                  in
                  (let qmb, nb = ab in
                   fun (dda, ac) ->
                     ( ( (qmb, nb)
                       , ( ins_dj_rm_basic_ops _A2 r qs
                         , ( dda
                           , ( isa
                             , if
                                 let q1a, q2a = q in
                                 bc
                                   (memb_rm_basic_ops _A2 q1a f1)
                                   (memb_rm_basic_ops _A2 q2a f2)
                               then ins_dj_rm_basic_ops _A2 r fs
                               else fs ) ) ) )
                     , ac ) )
                    bb )
               ba )
           ( ( (qm, n)
             , ( empty_rm_basic_ops _A2 ()
               , ( rs_lts_empty _A2
                     (linorder_list
                        (equal_prod _B1 _B1, linorder_prod _B2 _B2) )
                 , (is, empty_rm_basic_ops _A2 ()) ) ) )
           , product
               (g_to_list_dflt_basic_oops_rm_basic_ops _A2 i1)
               (g_to_list_dflt_basic_oops_rm_basic_ops _A2 i2) )
       in
       (let _, aaa = aa in
        fun _ -> aaa )
         ba )
      b

  let rec g_ball_dflt_basic_oops_rm_basic_ops _A s p =
    iteratei_bset_op_list_it_dflt_basic_oops_rm_basic_ops _A s
      (fun c -> c)
      (fun x _ -> p x)
      true

  let rec g_subset_dflt_basic_oops_rm_basic_ops _A s1 s2 =
    g_ball_dflt_basic_oops_rm_basic_ops _A s1 (fun x ->
        memb_rm_basic_ops _A x s2 )

  let rec rs_nfa_elim (_A1, _A2, _A3) (_B1, _B2) a =
    let x =
      let b =
        iteratei_set_op_list_it_rs_ops _A3 (fst a)
          (fun _ -> true)
          (fun x sigma ->
            let xa =
              iteratei_set_op_list_it_rs_ops (linorder_prod _A3 _A3)
                (fst (snd (snd a)))
                (fun _ -> true)
                (fun xa sigmaa ->
                  if eq _A1 (fst xa) x then
                    ins_rm_basic_ops _A3 (snd xa) sigmaa
                  else sigmaa )
                (ins_rm_basic_ops _A3 x (empty_rm_basic_ops _A3 ()))
            in
            insert _A3 x xa sigma )
          (empty _A3)
      in
      whilea
        (fun r ->
          not
            (g_ball_dflt_basic_oops_rm_basic_ops _A3 (fst a) (fun x ->
                 g_ball_dflt_basic_oops_rm_basic_ops _A3
                   (the (lookup _A3 r x))
                   (fun q ->
                     g_subset_dflt_basic_oops_rm_basic_ops _A3
                       (the (lookup _A3 r q))
                       (the (lookup _A3 r x)) ) ) ) )
        (fun xa ->
          iteratei_set_op_list_it_rs_ops _A3 (fst a)
            (fun _ -> true)
            (fun xb sigma ->
              let xc =
                let aa =
                  iteratei_set_op_list_it_rs_ops _A3
                    (the (lookup_aux (_A2, _A3) xb xa))
                    (fun _ -> true)
                    (fun xc sigmaa ->
                      g_union_dflt_basic_oops_rm_basic_ops _A3 sigmaa
                        (the (lookup _A3 xa xc)) )
                    (the (lookup _A3 xa xb))
                in
                g_sng_rm_basic_ops _A3 xb aa
              in
              insert _A3 xb (the (lookup _A3 xc xb)) sigma )
            (empty _A3) )
        b
    in
    let xa =
      rs_lts_it _A3
        (linorder_list (equal_prod _B1 _B1, linorder_prod _B2 _B2))
        _A3
        (fst (snd a))
        (fun _ -> true)
        (fun xa sigma ->
          let xb =
            iteratei_set_op_list_it_rs_ops _A3 (fst a)
              (fun _ -> true)
              (fun xb sigmaa ->
                if memb_rm_basic_ops _A3 (fst xa) (the (lookup _A3 x xb))
                then
                  rs_lts_add _A3
                    (linorder_list
                       (equal_prod _B1 _B1, linorder_prod _B2 _B2) )
                    xb
                    (fst (snd xa))
                    (snd (snd xa))
                    sigmaa
                else sigmaa )
              (rs_lts_empty _A3
                 (linorder_list (equal_prod _B1 _B1, linorder_prod _B2 _B2)) )
          in
          rs_lts_union _A3
            (linorder_list (equal_prod _B1 _B1, linorder_prod _B2 _B2))
            xb sigma )
        (rs_lts_empty _A3
           (linorder_list (equal_prod _B1 _B1, linorder_prod _B2 _B2)) )
    in
    let xb =
      iteratei_set_op_list_it_rs_ops _A3
        (fst (snd (snd (snd a))))
        (fun _ -> true)
        (fun xb sigma ->
          if is_none (lookup _A3 x xb) then sigma
          else
            g_union_dflt_basic_oops_rm_basic_ops _A3 sigma
              (the (lookup _A3 x xb)) )
        (empty_rm_basic_ops _A3 ())
    in
    let xc =
      iteratei_set_op_list_it_rs_ops _A3
        (snd (snd (snd (snd a))))
        (fun _ -> true)
        (fun xc sigma ->
          let xd =
            iteratei_set_op_list_it_rs_ops _A3 (fst a)
              (fun _ -> true)
              (fun xd sigmaa ->
                if memb_rm_basic_ops _A3 xc (the (lookup _A3 x xd)) then
                  ins_rm_basic_ops _A3 xd sigmaa
                else sigmaa )
              (empty_rm_basic_ops _A3 ())
          in
          g_union_dflt_basic_oops_rm_basic_ops _A3 xd sigma )
        (empty_rm_basic_ops _A3 ())
    in
    ( fst a
    , ( rs_lts_union _A3
          (linorder_list (equal_prod _B1 _B1, linorder_prod _B2 _B2))
          (fst (snd a))
          xa
      , ( g_union_dflt_basic_oops_rm_basic_ops _A3
            (fst (snd (snd (snd a))))
            xb
        , g_union_dflt_basic_oops_rm_basic_ops _A3
            (snd (snd (snd (snd a))))
            xc ) ) )

  let rec delete_rm_basic_ops _A x s = delete _A x s

  let rec g_diff_dflt_basic_oops_rm_basic_ops _A s1 s2 =
    iteratei_bset_op_list_it_dflt_basic_oops_rm_basic_ops _A s2
      (fun _ -> true)
      (delete_rm_basic_ops _A) s1

  let rec rs_nfa_complement (_A1, _A2) (q1, (a1, (d1, (i1, f1)))) =
    (q1, (a1, (d1, (i1, g_diff_dflt_basic_oops_rm_basic_ops _A2 q1 f1))))

  let rec g_disjoint_dflt_basic_oops_rm_basic_ops _A s1 s2 =
    g_ball_dflt_basic_oops_rm_basic_ops _A s1 (fun x ->
        not (memb_rm_basic_ops _A x s2) )

  let rec set_iterator_product it_a it_b =
   fun c f -> it_a c (fun a -> it_b a c (fun b -> f (a, b)))

  let rec rs_nfa_determinise (_A1, _A2) (_B1, _B2) (q1, (a1, (d1, (i1, f1))))
      =
    let a, b =
      foldl
        (fun (a, b) ->
          (let qm, n = a in
           fun is q ->
             ( ( insert linorder_nat
                   (iteratei_set_op_list_it_rs_ops _A2 q
                      (fun _ -> true)
                      (fun qa na ->
                        plus_nat na
                          (the
                             (lookup _A2
                                (fst
                                   (iteratei_set_op_list_it_rs_ops _A2 q1
                                      (fun _ -> true)
                                      (fun qb (m, nb) ->
                                        ( insert _A2 qb nb m
                                        , times_nat
                                            (nat_of_integer (Z.of_int 2))
                                            nb ) )
                                      (empty _A2, one_nat) ) )
                                qa ) ) )
                      zero_nat )
                   (states_enumerate _A1 n) qm
               , suc n )
             , ins_dj_rm_basic_ops _A2 (states_enumerate _A1 n) is ) )
            b )
        ((empty linorder_nat, zero_nat), empty_rm_basic_ops _A2 ())
        [i1]
    in
    (let qm, n = a in
     fun is ->
       let aa, ba =
         worklist
           (fun _ -> true)
           (fun (aa, ba) ->
             (let qma, na = aa in
              fun (qs, (siga, (dd, (isa, fs)))) q ->
                let r =
                  the
                    (lookup linorder_nat qma
                       (iteratei_set_op_list_it_rs_ops _A2 q
                          (fun _ -> true)
                          (fun qa nb ->
                            plus_nat nb
                              (the
                                 (lookup _A2
                                    (fst
                                       (iteratei_set_op_list_it_rs_ops _A2 q1
                                          (fun _ -> true)
                                          (fun qb (m, nc) ->
                                            ( insert _A2 qb nc m
                                            , times_nat
                                                (nat_of_integer (Z.of_int 2))
                                                nc ) )
                                          (empty _A2, one_nat) ) )
                                    qa ) ) )
                          zero_nat ) )
                in
                if memb_rm_basic_ops _A2 r qs then
                  (((qma, na), (qs, (siga, (dd, (isa, fs))))), [])
                else
                  let ab, bb =
                    set_iterator_image_filter
                      (fun ab ->
                        let nq =
                          set_iterator_product
                            (iteratei_set_op_list_it_rs_ops _A2 q)
                            (fun qa ->
                              rs_lts_succq_it _A2
                                (linorder_list
                                   (equal_prod _B1 _B1, linorder_prod _B2 _B2) )
                                d1 qa ab )
                            (fun _ -> true)
                            (fun (_, ac) -> ins_rm_basic_ops _A2 ac)
                            (empty_rm_basic_ops _A2 ())
                        in
                        if
                          not (g_isEmpty_dflt_basic_oops_rm_basic_ops _A2 nq)
                        then Some (ab, nq)
                        else None )
                      (iteratei_set_op_list_it_rs_ops
                         (linorder_list
                            (equal_prod _B1 _B1, linorder_prod _B2 _B2) )
                         (foldr
                            (fun e ->
                              ins_rm_basic_ops
                                (linorder_list
                                   (equal_prod _B1 _B1, linorder_prod _B2 _B2) )
                                (fst (snd e)) )
                            (rs_lts_to_list _A2
                               (linorder_list
                                  (equal_prod _B1 _B1, linorder_prod _B2 _B2) )
                               d1 )
                            (empty_rm_basic_ops
                               (linorder_list
                                  (equal_prod _B1 _B1, linorder_prod _B2 _B2) )
                               () ) ) )
                      (fun _ -> true)
                      (fun (ab, qa) (bb, c) ->
                        (let qmb, nb = bb in
                         fun (dda, naa) ->
                           if nemptyIs _B2 ab then
                             let r_opt =
                               lookup linorder_nat qmb
                                 (iteratei_set_op_list_it_rs_ops _A2 qa
                                    (fun _ -> true)
                                    (fun qb nc ->
                                      plus_nat nc
                                        (the
                                           (lookup _A2
                                              (fst
                                                 (iteratei_set_op_list_it_rs_ops
                                                    _A2 q1
                                                    (fun _ -> true)
                                                    (fun qc (m, nd) ->
                                                      ( insert _A2 qc nd m
                                                      , times_nat
                                                          (nat_of_integer
                                                             (Z.of_int 2) )
                                                          nd ) )
                                                    (empty _A2, one_nat) ) )
                                              qb ) ) )
                                    zero_nat )
                             in
                             let bc, ca =
                               if is_none r_opt then
                                 let ra = states_enumerate _A1 nb in
                                 ( ( insert linorder_nat
                                       (iteratei_set_op_list_it_rs_ops _A2 qa
                                          (fun _ -> true)
                                          (fun qb nc ->
                                            plus_nat nc
                                              (the
                                                 (lookup _A2
                                                    (fst
                                                       (iteratei_set_op_list_it_rs_ops
                                                          _A2 q1
                                                          (fun _ -> true)
                                                          (fun qc (m, nd) ->
                                                            ( insert _A2 qc
                                                                nd m
                                                            , times_nat
                                                                (nat_of_integer
                                                                   (Z.of_int
                                                                      2 ) )
                                                                nd ) )
                                                          (empty _A2, one_nat) ) )
                                                    qb ) ) )
                                          zero_nat )
                                       ra qmb
                                   , suc nb )
                                 , ra )
                               else ((qmb, nb), the r_opt)
                             in
                             (let qmc, nc = bc in
                              fun ra ->
                                ( (qmc, nc)
                                , ( rs_lts_add _A2
                                      (linorder_list
                                         ( equal_prod _B1 _B1
                                         , linorder_prod _B2 _B2 ) )
                                      r ab ra dda
                                  , qa :: naa ) ) )
                               ca
                           else ((qmb, nb), (dda, naa)) )
                          c )
                      ((qma, na), (dd, []))
                  in
                  (let qmb, nb = ab in
                   fun (dda, ac) ->
                     ( ( (qmb, nb)
                       , ( ins_dj_rm_basic_ops _A2 r qs
                         , ( siga
                           , ( dda
                             , ( isa
                               , if
                                   not
                                     (g_disjoint_dflt_basic_oops_rm_basic_ops
                                        _A2 q f1 )
                                 then ins_dj_rm_basic_ops _A2 r fs
                                 else fs ) ) ) ) )
                     , ac ) )
                    bb )
               ba )
           ( ( (qm, n)
             , ( empty_rm_basic_ops _A2 ()
               , ( a1
                 , ( rs_lts_empty _A2
                       (linorder_list
                          (equal_prod _B1 _B1, linorder_prod _B2 _B2) )
                   , (is, empty_rm_basic_ops _A2 ()) ) ) ) )
           , [i1] )
       in
       (let _, aaa = aa in
        fun _ -> aaa )
         ba )
      b

  let rec concat_rename_impl_aux c_inter c_nempty const f it_1 it_2 it_3 i1a
      i2a i1 f1a i2 fP1 fP2 rename1 rename2 f1 f2 =
   fun a1 a2 ->
    let aA1 = rename1 a1 f1 in
    let a = rename2 a2 f2 in
    concat_impl_aux c_inter c_nempty const f it_1 it_2 it_3 i1a i2a i1 f1a i2
      fP1 fP2 aA1 a

  let rec nfa_acceptingp a = snd (snd (snd a))

  let rec nfa_initialp a = fst (snd (snd a))

  let rec nfa_transp a = fst (snd a)

  let rec rs_nfa_concate_rename (_A1, _A2) (_B1, _B2) f1a f2a
      (q1, (d1, (i1, f1))) (q2, (d2, (i2, f2))) =
    concat_rename_impl_aux
      (g_inter_dflt_basic_oops_rm_basic_ops (linorder_prod _A2 _A2))
      (fun x ->
        not
          (g_isEmpty_dflt_basic_oops_rm_basic_ops (linorder_prod _A2 _A2) x) )
      (fun ff i fp d_it ->
        let a, b =
          foldl
            (fun (a, b) ->
              (let qm, n = a in
               fun is q ->
                 ( ( insert (linorder_prod _A2 _A2) (ff q)
                       (states_enumerate _A1 n) qm
                   , suc n )
                 , ins_dj_rm_basic_ops _A2 (states_enumerate _A1 n) is ) )
                b )
            ( (empty (linorder_prod _A2 _A2), zero_nat)
            , empty_rm_basic_ops _A2 () )
            i
        in
        (let qm, n = a in
         fun is ->
           let aa, ba =
             worklist
               (fun _ -> true)
               (fun (aa, ba) ->
                 (let qma, na = aa in
                  fun (qs, (dd, (isa, fs))) q ->
                    let r =
                      the (lookup (linorder_prod _A2 _A2) qma (ff q))
                    in
                    if memb_rm_basic_ops _A2 r qs then
                      (((qma, na), (qs, (dd, (isa, fs)))), [])
                    else
                      let ab, bb =
                        d_it q
                          (fun _ -> true)
                          (fun (ab, qa) (bb, c) ->
                            (let qmb, nb = bb in
                             fun (dda, naa) ->
                               if nemptyIs _B2 ab then
                                 let r_opt =
                                   lookup (linorder_prod _A2 _A2) qmb (ff qa)
                                 in
                                 let bc, ca =
                                   if is_none r_opt then
                                     let ra = states_enumerate _A1 nb in
                                     ( ( insert (linorder_prod _A2 _A2)
                                           (ff qa) ra qmb
                                       , suc nb )
                                     , ra )
                                   else ((qmb, nb), the r_opt)
                                 in
                                 (let qmc, nc = bc in
                                  fun ra ->
                                    ( (qmc, nc)
                                    , ( rs_lts_add _A2
                                          (linorder_list
                                             ( equal_prod _B1 _B1
                                             , linorder_prod _B2 _B2 ) )
                                          r ab ra dda
                                      , qa :: naa ) ) )
                                   ca
                               else ((qmb, nb), (dda, naa)) )
                              c )
                          ((qma, na), (dd, []))
                      in
                      (let qmb, nb = ab in
                       fun (dda, ac) ->
                         ( ( (qmb, nb)
                           , ( ins_dj_rm_basic_ops _A2 r qs
                             , ( dda
                               , ( isa
                                 , if fp q then ins_dj_rm_basic_ops _A2 r fs
                                   else fs ) ) ) )
                         , ac ) )
                        bb )
                   ba )
               ( ( (qm, n)
                 , ( empty_rm_basic_ops _A2 ()
                   , ( rs_lts_empty _A2
                         (linorder_list
                            (equal_prod _B1 _B1, linorder_prod _B2 _B2) )
                     , (is, empty_rm_basic_ops _A2 ()) ) ) )
               , i )
           in
           (let _, aaa = aa in
            fun _ -> aaa )
             ba )
          b )
      id
      (fun a ->
        rs_lts_succ_label_it (linorder_prod _A2 _A2)
          (linorder_list (equal_prod _B1 _B1, linorder_prod _B2 _B2))
          (nfa_transp a) )
      (fun a ->
        rs_lts_succ_label_it (linorder_prod _A2 _A2)
          (linorder_list (equal_prod _B1 _B1, linorder_prod _B2 _B2))
          (nfa_transp a) )
      (fun a ->
        rs_lts_connect_it (linorder_prod _A2 _A2)
          (linorder_list (equal_prod _B1 _B1, linorder_prod _B2 _B2))
          (linorder_prod _A2 _A2) (nfa_transp a) )
      (fun a ->
        g_to_list_dflt_basic_oops_rm_basic_ops (linorder_prod _A2 _A2)
          (nfa_initialp a) )
      (fun a ->
        g_to_list_dflt_basic_oops_rm_basic_ops (linorder_prod _A2 _A2)
          (nfa_initialp a) )
      nfa_initialp nfa_acceptingp nfa_initialp nfa_acceptingp
      (fun a q ->
        memb_rm_basic_ops (linorder_prod _A2 _A2) q (nfa_acceptingp a) )
      (rename_states_gen_impl
         (fun f s ->
           iteratei_set_op_list_it_rs_ops _A2 s
             (fun _ -> true)
             (fun b -> ins_rm_basic_ops (linorder_prod _A2 _A2) (f b))
             (empty_rm_basic_ops (linorder_prod _A2 _A2) ()) )
         (rs_lts_image _A2
            (linorder_list (equal_prod _B1 _B1, linorder_prod _B2 _B2))
            (linorder_prod _A2 _A2)
            (linorder_list (equal_prod _B1 _B1, linorder_prod _B2 _B2)) ) )
      (rename_states_gen_impl
         (fun f s ->
           iteratei_set_op_list_it_rs_ops _A2 s
             (fun _ -> true)
             (fun b -> ins_rm_basic_ops (linorder_prod _A2 _A2) (f b))
             (empty_rm_basic_ops (linorder_prod _A2 _A2) ()) )
         (rs_lts_image _A2
            (linorder_list (equal_prod _B1 _B1, linorder_prod _B2 _B2))
            (linorder_prod _A2 _A2)
            (linorder_list (equal_prod _B1 _B1, linorder_prod _B2 _B2)) ) )
      f1a f2a
      (q1, (d1, (i1, f1)))
      (q2, (d2, (i2, f2)))

  let rec rs_nfa_normal (_A1, _A2) (_B1, _B2) =
   fun a ->
    let b, c =
      foldl
        (fun (aa, b) ->
          (let qm, n = aa in
           fun is q ->
             ( ( insert (linorder_prod _A2 _A2) (id q)
                   (states_enumerate _A1 n) qm
               , suc n )
             , ins_dj_rm_basic_ops _A2 (states_enumerate _A1 n) is ) )
            b )
        ((empty (linorder_prod _A2 _A2), zero_nat), empty_rm_basic_ops _A2 ())
        (g_to_list_dflt_basic_oops_rm_basic_ops (linorder_prod _A2 _A2)
           (fst (snd (snd a))) )
    in
    (let qm, n = b in
     fun is ->
       let aa, ba =
         worklist
           (fun _ -> true)
           (fun (ba, ca) ->
             (let qma, na = ba in
              fun (qs, (dd, (isa, fs))) q ->
                let r = the (lookup (linorder_prod _A2 _A2) qma (id q)) in
                if memb_rm_basic_ops _A2 r qs then
                  (((qma, na), (qs, (dd, (isa, fs)))), [])
                else
                  let bb, cb =
                    rs_lts_succ_label_it (linorder_prod _A2 _A2)
                      (linorder_list
                         (equal_prod _B1 _B1, linorder_prod _B2 _B2) )
                      (fst (snd a))
                      q
                      (fun _ -> true)
                      (fun (aa, qa) (bb, cb) ->
                        (let qmb, nb = bb in
                         fun (dda, naa) ->
                           if nemptyIs _B2 aa then
                             let r_opt =
                               lookup (linorder_prod _A2 _A2) qmb (id qa)
                             in
                             let bc, cc =
                               if is_none r_opt then
                                 let ra = states_enumerate _A1 nb in
                                 ( ( insert (linorder_prod _A2 _A2) (id qa)
                                       ra qmb
                                   , suc nb )
                                 , ra )
                               else ((qmb, nb), the r_opt)
                             in
                             (let qmc, nc = bc in
                              fun ra ->
                                ( (qmc, nc)
                                , ( rs_lts_add _A2
                                      (linorder_list
                                         ( equal_prod _B1 _B1
                                         , linorder_prod _B2 _B2 ) )
                                      r aa ra dda
                                  , qa :: naa ) ) )
                               cc
                           else ((qmb, nb), (dda, naa)) )
                          cb )
                      ((qma, na), (dd, []))
                  in
                  (let qmb, nb = bb in
                   fun (dda, bc) ->
                     ( ( (qmb, nb)
                       , ( ins_dj_rm_basic_ops _A2 r qs
                         , ( dda
                           , ( isa
                             , if
                                 memb_rm_basic_ops (linorder_prod _A2 _A2) q
                                   (snd (snd (snd a)))
                               then ins_dj_rm_basic_ops _A2 r fs
                               else fs ) ) ) )
                     , bc ) )
                    cb )
               ca )
           ( ( (qm, n)
             , ( empty_rm_basic_ops _A2 ()
               , ( rs_lts_empty _A2
                     (linorder_list
                        (equal_prod _B1 _B1, linorder_prod _B2 _B2) )
                 , (is, empty_rm_basic_ops _A2 ()) ) ) )
           , g_to_list_dflt_basic_oops_rm_basic_ops (linorder_prod _A2 _A2)
               (fst (snd (snd a))) )
       in
       (let _, aaa = aa in
        fun _ -> aaa )
         ba )
      c

  let rec rs_nft_destruct (_A1, _A2) _C _D (q, (d, (i, (f, tF)))) =
    rs_nft_destruct (_A1, _A2) _C _D (q, (d, (i, (f, tF))))

  let rec rs_nfae_destruct (_A1, _A2) (_B1, _B2) (q, (d1, (d2, (i, f)))) =
    ( g_to_list_dflt_basic_oops_rm_basic_ops (linorder_prod _A2 _A2) q
    , ( rs_lts_to_collect_list (linorder_prod _A2 _A2)
          (linorder_list (equal_prod _B1 _B1, linorder_prod _B2 _B2))
          d1
      , ( g_to_list_dflt_basic_oops_rm_basic_ops
            (linorder_prod (linorder_prod _A2 _A2) (linorder_prod _A2 _A2))
            d2
        , ( g_to_list_dflt_basic_oops_rm_basic_ops (linorder_prod _A2 _A2) i
          , g_to_list_dflt_basic_oops_rm_basic_ops (linorder_prod _A2 _A2) f
          ) ) ) )

  let rec diffS _A f1 f2 i s = foldl (diffIs _A f1 f2) i s

  let rec rs_nfa_tran_complement (_A1, _A2) (_B1, _B2, _B3) f1 f2
      (q, (a, (d, (i, f)))) q_dead =
    (let qa, (aa, (da, (ia, fa))) = (q, (a, (d, (i, f)))) in
     fun q_deada ->
       let x =
         iteratei_set_op_list_it_rs_ops _B3 qa
           (fun _ -> true)
           (fun x sigma ->
             let xa =
               dnfa_iter _B3
                 (linorder_list (equal_prod _A1 _A1, linorder_prod _A2 _A2))
                 _B3 da
                 (fun _ -> true)
                 (fun xa sigmaa ->
                   if eq _B1 x (fst xa) then fst (snd xa) :: sigmaa
                   else sigmaa )
                 []
             in
             let xb = diffS _A2 f1 f2 aa xa in
             if emptyIs _A2 xb then sigma
             else
               rs_lts_add _B3
                 (linorder_list (equal_prod _A1 _A1, linorder_prod _A2 _A2))
                 x xb q_deada sigma )
           da
       in
       ( ins_rm_basic_ops _B3 q_deada qa
       , ( aa
         , ( rs_lts_add _B3
               (linorder_list (equal_prod _A1 _A1, linorder_prod _A2 _A2))
               q_deada aa q_deada x
           , (ia, fa) ) ) ) )
      q_dead

  let rec g_from_list_aux_dflt_basic_oops_rm_basic_ops _A accs x1 =
    match (accs, x1) with
    | accs, x :: l ->
        g_from_list_aux_dflt_basic_oops_rm_basic_ops _A
          (ins_rm_basic_ops _A x accs)
          l
    | y, [] -> y

  let rec g_from_list_dflt_basic_oops_rm_basic_ops _A l =
    g_from_list_aux_dflt_basic_oops_rm_basic_ops _A
      (empty_rm_basic_ops _A ())
      l

  let rec rs_nfa_construct_interval (_A1, _A2) (_B1, _B2) (ql, (dl, (il, fl)))
      =
    foldl
      (fun (q, (d, (i, f))) (q1, (l, q2)) ->
        ( ins_rm_basic_ops _A2 q1 (ins_rm_basic_ops _A2 q2 q)
        , ( rs_lts_add _A2
              (linorder_list (equal_prod _B1 _B1, linorder_prod _B2 _B2))
              q1 l q2 d
          , (i, f) ) ) )
      ( g_from_list_dflt_basic_oops_rm_basic_ops _A2 (ql @ il @ fl)
      , ( rs_lts_empty _A2
            (linorder_list (equal_prod _B1 _B1, linorder_prod _B2 _B2))
        , ( g_from_list_dflt_basic_oops_rm_basic_ops _A2 il
          , g_from_list_dflt_basic_oops_rm_basic_ops _A2 fl ) ) )
      dl

  let rec rs_nfa_construct_reachable (_A1, _A2) (_B1, _B2)
      (q2, (d2, (i2, f2))) =
    let a, b =
      foldl
        (fun (a, b) ->
          (let qm, n = a in
           fun is q ->
             ( (insert _A2 (id q) (states_enumerate _A1 n) qm, suc n)
             , ins_dj_rm_basic_ops _A2 (states_enumerate _A1 n) is ) )
            b )
        ((empty _A2, zero_nat), empty_rm_basic_ops _A2 ())
        (g_to_list_dflt_basic_oops_rm_basic_ops _A2 i2)
    in
    (let qm, n = a in
     fun is ->
       let aa, ba =
         worklist
           (fun _ -> true)
           (fun (aa, ba) ->
             (let qma, na = aa in
              fun (qs, (dd, (isa, fs))) q ->
                let r = the (lookup _A2 qma (id q)) in
                if memb_rm_basic_ops _A2 r qs then
                  (((qma, na), (qs, (dd, (isa, fs)))), [])
                else
                  let ab, bb =
                    rs_lts_succ_label_it _A2
                      (linorder_list
                         (equal_prod _B1 _B1, linorder_prod _B2 _B2) )
                      d2 q
                      (fun _ -> true)
                      (fun (ab, qa) (bb, c) ->
                        (let qmb, nb = bb in
                         fun (dda, naa) ->
                           if nemptyIs _B2 ab then
                             let r_opt = lookup _A2 qmb (id qa) in
                             let bc, ca =
                               if is_none r_opt then
                                 let ra = states_enumerate _A1 nb in
                                 ((insert _A2 (id qa) ra qmb, suc nb), ra)
                               else ((qmb, nb), the r_opt)
                             in
                             (let qmc, nc = bc in
                              fun ra ->
                                ( (qmc, nc)
                                , ( rs_lts_add _A2
                                      (linorder_list
                                         ( equal_prod _B1 _B1
                                         , linorder_prod _B2 _B2 ) )
                                      r ab ra dda
                                  , qa :: naa ) ) )
                               ca
                           else ((qmb, nb), (dda, naa)) )
                          c )
                      ((qma, na), (dd, []))
                  in
                  (let qmb, nb = ab in
                   fun (dda, ac) ->
                     ( ( (qmb, nb)
                       , ( ins_dj_rm_basic_ops _A2 r qs
                         , ( dda
                           , ( isa
                             , if memb_rm_basic_ops _A2 q f2 then
                                 ins_dj_rm_basic_ops _A2 r fs
                               else fs ) ) ) )
                     , ac ) )
                    bb )
               ba )
           ( ( (qm, n)
             , ( empty_rm_basic_ops _A2 ()
               , ( rs_lts_empty _A2
                     (linorder_list
                        (equal_prod _B1 _B1, linorder_prod _B2 _B2) )
                 , (is, empty_rm_basic_ops _A2 ()) ) ) )
           , g_to_list_dflt_basic_oops_rm_basic_ops _A2 i2 )
       in
       (let _, aaa = aa in
        fun _ -> aaa )
         ba )
      b

  let rec rs_product_transducer (_A1, _A2) (_B1, _B2) _C (_E1, _E2) t a f fe
      =
    let x =
      iteratei_set_op_list_it_rs_ops _A2 (fst t)
        (fun _ -> true)
        (fun x sigma ->
          let aa =
            iteratei_set_op_list_it_rs_ops _A2 (fst a)
              (fun _ -> true)
              (fun xa -> ins_rm_basic_ops (linorder_prod _A2 _A2) (x, xa))
              (empty_rm_basic_ops (linorder_prod _A2 _A2) ())
          in
          g_union_dflt_basic_oops_rm_basic_ops (linorder_prod _A2 _A2) sigma
            aa )
        (empty_rm_basic_ops (linorder_prod _A2 _A2) ())
    in
    let xa =
      rs_lts_it _A2
        (linorder_prod
           (linorder_option
              (linorder_list (equal_prod _B1 _B1, linorder_prod _B2 _B2)) )
           _C )
        _A2
        (fst (snd t))
        (fun _ -> true)
        (fun xa sigma ->
          if is_none (fst (fst (snd xa))) then
            if is_none (snd (snd (snd (snd t))) (snd (fst (snd xa))) None)
            then
              let xb =
                iteratei_set_op_list_it_rs_ops _A2 (fst a)
                  (fun _ -> true)
                  (fun xb ->
                    ins_rm_basic_ops
                      (linorder_prod (linorder_prod _A2 _A2)
                         (linorder_prod _A2 _A2) )
                      ((fst xa, xb), (snd (snd xa), xb)) )
                  (empty_rm_basic_ops
                     (linorder_prod (linorder_prod _A2 _A2)
                        (linorder_prod _A2 _A2) )
                     () )
              in
              ( fst sigma
              , g_union_dflt_basic_oops_rm_basic_ops
                  (linorder_prod (linorder_prod _A2 _A2)
                     (linorder_prod _A2 _A2) )
                  (snd sigma) xb )
            else
              let xb =
                iteratei_set_op_list_it_rs_ops _A2 (fst a)
                  (fun _ -> true)
                  (fun xb ->
                    rs_lts_add (linorder_prod _A2 _A2)
                      (linorder_list
                         (equal_prod _E1 _E1, linorder_prod _E2 _E2) )
                      (fst xa, xb)
                      (the
                         (snd (snd (snd (snd t))) (snd (fst (snd xa))) None) )
                      (snd (snd xa), xb) )
                  (rs_lts_empty (linorder_prod _A2 _A2)
                     (linorder_list
                        (equal_prod _E1 _E1, linorder_prod _E2 _E2) ) )
              in
              ( rs_lts_union (linorder_prod _A2 _A2)
                  (linorder_list
                     (equal_prod _E1 _E1, linorder_prod _E2 _E2) )
                  (fst sigma) xb
              , snd sigma )
          else
            rs_lts_it _A2
              (linorder_list (equal_prod _B1 _B1, linorder_prod _B2 _B2))
              _A2
              (fst (snd a))
              (fun _ -> true)
              (fun xb sigmaa ->
                if
                  nemptyIs _B2
                    (intersectIs _B2
                       (the (fst (fst (snd xa))))
                       (fst (snd xb)) )
                then
                  let xc =
                    if
                      not
                        (is_none
                           (f
                              (snd (snd (snd (snd t))) (snd (fst (snd xa))))
                              (intersectIs _B2
                                 (the (fst (fst (snd xa))))
                                 (fst (snd xb)) ) ) )
                    then
                      rs_lts_add (linorder_prod _A2 _A2)
                        (linorder_list
                           (equal_prod _E1 _E1, linorder_prod _E2 _E2) )
                        (fst xa, fst xb)
                        (the
                           (f
                              (snd (snd (snd (snd t))) (snd (fst (snd xa))))
                              (intersectIs _B2
                                 (the (fst (fst (snd xa))))
                                 (fst (snd xb)) ) ) )
                        (snd (snd xa), snd (snd xb))
                        (fst sigmaa)
                    else fst sigmaa
                  in
                  let aa =
                    if
                      fe
                        (snd (snd (snd (snd t))) (snd (fst (snd xa))))
                        (intersectIs _B2
                           (the (fst (fst (snd xa))))
                           (fst (snd xb)) )
                    then
                      ins_rm_basic_ops
                        (linorder_prod (linorder_prod _A2 _A2)
                           (linorder_prod _A2 _A2) )
                        ((fst xa, fst xb), (snd (snd xa), snd (snd xb)))
                        (snd sigmaa)
                    else snd sigmaa
                  in
                  (xc, aa)
                else sigmaa )
              sigma )
        ( rs_lts_empty (linorder_prod _A2 _A2)
            (linorder_list (equal_prod _E1 _E1, linorder_prod _E2 _E2))
        , empty_rm_basic_ops
            (linorder_prod (linorder_prod _A2 _A2) (linorder_prod _A2 _A2))
            () )
    in
    let xb =
      iteratei_set_op_list_it_rs_ops _A2
        (fst (snd (snd t)))
        (fun _ -> true)
        (fun xb sigma ->
          let aa =
            iteratei_set_op_list_it_rs_ops _A2
              (fst (snd (snd a)))
              (fun _ -> true)
              (fun xc -> ins_rm_basic_ops (linorder_prod _A2 _A2) (xb, xc))
              (empty_rm_basic_ops (linorder_prod _A2 _A2) ())
          in
          g_union_dflt_basic_oops_rm_basic_ops (linorder_prod _A2 _A2) sigma
            aa )
        (empty_rm_basic_ops (linorder_prod _A2 _A2) ())
    in
    let xc =
      iteratei_set_op_list_it_rs_ops _A2
        (fst (snd (snd (snd t))))
        (fun _ -> true)
        (fun xc sigma ->
          let aa =
            iteratei_set_op_list_it_rs_ops _A2
              (snd (snd (snd a)))
              (fun _ -> true)
              (fun xd -> ins_rm_basic_ops (linorder_prod _A2 _A2) (xc, xd))
              (empty_rm_basic_ops (linorder_prod _A2 _A2) ())
          in
          g_union_dflt_basic_oops_rm_basic_ops (linorder_prod _A2 _A2) sigma
            aa )
        (empty_rm_basic_ops (linorder_prod _A2 _A2) ())
    in
    (x, (fst xa, (snd xa, (xb, xc))))

  let rec rs_nft_construct_interval_aux (_A1, _A2) (_B1, _B2) _C _D =
   fun (q, (d, (i, (f, funa)))) (q1, (a, b)) ->
    (let l, fa = a in
     fun q2 ->
       ( ins_rm_basic_ops _A2 q1 (ins_rm_basic_ops _A2 q2 q)
       , ( rs_lts_add _A2
             (linorder_prod
                (linorder_option
                   (linorder_list
                      (equal_prod _B1 _B1, linorder_prod _B2 _B2) ) )
                _C )
             q1 (l, fa) q2 d
         , (i, (f, funa)) ) ) )
      b

  let rec rs_nft_construct_interval (_A1, _A2) (_B1, _B2) _C _D
      (ql, (dl, (il, (fl, funa)))) =
    foldl
      (rs_nft_construct_interval_aux (_A1, _A2) (_B1, _B2) _C _D)
      ( g_from_list_dflt_basic_oops_rm_basic_ops _A2 (ql @ il @ fl)
      , ( rs_lts_empty _A2
            (linorder_prod
               (linorder_option
                  (linorder_list (equal_prod _B1 _B1, linorder_prod _B2 _B2)) )
               _C )
        , ( g_from_list_dflt_basic_oops_rm_basic_ops _A2 il
          , (g_from_list_dflt_basic_oops_rm_basic_ops _A2 fl, funa) ) ) )
      dl

  let rec rs_gen_S_from_list _A l =
    foldl (fun s a -> ins_rm_basic_ops _A a s) (empty_rm_basic_ops _A ()) l

  let rec rs_gen_rm_from_list _A (_B1, _B2) _C l =
    foldl (fun m a -> insert _A (fst a) (snd a) m) (empty _A) l

  let rec rs_gen_rc_from_list _A l =
    foldl
      (fun m a ->
        insert _A (fst a)
          (foldl
             (fun s aa -> ins_rm_basic_ops (linorder_prod _A _A) aa s)
             (empty_rm_basic_ops (linorder_prod _A _A) ())
             (snd a) )
          m )
      (empty _A) l

  let rec rs_S_to_list _A s = g_to_list_dflt_basic_oops_rm_basic_ops _A s

  let rec rs_rm_to_list _A (_B1, _B2) _C l = g_to_list_rm_basic_ops _A l

  let rec member _A x0 y =
    match (x0, y) with
    | [], y -> false
    | x :: xs, y -> eq _A x y || member _A xs y

  let rec distinct _A = function
    | [] -> true
    | x :: xs -> (not (member _A xs x)) && distinct _A xs

  let rec lookup_aux _A v rc = lookup _A rc v

  let rec rs_indegree (_A1, _A2) s rc =
    let x =
      iteratei_set_op_list_it_rs_ops _A2 s
        (fun _ -> true)
        (fun x sigma ->
          let xa =
            if not (is_none (lookup _A2 rc x)) then
              iteratei_set_op_list_it_rs_ops (linorder_prod _A2 _A2)
                (the (lookup_aux _A2 x rc))
                (fun _ -> true)
                (fun xa sigmaa -> fst xa :: snd xa :: sigmaa)
                []
            else []
          in
          xa @ sigma )
        []
    in
    if distinct _A1 x then true else false
end
(*struct Automata_lib*)
