open Automata_lib

let less_eq_nat z1 z2 =
  Z.leq (Automata_lib.integer_of_nat z1) (Automata_lib.integer_of_nat z2)

let less_nat z1 z2 =
  Z.lt (Automata_lib.integer_of_nat z1) (Automata_lib.integer_of_nat z2)

let eq_nat n1 n2 =
  Automata_lib.integer_of_nat n1 = Automata_lib.integer_of_nat n2

let eq_Z n1 n2 = n1 = n2

let equal_nat = {Automata_lib.equal= eq_nat}

let equal_Z = {Automata_lib.equal= eq_Z}

let ord_Z = ({less_eq= Z.leq; less= Z.lt} : Z.t Automata_lib.ord)

let preorder_Z = ({ord_preorder= ord_Z} : Z.t Automata_lib.preorder)

let order_Z = ({preorder_order= preorder_Z} : Z.t Automata_lib.order)

let linorder_Z = ({order_linorder= order_Z} : Z.t Automata_lib.linorder)

let linorder_Z_list = Automata_lib.linorder_list (equal_Z, linorder_Z)

let ord_nat =
  ({less_eq= less_eq_nat; less= less_nat} : Automata_lib.nat Automata_lib.ord)

let preorder_nat =
  ({ord_preorder= ord_nat} : Automata_lib.nat Automata_lib.preorder)

let order_nat =
  ({preorder_order= preorder_nat} : Automata_lib.nat Automata_lib.order)

let linorder_nat =
  ({order_linorder= order_nat} : Automata_lib.nat Automata_lib.linorder)

let nFA_states_nat =
  ( {states_enumerate= (fun i -> i)}
    : Automata_lib.nat Automata_lib.nFA_states )

let z_to_int z = Automata_lib.nat_of_integer (Z.of_int z)

let nFA_states_natnat =
  ( {states_enumerate= (fun i -> (i, i))}
    : (Automata_lib.nat * Automata_lib.nat) Automata_lib.nFA_states )

let linorder_natnat = Automata_lib.linorder_prod linorder_nat linorder_nat

let less_eq_str (s1 : string) (s2 : string) = String.compare s1 s2 <= 0

let less_str (s1 : string) (s2 : string) = String.compare s1 s2 < 0

let ord_str =
  ({less_eq= less_eq_str; less= less_str} : string Automata_lib.ord)

let preorder_str = ({ord_preorder= ord_str} : string Automata_lib.preorder)

let order_str = ({preorder_order= preorder_str} : string Automata_lib.order)

let eq_str n1 n2 = String.compare n1 n2 == 0

let equal_str = {Automata_lib.equal= eq_str}

let order_string =
  ({preorder_order= preorder_str} : string Automata_lib.order)

let linorder_str =
  ({order_linorder= order_str} : string Automata_lib.linorder)

let linorder_str_list = Automata_lib.linorder_list (equal_str, linorder_str)

let eq_str_list l1 l2 =
  List.length l1 = List.length l2 && List.for_all2 eq_str l1 l2

let equal_str_list = {Automata_lib.equal= eq_str_list}
