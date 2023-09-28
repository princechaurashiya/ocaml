type nat = Zero | Succ of nat

(* type nat = Zero | Succ of nat *)

let zero = Zero
let one = Succ zero
let two = Succ one
let three = Succ two
let four = Succ three

(* val zero : nat = Zero
val one : nat = Succ Zero
val two : nat = Succ (Succ Zero)
val three : nat = Succ (Succ (Succ Zero))
val four : nat = Succ (Succ (Succ (Succ Zero))) *)







let iszero = function
  | Zero -> true
  | Succ _ -> false

let pred = function
  | Zero -> failwith "pred Zero is undefined"
  | Succ m -> m ;;
  
  (* val iszero : nat -> bool = <fun>
  val pred : nat -> nat = <fun> *)

  let rec add n1 n2 =
    match n1 with
    | Zero -> n2
    | Succ pred_n -> add pred_n (Succ n2)

    (* val add : nat -> nat -> nat = <fun> *)

    let rec int_of_nat = function
  | Zero -> 0
  | Succ m -> 1 + int_of_nat m

let rec nat_of_int = function
  | i when i = 0 -> Zero
  | i when i > 0 -> Succ (nat_of_int (i - 1))
  | _ -> failwith "nat_of_int is undefined on negative ints"

  (* val int_of_nat : nat -> int = <fun> *)
  (* val nat_of_int : int -> nat = <fun> *)

  let rec even = function Zero -> true | Succ m -> odd m
and odd = function Zero -> false | Succ m -> even m

(* val even : nat -> bool = <fun>
val odd : nat -> bool = <fun> *)