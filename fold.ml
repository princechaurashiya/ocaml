(** [sum lst] is the sum of all the elements of [lst]. *)
let rec sum = function
  | [] -> 0
  | h :: t -> h + sum t

let s = sum [1; 2; 3]
(* val sum : int list -> int = <fun> *)
(* val s : int = 6 *)


(** [concat lst] is the concatenation of all the elements of [lst]. *)
let rec concat = function
  | [] -> ""
  | h :: t -> h ^ concat t

let c = concat ["a"; "b"; "c"]
(* val concat : string list -> string = <fun> *)
(* val c : string = "abc" *)



let rec sum' init = function
  | [] -> init
  | h :: t -> h + sum' init t

let sum = sum' 0

let rec concat' init = function
  | [] -> init
  | h :: t -> h ^ concat' init t

let concat = concat' ""

(* val sum' : int -> int list -> int = <fun> *)
(* val sum : int list -> int = <fun> *)
(* val concat' : string -> string list -> string = <fun> *)
(* val concat : string list -> string = <fun> *)


let rec combine op init = function
  | [] -> init
  | h :: t -> op h (combine op init t)

let sum = combine ( + ) 0
let concat = combine ( ^ ) ""

(* val combine : ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b = <fun> *)

(* val sum : int list -> int = <fun> *)

(* val concat : string list -> string = <fun> *)


(* ____________fold right_____________ *)
let rec combine f acc = function
  | [] -> acc
  | h :: t -> f h (combine f acc t)
  (* val combine : ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b = <fun> *)

  let rec combine' f lst acc = match lst with
  | [] -> acc
  | h :: t -> f h (combine' f t acc)

let sum lst = combine' ( + ) lst 0
let concat lst = combine' ( ^ ) lst ""
(* val combine' : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b = <fun> *)

(* val sum : int list -> int = <fun> *)

(* val concat : string list -> string = <fun> *)



(* ___________List.fold_right____________     
              inbuild function                *)
let rec fold_right f lst acc = match lst with
  | [] -> acc
  | h :: t -> f h (fold_right f t acc)

  (*  val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b = <fun> *)


  (* Now Tail Recursion *)
  let rec combine_tr f acc = function
  | [] -> acc
  | h :: t -> combine_tr f (f acc h) t  (* only real change *)

  (* val combine_tr : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a = <fun> *)
 
  
  let rec combine f acc = function
  | [] -> acc
  | h :: t ->
    let acc' = combine f acc t in
    f h acc'

let rec combine_tr f acc = function
  | [] -> acc
  | h :: t ->
    let acc' = f acc h in
    combine_tr f acc' t
     (* val combine : ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b = <fun> *)

     (* val combine_tr : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a = <fun> *)

     let sum = combine_tr ( + ) 0
let s = sum [1; 2; 3]
(* val sum : int list -> int = <fun> *)
(* val s : int = 6 *)

let sub = combine ( - ) 0
let s = sub [3; 2; 1]

let sub_tr = combine_tr ( - ) 0
let s' = sub_tr [3; 2; 1]

(* val sub : int list -> int = <fun> *)
(* val s : int = 2 *)

(* val sub_tr : int list -> int = <fun> *)
(* val s' : int = -6 *)


(* ____________Fold Left________________ 
  _______________inbuild fuction___________
  ______________ List.fold_left___________*)
  let rec fold_left f acc = function
  | [] -> acc
  | h :: t -> fold_left f (f acc h) t

let sum = fold_left ( + ) 0
let concat = fold_left ( ^ ) ""
(* val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a = <fun> *)
(* val sum : int list -> int = <fun> *)
(* val concat : string list -> string = <fun> *)



(* _______Fold Right vs Fold Left________ *)
List.fold_left
List.fold_right;;
(* - : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a = <fun> *)
(* - : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b = <fun> *)


ListLabels.fold_left;;
ListLabels.fold_left ~f:(fun x y -> x - y) ~init:0 [1;2;3];;
(* - : f:('a -> 'b -> 'a) -> init:'a -> 'b list -> 'a = <fun> *)
(* - : int = -6 *)

ListLabels.fold_right;;
ListLabels.fold_right ~f:(fun y x -> x - y) ~init:0 [1;2;3];;
(* - : f:('a -> 'b -> 'b) -> 'a list -> init:'b -> 'b = <fun> *)
(* - : int = -6 *)

(*  Digression on Labeled Arguments and Fold *)
let rec fold_left ~op:(f: acc:'a -> elt:'b -> 'a) ~init:acc lst =
  match lst with
  | [] -> acc
  | h :: t -> fold_left ~op:f ~init:(f ~acc:acc ~elt:h) t

let rec fold_right ~op:(f: elt:'a -> acc:'b -> 'b) lst ~init:acc =
  match lst with
  | [] -> acc
  | h :: t -> f ~elt:h ~acc:(fold_right ~op:f t ~init:acc)
  (* val fold_left : op:(acc:'a -> elt:'b -> 'a) -> init:'a -> 'b list -> 'a =
  <fun> *)
  (* val fold_right : op:(elt:'a -> acc:'b -> 'b) -> 'a list -> init:'b -> 'b =
  <fun> *)

  let s = fold_left ~op:( + ) ~init:0 [1;2;3]

  (* File "[17]", line 1, characters 22-27:
1 | let s = fold_left ~op:( + ) ~init:0 [1;2;3]
                          ^^^^^
Error: This expression has type int -> int -> int
       but an expression was expected of type acc:'a -> elt:'b -> 'a *)

(* The problem is that the built-in + operator doesn’t have labeled arguments, so we can’t pass it in as the combining operator to our labeled functions. We’d have to define our own labeled version of it:

 *)



 (* Using Fold to Implement Other Functions *)
 let length lst =
  List.fold_left (fun acc _ -> acc + 1) 0 lst

let rev lst =
  List.fold_left (fun acc x -> x :: acc) [] lst

let map f lst =
  List.fold_right (fun x acc -> f x :: acc) lst []

let filter f lst =
  List.fold_right (fun x acc -> if f x then x :: acc else acc) lst []

  (* val length : 'a list -> int = <fun> *)
  (* val rev : 'a list -> 'a list = <fun> *)
  (* val map : ('a -> 'b) -> 'a list -> 'b list = <fun> *)
  (* val filter : ('a -> bool) -> 'a list -> 'a list = <fun> *)



  (* Fold vs. Recursive vs. Library *)
  let rec lst_and_rec = function
  | [] -> true
  | h :: t -> h && lst_and_rec t

let lst_and_fold =
	List.fold_left (fun acc elt -> acc && elt) true

let lst_and_lib =
	List.for_all (fun x -> x)
(* val lst_and_rec : bool list -> bool = <fun> *)
(* val lst_and_fold : bool list -> bool = <fun> *)
(* val lst_and_lib : bool list -> bool = <fun> *)


(* 

     # The first function, lst_and_rec has the advantage that it need not process the entire list. It will immediately return false the first time they discover a false element in the list.

    # The second function, lst_and_fold, will always process every element of the list.

    # As for the third function lst_and_lib, according to the documentation of List.for_all, it returns (p a1) && (p a2) && ... && (p an). So like lst_and_rec it need not process every element.
 *)
