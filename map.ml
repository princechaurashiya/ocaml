(* ___________________map__________________ *)

(** [add1 lst] adds 1 to each element of [lst] *)
let rec add1 = function
  | [] -> []
  | h :: t -> (h + 1) :: add1 t
let lst1 = add1 [1; 2; 3];;
(* val lst1 : int list = [2; 3; 4] *)


(** [concat_bang lst] concatenates "!" to each element of [lst] *)
let rec concat_bang = function
  | [] -> []
  | h :: t -> (h ^ "!") :: concat_bang t
let lst2 = concat_bang ["sweet"; "salty"]
(* val lst2 : string list = ["sweet!"; "salty!"] *)



(** [add1 lst] adds 1 to each element of [lst] *)
let rec add1 = function
  | [] -> []
  | h :: t ->
    let f = fun x -> x + 1 in
    f h :: add1 t
(** [concat_bang lst] concatenates "!" to each element of [lst] *)
let rec concat_bang = function
  | [] -> []
  | h :: t ->
    let f = fun x -> x ^ "!" in
    f h :: concat_bang t;;
    (* val add1 : int list -> int list = <fun> *)
    (* val concat_bang : string list -> string list = <fun> *)


    

    let rec add1' f = function
  | [] -> []
  | h :: t -> f h :: add1' f t

(** [add1 lst] adds 1 to each element of [lst] *)
let add1 = add1' (fun x -> x + 1)

let rec concat_bang' f = function
  | [] -> []
  | h :: t -> f h :: concat_bang' f t

(** [concat_bang lst] concatenates "!" to each element of [lst] *)
let concat_bang = concat_bang' (fun x -> x ^ "!")

(* val add1' : ('a -> 'b) -> 'a list -> 'b list = <fun> *)

(* val add1 : int list -> int list = <fun> *)

(* val concat_bang' : ('a -> 'b) -> 'a list -> 'b list = <fun> *)

(* val concat_bang : string list -> string list = <fun>
     *)



     let rec transform f = function
     | [] -> []
     | h :: t -> f h :: transform f t
   
   (** [add1 lst] adds 1 to each element of [lst] *)
   let add1 = transform (fun x -> x + 1)
   
   (** [concat_bang lst] concatenates "!" to each element of [lst] *)
   let concat_bang = transform (fun x -> x ^ "!");;

   (* val transform : ('a -> 'b) -> 'a list -> 'b list = <fun> *)

   (* val add1 : int list -> int list = <fun> *)

   (* val concat_bang : string list -> string list = <fun> *)
   let add1 lst = transform (fun x -> x + 1) lst

   let add1 = transform (fun x -> x + 1)
   
   


   let rec map f = function
  | [] -> []
  | h :: t -> f h :: map f t

(** [add1 lst] adds 1 to each element of [lst] *)
let add1 = map (fun x -> x + 1)

(** [concat_bang lst] concatenates "!" to each element of [lst] *)
let concat_bang = map (fun x -> x ^ "!")

(* val map : ('a -> 'b) -> 'a list -> 'b list = <fun> *)

(* val add1 : int list -> int list = <fun> *)
 
(* val concat_bang : string list -> string list = <fun> *)



let p x = print_int x; print_newline(); x + 1

let lst = map p [1; 2];;

(* val p : int -> int = <fun> *)
(*
2
2
val lst : int list = [2; 3] *)

let rec map f = function
  | [] -> []
  | h :: t -> let h' = f h in h' :: map f t

let lst2 = map p [1; 2]

(* val map : ('a -> 'b) -> 'a list -> 'b list = <fun> *)

(* 1
2
val lst2 : int list = [2; 3] *)

(*   inbuild function
       __List.map__      *)

      
let rec map_tr_aux f acc = function
    | [] -> acc
    | h :: t -> map_tr_aux f (acc @ [f h]) t
    
     let map_tr f = map_tr_aux f []
     
     let lst = map_tr (fun x -> x + 1) [1; 2; 3]

     (* val map_tr_aux : ('a -> 'b) -> 'b list -> 'a list -> 'b list = <fun> *)

     (* val map_tr : ('a -> 'b) -> 'a list -> 'b list = <fun> *)

     (* val lst : int list = [2; 3; 4] *)



     let rec map_tr_aux f acc = function
  | [] -> acc
  | h :: t -> map_tr_aux f (f h :: acc) t

let map_tr f = map_tr_aux f []

let lst = map_tr (fun x -> x + 1) [1; 2; 3]

(* val map_tr_aux : ('a -> 'b) -> 'b list -> 'a list -> 'b list = <fun> *)

(* val map_tr : ('a -> 'b) -> 'a list -> 'b list = <fun> *)

(* val lst : int list = [4; 3; 2] *)



(* _________List.rev_map___________ *)

let rec rev_map_aux f acc = function
  | [] -> acc
  | h :: t -> rev_map_aux f (f h :: acc) t

let rev_map f = rev_map_aux f []

let lst = rev_map (fun x -> x + 1) [1; 2; 3]

(* val rev_map_aux : ('a -> 'b) -> 'b list -> 'a list -> 'b list = <fun> *)

(* val rev_map : ('a -> 'b) -> 'a list -> 'b list = <fun> *)

(* val lst : int list = [4; 3; 2] *)

(* If you want the output in the “right” order, that’s easy: just apply   List.rev   to it: *)

let lst = List.rev (List.rev_map (fun x -> x + 1) [1; 2; 3])

(* val lst : int list = [2; 3; 4] *)