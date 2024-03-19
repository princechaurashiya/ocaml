(*
  A ref is like a pointer or reference in an imperative language. It is a location in memory whose contents may change. Refs are also called ref cells, the idea being that there’s a cell in memory that can change.   
*)
let x = ref 0;;
(*
  val x : int ref = {contents = 0}   
*)
!x;;
(*- : int = 0*)
x := 1;;
(* - : unit = () *)
!x;;

(* - : int = 1 *)

(**********  7.1.1. Aliasing  **********)

(*
  Now that we have refs, we have aliasing: two refs could point to the same memory location, hence updating through one causes the other to also be updated. For example,   
*)
let x = ref 42;;
let y = ref 42;;
let z = x;;
x := 43;;
let w = !y + !z;;

(*
    val x : int ref = {contents = 42}
    val y : int ref = {contents = 42}
    val z : int ref = {contents = 42}
    - : unit = ()
    val w : int = 85

    The result of executing that code is that w is bound to 85, because let z = x causes z and x to become aliases, hence updating x to be 43 also causes z to be 43.
*)


(************7.1.3. Sequencing of Effects***********)

(*
  The semicolon operator is used to sequence effects, such as mutating refs. We’ve seen semicolon occur previously with printing. Now that we’re studying mutability, it’s time to treat it formally.   
*)

(* Semicolon is almost just syntactic sugar:
   
  e1 ; e2
          means the same as 
  let () = e1 in e2

  Except: if it's not true that e1 : unit --

    let syntax type error

    semicolon syntax: type warning because you probably don't want to discard e1's value
      -if you do, use ignore : 'a -> unit
*)

      let print_and_add x y = print_int(x + y) ; print_newline () ; x + y;;
      print_and_add 0 1;;

      (*
          val print_and_add : int -> int -> int = <fun>   

         1
       - : int = 1

      *)

      print_and_add 0 1 ; print_and_add 1 2;;

      (*
          Line 1, characters 0-17:
Warning 10 [non-unit-statement]: this expression should have type unit.
Line 1, characters 0-17:
Warning 10 [non-unit-statement]: this expression should have type unit.
1
3
- : int = 3   
      *)

  ignore (print_and_add 0 1) ; print_and_add 1 3;;
 (*
  1
  4
  - : int = 4
   
 *)


 (***************7.1.4. Example: Mutable Counter*************)

 (*
    Here is code that implements a counter. Every time next_val is called, it returns one more than the previous time.   
 *)

 let counter = ref 0

let next_val =
  fun () ->
    counter := !counter + 1;
    !counter;;

    (*
        val counter : int ref = {contents = 0}
        val next_val : unit -> int = <fun>
    *)
    next_val ();;
    (* - : int = 1 *)
    next_val ();;
    (* - : int = 2 *)
    next_val ();;
    (* - : int = 3 *)
   

    let next_val =
      let counter = ref 0 in
      fun () ->
        incr counter;
        !counter
        (* val next_val : unit -> int = <fun> 
           
        this is same as above, it is also do incriment by 1 on every call
        *)

        let next_val_broken = fun () ->
          let counter = ref 0 in
          incr counter;
          !counter;;

          (*
            val next_val_broken : unit -> int = <fun> 
            
            t’s only a little different: the binding of counter occurs after the fun () -> instead of before. But it makes a huge difference:
          *)
 next_val_broken ();;
 next_val_broken ();;
 next_val_broken ();;
  
(*
    - : int = 1
    - : int = 1
    - : int = 1

    The problem is that every time next_val_broken is called, the first thing it does is to evaluate ref 0 to a new location that is initialized to 0. That location is then incremented to 1, and 1 is returned. 

    Every call to next_val_broken is thus allocating a new ref cell, whereas next_val allocates just one new ref cell.
*)

(****************7.1.5. Example: Pointers************)


type 'a pointer = 'a ref option
(* type 'a pointer = 'a ref option *)
let null : 'a pointer = None
(* val null : 'a pointer = None  *)
let malloc (x : 'a) : 'a pointer = Some (ref x)
(* val malloc : 'a -> 'a pointer = <fun> *)
let p = malloc 42
(* val p : int pointer = Some {contents = 42} *)

exception Segfault

let deref (ptr : 'a pointer) : 'a =
  match ptr with None -> raise Segfault | Some r -> !r;;

  (* 
  exception Segfault
  val deref : 'a pointer -> 'a = <fun>
  *)
  deref p;;
  (* - : int = 42 *)
  deref null
  (* deref null *)
  let ( ~* ) = deref;;
  ~*p
(*  
    val ( ~* ) : 'a pointer -> 'a = <fun>
       - : int = 42
*)


let assign (ptr : 'a pointer) (x : 'a) : unit =
  match ptr with None -> raise Segfault | Some r -> r := x;;

  (* val assign : 'a pointer -> 'a -> unit = <fun> *)

  assign p 2;
  deref p;;
  (*- : int = 2 *)
  assign null 0;;
(* Exception: Segfault. *)


let ( =* ) = assign;;
p =* 3;;
~*p
(*
     val ( =* ) : 'a pointer -> 'a -> unit = <fun>
     - : unit = ()
     - : int = 3
*)
(*  skip*)

(*********************7.1.6. Example: Recursion Without Rec**************)

                            (* Skip*)



      (************7.1.7. Weak Type Variables************)

                        (* Skip*)

 (****************7.1.8. Physical Equality*****************)

                        (* Skip*)


(*****************7.1.9. Example: Singly-linked List*************)

(** An ['a node] is a node of a mutable singly-linked list. It contains a value
    of type ['a] and a link to the [next] node. *)
    type 'a node = { next : 'a mlist; value : 'a }

    (** An ['a mlist] is a mutable singly-linked list with elements of type ['a].
        The [option] represents the possibility that the list is empty.
        RI: The list does not contain any cycles. *)
    and 'a mlist = 'a node option ref

    (*  type 'a node = { next : 'a mlist; value : 'a; }
        and 'a mlist = 'a node option ref 

      To create an empty list, we simply return a ref to None:  
    *)

    (** [empty ()] is an empty singly-linked list. *)
let empty () : 'a mlist = ref None

(* val empty : unit -> 'a mlist = <fun> *)

(** [insert_first lst v] mutates mlist [lst] by inserting value [v] as the
    first value in the list. *)
    let insert_first (lst : 'a mlist) (v : 'a) : unit =
      lst := Some { next = ref !lst; value = v }

      (* val insert_first : 'a mlist -> 'a -> unit = <fun> *)


      (** [to_list lst] is an OCaml list containing the same values as [lst]
    in the same order. Not tail recursive. *)
let rec to_list (lst : 'a mlist) : 'a list =
  match !lst with None -> [] | Some { next; value } -> value :: to_list next

  (* val to_list : 'a mlist -> 'a list = <fun> *)

  let lst0 = empty ();;
let lst1 = lst0;;
insert_first lst0 1;;
to_list lst1;;

(*
    val lst0 : '_weak2 mlist = {contents = None}
    val lst1 : '_weak2 mlist = {contents = None}
    - : unit = ()
    - : int list = [1]

*)

(* The type of empty. Returning to empty, why must it be a function? It might seem as though we could define it more simply as follows:*)

let empty = ref None
(* val empty : '_weak3 option ref = {contents = None} *)


let lst2 = empty;;
let lst3 = empty;;
insert_first lst2 2;;
insert_first lst3 3;;
to_list lst2;;
to_list lst3;;

(*
    val lst2 : '_weak3 option ref = {contents = None}
    val lst3 : '_weak3 option ref = {contents = None}
    - : unit = ()
    - : unit = ()
    - : int list = [3; 2]
    - : int list = [3; 2]

 *)

 let empty () = ref None
 (* val empty : unit -> 'a option ref = <fun> *)

 let empty _ = ref None
let empty (b : bool) = ref None
let empty (n : int) = ref None
(* etc. *)
(*
  val empty : 'a -> 'b option ref = <fun>
  val empty : bool -> 'a option ref = <fun>
  val empty : int -> 'a option ref = <fun>
*)

type 'a node = { next : 'a mlist; value : 'a ref }
and 'a mlist = 'a node option ref

let empty () : 'a mlist = ref None

let insert_first (lst : 'a mlist) (v : 'a) : unit =
  lst := Some { next = ref !lst; value = ref v }

let rec set (lst : 'a mlist) (n : int) (v : 'a) : unit =
  match (!lst, n) with
  | None, _ -> invalid_arg "out of bounds"
  | Some { value }, 0 -> value := v
  | Some { next }, _ -> set next (n - 1) v

let rec to_list (lst : 'a mlist) : 'a list =
  match !lst with None -> [] | Some { next; value } -> !value :: to_list next

  (*
      type 'a node = { next : 'a mlist; value : 'a ref; }
      and 'a mlist = 'a node option ref
      val empty : unit -> 'a mlist = <fun>
      val insert_first : 'a mlist -> 'a -> unit = <fun>
      val set : 'a mlist -> int -> 'a -> unit = <fun>
      val to_list : 'a mlist -> 'a list = <fun>
  *)

  let lst = empty ();;
insert_first lst 42;;
insert_first lst 41;;
to_list lst;;
set lst 1 43;;
to_list lst;;

(*
    val lst : '_weak4 mlist = {contents = None}
    - : unit = ()
    - : unit = ()
    - : int list = [41; 42]
    - : unit = ()
    - : int list = [41; 43]
*)