(*
   Example of Association List *)

   let d = [("rectangle", 4); ("nonagon", 9); ("icosagon", 20)]

   (*
    val d : (string * int) list =
  [("rectangle", 4); ("nonagon", 9); ("icosagon", 20)] *)

  (*
      Note that an association list isn’t so much a built-in data type in OCaml as a combination of two other types: lists and pairs.   
  *)

  (** [insert k v lst] is an association list that binds key [k] to value [v]
    and otherwise is the same as [lst] *)
let insert k v lst = (k, v) :: lst

(** [lookup k lst] is [Some v] if association list [lst] binds key [k] to
    value [v]; and is [None] if [lst] does not bind [k]. *)
let rec lookup k = function
| [] -> None
| (k', v) :: t -> if k = k' then Some v else lookup k t ;;


(*
  val insert : 'a -> 'b -> ('a * 'b) list -> ('a * 'b) list = <fun>   

  val lookup : 'a -> ('a * 'b) list -> 'b option = <fun>
*)

(* They are already defined in standard library *)
List.assoc;;
List.assoc_opt;;
(*
  - : 'a -> ('a * 'b) list -> 'b = <fun>
  - : 'a -> ('a * 'b) list -> 'b option = <fun>
  
*)