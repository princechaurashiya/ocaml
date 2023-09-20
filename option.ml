let get_val o =
  match o with
  |None -> false
  |Some p -> p;;
  (* val get_val : bool option -> bool = <fun> *)

  let getval default = function
  |None -> default
  |Some x ->x;;
  (* val getval : 'a -> 'a option -> 'a = <fun> *)

  let rec list_max (lst: 'a list ): 'a option =
    match lst with 
  | [] ->None
  | h::t -> match list_max t with
    | None ->Some h
    | Some m -> Some(max m h);;
 (* val list_max : 'a list -> 'a option = <fun> *)
