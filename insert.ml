let insert k v lst =(k,v)::lst;;
(* val insert : 'a -> 'b -> ('a * 'b) list -> ('a * 'b) list = <fun> *)
insert "hii" 50 [];;
(* - : (string * int) list = [("hii", 50)] *)


let rec lookup k = function 
  |[] -> None
  |(k', v)::t -> if k = k' then Some v else lookup k t;;
  (* val lookpu : 'a -> ('a * 'b) list -> 'b option = <fun> *)

  
  lookup "hello"  [("key1", 42); ("key2", 15); ("key3", 99)];;
  (* - : int option = None *)
  lookup "hello"  [("key1", 42); ("hello", 15); ("key3", 99)];;
  (* - : int option = Some 15 *)
  

  (* for lookpu function we can use pre-define function called List.assoc. *)
  List.assoc "hello"  [("key1", 42); ("hello", 15); ("key3", 99)];;
(* - : int = 15 *)
