module Mymodule = struct
  let inc x = x+1

  let dec x = x-1

  let empty =[]

  let is_impty = function
    |[] ->true
    |h::t -> false

    let push x y = x::y

    exception Empty

    let pik = function
    | []->0
    |h::_ ->h

    let pop = function 
    |[] ->raise Empty
    |_::t -> t

end
;;

(* module Mymodule :
  sig
    val inc : int -> int
    val dec : int -> int
    val empty : 'a list
    val is_impty : 'a list -> bool
    val push : 'a -> 'a list -> 'a list
    exception Empty
    val pik : int list -> int
    val pop : 'a list -> 'a list
  end
 *)
Mymodule.inc 5;;
Mymodule.dec 5;;
Mymodule.empty;;
Mymodule.is_impty [];;
Mymodule.is_impty [4;6;];;
Mymodule.push 5 [3;4];;
Mymodule.Empty;;
Mymodule.pik [4;3;5;6;7];;
Mymodule.pop [1;2;3;4;5];;
Mymodule.pop [];;