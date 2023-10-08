(* Simple example of functors :
   Functors are a kind function on module .
   which can take module as a input and produce module as output *)
   
module type X= sig 
  val x : int 
end
module A : X = struct
let x = 0
end 
module IncX = functor (M:X)-> struct
let x = M.x +1
end

(* module type X = sig val x : int end
module A : X
module IncX : functor (M : X) -> sig val x : int end
 *)
 module B =IncX (A);;
(* module B : sig val x : int end
 *)

 B.x;;
 (* - : int = 1
 *)

 module C =IncX (B);;
 (* module C : sig val x : int end
 *)

 C.x;;
(* - : int = 2
 *)

 A.x;;
 (* - : int = 0
 *)





