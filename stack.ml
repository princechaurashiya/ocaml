
module ListStack =struct
  type 'a stack = 'a list
  let empty = []
  let push x s = x::s
  let peek = function
  |[] -> failwith"empty"
  |x::_ -> x
  let pop =function
  |[]->failwith"Empty"
  |_::x -> x
end;;

let a = ListStack.empty;;
(* val a : 'a list = []
 *)
let b = ListStack.push 4 a;;
(* val b : int list = [4]
 *)
let b' = ListStack.push 5 b;;
(* val b' : int list = [5; 4]
 *)
let b'' = ListStack.push 6 b';;
(* val b'' : int list = [6; 5; 4] *)
let p = ListStack.peek b'';;
(* val p : int = 6
 *)
let delete = ListStack.pop b'';;
(* val delete : int list = [5; 4]
 *)
let p' =ListStack.peek delete;;
(* val p' : int = 5 *)
