let rec odds = function
|[] -> []
|h::t -> if h mod 2 = 1 then h::odds t else odds t;;

let rec evens = function
|[]->[]
|h::t -> if h mod 2 =0 then h::evens t else evens t;;



let rec filter p = function
|[]->[]
|h::t -> if p h then h:: filter p t else filter p t;;

let even x = x mod 2 =0 ;;
let odd x = x mod 2 =1 ;;

filter even [1;2;3;4;5;6;7;8];;
let evens' lst = filter even lst;;
let odds' lst = filter odd lst;;

(* this is not a tel recursive  *)


let rec filter_aux p acc = function
|[] -> []
|h::t -> if p h then h::filter_aux p acc t else filter_aux p acc t 

let rec filter' p lst = filter_aux p [] lst;;
let  f_filter lst = filter' even lst ;;

(* now we write a tail recursive function *)
let rec filt p acc = function 
|[]-> acc
|h::t -> filt p ( if p h then h::acc  else  acc )t ;;

(* OR *)

let rec filter_aux' p acc = function 
|[]->[]
|h::t-> if p h then filter_aux' p (h :: acc) t else filter_aux' p acc t
 


let final_f p lst = filter_aux p [] lst;;
let f_odd_filter lst = final_f odd lst;;

f_odd_filter [1;2;3;4;5];;
let f_even_filter lst = final_f even lst;;


let rec filt p acc = function 
|[]-> acc
|h::t -> filt p ( if p h then h::acc  else  acc )t ;;


 (* This is inbuild function 

_______________List.filter____________________ *)



