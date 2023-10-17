module type Fact= sig
  val fact : int-> int
end
module RecFact:Fact = struct
  let rec fact n= 
  if n= 0 then 1 else n* fact (n-1)  
end

module TailRec:Fact = struct
  let rec fact_aux n acc = 
    if n = 0 then acc 
else fact_aux (n-1) (n*acc)
let fact n = fact_aux n 1
end






module type MATH = sig
  (** [fact n] is [n!]. *)
  val fact : int -> int
end

module Math : MATH = struct
  (** [fact_aux n acc] is [n! * acc]. *)
  let rec fact_aux n acc =
    if n = 0 then acc else fact_aux (n - 1) (n * acc)

  let fact n = fact_aux n 1
end