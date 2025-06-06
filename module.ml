module MyList = struct
type 'a mylist = |Nil |Cons of 'a * 'a mylist
let rec map f = function 
|Nil -> Nil
|Cons (h,t) ->  Cons (f h ,map f t)
end
  
let lst = MyList.map succ (Cons (1, Nil));;



module Tree =struct
type 'a tree = Leaf |Node of 'a * 'a tree * 'a tree
let rec map f = function 
|Leaf -> Leaf
|Node (v,l,r)-> Node(f v ,map f l ,map f r)
end

let tr = Tree.map succ (Node(1, Node(5,Leaf,Leaf),Node(2,Leaf,Leaf)));;


