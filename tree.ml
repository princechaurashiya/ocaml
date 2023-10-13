type 'a tree =
|Leaf
| Node of 'a * 'a tree * 'a tree



let t =
  Node(5,
  Node (4,
  Node(2,Leaf,Leaf),
  Node (1,Leaf,Leaf)
  ),
  Node(3,
  Node(6,Leaf,Leaf),
  Node(7,Leaf,Leaf)
  )
  )

  type 'a tree = 
  |Leaf
  |Node of 'a node 

  and 'a node ={
    value:'a;
     left:'a tree;
    right:'a tree
  }

    let tr =
      Node{
        value = 2;
        left = Node {value=5; left = Leaf ; right= Leaf};
        right= Node {value =3; left =Leaf; right = Leaf}
      }

      (* represents
      2
     / \
    1   3  *)
(* let t =
  Node {
    value = 2;
    left = Node {value = 1; left = Leaf; right = Leaf};
    right = Node {value = 3; left = Leaf; right = Leaf}
  } *)

type 'a tree = 
|Leaf 
|Node of 'a * 'a tree *'a tree;;
 
let b =
  Node(5,
    Node (4,
    Node(1,Leaf,Leaf),
    Node(2, Leaf , Leaf)
     ),
     Node(10,
     Node(3,Leaf,Leaf),
     Node(6,Leaf,Leaf)
     )
     );;




     type 'a tree =
     |Leaf
     |Node of 'a node
     and 'a node ={  
      value:'a;
     right : 'a tree;
     left :'a tree;}

     let t =
      Node {
        value = 2;
        left = Node {value = 1; left = Leaf; right = Leaf};
        right = Node {value = 3; left = Leaf; right = Leaf}
      };;  
      




      

      type nat = 
      Zero
      |Succ of nat
      let rec even = function |Zero -> true | Succ m -> odd m
      and odd = function |Zero -> false | Succ m -> even m