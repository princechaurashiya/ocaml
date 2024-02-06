

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
     (* or *)
     let t  = 
     Node(4,
        Node(5,
            Node(6,
                 Node(1,Leaf,Leaf),
                  Node(1,Leaf,Leaf)),
            Node(7,
                 Node(1,Leaf,Leaf),
                 Node(1,Leaf,Leaf))
             ),
        Node(6,
           Node(8,
             Node(1,Leaf,Leaf),
             Node(1,Leaf,Leaf)),
           Node(9,
              Node(1,Leaf,Leaf),
              Node(1,Leaf,Leaf))));;

      (*
          3.11.2. Representation with Records   
      *)
     type 'a tree =
     |Leaf
     |Node of 'a node
     and 'a node ={  
      value:'a;
     right : 'a tree;
     left :'a tree};;

     let t =
      Node {
        value = 2;
        left = Node {value = 1; left = Leaf; right = Leaf};
        right = Node {value = 3; left = Leaf; right = Leaf}
      };;

      (*or*)
         let tre = 
                Node{
                   value =8;
             
               left= Node{ 
                value = 2;
                left = Node { value = 2; left = Leaf; right = Leaf};
                right = Node { value = 6; left = Leaf; right = Leaf}
                 };
                right = Node {
                value =9;
                left = Node { value = 1; left = Leaf; right = Leaf};
                right = Node { value = 8 ; left = Leaf;right = Leaf} } };;
      




      

      type nat = 
      Zero
      |Succ of nat
      let rec even = function |Zero -> true | Succ m -> odd m
      and odd = function |Zero -> false | Succ m -> even m;;



    
    
                