[| 3 |];;

[| 1; 2; 3|];;

let a = [| 1 |];;

a.(0);;

a.(0) <- 5;;
a.(0);;



let vec_print v = 
  for i = 0 to Array.length v - 1 do 
    print_float v.(i); print_newline ()
  done;;

  let v = [|1.;0.|];;

  let vec_print' v=
    let print_elt n = 
      print_float n; print_newline ()
    in Array.iter print_elt v;;

    let vec_print'' v = 
      Array.iter (Printf.printf "%f\n") v

      (*
        [vec_add v1 v2 ] is the sum of vectors [v1] and [v2].contents
        Example: [vec_add [|1.;2.|] [|3.;4.|]]
        is [| 4.;6.|]   
      *)
       let vec_add v1 v2 = 
        let len1, len2 = Array.length v1 , Array.length v2 in
        if len1 <> len2 then invalid_arg "different lengths" else
          let v3 = Array.make len1 0. in
          for i = 0 to len1 -1 do
            v3.(i) <- v1.(i) +. v2.(i)
          done


          let vec_add' v1 v2 = 
            let len1, len2 = Array.length v1, Array.length v2 in
            if len1 <> len2 then invalid_arg "different lengths" else
              let elt i = v1.(i) +. v2.(i) in
              Array.init len1 elt


            let vec_add'' v1 v2 =
              Array.map2 ( +. ) v1 v2