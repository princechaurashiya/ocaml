let safeDiv s y =
   try s/y with
   |Division_by_zero -> 0

   let p a b =
      try a /b with
      | Division_by_zero ->0
let mat lst =List.hd lst


  match List.hd with
  | [] -> "empty"
  | _ :: _ -> "nonempty"
  | exception (Failure s) -> s


  match List.hd [4;7;] with
  | [] -> "empty"
  | _ :: _ -> "nonempty"
  | exception (Failure s) -> s