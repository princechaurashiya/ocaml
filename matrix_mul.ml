
let rec matrix_transpose = function
|[] -> []
|[]::_ -> []
|m -> (List.map List.hd m)::(List.map List.tl m);;

