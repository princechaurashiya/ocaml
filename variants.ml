type primary_colour = Red | Green |Blue
let r = Red
type point = float * float 

type shape =
| Circle of { center : point ; radius : float}
| Rectangle of {lower_left : point ; upper_right :point}
|Point of point

let c1 = Circle {center = (0.,0.) ;radius = 1.}
let r1 = Rectangle { lower_left = (1.,1.) ; upper_right = (2.,2.)}
let p1 = Point (31. ,10.)

let avg a b =  
  (a+.b)/. 2. 

  let center1 s = 
    match s with 
    | Circle {center ; radius } -> center

     (* | Rectangle {lower_left ; upper_right} -> 
      (* failwith "TODO";;  *)

      let (xl , yl) = lower_left in 
      let (xr , yr) = upper_right in 
      (avg xl yr , avg xl yr);; 
        OR   *)

      |Rectangle { lower_left = (xl , yl) ;  upper_right = (xr , yr) } ->
         avg xl xr ,avg yl yr
      | Point  p-> p;;
