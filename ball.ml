(*
   Pick random points within a unit ball.
*)

(*
   Pick a random point on the unit ball in a d-dimensional euclidean space.
*)
let pick d =
  let v0 = Sphere.pick d in
  let r = Random.float 1. ** (1. /. float d) in
  Array.map (fun x -> r *. x) v0
