(*
   Pick random points within a unit sphere.
*)

let normalize v =
  let norm = Vector.norm v in
  Array.map (fun x -> x /. norm) v

(*
   Pick a random point on the unit sphere in a d-dimensional euclidean space.
*)
let pick d =
  let v = Array.init d (fun i -> Normal.pick ()) in
  normalize v
