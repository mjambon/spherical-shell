(*
   Pick random points with a spherical shell.
*)

(*
   Pick a point within spherical shell of radii r1 and r2,
   in a d-dimensional euclidean space.
*)
let pick d r1 r2 =
  if d < 1 then
    invalid_arg "Shell.pick";
  if not (r1 >= 0. && r2 > r1 && r2 -. r1 < infinity) then
    invalid_arg "Shell.pick";
  let v0 = Sphere.pick d in
  let d' = float d in
  let r1_d = r1 ** d' in
  let r2_d = r2 ** d' in
  let r = (Random.float (r2_d -. r1_d) +. r1_d) ** (1. /. d') in
  Array.map (fun x -> r *. x) v0
