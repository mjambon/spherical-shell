(*
   Pick random points with a spherical shell.
*)

open Printf

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

(*
   Pick as many points as possible within a spherical shell,
   such that no two points are closer than a certain distance.
*)
let pick_excl ~dim ~r1 ~r2 ~excl_radius =
  let rec loop num_attempts num_rejected_since_last_pick acc =
    eprintf "%i %i %i\n%!"
      (List.length acc) num_attempts num_rejected_since_last_pick;
    if num_rejected_since_last_pick > max 30 (num_attempts / 2) then
      acc
    else
      let v = pick dim r1 r2 in
      if List.for_all (fun w ->
          Vector.dist v w >= excl_radius
        ) acc
      then
        loop (num_attempts + 1) 0 (v :: acc)
      else
        loop (num_attempts + 1) (num_rejected_since_last_pick + 1) acc
  in
  loop 0 0 []
