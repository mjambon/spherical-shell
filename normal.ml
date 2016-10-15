(*
   Box–Muller transform

   See https://en.wikipedia.org/wiki/Box%E2%80%93Muller_transform
*)

(*
   Return a pair of independent random numbers following
   the standard normal distribution (mean = 0, stdev = 1).
*)
let rec pick_pair () =
  let u = Random.float 2. -. 1. in
  let v = Random.float 2. -. 1. in
  let s = u *. u +. v *. v in
  if s = 0. || s >= 1. then
    pick_pair ()
  else
    let z0 = u *. sqrt (-. 2. *. log s /. s) in
    let z1 = v *. sqrt (-. 2. *. log s /. s) in
    z0, z1

(*
   Return a single sample from the standard normal distribution.
*)
let pick () =
  fst (pick_pair ())
