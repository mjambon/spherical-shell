type t = float array

let norm2 v = Array.fold_left (fun acc x -> acc +. x *. x) 0. v

let norm v = sqrt (norm2 v)

let dist2 v w =
  let len = Array.length v in
  if len = 0 || Array.length w <> len then
    invalid_arg "Vector.dist2";
  let acc = ref 0. in
  for i = 0 to len - 1 do
    acc := !acc +. (v.(i) -. w.(i)) ** 2.
  done;
  !acc

let dist v w =
  sqrt (dist2 v w)
