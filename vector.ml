type t = float array

let norm2 v = Array.fold_left (fun acc x -> acc +. x *. x) 0. v

let norm v = sqrt (norm2 v)
