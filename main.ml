open Printf

let print_header d =
  printf "d,%s\n"
    (String.concat ","
       (Array.to_list (Array.init d (fun i -> sprintf "x%i" (i + 1)))))

let print_vector v =
  let l = Array.to_list v in
  printf "%g,%s\n"
    (Vector.norm v)
    (String.concat "," (List.map (fun x -> sprintf "%g" x) l))

let main () =
  let d = 20 in
  let r1 = 0.9 in
  let r2 = 1. in
  print_header d;
  for i = 1 to 100 do
    print_vector (Shell.pick d r1 r2)
  done

let () = main ()
