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
  Random.self_init ();
  let dim = 100 in
  let r1 = 0.95 in
  let r2 = 1. in
  let excl_radius = sqrt 2. *. r2 in
  let points = Shell.pick_excl ~dim ~r1 ~r2 ~excl_radius in
  print_header dim;
  List.iter print_vector points;
  flush stdout;

  eprintf "\
Number of dimensions: %i
r1 = %g
r2 = %g
Exclusion radius: %g
Number of points generated: %i\n%!"
    dim
    r1
    r2
    excl_radius
    (List.length points)

let () = main ()
