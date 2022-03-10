#load "unix.cma"

let memoize f =
  let ht = Hashtbl.create 1 in
  fun a ->
    match Hashtbl.find_opt ht a with
    | Some b -> b
    | None ->
        let b = f a in
        Hashtbl.add ht a b;
        b

let slow_add2 n =
  Unix.sleep 10;
  n + 2

let slow_add2_memo = memoize slow_add2

let random_from_seed n =
  Random.init n;
  Random.bits ()

let random_memo = memoize random_from_seed

let id : bool -> bool = fun a -> a
let not : bool -> bool = fun a -> if a then false else true
let const_true : bool -> bool = fun _ -> true
let const_false : bool -> bool = fun _ -> false

let print_add2_result =
  print_endline
    ("Add 2 - First call complete - result was: "
    ^ string_of_int (slow_add2_memo 2));
  print_endline
    ("Add 2 - Second call complete - result was: "
    ^ string_of_int (slow_add2_memo 2))

let print_random_result =
  print_endline
    ("Random result with seed of 1: " ^ string_of_int (random_memo 1));
  print_endline
    ("Random result with seed of 2: " ^ string_of_int (random_memo 2));
  print_endline
    ("Random result with seed of 2: " ^ string_of_int (random_memo 2))

let () =
  print_add2_result;
  print_random_result