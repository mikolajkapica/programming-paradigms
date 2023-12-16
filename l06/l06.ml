(* 
Lista 6 
Efekty obliczeniowe. Programowanie imperatywne.
*)

(* a *)

let rec fib = function
  | n when n < 1 -> failwith "fib n: n < 1"
  | n when n < 3 -> 1
  | n -> fib (n-1) + fib (n-2)

let fib_memoized =
  let cache = Hashtbl.create 100 in
  let rec fib = function
    | n when n < 1 -> failwith "fib n: n < 1"
    | n when n < 3 -> 1
    | n ->
      try Hashtbl.find cache n
      with Not_found ->
        let result = fib (n-1) + fib (n-2) in
        Hashtbl.add cache n result;
        result
  in fib

let rec skipponacci = function
  | (1, n) -> fib n
  | (m, n) -> skipponacci (m-1, n) + skipponacci (m-1, n+1)

let skipponacci_memoized =
  let cache = Hashtbl.create 100 in
  let rec skipponacci = function
    | (1, n) -> fib_memoized n
    | (m, n) ->
      try Hashtbl.find cache (m, n)
      with Not_found ->
        let result = skipponacci (m-1, n) + skipponacci (m-1, n+1) in
        Hashtbl.add cache (m, n) result;
        result
  in skipponacci

let tests_a () =
  print_endline @@ "skipponacci (2, 2) = " ^ string_of_int @@ skipponacci (2, 2);
  print_endline @@ "skipponacci_memoized (2, 2) = " ^ string_of_int @@ skipponacci_memoized (2, 2);
  print_endline @@ "skipponacci (5, 3) = " ^ string_of_int @@ skipponacci (5, 3);
  print_endline @@ "skipponacci_memoized (5, 3) = " ^ string_of_int @@ skipponacci_memoized (5, 3); 
  print_endline @@ "skipponacci_memoized (12, 4) = " ^ string_of_int @@ skipponacci_memoized (12, 4);
  ()


(* b *)
(* imperatively *)
let fib_imp n =
  if n = 1 || n = 2 then 1 
  else 
    let a = ref 1 in
    let b = ref 1 in
    let c = ref 0 in
    for i = 3 to n do
      c := !a + !b;
      a := !b;
      b := !c;
    done;
    !c

let fib_triangle row =
  let triangle = Array.make_matrix row row 0 in
  for i = 0 to row-1 do
    triangle.(i).(0) <- 1;
    triangle.(i).(i) <- 1;
  done;
  for i = 2 to row-1 do
    for j = 1 to i-1 do
      triangle.(i).(j) <- triangle.(i-1).(j-1) + triangle.(i-1).(j);
    done;
  done;
  triangle.(row-1)


let power n =
  let res = ref 1 in
  for i = 2 to n do
    res := !res * i;
  done;
  !res

let binomial_coefficient n k = 
  power n / (power k * power (n-k))
  

let skip_imp (m, n) =
  if m = 1 then fib_imp n
  else
    let sum = ref 0 in
    (* let coeffs = fib_triangle m in *)
    for i = 0 to (m-1) do
      (* sum := !sum + coeffs.(i) * (fib_imp (i + n)); *)
      sum := !sum + binomial_coefficient (m-1) i * (fib_imp (i + n));
    done;
    !sum

let tests_b () =
  print_endline @@ "skip_imp (2, 2) = " ^ string_of_int @@ skip_imp (2, 2);
  print_endline @@ "skip_imp (5, 3) = " ^ string_of_int @@ skip_imp (5, 3);
  print_endline @@ "skip_imp (12, 4) = " ^ string_of_int @@ skip_imp (12, 4);
  ()

let () =
  tests_a ();
  tests_b ();
  ()