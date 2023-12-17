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

let make_skipponacci_list m num_of_elem =
  let rec make_skipponacci_list m num_of_elem =
    if num_of_elem = 0 then []
    else skipponacci (m, num_of_elem) :: make_skipponacci_list m (num_of_elem-1)
  in List.rev @@ make_skipponacci_list m num_of_elem

let tests_a () =
  print_endline @@ "skipponacci (2, 2) = " ^ string_of_int @@ skipponacci (2, 2);
  print_endline @@ "skipponacci_memoized (2, 2) = " ^ string_of_int @@ skipponacci_memoized (2, 2);
  print_endline @@ "skipponacci (5, 3) = " ^ string_of_int @@ skipponacci (5, 3);
  print_endline @@ "skipponacci_memoized (5, 3) = " ^ string_of_int @@ skipponacci_memoized (5, 3); 
  print_endline @@ "skipponacci_memoized (12, 4) = " ^ string_of_int @@ skipponacci_memoized (12, 4);
  print_endline @@ "list_of_skipponacci 3: ";
  List.iter (fun x -> print_int x; print_string " ") @@ make_skipponacci_list 3 10;
  print_newline
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

let pascal_triangle row =
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
  else if n < 1 then 0
  else
    let sum = ref 0 in
    (* let coeffs = pascal_triangle m in *)
    for i = 0 to (m-1) do
      (* sum := !sum + coeffs.(i) * (fib_imp (i + n)); *)
      sum := !sum + binomial_coefficient (m-1) i * (fib_imp (i + n));
    done;
    !sum

let skip_naive (m, n) =
  fib_imp (2 * (m - 1) + n)

let make_array_of_skip m num_of_elem =
  let arr = Array.make num_of_elem 0 in
  for i = 1 to num_of_elem do
    arr.(i-1) <- skip_imp (m, i);
  done;
  arr

let print_array_of_skip m num_of_elem =
  let arr = make_array_of_skip m num_of_elem in
  Array.iter (fun x -> print_int x; print_string " ") arr;
  print_newline
  ()

let print_list_of_skip m num_of_elem =
  let list = make_skipponacci_list m num_of_elem in
  List.iter (fun x -> print_int x; print_string " ") list;
  print_newline
  ()

let tests_b () =
  print_endline "TESTS B";
  (* print_endline @@ "skip_imp (2, 2) = " ^ string_of_int @@ skip_imp (2, 2);
  print_endline @@ "skip_imp (5, 3) = " ^ string_of_int @@ skip_imp (5, 3);
  print_endline @@ "skip_imp (3, 5) = " ^ string_of_int @@ skip_imp (3, 5);
  print_endline @@ "skip_imp (12, 4) = " ^ string_of_int @@ skip_imp (12, 4);
  print_endline @@ "skip_naive (12, 4) = " ^ string_of_int @@ skip_naive (12, 4);
  print_endline @@ "skip_naive (3, 5) = " ^ string_of_int @@ skip_naive (3, 5);
  print_endline @@ "skip_naive (1, 1) = " ^ string_of_int @@ skip_naive (1, 1);
  print_endline @@ "make_array_of_skip 3: ";
  Array.iter (fun x -> print_int x; print_string " ") @@ make_array_of_skip 3 10; *)
  print_array_of_skip 1 10;
  print_list_of_skip 1 10;
  print_newline
  ()

let () =
  tests_a ();
  tests_b ();
  ()