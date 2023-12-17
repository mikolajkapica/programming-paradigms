(* 
Lista 6 
Efekty obliczeniowe. Programowanie imperatywne.
*)

(* a *)

let rec fib = function
  | n when n < 1 -> failwith "fib n: n < 1"
  | n when n < 3 -> 1
  | n -> fib (n-1) + fib (n-2)

let rec skip = function
  | (m, n) when m < 1 || n < 1 -> failwith "skipponacci (m, n): m < 1 || n < 1"
  | (1, n) -> fib n
  | (m, n) -> skip (m-1, n) + skip (m-1, n+1)

let make_skipponacci_list m num_of_elem = 
  let rec aux acc last_two = function
    | 0 -> acc 
    | n -> match last_two with
      | (a, b) -> aux (a::acc) (b, a+b) (n-1)
  in
  let fst_e = skip (m, 1) in
  let snd_e = skip (m, 2) in
  List.rev @@ aux [] (fst_e, snd_e) num_of_elem


(* b *)

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

let skip_imp (m, n) = 
  fib_imp (2 * (m - 1) + n)

(* 
  PROOF
    Let's guess the formula of skipponacci (m, n) = fib (2 * (m - 1) + n)
  Base case: 
    skipponacci(1,n) = fib (2 * (m - 1) + n)
  Induction step:
    IH: skipponacci (m, n) = fib (2 * (m - 1) + n)
    Then: skipponacci (m+1, n) = fib (2 * (m - 1) + n + 2)
    skipponacci (m+1, n) = skipponacci (m, n) + skipponacci (m, n+1)
                        = fib (2 * (m - 1) + n) + fib (2 * (m - 1) + n + 1)
                        [knowing that fib n + fib (n+1) = fib (n+2)]
                        = fib (2 * (m - 1) + n + 2)
  QED
*)


let make_array_of_skip m num_of_elem =
  let arr = Array.make num_of_elem 0 in
  for i = 1 to num_of_elem do
    arr.(i-1) <- skip_imp (m, i);
  done;
  arr

(* TESTS *)
let tests = 
  let test1 = make_skipponacci_list 1 10 = [1;1;2;3;5;8;13;21;34;55] in
  let test2 = make_array_of_skip    1 10 = [|1;1;2;3;5;8;13;21;34;55|] in
  let test3 = make_skipponacci_list 2 10 = [2;3;5;8;13;21;34;55;89;144] in
  let test4 = make_array_of_skip    2 10 = [|2;3;5;8;13;21;34;55;89;144|] in
  let test5 = make_skipponacci_list 3 10 = [5;8;13;21;34;55;89;144;233;377] in
  let test6 = make_array_of_skip    3 10 = [|5;8;13;21;34;55;89;144;233;377|] in
  let test7 = make_skipponacci_list 4 10 = [13;21;34;55;89;144;233;377;610;987] in
  let test8 = make_array_of_skip    4 10 = [|13;21;34;55;89;144;233;377;610;987|] in
  test1 && test2 && test3 && test4 && test5 && test6 && test7 && test8