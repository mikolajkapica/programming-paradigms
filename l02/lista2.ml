(* let cutAndMend a b =
    let rec aux current lst =
      match lst with
      | [] -> []
      | h :: t when current >= a && current <= b -> aux (current + 1) t
      | h :: t -> h :: aux (current + 1) t
    in aux 0 *)
 

let cutAndMend a b =
  let rec aux current lst =
    match (lst, a <= current && current <= b) with
    | ([], _) -> []
    | (h :: t, true) -> aux (current + 1) t
    | (h :: t, false) -> h :: aux (current + 1) t
  in aux 0

let cutAndMendTests =
  let test1 = cutAndMend 0 0 [1;2;3;4;5;6;7;8;9;10] = [2;3;4;5;6;7;8;9;10]
  and test2 = cutAndMend 1 1 [1;2;3;4;5;6;7;8;9;10] = [1;3;4;5;6;7;8;9;10]
  and test3 = cutAndMend 2 2 [1;2;3;4;5;6;7;8;9;10] = [1;2;4;5;6;7;8;9;10]
  and test4 = cutAndMend (-1) (-2) [1;2;3] = [1;2;3]
  and test5 = cutAndMend 0 2 [1;2;3] = []
  and test6 = cutAndMend 1 5 [] = [] in
  test1 && test2 && test3 && test4 && test5 && test6

let split2Rec lst =
  let rec aux = function
      | [] -> ([],[])
      | _ :: [] -> ([],[])
      | x :: y :: rest -> 
        let (a, b) = aux rest in
        (x :: a, y :: b)
  in aux lst

let split2RecTests =
  let test1 = split2Rec [1;2;3;4] = ([1;3], [2;4])
  and test2 = split2Rec [1;2;3;4;5] = ([1;3], [2;4])
  and test3 = split2Rec [1;2;3;4;5;6] = ([1;3;5], [2;4;6])
  and test4 = split2Rec [] = ([],[])
  and test5 = split2Rec [1] = ([],[])
  and test6 = split2Rec [1;2] = ([1], [2]) in
  test1 && test2 && test3 && test4 && test5 && test6

let rec list_rev lst = 
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (h :: acc) t
  in aux [] lst
   
let split2Acc lst =
  let rec aux l1 l2 = function
    | [] -> (l1, l2)
    | [x] -> (l1, l2)
    | x :: y :: rest -> aux (x :: l1) (y :: l2) rest
  in 
  let (a, b) = aux [] [] lst in
  (list_rev a, list_rev b)

let split2AccTests =
  let test1 = split2Acc [1;2;3;4] = ([1;3], [2;4])
  and test2 = split2Acc [1;2;3;4;5] = ([1;3], [2;4])
  and test3 = split2Acc [1;2;3;4;5;6] = ([1;3;5], [2;4;6])
  and test4 = split2Acc [] = ([],[])
  and test5 = split2Acc [1] = ([],[])
  and test6 = split2Acc [1;2] = ([1], [2]) in
  test1 && test2 && test3 && test4 && test5 && test6


let split2RecOnHalf lst =
  let rec aux lst n =
    match lst with
    | [] -> (([], []), n)
    | h :: t -> 
      let ((lst1, lst2), lst_len) = aux t (n + 1) in
      match n < lst_len / 2, n < (lst_len / 2) * 2 with
      | true, _ -> (h :: lst1, lst2), lst_len
      | _, true -> (lst1, h :: lst2), lst_len
      | _ -> ((lst1, lst2), lst_len)
    in aux lst 0 |> fst

let split2RecOnHalfTests =
  let test1 = split2RecOnHalf [1;2;3;4] = ([1;2], [3;4])
  and test2 = split2RecOnHalf [1;2;3;4;5] = ([1;2], [3;4])
  and test3 = split2RecOnHalf [1;2;3;4;5;6] = ([1;2;3], [4;5;6])
  and test4 = split2RecOnHalf [] = ([],[])
  and test5 = split2RecOnHalf [1] = ([],[])
  and test6 = split2RecOnHalf [1;2] = ([1], [2]) in
  test1 && test2 && test3 && test4 && test5 && test6

let rec list_len = function
  | [] -> 0
  | h :: t -> 1 + list_len t

let split2AccOnHalf lst =
  let len = list_len lst in
  let rec aux lst acc1 acc2 n =
    match lst with
    | [] -> (acc1, acc2)
    | h :: t -> 
      match (n < len / 2, n < (len / 2) * 2) with
      | true, _ -> aux t (h :: acc1) acc2 (n + 1)
      | _, true -> aux t acc1 (h :: acc2) (n + 1)
      | _ -> (acc1, acc2)
    in 
    let (a, b) = aux lst [] [] 0
    in (list_rev a, list_rev b)

let split2AccOnHalfTests =
  let test1 = split2AccOnHalf [1;2;3;4] = ([1;2], [3;4])
  and test2 = split2AccOnHalf [1;2;3;4;5] = ([1;2], [3;4])
  and test3 = split2AccOnHalf [1;2;3;4;5;6] = ([1;2;3], [4;5;6])
  and test4 = split2AccOnHalf [] = ([],[])
  and test5 = split2AccOnHalf [1] = ([],[])
  and test6 = split2AccOnHalf [1;2] = ([1], [2]) in
  test1 && test2 && test3 && test4 && test5 && test6

let () =
  Printf.printf "cutAndMendTests: %b\n" cutAndMendTests;
  Printf.printf "split2RecTests: %b\n" split2RecTests;
  Printf.printf "split2AccTests: %b\n" split2AccTests;
  Printf.printf "split2RecOnHalfTests: %b\n" split2RecOnHalfTests;
  Printf.printf "split2AccOnHalfTests: %b\n" split2AccOnHalfTests;
  ()