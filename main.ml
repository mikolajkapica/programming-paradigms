module Zadanie1 = struct 
  let flattenTwoLists (xs: 'a list) (ys: 'a list): 'a list =
    let rec aux xs2 ys2 =
      match xs2 with
      | [] -> ys2
      | h :: t -> aux t (h :: ys2)
    in
    aux (List.rev xs) ys

  (* 1 opcja *)
  let flatten1 xss =
    let rec aux lists acc =
      match lists with
      | [] -> acc
      | h :: t -> aux t (flattenTwoLists h acc)
    in
    aux (List.rev xss) []

  (* 2 opcja *)
  let flatten1 xss =
    List.fold_left (fun acc xs -> flattenTwoLists xs acc) [] (List.rev xss)
end;;

module Zadanie2 = struct 
  let count_num n = List.fold_left (fun acc x -> if x = n then acc + 1 else acc) 0
end;;

    

(*zadanie 6*)
(* let rec list_length = function
  | [] -> 0
  | _ :: t -> 1 + list_length t *)


(* let () =
  let a = list_length [1; 2; 3; 4; 5] in
  print_string (string_of_int a) *)

let () =
  let l = Zadanie1.flattenTwoLists [1;2;3] [4;5;6] in
  List.iter (fun e -> print_string (string_of_int e)) l;
  print_endline "";
  
  let l = Zadanie2.count_num 1 [1;2;3;1;1;1;1;1;1;1;1;1;1;1;1;1;1] in
  print_string (string_of_int l);
  print_endline "";
