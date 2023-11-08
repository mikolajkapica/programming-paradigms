module Zadanie1Easy = struct
  let flatten1 = List.fold_left (fun acc xs -> acc @ xs) []
end

module Zadanie2 = struct 
  (* let count_num n = List.fold_left (fun acc x -> if x = n then acc + 1 else acc) 0 *)
  let count_num e lst =
    let rec aux acc lst =
      if lst = [] then acc
      else if e = (List.hd lst) then aux (acc + 1) (List.tl lst) 
      else aux acc (List.tl lst)
    in aux 0 lst
end;;

module Zadanie3 = struct 
  let replicate str n =
    let rec aux acc n =
      if n = 0 then acc else aux (str :: acc) (n - 1)
    in
    if n < 0 then failwith "n < 0"
    else
    aux [] n

  (* let replicate str n = List.init n (fun _ -> str) *)
end;;

module Zadanie4 = struct 
  let sqrList xs = List.map (fun x -> x * x) xs
end;;

module Zadanie5 = struct 
  let palindrome xs = xs = List.rev xs
end;;

module Zadanie6 = struct 
  let list_length = List.fold_left (fun acc _ -> acc + 1) 0
end;;
    
module Zadanie7 = struct 
  let logb2 = 
    let rec aux acc n = 
      if n < 2 then acc else aux (acc + 1) (n / 2)
    in aux 0

  let rec solve = function
    | 1 -> 1
    | n -> solve @@ logb2 n + solve n / 2
end;;

module Zadanie1 = struct 
  let flatten_two_lists xs ys =
    let rec aux xs ys =
      match xs with
      | [] -> ys
      | h :: t -> aux t (h :: ys)
    in
    aux (List.rev xs) ys

  (* 1 opcja *)
  let flatten1 xss =
    let rec aux lists acc =
      match lists with
      | [] -> acc
      | h :: t -> aux t (flatten_two_lists h acc)
    in
    aux (List.rev xss) []

  (* 2 opcja *)
  let flatten1 xss =
    List.fold_left (fun acc xs -> flatten_two_lists xs acc) [] (List.rev xss)
end;;
