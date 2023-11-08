let argmax lst = 
  let rec aux lst current_idx max_num indecies =
    if lst = [] then List.rev indecies
    else if List.hd lst > max_num then aux (List.tl lst) (current_idx + 1) (List.hd lst) (current_idx :: [])
    else if List.hd lst = max_num then aux (List.tl lst) (current_idx + 1) max_num (current_idx :: indecies)
    else aux (List.tl lst) (current_idx + 1) max_num indecies
  in
  if lst = [] then [] 
  else aux lst 0 (List.hd lst) []

(* let argmax lst = 
  let rec aux lst current_idx max_num =
    if lst = [] then []
    else if List.hd lst > max_num then aux (List.tl lst) (current_idx + 1) (List.hd lst)
    else if List.hd lst = max_num then current_idx :: aux (List.tl lst) (current_idx + 1) max_num
    else aux (List.tl lst) (current_idx + 1) max_num
  in
  if lst = [] then [] 
  else aux lst 0 (List.hd lst) *)

let tests =
  let test1 = ((argmax [7;2;1;3;7;0]) = [0;4])
  and test2 = ((argmax [-10;-5;-4;-2;-3]) = [3])
  and test3 = ((argmax []) = [])
  and test4 = ((argmax [-10]) = [0])
  and test5 = ((argmax [10]) = [0])
  and test6 = ((argmax [7;7;7]) = [0;1;2]) in
  test1 && test2 && test3 && test4 && test5 && test6
  
let () =
  Printf.printf "tests: %b\n" tests