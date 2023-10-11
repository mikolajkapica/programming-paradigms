  let flatten1 = List.fold_left (fun acc xs -> acc @ xs) []
  let count_num n = List.fold_left (fun acc x -> if x = n then acc + 1 else acc) 0
  let replicate str n = List.init n (fun _ -> str)
  let sqrList = List.map (fun x -> x * x)
  let palindrome xs = xs = List.rev xs
  let list_length = List.fold_left (fun acc _ -> acc + 1) 0
  let rec solve = 
    let logb2 = 
      let rec aux acc n = 
        if n < 2 then acc else aux (acc + 1) (n / 2)
      in aux 0
    in function
    | 1 -> 1
    | n -> solve @@ logb2 n + solve n / 2