let reverse4 (x1, x2, x3, x4) = (x4, x3, x2, x1)

let reverse4_tests =
  let test1 = (reverse4 (1, 2, 3, 4) = (4, 3, 2, 1)) = true in
  let test2 = (reverse4 (1, 2, 3, 4) = (1, 2, 3, 4)) = false in
  test1 && test2

let sumProd s e = 
  if (s > e) then raise (Invalid_argument "s > e") else
  if (e = 0) then (0, 0) else
  let rec aux current (sum, prod) = 
    if current > e then (sum, prod)
    else aux (current + 1) (sum + current, prod * current)
  in
  aux s (0, 1)

let sumProd_tests =
  let test1 = sumProd 1 5 = (15, 120)
  and test2 = sumProd 1 1 = (1, 1)
  and test3 = sumProd 1 10 = (55, 3628800)
  and test4 = sumProd (-4) 10 = (-4-3-2-1+0+1+2+3+4+5+6+7+8+9+10, 0)
  and test5 = sumProd 3 3 = (3, 3)
  and test6 = sumProd 0 0 = (0, 0) 
  in
  test1 && test2 && test3 && test4 && test5 && test6

let isPerfect n = 
  if (n < 1) then false else
  let divisors = 
    let rec aux current divisors = 
      if current = 0 then divisors
      else if n mod current = 0 then aux (current - 1) (current :: divisors)
      else aux (current - 1) divisors
    in
    aux (n - 1) []
  in
  let divisors_sum = List.fold_left (+) 0 divisors in
  divisors_sum = n


let isPerfect_tests = 
  let test1 = isPerfect 6 = true
  and test2 = isPerfect 28 = true
  and test3 = isPerfect 496 = true
  and test4 = isPerfect 7 = false
  and test5 = isPerfect 12 = false in
  test1 && test2 && test3 && test4 && test5


  (* insert without List.rev, and List.rev_append *)
let insert lst x pos = 
  let rec aux lst current =
    match lst with
    | [] -> [x]
    | hd :: tl ->
      if current = pos then x :: lst
      else hd :: aux tl (current + 1)
  in
  if pos < 0 then x :: lst 
  else aux lst 0

let insert_tests =
  let test1 = insert [1; 2; 3; 4] 0 2 = [1; 2; 0; 3; 4]
  and test2 = insert [1; 2; 3; 4] 0 0 = [0; 1; 2; 3; 4]
  and test3 = insert [1; 2; 3; 4] 0 4 = [1; 2; 3; 4; 0]
  and test4 = insert [1; 2; 3; 4] 0 5 = [1; 2; 3; 4; 0]
  and test5 = insert [1; 2; 3; 4] 0 (-1) = [0; 1; 2; 3; 4] in
  test1 && test2 && test3 && test4 && test5