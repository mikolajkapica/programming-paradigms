let (>>) f n =
  fun x ->
  let rec aux g n lst =
    if n = 0 then lst
    else aux (fun x -> g @@ f x) (n-1) (g x :: lst)
  in
  if n <= 0 then []
  else aux f (n-1) [x]

let (>>^) f n =
  fun x ->
  let rec aux f n =
    if n = 0 then (x, x :: [])
    else 
      let back = aux f (n-1) in
      (f @@ fst back, (f @@ fst back) :: snd back)
  in 
  if n <= 0 then []
  else snd @@ aux f (n-1)


let print_list_ints = List.iter (fun x -> print_endline @@ string_of_int x)
let print_list_strings = List.iter (fun x -> print_endline x)



(* modyfikacja *)

let (>=>) f n =
  fun x ->
  let rec aux f n =
    if n = 0 then (x, x - 1 :: [])
    else 
      let back = aux f (n-1) in
      (f @@ fst back - 1, (f @@ fst back - 1) :: snd back)
  in 
  if n <= 0 then []
  else snd @@ aux f (n-1)

let () =
  let succ = fun x -> x + 1 in
  let test1 = (succ >> 5) 5 = [9;8;7;6;5] in
  let test2 = (succ >>^ 5) 5 = [9;8;7;6;5] in

  let test3 = (succ >> -4) 5 = [] in
  let test4 = (succ >>^ 0) 5 = [] in

  let exclaim = fun x -> x ^ "!" in
  let test5 = (exclaim >> 5) "ha" = ["ha!!!!"; "ha!!!"; "ha!!"; "ha!"; "ha"] in
  let test6 = (exclaim >>^ 5) "ha" = ["ha!!!!"; "ha!!!"; "ha!!"; "ha!"; "ha"] in

  let test7 = (exclaim >> -1) "ha" = [] in
  let test8 = (exclaim >>^ 0) "ha" = [] in

  print_endline @@ string_of_bool test1;
  print_endline @@ string_of_bool test2;
  print_endline @@ string_of_bool test3;
  print_endline @@ string_of_bool test4;
  print_endline @@ string_of_bool test5;
  print_endline @@ string_of_bool test6;
  print_endline @@ string_of_bool test7;
  print_endline @@ string_of_bool test8;

  print_endline "modyfikacja";
  let test1 = ((fun x -> x + 1) >=> 5) 5 = [5; 5; 5; 5; 4] in
  let test2 = ((fun x -> x * x) >=> 5) 5 = [2517530625; 50176; 225; 16; 4] in
  let test3 = ((fun x -> x + x) >=>  0) 0 = [] in
  let test4 = ((fun x -> x + x) >=>  5) 1 = [-14; -6; -2; 0; 0] in
  print_endline @@ string_of_bool test1;
  print_endline @@ string_of_bool test2;
  print_endline @@ string_of_bool test3;
  print_endline @@ string_of_bool test4;