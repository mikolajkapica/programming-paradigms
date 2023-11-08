(* tail recursive *)
(* let (>>) f n =
  let rec aux g n lst x =
    print_endline (string_of_int n);
    if n <= 0 then lst
    else aux (fun x -> g @@ f x) (n-1) (g x :: lst) x
  in
  aux f n [] *)


(* non tail recursive *)
let (>>) f n =
  let rec aux f n x =
    match n with
    | 0 -> (x, x :: [])
    | _ -> (
      let back = aux f (n-1) x in
      (f @@ fst back, (f @@ fst back) :: snd back)
    )
  in aux f n 

  (*??*)

  

  





