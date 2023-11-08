let rec print_table lst = 
  match lst with
  | [] -> ()
  | h :: t -> print_int h; print_string " "; print_table t

let arg_max lst_orig =
  let rec aux lst idx =
    if lst = [] then (List.hd lst_orig, [])
    else 
      let (a, b) = aux (List.tl lst) (idx + 1) in
      if (List.hd lst) = a then (List.hd lst, idx::b)
      else if (List.hd lst) > a then (List.hd lst, idx::[])
      else (a, b)
  in
  if lst_orig = [] then []
  else snd (aux lst_orig 0)

let () =
  let a = arg_max [1;7;2;4;7;0;0]
  in print_table a
