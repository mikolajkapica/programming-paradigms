type 'a llist = LNil | LCons of 'a * 'a llist Lazy.t

let prepend element llist n =
  let rec prepend_aux llist n =
    match n with
    | 0 -> llist
    | n when n < 0 -> failwith "n must be positive"
    | _ -> prepend_aux (LCons (element, lazy llist)) (n - 1)
  in
  prepend_aux llist n

let reverse llist = 
  let rec aux llist new_list =
    match llist with
    | LNil -> new_list
    | LCons (element, lazy llist) -> aux llist (LCons (element, lazy new_list))
  in aux llist LNil

let lrepeat n llist =
  let rec aux llist new_list =
    match llist with
    | LNil -> new_list
    | LCons (element, lazy llist) ->
      aux llist (prepend element new_list n)
  in 
  reverse (aux llist LNil)

let rec print_llist llist =
  match llist with
  | LNil -> print_string "\n"
  | LCons (element, lazy llist) ->
    print_int element;
    print_string " ";
    print_llist llist

type 'a lBT = LEmpty | LNode of 'a * (unit ->'a lBT) * (unit -> 'a lBT)


let take n llist =
  let rec aux n llist new_list =
    match n with
    | 0 -> new_list
    | n when n < 0 -> failwith "n must be positive"
    | _ -> 
      match llist with
      | LNil -> new_list
      | LCons (element, lazy llist) -> aux (n - 1) llist (element :: new_list)
  in
  List.rev (aux n llist [])

let ibreadth (tree: 'a lBT): ('a llist) =
  let rec aux queue =
    match queue with
    | [] -> LNil
    | LEmpty :: queue -> aux queue
    | LNode (element, left, right) :: queue ->
      LCons (element, lazy (aux (queue @ [left (); right ()])))
  in
  aux [tree]



let rec itree n =
  LNode (n, (fun x -> (itree (2 * n))), (fun x -> (itree (2 * n + 1))))

let () = 
  let llist = LCons (1, lazy (LCons (2, lazy (LCons (3, lazy LNil))))) in
  print_llist (lrepeat 3 llist);
  let a = ibreadth (LNode (1, (fun x -> LNode (2, (fun x -> LEmpty), (fun x -> LEmpty))), (fun x -> (LNode (3, (fun x -> LEmpty), (fun x -> LEmpty))))))
  in print_string "ibreadth: ";
  print_llist a;
