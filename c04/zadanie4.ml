type 'a bt = Empty | Node of 'a * 'a bt * 'a bt;;

let tt = Node(1, Node(2, Node(4, Empty, Empty ), Empty ), Node(3,Node(5, Empty, Node(6, Empty, Empty ) ), Empty ) );; 

(*liczenie ścieżki zewnętrznej, licznie ile do każdego Empty jest Node'ów*)

let outer_path tree = 
  let rec outer_helper tree amount =
    let amount = amount+1 in
    match tree with
    |Empty -> amount
    |Node(_, n1,n2) -> outer_helper n1 amount + outer_helper n2 amount
  in outer_helper tree 0
 ;;

(*liczenie ścieżki wewnętrznej ile do każdego węzła jest node'ów*)

let inter_path tree = 
  let rec inter_help tree amount =
    match tree with
    | Empty -> 0 
    | Node(_, n1, n2) -> (inter_help n1 (amount + 1)) + (inter_help n2 (amount + 1)) + amount in
  
  inter_help tree 0