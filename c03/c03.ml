let insertion_sort list =
  let rec insert x = function
    | [] -> [x]
    | h :: t -> if x <= h then x :: h :: t else h :: insert x t
  in
  let rec aux = function
    | [] -> []
    | h :: t -> insert h (aux t)
  in
  aux list

let rec merge_sort = function
  | [] -> []
  | [x] -> [x]
  | list ->
      let rec split left right = function
        | [] -> (left, right)
        | [x] -> (x :: left, right)
        | x :: y :: t -> split (x :: left) (y :: right) t
      in
      let rec merge left right =
        match (left, right) with
        | ([], _) -> right
        | (_, []) -> left
        | (x :: xs, y :: ys) ->
            if x <= y then x :: merge xs right else y :: merge left ys
      in
      let (left, right) = split [] [] list in
      merge (merge_sort left) (merge_sort right)
