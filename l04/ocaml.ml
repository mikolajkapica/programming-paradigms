type tree = Node of string * tree list

let rec bfs_tree start_node goal_node tree =
  let rec bfs_aux queue visited =
    match queue with
    | [] -> []  (* No path found *)
    | (current_node, current_path) :: rest ->
      if current_node = goal_node then
        List.rev (current_node :: current_path)  (* Goal node found, return the path *)
      else if List.mem current_node visited then
        bfs_aux rest visited  (* Node already visited, continue with the next one *)
      else
        match current_node with
        | Node (value, children) ->
          let new_paths = List.map (fun child -> (child, child :: current_path)) children in
          let new_queue = rest @ new_paths in
          let new_visited = current_node :: visited in
          bfs_aux new_queue new_visited
  in
  bfs_aux [(start_node, [start_node])] []

(* Example usage *)
let example_tree =
  Node ("A", [
    Node ("B", [
      Node ("D", []);
      Node ("E", []);
    ]);
    Node ("C", [
      Node ("F", []);
    ]);
  ])

let start_node = Node ("A", [])
let goal_node = Node ("F", [])

let result_path = bfs_tree start_node goal_node example_tree

(* Print the result *)
let () =
  match result_path with
  | [] -> print_endline "No path found."
  | path ->
    let path_str = String.concat " -> " (List.map (fun (Node (value, _)) -> value) path) in
    Printf.printf "Path found: %s\n" path_str
