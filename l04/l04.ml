module Utilities = struct 
  let string_remove_last str =
    String.sub str 0 (String.length str - 1)

  let list_map f lst = 
    match lst with
    | [] -> []
    | x :: xs -> f x :: List.map f xs

  let rec list_filter f lst =
    match lst with
    | [] -> []
    | x :: xs -> if f x then x :: list_filter f xs else list_filter f xs

  let rec list_fold_left f acc lst =
    match lst with
    | [] -> acc
    | x :: xs -> list_fold_left f (f acc x) xs

  let list_rev lst =
    let rec aux acc = function
      | [] -> acc
      | x :: xs -> aux (x :: acc) xs
    in aux [] lst


  module Queue = struct
    (*a queue [1;2;3;4;5] is represented by
        front [1;2]
        back  [5;4;3]
        queue = front * back
    *)
    type 'a queue = 'a list * 'a list

    let empty = ([], [])

    let rec enqueue_lst queue lst =
      match queue, lst with
      | (front, back), [] -> (front, back)
      | (front, back), h::t -> enqueue_lst (front, h :: back) t

    let enqueue queue e =
      match queue with
      | front, back -> (front, e :: back)

    let dequeue queue =
      match queue with
      | x :: front, back -> (front, back)
      | [], back -> match list_rev back with
                    | [] -> ([], [])
                    | h :: t -> (t, [])
    
    let peek queue =
      match queue with
      | [], [] -> None
      | [], back -> Some (List.hd (list_rev back))
      | h :: t, _ -> Some h
    
    let to_string queue =
      match queue with
      | front, back -> list_fold_left (fun acc x -> acc ^ " " ^ x) "" front ^ List.fold_left (fun acc x -> acc ^ " " ^ x) "" (List.rev back)
  end
end

open Utilities

type 'a tree3 = Empty | Node of 'a * 'a tree3 * 'a tree3 * 'a tree3

let rec mapTree3 f = function 
    | Empty -> Empty
    | Node (e, left, middle, right) -> Node (f e, mapTree3 f left, mapTree3 f middle, mapTree3 f right)

let rec foldTree3 f acc tree =
  match tree with
  | Empty -> acc
  | Node (e, left, middle, right) -> f (foldTree3 f (foldTree3 f (foldTree3 f acc right) middle) left) e

let test_tree () =
  Node (1, 
    Node (2, 
      Node (3, Node (4, Empty, Empty, Empty), Empty, Empty), 
      Node (5, Empty, Empty, Empty), 
      Node (6, Empty, Empty, Empty)
    ), 
    Node (7, 
      Node (8, Empty, Empty, Empty), 
      Node (9, Empty, Empty, Empty), 
      Node (10, Empty, Empty, Empty)
    ), 
    Node (11, 
      Node (12, Empty, Empty, Empty), 
      Node (13, Empty, Empty, Empty), 
      Node (14, Empty, Empty, Empty)
    )
  ) 
  |> mapTree3 (fun x -> x * 3)
  (* fold that to a string to print*)
  |> foldTree3 (fun acc x -> string_of_int x ^ " " ^ acc) ""
  |> string_remove_last


type data = string
and name = string
and letter = char
and file = name * data
and folder = (name * (item list))
and item = File of file | Folder of folder
and disk = Disk of letter * item list

let disk = 
  Disk (('C',
    [Folder ("Program Files",
        [
          Folder ("Microsoft Office",
          [ 
            File ("Word.exe", "10101010");
            Folder ("Browsers",
            [ 
              File ("Firefox.exe", "111101011111");
            ];);
            File ("Excel.exe", "101010000111");
            File ("PowerPoint.exe", "1100110011001100110011001100110011001100110011001100110011001100");
          ];);

          Folder("Office stuff", 
          [ 
            File ("Word.exe", "10101010");
            File ("Excel.exe", "101010000111");
            File ("PowerPoint.exe", "1100110011001100110011001100110011001100110011001100110011001100");
            File ("Firefox.exe", "111101011111");
          ];);

          File ("Word.exe", "10101010");
        ];
      )
    ]
  ))


(* DFS *)
let path item disk =
  let rec aux item current_place current_path level = 
    match current_place with
    | File (name, data) as f -> if item = f then Some (current_path ^ "\\" ^ name, level) else None
    | Folder (name, items) -> 
      let sorted_paths =
        List.map (fun x -> aux item x (current_path ^ "\\" ^ name) (level+1)) items
        |> List.filter (fun x -> x <> None)
        |> List.map (fun x -> Option.get x) 
        |> List.sort (fun (x1,y1) (x2,y2) -> if y1 > y2 then 1 else 0)
      in match sorted_paths with
      | x :: _ -> Some x
      | [] -> None
  in match disk with
  | Disk (letter, folders) -> 
    aux item (Folder (String.make 1 letter ^ ":", folders)) "" 0
    |> Option.map (fun (x,y) -> (String.sub x 1 (String.length x - 1)) ^ "\\")



(* BFS *)
let pathBFS item disk =
  let rec aux queue visited =
    match Queue.peek queue with
    | None -> None
    | Some(File (name, data) as f, path) -> if item = f then Some (path ^ "\\" ^ name) else aux (Queue.dequeue queue) (f :: visited)
    | Some(Folder (name, items) as f, path) -> 
      let new_queue = list_fold_left (fun acc x -> Queue.enqueue acc (x, path ^ "\\" ^ name)) (Queue.dequeue queue) items in
      aux new_queue (f :: visited)
  in match disk with
  | Disk (letter, folders) -> 
    aux (Queue.enqueue Queue.empty ((Folder (String.make 1 letter ^ ":", folders)), "")) []
    |> Option.map (fun x -> (String.sub x 1 (String.length x - 1)) ^ "\\")
  
(* MODIFICATION *)
let insert disk path item = 
  let rec aux items_left path_left =
    match path_left, items_left with
    | h :: [], Folder(name, items) :: folder_tail -> 
      if h = name then Folder(name, item :: items) :: folder_tail
      else aux (List.tl items_left) path_left
    | h :: t, Folder(name, items) :: folder_tail -> 
        if h = name then Folder(name, aux items t) :: folder_tail
        else aux (List.tl items_left) (path_left)
    | h :: t, File(name, data) :: folder_tail ->
        aux (List.tl items_left) (path_left)
    | h :: [], [] -> Folder(h, [item]) :: []
    | h :: t, [] -> Folder(h, aux [] t) :: []
    | [], _ -> failwith "path is not found"
  in
  match disk with
  | Disk (letter, folders) -> Disk (letter, aux folders path)

let () =
    (* these should return SOME*)
    let test1bfs () = pathBFS (File ("Word.exe", "10101010")) disk in
    let test1dfs () = path (File ("Word.exe", "10101010")) disk in
    let test2bfs () = pathBFS (File ("Firefox.exe", "111101011111")) disk in
    let test2dfs () = path (File ("Firefox.exe", "111101011111")) disk in

    (* these should return NONE*)
    let test3bfs () = pathBFS (File ("Firefox.exe", "11")) disk in
    let test3dfs () = path (File ("Firefox.exe", "011")) disk in

    let tests = [test1bfs (); test1dfs (); test2bfs (); test2dfs (); test3bfs (); test3dfs ()] in
    List.iter (
      fun x -> match x with
      | Some x -> print_endline x
      | None -> print_endline "None"
    ) tests;

    let inserttest1 = (insert disk ["Program Files"; "Microsoft Office"] (File ("emacs.exe", "010100")) = Disk ('C', [Folder ("Program Files", [Folder
     ("Microsoft Office",
      [File ("emacs.exe", "010100"); File ("Word.exe", "10101010");
       Folder ("Browsers", [File ("Firefox.exe", "111101011111")]);
       File ("Excel.exe", "101010000111");
       File
        ("PowerPoint.exe",
         "1100110011001100110011001100110011001100110011001100110011001100")]);
    Folder
     ("Office stuff",
      [File ("Word.exe", "10101010"); File ("Excel.exe", "101010000111");
       File
        ("PowerPoint.exe",
         "1100110011001100110011001100110011001100110011001100110011001100");
       File ("Firefox.exe", "111101011111")]);
    File ("Word.exe", "10101010")])]))
    in
    let inserttest2 = (insert disk ["Program Files"; "Microsoft Office"; "Browsers2"] (File ("librewolf.exe", "010100")) = Disk ('C', [Folder ("Program Files", [Folder
      ("Microsoft Office",
       [Folder ("Browsers2", [File ("librewolf.exe", "010100")])]);
     Folder
      ("Office stuff",
       [File ("Word.exe", "10101010"); File ("Excel.exe", "101010000111");
        File
         ("PowerPoint.exe",
          "1100110011001100110011001100110011001100110011001100110011001100");
        File ("Firefox.exe", "111101011111")]);
     File ("Word.exe", "10101010")])]))
    in 
    print_endline (string_of_bool inserttest1);
    print_endline (string_of_bool inserttest2);


  
