type 'a tree3 = Empty | Node of 'a * 'a tree3 * 'a tree3 * 'a tree3

let rec mapTree3 f = function 
    | Empty -> Empty
    | Node (e, left, middle, right) -> Node (f e, mapTree3 f left, mapTree3 f middle, mapTree3 f right)

type data = string
and name = string
and letter = char
and file = name * data
and folder = (name * (item list))
and disk = Disk of letter * item list
and item = File of file | Folder of folder


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

let list_map f lst = 
  match lst with
  | [] -> []
  | x :: xs -> f x :: List.map f xs

let rec list_filter f lst =
  match lst with
  | [] -> []
  | x :: xs -> if f x then x :: list_filter f xs else list_filter f xs

let rec list_first_some lst = 
  match lst with
  | [] -> None
  | x :: [] -> x
  | Some x :: _ -> Some x
  | None :: xs -> list_first_some xs


(* DFS *)
let path item disk =
  let rec aux item current_place current_path level = 
    match current_place with
    | File (name, data) as f -> if item = f then Some (current_path ^ "/" ^ name, level) else None
    | Folder (name, items) -> 
      let sorted_paths =
        List.sort (fun (x1,y1) (x2,y2) -> if y1 > y2 then 1 else 0) @@ 
        List.map (fun x -> Option.get x) @@ List.filter (fun x -> x <> None) @@ 
        List.map (fun x -> aux item x (current_path ^ "/" ^ name) (level+1)) items
      in match sorted_paths with
      | x :: _ -> Some x
      | [] -> None
  in match disk with
  | Disk (letter, folders) -> 
    aux item (Folder ("", folders)) (String.make 1 letter) 0


(* BFS *)

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
    | [], back -> match List.rev back with
                  | [] -> ([], [])
                  | h :: t -> (t, [])
  
  let peek queue =
    match queue with
    | [], [] -> None
    | [], back -> Some (List.hd (List.rev back))
    | h :: t, _ -> Some h
  
  let to_string queue =
    match queue with
    | front, back -> List.fold_left (fun acc x -> acc ^ " " ^ x) "" front ^ List.fold_left (fun acc x -> acc ^ " " ^ x) "" (List.rev back)

end

let item_to_string item =
  match item with
  | File (name, data) -> name
  | Folder (name, items) -> name

let items_to_string items =
  List.iter (fun x -> x) items

let rec print_queue queue =
  match queue with
  | (File(name, data)) :: front, back -> print_endline name ; print_queue (front, back)
  | (Folder(name, items)) :: front, back -> print_endline name; print_queue (front, back)
  | [], [] -> ();
  | [], back -> print_queue (List.rev back, [])


(* let pathBFS item disk =
  let rec aux item current_place (visited) (queue: 'a list * 'a list) =
    match current_place with
    | File (name, data) as f -> print_endline @@ "FILE: " ^ item_to_string f; (if f = item then (true, [name] , queue) else (false, [], queue))
    | Folder (name, items) as f -> 
      let new_queue = List.fold_left (fun acc x -> Queue.enqueue acc x) queue items in
      let new_place = Queue.peek new_queue in
      let new_queue = Queue.dequeue new_queue in
      let (is_found, path, queue) = aux item (Option.get new_place) (f :: visited) new_queue in
      if is_found then (true, name :: path, queue) else (false, [], queue)
    in match disk with
    | Disk (letter, folders) -> 
      let (is_found, path, queue) = aux item (Folder ("", folders)) [] Queue.empty in
      if is_found then String.make 1 letter :: path else ["NONE!"] *)

(* let pathBFS item disk =
  let rec aux queue visited =
    match queue with
    | [] -> None
    | (File (name, _) as current_file, current_path) :: front -> 
      if item = current_file then Some (current_path ^ "/" ^ name) else None
    | (Folder (name, items) as current_folder, current_path) :: front -> 
      let new_queue = List.fold_left (fun acc x -> acc @ [(x, current_path ^ "/" ^ name)]) front items in
      aux new_queue (current_folder :: visited)
  in match disk with
  | Disk (letter, folders) -> 
    let res = aux [(Folder (String.make 1 letter, folders), "")] []
    in match res with
    | Some x -> String.sub x 1 (String.length x - 1)
    | None -> "There is no such file or folder!" *)


let pathBFS item disk =
  let rec aux queue visited =
    match (Queue.peek queue, Queue.dequeue queue) with
    | None, _ -> None
    | Some(File (name, _) as current_file, current_path), front -> 
      if item = current_file then Some (current_path ^ "/" ^ name) else None
    | Some(Folder (name, items) as current_folder, current_path), front -> 
      let new_queue = List.fold_left (fun acc x -> Queue.enqueue acc (x, current_path ^ "/" ^ name)) front items in
      aux new_queue (current_folder :: visited)
  in match disk with
  | Disk (letter, folders) -> 
    let first_element = ((Folder (String.make 1 letter, folders)), "") in
    let res = aux (Queue.enqueue Queue.empty first_element) []
    in match res with
    | Some x -> String.sub x 1 (String.length x - 1)
    | None -> "There is no such file or folder!"


let test1 () = pathBFS (File ("Word.exe", "10101010")) disk

let test2 () = pathBFS (File ("Firefox.exe", "111101011111")) disk




  
