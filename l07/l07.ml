module type ICoprocessor = sig
  type element 
  type stack
  type instruction = Rst | LoadF of float | LoadI of int | Cpy | Add | Sub | Mul | Div

  val init : unit -> stack
  val result : stack -> element 
  val execute : stack  -> instruction list -> unit 
  val put : stack -> unit
end

module StackMachineList = struct
  type element = float
  type stack = { mutable l : element list}
  type instruction = Rst | LoadF of float | LoadI of int | Cpy | Add | Sub | Mul | Div


  exception DivisionByZero
  exception StackUnderflow

  let init : unit -> stack = fun () -> { l = [] }

  let execute_instruction s = function
    | Rst -> s.l <- []
    | LoadF f -> s.l <- f :: s.l
    | LoadI i -> s.l <- float_of_int i :: s.l
    | Cpy -> s.l <- (match s.l with
        | x :: l -> x :: x :: l
        | _ -> raise StackUnderflow)
    | Add -> s.l <- (match s.l with
        | x :: y :: l -> (x +. y) :: l
        | _ -> raise StackUnderflow)
    | Sub -> s.l <- (match s.l with
        | x :: y :: l -> (x -. y) :: l
        | _ -> raise StackUnderflow)
    | Mul -> s.l <- (match s.l with
        | x :: y :: l -> (x *. y) :: l
        | _ -> raise StackUnderflow)
    | Div -> s.l <- (match s.l with
        | x :: y :: l -> (if y = 0. then raise DivisionByZero else x /. y) :: l
        | _ -> raise StackUnderflow)

  let result s = match s.l with
    | x :: r -> x
    | _ -> raise StackUnderflow

  let execute s p =
    List.iter (execute_instruction s) p

  let put s =
    print_string "[";
    List.iter (fun x -> print_float x; print_string ";") s.l;
    print_string "]";
    print_newline ()
end

module StackMachineArray = struct
  type element = float
  type stack = { mutable arr : element array; mutable len : int }
  type instruction = Rst | LoadF of float | LoadI of int | Cpy | Add | Sub | Mul | Div

  exception DivisionByZero
  exception StackUnderflow

  let init : unit -> stack = fun () -> { arr = Array.make 100 0.; len = 0 }

  let double s =
    let new_arr = Array.make (2 * Array.length s.arr) 0. in
    Array.blit s.arr 0 new_arr 0 s.len;
    s.arr <- new_arr
  let execute_instruction s = function
    | Rst -> s.len <- 0
    | LoadF f -> s.arr.(s.len) <- f; s.len <- s.len + 1
    | LoadI i -> s.arr.(s.len) <- float_of_int i; s.len <- s.len + 1
    | Cpy -> if s.len = 0 
      then raise StackUnderflow 
      else s.arr.(s.len) <- s.arr.(s.len - 1); 
      s.len <- s.len + 1
    | Add | Sub | Mul | Div as inst -> if s.len < 2 
      then raise StackUnderflow 
      else let x = s.arr.(s.len - 2) and y = s.arr.(s.len - 1) in
        s.len <- s.len - 1;
        s.arr.(s.len - 1) <- (match (x, y) with
          | (x, y) when inst = Add -> x +. y
          | (x, y) when inst = Sub -> x -. y
          | (x, y) when inst = Mul -> x *. y
          | (x, y) when inst = Div -> if y = 0. then raise DivisionByZero else x /. y
          | _ -> raise StackUnderflow)

  let result s = if s.len = 0 then raise StackUnderflow else s.arr.(s.len - 1)

  let execute s p =
    List.iter (execute_instruction s) p

  let put s =
    print_string "[";
    for i = 0 to s.len - 1 do
      print_float s.arr.(i);
      print_string ";"
    done;
    print_string "]";
    print_newline ()
end

module CoprocessorList: ICoprocessor with type element = float = StackMachineList

module CoprocessorArray: ICoprocessor with type element = float = StackMachineArray


let coprocessor_list_test () =
  let s = CoprocessorList.init () in

  CoprocessorList.execute s [LoadI 1; Cpy];
  CoprocessorList.put s;

  CoprocessorList.execute s [Add; LoadI 3; Mul];
  print_endline @@ string_of_float (CoprocessorList.result s);
  CoprocessorList.put s;

  CoprocessorList.execute s [Rst];
  CoprocessorList.put s;
  ()

let coprocessor_array_test () =
  let s = CoprocessorArray.init () in

  CoprocessorArray.execute s [LoadI 1; Cpy];
  CoprocessorArray.put s;

  CoprocessorArray.execute s [Add; LoadI 3; Mul];
  print_endline @@ string_of_float (CoprocessorArray.result s);
  CoprocessorArray.put s;

  CoprocessorArray.execute s [Rst];
  CoprocessorArray.put s;
  ()

let () =
  print_endline "List:";
  coprocessor_list_test ();
  print_endline "\nArray:";
  coprocessor_array_test ();
  ()