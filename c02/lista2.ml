// Zad 2
let rec fibA n = 
    match n with
        0 -> 0
        | 1 -> 1
        | _ -> if n > 0 then fibA(n-1) + fibA(n-2) else failwith "Ujemny argument";;
    
let fibB n = 
    let rec fibIn(n, p, s) = 
        match n with
            0 -> p
            | 1 -> s
            | _ -> fibIn(n-1, s, p+s) in
    if n < 0 then failwith "Ujemny argument"
    else fibIn(n, 0, 1);;
            
// Zad 3
let root3(a, e) = 
    let rec cbrtIn(a, i, s, e) =
        if abs_float((s*.s*.s -. a)) < e*.abs_float(a) then
            s
        else 
            match i with
                0  when  a  > 1. -> cbrtIn(a, i+1, a/.3., e)
                | 0 -> cbrtIn(a, i+1, a, e)
                | _ -> cbrtIn(a, i+1, s +. (a/.(s*.s)-.s)/.3., e) in
    if e < 0. then failwith "Dokładność musi być dodatnia"
    else cbrtIn(a, 0, 0., e);;
        
// Zad 4
let matchingA a = 
    let [_; _; x; _; _] = a in
    x;;
    
let matchingB a = 
    let [_; (x, _)] = a in
    x;;

// Zad 5
let rec initSegment l =
     match l with 
      ([], _) -> true
      | (_,[]) -> false
      | (x, y) -> if List.hd x = List.hd y then initSegment(List.tl x, List.tl y) else false;;
      
// Zad 6
let replaceNth list pos elem = 
    let rec replace(list, pos, elem, index, ret) = 
      if pos = index then 
        ret@[elem]@List.tl list
      else 
        replace(List.tl list, pos, elem, index + 1, ret@[List.hd list]) in
    if pos < 0 || pos > List.length list-1 then failwith "Błędna długość"
    else replace(list, pos, elem, 0, []);;
