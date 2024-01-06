let composites n =
  let composites = Array.init n (fun x -> 0) in
  let sqrtn = int_of_float @@ sqrt @@ float_of_int n in
  for i = 2 to sqrtn do
      for j = 2 to n / i do
        composites.(i * j - 1) <- i * j;
      done;
  done;

  let quantity = ref 0 in
  for i = 0 to n - 1 do
    if composites.(i) <> 0 then
      quantity := !quantity + 1;
  done;

  let composites_only = Array.init !quantity (fun x -> 0) in
  let position = ref 0 in
  for i = 0 to n - 1 do
    if composites.(i) <> 0 then (
      composites_only.(!position) <- composites.(i);
      position := !position + 1;
      )
  done;
  composites_only

let composites_fun n =
let primes = 
let rec sieve = function
LCons(p,nf) -> LCons( p, 
function () -> sieve
(lfilter (function n -> n mod p <> 0)
(nf())
)
)
| LNil -> failwith "Impossible! Internal error."
in sieve (lfrom 2)

