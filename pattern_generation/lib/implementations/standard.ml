(* Standard accumulator map, no optimizations *)
let rec rev (acc: 'a list): 'a list -> 'a list = function
  | [] -> acc
  | x :: xs -> rev (x :: acc) xs

let rec map (f: 'a -> 'b) (acc: 'b list): 'a list -> 'b list = function
  | [] -> rev [] acc
  | x :: xs ->
     map f (f x :: acc) xs
                                           
(* Non tail-recursive map *)
let rec nmap (f: 'a -> 'b): 'a list -> 'b list = function
  | [] -> []
  | x :: xs -> let y = f x in y :: nmap f xs