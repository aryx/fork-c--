(*s: commons2/auxfuns.ml *)
(*s: auxfuns.ml *)
type void = Void of void (* used as placeholder for polymorphic variable *)

module Option = struct
    let is_some = function Some _ -> true  | None -> false 
    let is_none = function Some _ -> false | None -> true
    let get x = function
        | Some y -> y
        | None   -> x
    let map f = function Some x -> Some (f x) | None -> None
end
(*x: auxfuns.ml *)
let round_up_to ~multiple_of:n k = n * ((k+(n-1)) / n)
(*x: auxfuns.ml *)
let foldri f l z =
  let rec next n = function
    | [] -> z
    | x :: xs -> f n x (next (n+1) xs) in
  next 0 l

let rec from first ~upto = 
    if first > upto then [] else first :: from (first+1) ~upto

let substr start stop str = 
    let start = if start <  0 then String.length str + start else start in
    let stop  = if stop  <= 0 then String.length str + stop  else stop  in
        String.sub str start (stop - start)
(*x: auxfuns.ml *)
module String = struct
  let foldr f s z =
    let rec down_from n z =
      if n < 0 then z else down_from (n-1) (f (String.get s n) z) in
    down_from (String.length s - 1) z
end
(*x: auxfuns.ml *)
let rec last = function
  | [] -> raise (Invalid_argument "empty list")
  | [x] -> x
  | x :: xs -> last xs
(*x: auxfuns.ml *)
let rec map_partial f = function
  | [] -> []
  | x :: xs ->
      match f x with
      | Some y -> y :: map_partial f xs
      | None -> map_partial f xs
(*x: auxfuns.ml *)
let rec compare_list cmp x y = match x, y with
| [], [] -> 0
| [], _ :: _ -> -1
| _ :: _, [] ->  1
| x :: xs, y :: ys ->
    match cmp x y with
    | 0 -> compare_list cmp xs ys
    | diff -> diff
(*x: auxfuns.ml *)
module List = struct
  let rec take n xs = match xs with
  | [] -> []
  | _ :: _ when n = 0 -> []
  | x :: xs -> x :: take (n-1) xs
end
(*e: auxfuns.ml *)
(*e: commons2/auxfuns.ml *)
