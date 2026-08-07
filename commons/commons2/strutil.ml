(*s: commons2/strutil.ml *)
(*s: strutil.ml *)
let compares (x:string) (y:string) = compare x y

module Compare = struct type t = string let compare=compares end

(* old:
module Set = Set.Make(Compare)
module Map = Map.Make(Compare)
*)

module Set = struct
  include Set.Make(Compare)

  let pp fmt s =
    Format.fprintf fmt "{%a}"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
         (fun fmt s -> Format.fprintf fmt "%S" s))
      (elements s)
end

module Map = struct
  include Map.Make(Compare)

  let pp pp_value fmt m =
    let pp_binding fmt (k, v) =
      Format.fprintf fmt "%S -> %a" k pp_value v
    in
    Format.fprintf fmt "{%a}"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
         pp_binding)
      (bindings m)
end

let assoc2map pairs =
    let f map (key,value) = Map.add key value map in
        List.fold_left f Map.empty pairs

let from_list xs = List.fold_left (fun set x -> Set.add x set) Set.empty xs
(*e: strutil.ml *)
(*e: commons2/strutil.ml *)
