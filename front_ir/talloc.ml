(*s: front_ir/talloc.ml *)
(*s: talloc.ml *)
open Nopoly

module S = Space
(*s: auxiliaries(talloc.nw) *)
let existsEq v =
  let rec exists = function
  | [] -> false
  | h :: t -> h = v || exists t
  in exists
(*x: auxiliaries(talloc.nw) *)
let divideby = function
  | 32 -> fun n -> n / 32
  | 8  -> fun n -> n / 8
  | 64 -> fun n -> n / 64
  | m  -> fun n -> n / m
(*e: auxiliaries(talloc.nw) *)
module Single = struct
  type t = ((*width*)int -> Register.t) * ((*width*)int -> Rtl.loc)
        
  let for_space space =
    let next = ref 1 in
    let (_, _, cell) = space.S.space in
    let to_count = Cell.to_count cell in
    (fun width ->
      (*s: use [[width]] to make [[k]] be the next index, updating [[next]] *)
      let () = if not (existsEq width space.S.widths) then
                 Impossible.impossible ("Asked for temporary in space `" ^
                                        space.S.doc ^ "' with unsupported width " ^
                                        string_of_int width) in
      let Cell.C n = to_count width in
      let k = !next in
      let _ = next := k + n in
      (*e: use [[width]] to make [[k]] be the next index, updating [[next]] *)
      (space.S.space, k, to_count width)),
    (fun width ->
      (*s: use [[width]] to make [[k]] be the next index, updating [[next]] *)
      let () = if not (existsEq width space.S.widths) then
                 Impossible.impossible ("Asked for temporary in space `" ^
                                        space.S.doc ^ "' with unsupported width " ^
                                        string_of_int width) in
      let Cell.C n = to_count width in
      let k = !next in
      let _ = next := k + n in
      (*e: use [[width]] to make [[k]] be the next index, updating [[next]] *)
      Rtl.reg (space.S.space, k, to_count width))

  let _ = (for_space : Space.t -> t)

  let reg (reg, _) = reg
  let loc (_, loc) = loc
end
(*x: talloc.ml *)
module Multiple = struct
  let fail c =
    prerr_string ("Space '" ^ Char.escaped c ^ "' is not a temporary space\n");
    flush stderr;
    assert false

  let is_temp s = match s.S.classification with
  | S.Temp _ -> true
  | _ -> false

  type t = char -> Single.t

  let for_spaces spaces =
    List.fold_right
      (fun s rest ->
         if is_temp s then
           let a = Single.for_space s in
           let named_by (c', _, _) c = c =<= c' in
           fun c -> if named_by s.S.space c then a else rest c
         else
           rest)
      spaces fail

  let _ = (for_spaces : Space.t list -> t)

  let reg c t = Single.reg (t c)
  let loc t c = Single.loc (t c)

  let reg_like (t:t) ((c, _, ms) as _space, _, ct) =
    Single.reg (t c) (Cell.to_width ms ct)
end
(*e: talloc.ml *)
(*e: front_ir/talloc.ml *)
