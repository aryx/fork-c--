(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *)
module GR  = Zipcfg.Rep
module RSX = Register.SetX

let middles_of tail =
  let rec go acc = function
    | GR.Last _ -> List.rev acc
    | GR.Tail (m, t) -> go (m :: acc) t
  in go [] tail

let append_before_last ms tail =
  let rec go = function
    | GR.Last l -> List.fold_right (fun m acc -> GR.Tail (m, acc)) ms (GR.Last l)
    | GR.Tail (m, t) -> GR.Tail (m, go t)
  in go tail

let replace_middle target replacement tail =
  let rec go = function
    | GR.Last _ as l -> l
    | GR.Tail (m, t) -> if m == target then GR.Tail (replacement, t) else GR.Tail (m, go t)
  in go tail

let overlaps r set =
  RSX.exists (fun r' -> Register.contains ~outer:r' ~inner:r || Register.contains ~outer:r ~inner:r') set

let writes_of m =
  if GR.is_executable m then
    let (_, w, _) = Rtlutil.ReadWriteKill.sets (GR.mid_instr m) in w
  else RSX.empty

let written_in all = List.fold_left (fun acc m -> RSX.union (writes_of m) acc) RSX.empty all

let written_excluding all m =
  List.fold_left (fun acc m' -> if m' == m then acc else RSX.union (writes_of m') acc) RSX.empty all
