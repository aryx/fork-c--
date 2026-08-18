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

(* Small helpers shared by opti/licm.ml and opti/strength_reduction.ml -
 * both walk a Dominator.Query.loop's middle instructions and both edit a
 * block's tail without disturbing its terminator. Not a Cminusminus_extra
 * view (no syncweb chunk markers): this is new code the fork wrote
 * directly, not ported from a TODO/*.nw file. *)

(* a loop body block's middle nodes, in program order *)
val middles_of : Zipcfg.Rep.tail -> Zipcfg.Rep.middle list

(* splice [ms] into [tail] immediately before its terminator, preserving
 * their relative order *)
val append_before_last : Zipcfg.Rep.middle list -> Zipcfg.Rep.tail -> Zipcfg.Rep.tail

(* replace the (physically) first occurrence of [target] in [tail] with
 * [replacement], leaving everything else untouched *)
val replace_middle :
  Zipcfg.Rep.middle -> Zipcfg.Rep.middle -> Zipcfg.Rep.tail -> Zipcfg.Rep.tail

(* do [r] and some register in [set] denote overlapping storage? (either
 * direction - a set member may be the wider register, or [r] may be) *)
val overlaps : Register.x -> Register.SetX.t -> bool

(* registers written by [m] (empty for anything but a real Instruction) *)
val writes_of : Zipcfg.Rep.middle -> Register.SetX.t

(* union of writes_of over every middle in [all] *)
val written_in : Zipcfg.Rep.middle list -> Register.SetX.t

(* union of writes_of over every middle in [all] other than [m] itself
 * (compared physically, so a textually-identical middle elsewhere still
 * counts as "other") *)
val written_excluding : Zipcfg.Rep.middle list -> Zipcfg.Rep.middle -> Register.SetX.t
