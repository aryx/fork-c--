(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation, with the special
 * exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)

val is_instruction: Rtl.rtl -> bool
(* claude: ~mach:true selects Mach-O syntax for the address-of-symbol
 * adrp/add idiom ("symbol@PAGE"/"symbol@PAGEOFF") instead of the default
 * GNU-as/ELF one (":lo12:symbol") - see arm64rec.mlb's own adrp_add.
 * ELF is the default (mach defaults to false), not Mach-O: ELF/Linux is
 * this fork's actual target platform for arm64/amd64, same "bare = ELF"
 * convention as driver/main.ml's -arm64/-arm64-mach-o flags. arm64asm.ml
 * passes neither; arm64mach.ml (Mach-O) passes ~mach:true. *)
val to_string:      ?mach:bool -> Rtl.rtl -> string
