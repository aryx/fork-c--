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

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* What the virtual frame pointer *is*: register 0 of space 'V'.
 *
 * Front ends and back ends both address the frame through this fictional
 * pointer, and the rmvfp phase later rewrites it into sp plus a frame
 * offset - see layout/vfprewrite.ml, which holds that rewrite.
 *
 * The two halves are separate because they sit at very different levels.
 * This one needs nothing but Rtl, Cell and Space, whereas the rewrite needs
 * Zipcfg and Dataflow, so keeping them together forced every user of "what
 * is the vfp" to depend on the whole dataflow framework. That was not
 * possible for the modules that needed it most, and three of them worked
 * around it separately:
 *
 *   - codegen/runtimedata.ml stubbed the test out entirely -
 *     "if false (* Vfp.is_vfp l *)" - which silently disabled the code that
 *     turns a vfp-relative span address into a frame offset, so no .pcmap
 *     data could be emitted;
 *   - ir/block.ml declared a _empty_vfp_hook ref for the caller to fill in,
 *     marked "pad: ugly";
 *   - elab/nelab.ml copied mk outright, noting "pad: copy paste of Vfp.mk
 *     but brought too many dependencies".
 *
 * Living in ir/ instead, next to Space and Rtl, it is visible to all of
 * them.
 *)

module RP = Rtl.Private

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let mk w = Rtl.fetch (Rtl.reg (('V', Rtl.Identity, Cell.of_size w), 0, Rtl.C 1)) w

let is_vfp = function
  | RP.Reg (('V', _, _), 0, _) -> true
  | _ -> false

let mk_space w =
    { Space.space = ('V', Rtl.Identity, Cell.of_size w)
    ; Space.doc = "holds the virtual frame pointer"
    ; Space.indexwidth = w
    ; Space.indexlimit = None
    ; Space.widths = [w]
    ; Space.classification = Space.Fixed
    }
