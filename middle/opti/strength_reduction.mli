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

(* Induction-variable strength reduction, built on the same
 * Dominator.Query.loops as opti/licm.ml. See opti/strength_reduction.ml's
 * header comment for the exact (deliberately narrow) pattern it
 * recognizes - tests/optimizer/strength_reduction.c-- is a worked
 * example. *)
val reduce : 'a -> Ast2ir.proc -> Ast2ir.proc * bool
