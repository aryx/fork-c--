(*s: x86regs.ml *)
module R = Rtl
let rspace = ('r', Rtl.Identity, Cell.of_size 32)
let eax = (rspace, 0, Register.C 1)
let ecx = (rspace, 1, Register.C 1)
let edx = (rspace, 2, Register.C 1)
let ebx = (rspace, 3, Register.C 1)
let esp = (rspace, 4, Register.C 1)
let ebp = (rspace, 5, Register.C 1)
let esi = (rspace, 6, Register.C 1)
let edi = (rspace, 7, Register.C 1)

let ah = R.slice  8 ~lsb:8 (R.reg eax)
let ax = R.slice 16 ~lsb:0 (R.reg eax)
let cl = R.slice  8 ~lsb:0 (R.reg ecx)
(*x: x86regs.ml *)
let espace = ('e', Rtl.Identity, Cell.of_size 16)
let fpuc n = (espace, n, Rtl.C 1)
let fpuctl    = fpuc 0
let fputag    = Rtl.reg (fpuc 2)
let fpustatus = Rtl.reg (fpuc 1)
let fpcc    = Rtl.slice 3 ~lsb:8 fpustatus (* ignores bit 14 == C3 [probably OK] *)
let fpround = Register.Slice (2, 10, fpuctl)
(*x: x86regs.ml *)
let r8_lo  = [| "%al"; "%cl"; "%dl"; "%bl"|]
let r8_hi  = [| "%ah"; "%ch"; "%dh"; "%bh"|]

let regname8 ~lsb ~base = match lsb with
| 0 -> Array.get r8_lo base
| 8 -> Array.get r8_hi base
| _ -> Impossible.impossible "bad lsb in slice denoting 8-bit register"

(*e: x86regs.ml *)
