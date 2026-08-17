/*s: qc--runtime.h */
#ifndef QCMM_RUNTIME_H
#define QCMM_RUNTIME_H

#include <stdint.h>

/*s: machine-dependent macro definitions for the public interface */
/* claude: NUM_REGS/FP_REG must match the TARGET's own flat register-index
 * numbering (target/target.ml's mk_reg_ix_map: every Reg/Fixed-classified
 * space, sorted by space character, concatenated - see each arch's own
 * Spaces module for its per-space counts/order). Cmm_Activation.regs[] is
 * a fixed-size array with no bounds check in update_saved_regs, so if
 * NUM_REGS is too small for a given target, writes past the end silently
 * corrupt whatever follows it on the stack - this is what "ppc/sparc/
 * riscv/alpha were all silently running with x86's NUM_REGS=12" actually
 * meant each time it was found (see git log). A platform this chain
 * doesn't recognize gets a #error instead of silently reusing x86's
 * values, which would reintroduce exactly that bug for whatever's next. */
#ifdef __powerpc__
#define NUM_REGS 96 /* c(7)->0-7, f(32)->8-40, r(32)->41-73 - confirmed via gdb */
#elif defined(__sparc__)
#define NUM_REGS 128 /* r/f/c/k, ~49 indices - generous, not pinned down exactly */
#elif defined(__riscv)
#define NUM_REGS 96 /* c(6)->0-5, f(32)->6-37, r(32)->38-69 = 70; both RV32/RV64 */
#elif defined(__alpha__)
#define NUM_REGS 96 /* c(6)->0-5, f(32)->6-37, r(32)->38-69 = 70, same shape as riscv's */
#elif defined(__mips__)
#define NUM_REGS 96 /* c(6)->0-5, f(32)->6-37, r(32)->38-69 = 70, same shape as riscv/alpha */
#elif defined(__arm__)
#define NUM_REGS 32 /* c(3)->0-2, r(16)->3-18 = 19; no float space (arm.ml: no FPU) */
#elif defined(__i386__) || defined(__x86_64__)
/*s: machine-dependent macro definitions for the public interface ((x86-linux)) */
#define NUM_REGS 12
/*e: machine-dependent macro definitions for the public interface ((x86-linux)) */
#else
#error "NUM_REGS/FP_REG not defined for this platform - see qc--runtime.h/gcc-linux.c"
#endif
/*e: machine-dependent macro definitions for the public interface */

/*s: data structures */
/* claude: qc-- emits every pc_map_entry field (locations, register/local/
 * span counts, the location-encoded offsets themselves) at the target's
 * native pointer width, not a fixed 32 bits - confirmed by hand from a
 * -riscv64 .s dump, where inalloc/outalloc/return_addressp/num_registers/
 * etc. are all ".dword" (8 bytes), and MKOFFSET(0) shows up as
 * -9223372036854775808 (bit 63 set), not bit 31. uintptr_t equals plain
 * "unsigned" on every existing 32-bit backend, so this is a no-op there.
 * pcmap.c's Cmm_loctype/Cmm_as_register/Cmm_as_offset decode this
 * generically off sizeof() of their own local variable, so they need no
 * further change beyond using a same-width signed type (see that file).
 * This is a different table from fork-tiger's own "bits32[] {...}" GC
 * descriptor tables (frontend/frame.ml's output_footer there is a
 * deliberate, separate choice to stay 32-bit regardless of target) -
 * those are addressed *through* a span value (itself pointer-width, as
 * elab/nelab.ml's own span-value check requires), not part of this
 * struct, so they are unaffected by this typedef. */
typedef uintptr_t Cmm_Word;
typedef void*     Cmm_Dataptr;
typedef void    (*Cmm_Codeptr)();
/*x: data structures */
typedef struct cmm_cont Cmm_Cont;
typedef struct cmm_activation Cmm_Activation;
/*e: data structures */
/*s: exposed private data structures */
/*s: definition of [[struct cmm_cont]] */
struct cmm_cont {
  Cmm_Codeptr pc;
  Cmm_Word*   sp;
};
/*e: definition of [[struct cmm_cont]] */
struct cmm_activation {
  struct cmm_activation_methods *methods;
  char *vfp;   /* declared char* so that arithmetic works on addresses */
  struct cmm_pc_map_entry *rtdata;  
  Cmm_Word* regs[NUM_REGS];
  /*s: other fields of an activation */
  Cmm_Codeptr pc;   /* used only for debugging */
  union {  /* following fields are temporary space to avoid allocation */
    struct {
      Cmm_Cont   k;
    } unwind;
    struct {
      unsigned tries;
    } cwalk;
  } u;
  /*e: other fields of an activation */
};
/*e: exposed private data structures */
/*s: public functions */
extern void Cmm_show_activation(const Cmm_Activation *a); /* for debugging */
/*x: public functions */
Cmm_Activation Cmm_YoungestActivation (const Cmm_Cont *t);
int            Cmm_isOldestActivation (const Cmm_Activation *a);
Cmm_Activation Cmm_NextActivation     (const Cmm_Activation *a);
int            Cmm_ChangeActivation   (Cmm_Activation *a);
Cmm_Cont*      Cmm_MakeUnwindCont     (Cmm_Activation *a, Cmm_Word index, ...);
/*x: public functions */
Cmm_Dataptr Cmm_GetDescriptor(const Cmm_Activation *a, Cmm_Word token);
/*x: public functions */
void      Cmm_LocalVarWritten  (const Cmm_Activation *a, unsigned n);
unsigned  Cmm_LocalVarCount    (const Cmm_Activation *a);
void*     Cmm_FindLocalVar     (const Cmm_Activation *a, unsigned n);
void*     Cmm_FindDeadLocalVar (const Cmm_Activation *a, unsigned n);
void*     Cmm_FindStackLabel   (const Cmm_Activation *a, unsigned n);
/*x: public functions */
void      Cmm_CutTo            (const Cmm_Cont *k);
/*x: public functions */
Cmm_Cont* Cmm_CreateThread     (Cmm_Codeptr f, Cmm_Dataptr x, void *s, unsigned n);
void Cmm_Yield (Cmm_Cont *k, Cmm_Cont *kold);
/*e: public functions */
/*s: exposed private functions */
int Cmm_is_thread_start_frame(struct cmm_pc_map_entry *rtdata, Cmm_Codeptr pc);
/*e: exposed private functions */
#endif /* QCMM_RUNTIME_H */
/*e: qc--runtime.h */
