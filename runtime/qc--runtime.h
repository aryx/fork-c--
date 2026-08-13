/*s: qc--runtime.h */
#ifndef QCMM_RUNTIME_H
#define QCMM_RUNTIME_H

/*s: machine-dependent macro definitions for the public interface */
#ifdef __powerpc__
/* claude: ppc's flat register-index numbering (target/target.ml's
 * mk_reg_ix_map, applied to arch/ppc/ppc.ml's Spaces module) runs the
 * control space 'c' (7 registers -> indices 0-7), then 'f' (32 float
 * registers -> indices 8-40), then 'r' (32 int registers -> indices
 * 41-73) - confirmed empirically via gdb (a running program's own
 * pcmap entries), not just by reading the space declarations. The
 * x86-sized array below silently overflowed for every ppc register
 * above index 11 (i.e. almost all of them), corrupting whatever
 * memory follows a Cmm_Activation on the stack. 96 leaves headroom. */
#define NUM_REGS 96
#elif defined(__sparc__)
/* claude: same overflow bug as ppc's (see the comment above) - the
 * x86-sized array below silently overflowed for sparc too, since
 * update_saved_regs's "new->regs[regs[i].index] = ..." has no bounds
 * check. sparc's own flat register-index numbering (target/target.ml's
 * mk_reg_ix_map, spaces filtered to Reg/Fixed and taken in
 * arch/sparc/sparc.ml's own list order: r, f, c, k - m/t/u/q are
 * Memory/Temp classified and excluded) covers roughly 49 indices (32
 * general + 9 float + 6 control + 2 window-pointer registers); 128
 * leaves headroom the same way ppc's 96 does. Not yet pinned down to
 * the exact count the way ppc's was (still confirming FP_REG - see
 * gcc-linux.c), so generous rather than tight. */
#define NUM_REGS 128
#else
/*s: machine-dependent macro definitions for the public interface ((x86-linux)) */
#define NUM_REGS 12
/*e: machine-dependent macro definitions for the public interface ((x86-linux)) */
#endif
/*e: machine-dependent macro definitions for the public interface */

/*s: data structures */
typedef unsigned  Cmm_Word;
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
