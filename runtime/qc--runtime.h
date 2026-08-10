/*s: qc--runtime.h */
#ifndef QCMM_RUNTIME_H
#define QCMM_RUNTIME_H

/*s: machine-dependent macro definitions for the public interface */
/*s: machine-dependent macro definitions for the public interface ((x86-linux)) */
#define NUM_REGS 12
/*e: machine-dependent macro definitions for the public interface ((x86-linux)) */
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
