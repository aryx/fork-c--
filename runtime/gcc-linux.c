/*s: gcc-linux.c */
/*s: gcc-linux.c  */
#include "qc--runtime.h"
#include "pcmap.h"
#include <assert.h>
#include <stdio.h>

/* claude: FP_REG is the activation-index of whichever callee-saved
 * register real gcc-compiled C code (compiled -fno-omit-frame-pointer,
 * see runtime/regenerate-ppc.sh) uses as its own frame-pointer chain -
 * this is what lets the walk below continue from our outermost C--
 * frame into genuine C frames above it (main's caller, libc, ...).
 * x86: %ebp (index 9, see qc--runtime.h's NUM_REGS comment for how
 * ppc's own numbering was derived). ppc: r31 (index 72) - confirmed by
 * compiling a probe with powerpc-linux-gnu-gcc -fno-omit-frame-pointer
 * and seeing "mr 31,1" in the prologue, gcc's ppc equivalent of x86's
 * "push %ebp; mov %esp,%ebp". */
#ifdef __powerpc__
#define FP_REG 72 /* r31 */
#else
#define FP_REG 9 /* %ebp, r[5] */
#endif
/*x: gcc-linux.c  */
void Cmm_init_c_frame(Cmm_Activation *a, char *young_in_overflow) {
  Cmm_Word fp;
  assert(a->regs[FP_REG]);
  fp = *a->regs[FP_REG];
  /* claude: main (a foreign "C" C-- procedure) blanket-preserves FP_REG
   * like any other callee-saved register, with no notion that it is
   * "the" frame pointer - so what we recover here is genuinely just
   * whatever FP_REG held when something called main, which is only a
   * valid frame pointer if that something was ALSO compiled with frame
   * pointers on (see Cmm_c_change_activation's big comment below for
   * the precedent: modern glibc/crt usually is not). The x86 case
   * happened to produce an obviously-wrong value (1, unaligned); on
   * ppc it produced a plausible-looking one (r31 left holding main's
   * own code address by whatever loaded it before the call) - aligned
   * and non-null, so alignment alone doesn't catch it. A real caller
   * frame has to live at or above our own frame's stack extent
   * (stacks grow down), so bound-check against that instead. Treat
   * "not a frame pointer" the same as "nothing more to walk": a->vfp=0
   * makes the very next Cmm_c_change_activation call return 0 via its
   * own existing guard. */
  if (fp < (Cmm_Word) young_in_overflow) {
    a->vfp = 0;
  } else {
    a->vfp = (char *) fp;
  }
  a->u.cwalk.tries = 0;   /* logging only */
}
extern struct cmm_activation_methods *Cmm_cmm_frame_methods;
/*x: gcc-linux.c  */
#define NOISY 0

int Cmm_c_change_activation(Cmm_Activation *a) {
  Cmm_Word callerfp;
  Cmm_Codeptr ra;
  pc_map_entry *entry;
  int i;

  /* claude: this must come before the assert below, and must accept 0.
   *
   * a->vfp here is a frame-pointer value recovered from a C frame (see
   * FP_REG above), and the whole C walk assumes it chains: word 0 of the
   * frame is the caller's own frame pointer, ending in 0. That held when
   * this was written for x86, but modern glibc and crt are built with
   * -fomit-frame-pointer and use %ebp as an ordinary register, so whatever
   * it holds when main is called is not a frame pointer. What that is
   * varies by libc: 0x1 on this machine's glibc 2.39, 0 on the 2.35 in the
   * Docker image - and 0 tripped the assert, which is why the suite passed
   * here and failed there. (ppc's r31, by contrast, is the ABI's own
   * back-chain register whenever -fno-omit-frame-pointer is on - see
   * runtime/regenerate-ppc.sh - so this degenerate case shouldn't arise
   * there, but the same guard costs nothing to keep for both.)
   *
   * There is no way to walk a C stack that has no frame pointers, so an
   * implausible one ends the walk instead. A real frame pointer is a
   * non-null word-aligned stack address.
   *
   * For a C-- program called from main this loses nothing: the frames above
   * the outermost C-- one belong to crt and libc and hold no C-- data.
   */
  if (a->vfp == 0 || ((Cmm_Word)a->vfp & (sizeof(Cmm_Word) - 1)) != 0)
    return 0;

  assert(a->vfp != 0);   /* protect against an unexpected error */
  /*s: possibly shout about our departure, showing arguments */
  #if NOISY
   {
    Cmm_Word *myfp = (Cmm_Word*) a->vfp;
    fprintf(stderr, "Leaving C activation; my fp = %p, ra = %p, caller's fp = %p\n",
            (void*)myfp, (void*)myfp[1], (void*)myfp[0]);
    for (i = 0; i < 3; i++) fprintf(stderr, "  arg[%d] = 0x%08x\n", i, myfp[i+2]);
   }
  #endif
  /*e: possibly shout about our departure, showing arguments */
  callerfp = *(Cmm_Word *)a->vfp;
  /*s: possibly announce caller's ebp */
  #if 0
    fprintf(stderr, "walked to caller fp == 0x%08x\n", callerfp);
  #endif
  /*e: possibly announce caller's ebp */
  if (callerfp == 0) {
    /*s: possibly shout about finishing tries */
    #if NOISY
        fprintf(stderr, "finished C walk with callerfp = 0x%08x, tries = %d\n",
                (unsigned)callerfp, a->u.cwalk.tries);
    #endif
    /*e: possibly shout about finishing tries */
    return 0;
  }
  for(i = 0; i < NUM_REGS; i++)  /* registers cannot be restored */
    a->regs[i] = NULL;
  a->regs[FP_REG] = (Cmm_Word*)a->vfp;  /* point to location of caller's fp */

  ra = ((Cmm_Codeptr *)a->vfp)[1];
  entry = Cmm_lookup_entry(ra);
  if (entry) { /* next frame is a C-- frame */
    if (Cmm_is_thread_start_frame(entry, ra))
      return 0;  /* such a frame must not be seen */
    a->rtdata = entry;
    /* deallocation point = vfp + 8         (because caller deallocates args)
       deallocation point = callervfp + entry->outalloc

       so  callervfp = vfp + 8 - entry-outalloc  */
    a->vfp += 8 - Cmm_as_offset(entry->outalloc);
    a->pc = ra;  /* for debugging only */
    a->methods = Cmm_cmm_frame_methods;
  } else {
    a->vfp = (char *)callerfp;
    a->pc  = ra;  /* for debugging only */
  }
  return 1;
}
/*e: gcc-linux.c  */
/*e: gcc-linux.c */
