/*s: gcc-linux.c */
/*s: gcc-linux.c  */
#include "qc--runtime.h"
#include "pcmap.h"
#include <assert.h>
#include <stdio.h>

#define EBP 9 /* r[5] */
/*x: gcc-linux.c  */
void Cmm_init_c_frame(Cmm_Activation *a, char *young_in_overflow) {
  Cmm_Word ebp;
  assert(a->regs[EBP]);
  ebp = *a->regs[EBP];
  a->vfp = (char *) ebp;
  a->u.cwalk.tries = 0;   /* logging only */
}
extern struct cmm_activation_methods *Cmm_cmm_frame_methods;
/*x: gcc-linux.c  */
#define NOISY 0

int Cmm_c_change_activation(Cmm_Activation *a) {
  Cmm_Word callerebp;
  Cmm_Codeptr ra;
  pc_map_entry *entry;
  int i;

  assert(a->vfp != 0);   /* protect against an unexpected error */
  /*s: possibly shout about our departure, showing arguments */
  #if NOISY
   {
    Cmm_Word *myebp = (Cmm_Word*) a->vfp;
    fprintf(stderr, "Leaving C activation; my ebp = %p, ra = %p, caller's EBP = %p\n",
            (void*)myebp, (void*)myebp[1], (void*)myebp[0]);
    for (i = 0; i < 3; i++) fprintf(stderr, "  arg[%d] = 0x%08x\n", i, myebp[i+2]);
   }
  #endif
  /*e: possibly shout about our departure, showing arguments */
  callerebp = *(Cmm_Word *)a->vfp;
  /*s: possibly announce caller's ebp */
  #if 0
    fprintf(stderr, "walked to caller %%ebp == 0x%08x\n", callerebp);
  #endif
  /*e: possibly announce caller's ebp */
  if (callerebp == 0) {
    /*s: possibly shout about finishing tries */
    #if NOISY
        fprintf(stderr, "finished C walk with callerebp = 0x%08x, tries = %d\n",
                (unsigned)callerebp, a->u.cwalk.tries);
    #endif
    /*e: possibly shout about finishing tries */
    return 0;
  }
  for(i = 0; i < NUM_REGS; i++)  /* registers cannot be restored */
    a->regs[i] = NULL;
  a->regs[EBP] = (Cmm_Word*)a->vfp;  /* point to location of caller's EBP */

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
    a->vfp = (char *)callerebp;
    a->pc  = ra;  /* for debugging only */
  }
  return 1;
}
/*e: gcc-linux.c  */
/*e: gcc-linux.c */
