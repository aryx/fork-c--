/*s: pcmap.c */
/*s: pcmap.c  */
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "qc--runtime.h"
#include "pcmap.h"
/*x: pcmap.c  */
/* claude: (Cmm_Word)1, not a bare 1 (int) - shifting by sizeof(location)*8-1
 * is 63 on a 64-bit target now that Cmm_Word is uintptr_t, which is
 * undefined behavior for a 32-bit int shift. */
#define MKOFFSET(N) { ((Cmm_Word)1 << (sizeof(location)*8 - 1)) | (N) }
#define MKDEAD      { 0 }
static struct sd nostackdata = { 0, { MKDEAD } };
/*x: pcmap.c  */
static struct cmm_pc_map_entry empty = {
  MKOFFSET(0),
  MKOFFSET(0),
  MKOFFSET(0),
  &nostackdata,
  0, 0, 0, 0, { MKDEAD }
};
pc_map_entry* Cmm_empty_pcmap_entry = &empty;
/*x: pcmap.c  */
static struct cmm_pc_map_entry startdn = {
  MKOFFSET(0),
  MKOFFSET(0),
  MKDEAD,
  &nostackdata,
  0, 2, 0, 0, { MKOFFSET(0), MKOFFSET(sizeof(Cmm_Word)) }
};
static struct cmm_pc_map_entry startup = {
  MKOFFSET(0),
  MKOFFSET(0),
  MKDEAD,
  &nostackdata,
  0, 2, 0, 0, { MKOFFSET(0), MKOFFSET(-sizeof(Cmm_Word)) }
};
pc_map_entry* Cmm_thread_start_up_pcmap_entry = &startup;
pc_map_entry* Cmm_thread_start_dn_pcmap_entry = &startdn;
/*x: pcmap.c  */
/* claude: on every ELF host, Cmm_pc_map/Cmm_pc_map_limit are bound by
 * pcmap.ld (a GNU-ld "-T" linker-script fragment that places every input
 * object's .pcmap section together and defines these two symbols around
 * it - see that file). Mach-O has no equivalent to "-T": ld64 does not
 * support linker scripts at all. Empirically confirmed replacement: ld64
 * already auto-synthesizes a "section$start$SEGMENT$section"/
 * "section$end$SEGMENT$section" symbol pair for every section, with no
 * script needed (verified with a standalone hand-assembled test .s/.c
 * pair before relying on it here - see docs/claude_notes/notes_arm64.txt).
 * arm64asm.ml emits the pcmap section as ".section __DATA,pcmap", so the
 * two symbols below are exactly its start/end. No macOS counterpart to
 * pcmap.ld is needed at all - this #ifdef is the whole story. */
#ifdef __APPLE__
extern pc_map_index Cmm_pc_map[]       __asm("section$start$__DATA$pcmap");
extern pc_map_index Cmm_pc_map_limit[] __asm("section$end$__DATA$pcmap");
#else
extern pc_map_index Cmm_pc_map[];
extern pc_map_index Cmm_pc_map_limit[];
#endif
#define pc_map_size (Cmm_pc_map_limit - Cmm_pc_map)
/*x: pcmap.c  */
static int is_sorted(void) {
  pc_map_index *p;
  for (p = Cmm_pc_map+1; p < Cmm_pc_map_limit; p++)
    if ((unsigned)p[-1].ra > (unsigned)p[0].ra) {  
      /*s: conditionally spray information about the [[pc_map]] array */
      #define SHOW_UNSORTED 1
      { char *debug = getenv("QCDEBUG");
        if (SHOW_UNSORTED || (debug && strstr(debug, "pcmap"))) {
          fprintf(stderr, "C-- Surprise! PCMAP array is unsorted!\n");
          for (p = Cmm_pc_map; p < Cmm_pc_map_limit; p++) {
            fprintf(stderr, "  ra = %8p", (void*)p->ra);
            if (p > Cmm_pc_map && (unsigned)p[-1].ra > (unsigned)p[0].ra)
              fprintf(stderr, " *");
            fprintf(stderr, "\n");
          }
        }
      }
      /*e: conditionally spray information about the [[pc_map]] array */
      return 0;
    }
  /*s: conditionally announce that [[pc_map]] is sorted */
  { char *debug = getenv("QCDEBUG");
    if (debug && strstr(debug, "pcmap"))
      fprintf(stderr, "C-- info: PCMAP array is sorted, as expected\n");
  }
  /*e: conditionally announce that [[pc_map]] is sorted */
  return 1;
}
/*x: pcmap.c  */
static int compare(const void *x, const void *y) {
  const pc_map_index *p = y;
  return (unsigned) x - (unsigned) p->ra;
}

static pc_map_entry *binlookup(const Cmm_Codeptr caller) {
  pc_map_index *p;
  p = bsearch((void*)caller, Cmm_pc_map, pc_map_size, sizeof(Cmm_pc_map[0]), compare);
  if (p)
    return p->entry;
  else
    return NULL;
}
/*x: pcmap.c  */
static pc_map_entry* linlookup(const Cmm_Codeptr caller) {
  unsigned i = 0;
  for(i = 0; i < pc_map_size; ++i)
    if (Cmm_pc_map[i].ra == caller)
      return Cmm_pc_map[i].entry;
  return NULL;
}
/*x: pcmap.c  */
pc_map_entry* Cmm_lookup_entry(const Cmm_Codeptr caller)
{
  static pc_map_entry *(*lookup)(const Cmm_Codeptr caller) = NULL;
  if (lookup == NULL)
    lookup = is_sorted() ? binlookup : linlookup;
  return lookup(caller);
}
/*x: pcmap.c  */
/* claude: local's own width, not a fixed 32 bits - GOODBITS has to match
 * whatever width qc-- actually packed this location as (see qc--runtime.h's
 * Cmm_Word claude: comment): 30 on every existing 32-bit backend
 * (unchanged), 62 on a 64-bit one. */
#define GOODBITS ((intptr_t)sizeof(local)*8 - 2)

loctype Cmm_loctype(location l) {
  intptr_t local = (intptr_t) l.l;
  local = local >> GOODBITS;
  if (local & 2)
    return OFFSET;
  else
    return (loctype) local;
}
/*x: pcmap.c  */
unsigned Cmm_as_register(location l) {
  intptr_t local = (intptr_t) l.l;
  intptr_t mask   = GOODBITS / 2;
  intptr_t slice  = (local & (((intptr_t)1 << GOODBITS) - 1))
            >> mask;
  intptr_t offset =  local & (((intptr_t)1 << mask    ) - 1);
  assert(Cmm_loctype(l) == REGISTER);

  if (slice) {
    fprintf(stderr, "register slices not supported.\n");
    assert(0);
  }
  return (unsigned) offset;
}
/*x: pcmap.c  */
intptr_t Cmm_as_offset(location l) {
  intptr_t local = (intptr_t) l.l;
  return (local << 1)
      >> 1;
}
/*x: pcmap.c  */
static void printloc(location loc) {
  switch (Cmm_loctype(loc)) {
    case REGISTER:
      printf ("REG %d", Cmm_as_register(loc));
      break;
    case DEAD:
      printf ("DEAD");
      break;
    case OFFSET:
      printf ("OFFSET %d", Cmm_as_offset(loc));
      break;
    default:
      printf ("<MALFORMED %X>", loc.l);
      break;
  }
}
/*x: pcmap.c  */
/*s: private show functions */
const char *typestring(enum ctypes t) {
#define xx(T) case T: return #T;
  switch(t) {
    xx(CHAR) xx(DOUBLE) xx(FLOAT) xx(INT) xx(LONGDOUBLE) xx(LONGINT)
    xx(LONGLONGINT) xx(SHORT) xx(SIGNEDCHAR) xx(UNSIGNEDCHAR)
    xx(UNSIGNEDLONG) xx(UNSIGNEDSHORT) xx(UNSIGNEDINT)
    xx(UNSIGNEDLONGLONG) xx(ADDRESS) 
    default: return "unknown-type";
  }
}
/*x: private show functions */
static void show_cont(struct conts *conts, int contnum, const char *indent) {
  struct contblock *block;
  int i;

  printf("%s    unwind cont%2d (", indent, contnum);
  block = (struct contblock *) ((Cmm_Word *) conts + conts->entries[contnum]);
  for (i = 0; i < block->num_vars; i++) {
    printf("%s%s local%d", i > 0 ? ", " : "", typestring(block->vars[i].ctype),
           block->vars[i].localnum);
  }
  printf(") = <pc=%p, sp=", (void*)block->pc);
  printloc(block->sp);
  printf(">\n");
}
/*e: private show functions */

void Cmm_show_map(Cmm_span_shower show_span, void *closure) {
  pc_map_index *idx;
  printf("pc_map_size %d\n", pc_map_size);
  for (idx = Cmm_pc_map; idx < Cmm_pc_map_limit; idx++) {
    Cmm_show_map_entry(idx->entry, idx - Cmm_pc_map, idx->ra, show_span, closure);
  }
}
/*x: pcmap.c  */
void Cmm_show_map_entry(pc_map_entry *entry, int index, Cmm_Codeptr ra,
                        Cmm_span_shower show_span, void *closure)
{
  struct sd *sdt;
  char *indent = index < 0 ? "    " : "";
  if (index >= 0) 
    printf("%sentry%3d @ %p (ra = %p):\n", indent, index, (void *)entry, (void *) ra);
  else
    printf("%sentry @ %p (ra = %p):\n", indent, (void *)entry, (void *) ra);
  printf("%s  inalloc = %d (coded %X), outalloc = %d (coded %X)\n"
         "%s  num_regs = %d, num_locals = %d, num_spans = %d,"
         " size of cont block = %d\n",
         indent,
         Cmm_as_offset(entry->inalloc),
         entry->inalloc.l,
         Cmm_as_offset(entry->outalloc),
         entry->outalloc.l,
         indent,
         entry->num_registers,
         entry->num_locals,
         entry->num_spans,
         entry->cont_block_size);
  printf("%s  return address at ", indent);
  printloc(entry->return_addressp);
  printf("\n");
  sdt = entry->stackdata_table;
  /*s: show locals */
  {
    int n = entry->num_locals;
    if (n == 0) {
      printf("%s    (no locals)\n", indent);
    } else {
      int i;
      for (i = 0; i < n; i++) {
        printf("%s    local%3d at ", indent, i);
        printloc((locals(entry))[i]);
        printf("\n");
      }
    }
  }
  /*e: show locals */
  /*s: show registers */
  {
    int r = entry->num_registers;
    int i;
    struct reg* regs = registers(entry);
    for (i = 0; i < r; i++) {
      printf("%s      pair %2d: caller register %d at ", indent, i, regs[i].index);
      printloc(regs[i].saved);
      printf("\n");
    }
  }
  /*e: show registers */
  /*s: show continuations */
  {
    int i;
    struct conts *conts;
    conts = continuations(entry);

    if (conts->num_entries == 0) {
      printf("%s    (no continuations)\n", indent);
    } else {
      for (i = 0; i < conts->num_entries; i++)
        show_cont(conts, i, indent);
    }
  }
  /*e: show continuations */
  /*s: show stack-data table [[sdt]] */
  assert(sdt);
  if (sdt->num_entries == 0) {
    printf ("%s    (no stackdata)\n", indent);
  } else {
    int i;
    for (i = 0; i < sdt->num_entries; i++) {
      printf("%s    stacklabel%3d = ", indent, i);
      printloc(sdt->entries[i]);
      printf("\n");
    }
  }
  /*e: show stack-data table [[sdt]] */
  /*s: show spans */
  { unsigned i;
    Cmm_Word *descs = spans(entry);
    for (i = 0; i < entry->num_spans; i++)
      show_span(i, (void *)descs[i], closure);
  }
  /*e: show spans */
}
/*e: pcmap.c  */
/*e: pcmap.c */
