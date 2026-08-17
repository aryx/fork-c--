/*s: pcmap.h */
#ifndef PC_MAP_H
#define PC_MAP_H

/*s: exported type, procedure, and macro declarations */
typedef struct coded_location { Cmm_Word l; } location;
typedef enum { DEAD=0, REGISTER=1, OFFSET=2 } loctype; 
loctype  Cmm_loctype(location l);
unsigned Cmm_as_register(location l);
intptr_t Cmm_as_offset  (location l);
#define isdead(LOC) ((LOC).l == 0)
/*x: exported type, procedure, and macro declarations */
typedef struct {
  Cmm_Codeptr ra;
  struct cmm_pc_map_entry* entry;
} pc_map_index;

typedef struct cmm_pc_map_entry pc_map_entry;
struct cmm_pc_map_entry {
  location   inalloc;          /* incoming dealloc point on stack (always offset) */
  location   outalloc;         /* outgoing dealloc point on stack (always offset) */
  location   return_addressp;  /* where return address is saved */
  struct sd *stackdata_table;
  Cmm_Word   num_registers;
  Cmm_Word   num_locals;
  Cmm_Word   num_spans;
  Cmm_Word   cont_block_size;
  location   data[2]; /* registers, locals, continuations, spans */
};
/*x: exported type, procedure, and macro declarations */
struct sd {
  unsigned num_entries;
  location entries[1];  /* variable length */
};
/*x: exported type, procedure, and macro declarations */
struct reg {
  unsigned index;               /* machine-specific index */
  location saved;               /* where saved (or dead) */
};
/*x: exported type, procedure, and macro declarations */
struct conts {
  unsigned num_entries;
  int entries[1]; /* variable length (offset of contblock) */
};

struct contblock {
  unsigned num_vars;
  Cmm_Codeptr pc;
  location sp;
  struct contarg {
    Cmm_Word localnum;
    Cmm_Word ctype;
  } vars[1]; /* variable length */
};
/*x: exported type, procedure, and macro declarations */
enum ctypes { CHAR = 0, DOUBLE = 1, FLOAT = 2, INT = 3, LONGDOUBLE = 4, LONGINT = 5
            , LONGLONGINT = 6, SHORT = 7, SIGNEDCHAR = 8, UNSIGNEDCHAR = 9
            , UNSIGNEDLONG = 10, UNSIGNEDSHORT = 11, UNSIGNEDINT = 12
            , UNSIGNEDLONGLONG = 13, ADDRESS = 14 
            };
/*x: exported type, procedure, and macro declarations */
#define registersA(e)     ((e)->data)
#define localsA(e)        (registersA(e)     + 2 * (e)->num_registers)
#define continuationsA(e) (localsA(e)        +     (e)->num_locals)
#define spansA(e)         (continuationsA(e) +     (e)->cont_block_size)

#define registers(e)      ((struct reg*)registersA(e))
#define locals(e)         ((location *)localsA(e))
#define continuations(e)  ((struct conts *)continuationsA(e))
#define spans(e)          ((Cmm_Word*)spansA(e))
/*x: exported type, procedure, and macro declarations */
pc_map_entry* Cmm_lookup_entry(const Cmm_Codeptr caller);
pc_map_entry* Cmm_empty_pcmap_entry;
pc_map_entry* Cmm_thread_start_up_pcmap_entry;
pc_map_entry* Cmm_thread_start_dn_pcmap_entry;
/*x: exported type, procedure, and macro declarations */
typedef void (*Cmm_span_shower)(unsigned key, void *value, void *closure);
void Cmm_show_map(Cmm_span_shower, void *); /* for debugging */
void Cmm_show_map_entry(pc_map_entry *entry, int index, Cmm_Codeptr ra,
                        Cmm_span_shower show_span, void *closure);
/*e: exported type, procedure, and macro declarations */

#endif /* PC_MAP_H */
/*e: pcmap.h */
