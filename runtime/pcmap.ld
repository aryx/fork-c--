/*s: pcmap.ld */
SECTIONS {
 .rodata : {
   . =  (. + 3) & ~ 3;
   Cmm_pc_map = .;
   *(.pcmap)
   Cmm_pc_map_limit = .;
   *(.pcmap_data)
 }
}
/*e: pcmap.ld */
