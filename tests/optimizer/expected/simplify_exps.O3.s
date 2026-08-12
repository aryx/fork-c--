.globl addzero
.section .data
Cmm.ref_to_global_area:
/* reference to global-register signature */
.long Cmm.globalsig.aQOYZWMPACZAJaMABGMOZeCCPY
.section .text
addzero:
	movl %esp,%ecx
	movl (%ecx),%ecx
.Linitialize_continuations_l3:
.Lproc_body_start_l2:
	movl %ecx,(%esp)
	ret
.section .text
