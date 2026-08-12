.globl chain
.section .data
Cmm.ref_to_global_area:
/* reference to global-register signature */
.long Cmm.globalsig.aQOYZWMPACZAJaMABGMOZeCCPY
.section .text
chain:
	movl %esp,%ecx
	movl (%ecx),%ecx
.Linitialize_continuations_l3:
.Lproc_body_start_l2:
	leal 2(%eax), %eax
	movl %ecx,(%esp)
	ret
.section .text
