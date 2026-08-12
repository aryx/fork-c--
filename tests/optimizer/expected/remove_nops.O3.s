.globl foo
.section .data
Cmm.ref_to_global_area:
/* reference to global-register signature */
.long Cmm.globalsig.aQOYZWMPACZAJaMABGMOZeCCPY
.section .text
foo:
	movl %esp,%eax
	movl (%eax),%eax
.Linitialize_continuations_l3:
.Lproc_body_start_l2:
	movl %esp,%ecx
	movl $0,%edx
	addl %edx,%ecx
	movl %eax,(%ecx)
	ret
.section .text
