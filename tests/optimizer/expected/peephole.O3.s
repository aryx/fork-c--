.globl chain
.section .data
Cmm.ref_to_global_area:
/* reference to global-register signature */
.long Cmm.globalsig.aQOYZWMPACZAJaMABGMOZeCCPY
.section .text
chain:
	movl %esp,%ecx
	movl (%ecx),%edx
	movl (%ecx),%ecx
.Linitialize_continuations_l3:
.Lproc_body_start_l2:
	movl $1,%edx
	addl $1,%eax
	leal 1(%eax), %edx
	movl $1,%edx
	leal 1(%eax), %edx
	addl $1,%edx
	leal 2(%eax), %eax
	movl %esp,%edx
	movl $0,%edx
	movl %esp,%edx
	addl $0,%edx
	movl %ecx,(%esp)
	ret
.section .text
