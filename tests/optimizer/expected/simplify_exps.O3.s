.globl addzero
.section .data
Cmm.ref_to_global_area:
/* reference to global-register signature */
.long Cmm.globalsig.aQOYZWMPACZAJaMABGMOZeCCPY
.section .text
addzero:
	leal 4294967292(%esp), %esp
	leal 4(%esp), %ecx
	movl (%ecx),%ecx
.Linitialize_continuations_l3:
.Lproc_body_start_l2:
	leal 4(%esp), %edx
	movl %ebx,(%esp)
	movl $0,%ebx
	addl %ebx,%edx
	movl %ecx,(%edx)
	movl (%esp),%ebx
	leal 4(%esp), %esp
	ret
.section .text
