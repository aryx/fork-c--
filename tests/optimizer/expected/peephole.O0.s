.globl chain
.section .data
Cmm.ref_to_global_area:
/* reference to global-register signature */
.long Cmm.globalsig.aQOYZWMPACZAJaMABGMOZeCCPY
.section .text
chain:
	leal 4294967292(%esp), %esp
	movl %eax,%eax
	nop
	movl %edi,%edi
	movl %esi,%esi
	movl %ebp,%ebp
	movl %ebx,%ebx
	leal 4(%esp), %ecx
	movl (%ecx),%ecx
	movl %ecx,%ecx
.Linitialize_continuations_l3:
.Lproc_body_start_l2:
	movl $1,%edx
	movl %eax,%eax
	addl %edx,%eax
	movl %eax,%eax
	nop
	movl $1,%edx
	movl %eax,%eax
	addl %edx,%eax
	movl %eax,%eax
	leal 4(%esp), %edx
	movl %ebx,(%esp)
	movl $0,%ebx
	movl %edx,%edx
	addl %ebx,%edx
	movl %ecx,(%edx)
	movl %edi,%edi
	movl %esi,%esi
	movl %ebp,%ebp
	movl (%esp),%ebx
	movl %ebx,%ebx
	leal 4(%esp), %esp
	ret
.section .text
