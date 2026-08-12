.globl at
.section .data
Cmm.ref_to_global_area:
/* reference to global-register signature */
.long Cmm.globalsig.aQOYZWMPACZAJaMABGMOZeCCPY
.section .text
at:
	leal 4294967292(%esp), %esp
	movl %eax,%eax
	leal 4(%esp), %ecx
	movl $4,%edx
	movl %ecx,%ecx
	addl %edx,%ecx
	movl (%ecx),%ecx
	movl %ecx,%ecx
	nop
	movl %edi,%edi
	movl %esi,%esi
	movl %ebp,%ebp
	movl %ebx,%ebx
	leal 4(%esp), %edx
	movl (%edx),%edx
	movl %edx,%edx
.Linitialize_continuations_l3:
.Lproc_body_start_l2:
	movl %ebx,(%esp)
	movl $4,%ebx
	movl %ebx,%ebx
	imull %ecx,%ebx
	movl %eax,%eax
	addl %ebx,%eax
	movl (%eax),%eax
	movl %eax,%eax
	nop
	movl %eax,%eax
	leal 4(%esp), %ecx
	movl $4,%ebx
	movl %ecx,%ecx
	addl %ebx,%ecx
	movl %edx,(%ecx)
	movl %edi,%edi
	movl %esi,%esi
	movl %ebp,%ebp
	movl (%esp),%ebx
	movl %ebx,%ebx
	leal 8(%esp), %esp
	ret
.section .text
