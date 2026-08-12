.globl foo
.section .data
Cmm.ref_to_global_area:
/* reference to global-register signature */
.long Cmm.globalsig.aQOYZWMPACZAJaMABGMOZeCCPY
.section .text
foo:
	nop
	movl %eax,%eax
	nop
	movl %edi,%edi
	movl %esi,%esi
	movl %ebp,%ebp
	movl %ebx,%ebx
	movl %esp,%eax
	movl (%eax),%eax
	movl %eax,%eax
.Linitialize_continuations_l3:
.Lproc_body_start_l2:
	nop
	movl %esp,%ecx
	movl $0,%edx
	movl %ecx,%ecx
	addl %edx,%ecx
	movl %eax,(%ecx)
	movl %edi,%edi
	movl %esi,%esi
	movl %ebp,%ebp
	movl %ebx,%ebx
	nop
	ret
.section .text
