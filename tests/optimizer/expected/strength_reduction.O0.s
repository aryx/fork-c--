.globl strength_reduction
.section .data
Cmm.ref_to_global_area:
/* reference to global-register signature */
.long Cmm.globalsig.aQOYZWMPACZAJaMABGMOZeCCPY
.section .text
strength_reduction:
	leal 4294967284(%esp), %esp
	movl %eax,%eax
	nop
	movl %edi,%edi
	movl %esi,%esi
	movl %ebp,%ebp
	movl %ebx,%ebx
	leal 12(%esp), %ecx
	movl (%ecx),%ecx
	movl %ecx,%ecx
.Linitialize_continuations_l3:
.Lproc_body_start_l2:
	movl $0,%edx
	movl %edx,%edx
	movl %ebx,(%esp)
	movl $0,%ebx
	movl %ebx,%ebx
	movl %ebp,8(%esp)
	movl %ebx,4(%esp)
L:
.Lbranch_target_l7:
.Lbranch_target_l11:
	cmpl %eax,%edx
	jb .Lbranch_target_l6
.Lbranch_target_l5:
	nop
	movl 4(%esp),%eax
	movl %eax,%eax
	leal 12(%esp), %edx
	movl $0,%ebx
	movl %edx,%edx
	addl %ebx,%edx
	movl %ecx,(%edx)
	movl %edi,%edi
	movl %esi,%esi
	movl 8(%esp),%ebp
	movl %ebp,%ebp
	movl (%esp),%ebx
	movl %ebx,%ebx
	leal 12(%esp), %esp
	ret
.Lbranch_target_l6:
	movl $4,%ebx
	movl %edx,%ebp
	imull %ebx,%ebp
	movl %ebp,%ebp
	movl $1,%ebx
	movl %edx,%edx
	addl %ebx,%edx
	movl %edx,%edx
	movl %ebp,4(%esp)
	jmp L
.section .text
