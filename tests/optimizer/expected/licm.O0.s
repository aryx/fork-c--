.globl licm
.section .data
Cmm.ref_to_global_area:
/* reference to global-register signature */
.long Cmm.globalsig.aQOYZWMPACZAJaMABGMOZeCCPY
.section .text
licm:
	leal 4294967284(%esp), %esp
	movl %eax,%eax
	leal 12(%esp), %ecx
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
	leal 12(%esp), %edx
	movl (%edx),%edx
	movl %edx,%edx
.Linitialize_continuations_l3:
.Lproc_body_start_l2:
	movl %ebx,(%esp)
	movl $0,%ebx
	movl %ebx,%ebx
	movl %ebp,8(%esp)
	movl %esi,4(%esp)
L:
	movl $2,%ebp
	movl %eax,%esi
	imull %ebp,%esi
	movl %esi,%esi
.Lbranch_target_l7:
.Lbranch_target_l11:
	cmpl %ecx,%ebx
	jb .Lbranch_target_l6
.Lbranch_target_l5:
	nop
	movl %ebx,%eax
	leal 12(%esp), %ecx
	movl $4,%ebx
	movl %ecx,%ecx
	addl %ebx,%ecx
	movl %edx,(%ecx)
	movl %edi,%edi
	movl 4(%esp),%esi
	movl %esi,%esi
	movl 8(%esp),%ebp
	movl %ebp,%ebp
	movl (%esp),%ebx
	movl %ebx,%ebx
	leal 16(%esp), %esp
	ret
.Lbranch_target_l6:
	movl %ebx,%ebx
	addl %esi,%ebx
	movl %ebx,%ebx
	jmp L
.section .text
