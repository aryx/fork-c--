.globl max
.section .data
Cmm.ref_to_global_area:
/* reference to global-register signature */
.long Cmm.globalsig.aQOYZWMPACZAJaMABGMOZeCCPY
.section .text
max:
	leal 4294967288(%esp), %esp
	movl %eax,%eax
	leal 8(%esp), %ecx
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
	leal 8(%esp), %edx
	movl (%edx),%edx
	movl %edx,%edx
.Linitialize_continuations_l3:
.Lproc_body_start_l2:
.Lbranch_target_l8:
.Lbranch_target_l12:
	cmpl %ecx,%eax
	ja .Lbranch_target_l7
.Lbranch_target_l6:
	movl %ecx,%ecx
	movl %ecx,(%esp)
	jmp .Lbranch_target_l5
.Lbranch_target_l7:
	movl %eax,%eax
	movl %eax,(%esp)
.Lbranch_target_l5:
	nop
	movl (%esp),%eax
	movl %eax,%eax
	leal 8(%esp), %ecx
	movl %ebx,4(%esp)
	movl $4,%ebx
	movl %ecx,%ecx
	addl %ebx,%ecx
	movl %edx,(%ecx)
	movl %edi,%edi
	movl %esi,%esi
	movl %ebp,%ebp
	movl 4(%esp),%ebx
	movl %ebx,%ebx
	leal 12(%esp), %esp
	ret
.section .text
