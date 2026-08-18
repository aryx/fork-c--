.globl strength_reduction
.section .data
Cmm.ref_to_global_area:
/* reference to global-register signature */
.long Cmm.globalsig.aQOYZWMPACZAJaMABGMOZeCCPY
.section .text
strength_reduction:
	leal 4294967292(%esp), %esp
	leal 4(%esp), %ecx
	movl (%ecx),%ecx
.Linitialize_continuations_l3:
.Lproc_body_start_l2:
	movl $0,%edx
	movl %ecx,(%esp)
	movl $0,%ecx
L:
.Lbranch_target_l7:
.Lbranch_target_l11:
	cmpl %eax,%edx
	jb .Lbranch_target_l6
.Lbranch_target_l5:
	movl %ecx,%eax
	movl (%esp),%ecx
	movl %ecx,4(%esp)
	leal 4(%esp), %esp
	ret
.Lbranch_target_l6:
	leal 4(%ecx), %ecx
	leal 1(%edx), %edx
	jmp L
.section .text
