.globl licm
.section .data
Cmm.ref_to_global_area:
/* reference to global-register signature */
.long Cmm.globalsig.aQOYZWMPACZAJaMABGMOZeCCPY
.section .text
licm:
	movl %esp,%ecx
	movl (%ecx),%edx
.Linitialize_continuations_l3:
.Lproc_body_start_l2:
	movl $0,%ecx
	imull $2,%eax
L:
.Lbranch_target_l7:
.Lbranch_target_l11:
	cmpl 4(%esp),%ecx
	jb .Lbranch_target_l6
.Lbranch_target_l5:
	movl %ecx,%eax
	movl %edx,4(%esp)
	leal 4(%esp), %esp
	ret
.Lbranch_target_l6:
	addl %eax,%ecx
	jmp L
.section .text
