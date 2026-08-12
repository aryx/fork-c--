.globl chain
.section .data
Cmm.ref_to_global_area:
/* reference to global-register signature */
.long Cmm.globalsig.aQOYZWMPACZAJaMABGMOZeCCPY
.section .text
chain:
	movl %esp,%ecx
	movl (%ecx),%edx
.Linitialize_continuations_l3:
.Lproc_body_start_l2:
	movl $0,%ecx
L1:
.Lbranch_target_l7:
.Lbranch_target_l11:
	cmpl %eax,%ecx
	jb .Lbranch_target_l6
.Lbranch_target_l5:
	movl %ecx,%eax
	movl %edx,(%esp)
	ret
.Lbranch_target_l6:
	leal 1(%ecx), %ecx
	jmp L1
.section .text
