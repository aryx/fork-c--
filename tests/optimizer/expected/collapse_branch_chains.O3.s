.globl chain
.section .data
Cmm.ref_to_global_area:
/* reference to global-register signature */
.long Cmm.globalsig.aQOYZWMPACZAJaMABGMOZeCCPY
.section .text
chain:
	leal 4294967292(%esp), %esp
	leal 4(%esp), %ecx
	movl (%ecx),%edx
	movl (%ecx),%ecx
.Linitialize_continuations_l3:
.Lproc_body_start_l2:
	movl $0,%edx
	movl $0,%edx
	movl %ecx,(%esp)
L1:
.Lbranch_target_l7:
.Lbranch_target_l11:
	cmpl %eax,%edx
	jb .Lbranch_target_l6
.Lbranch_target_l5:
	movl %edx,%eax
	leal 4(%esp), %ecx
	movl $0,%ecx
	leal 4(%esp), %ecx
	addl $0,%ecx
	movl (%esp),%ecx
	movl %ecx,4(%esp)
	leal 4(%esp), %esp
	ret
.Lbranch_target_l6:
	movl $1,%ecx
	movl %edx,%ecx
	addl $1,%ecx
	leal 1(%edx), %edx
	jmp L1
.section .text
