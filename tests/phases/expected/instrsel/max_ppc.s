.globl max
.section .data
Cmm.ref_to_global_area:
# reference to global-register signature
.long Cmm.globalsig.aQOYZWMPACZAJaMABGMOZeCCPY
.section .text
max:
	addi %r1,%r1,-80
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
.Linitialize_continuations_l3:
.Lproc_body_start_l2:
.Lbranch_target_l8:
.Lbranch_target_l12:
	cmplw %cr0,%r3,%r4
	bgt .Lbranch_target_l7
.Lbranch_target_l6:
	nop
	stw %r4,64(%r1)
	b .Lbranch_target_l5
.Lbranch_target_l7:
	nop
	stw %r3,64(%r1)
.Lbranch_target_l5:
	nop
	lwz %r3,64(%r1)
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	addi %r1,%r1,80
	blr
.section .text
