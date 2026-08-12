.globl at
.section .data
Cmm.ref_to_global_area:
# reference to global-register signature
.long Cmm.globalsig.aQOYZWMPACZAJaMABGMOZeCCPY
.section .text
at:
	addi %r1,%r1,-64
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
	addi %r5,0,0
	addi %r5,%r5,4
	mullw  %r4,%r5,%r4
	add %r3,%r3,%r4
	lwz %r3,0(%r3)
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
	addi %r1,%r1,64
	blr
.section .text
