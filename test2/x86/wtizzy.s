.globl main
.globl Cmm.global_area
.globl Cmm.globalsig.bcPDWKVXdYZZBPSFTVVJOBXQNc
.section .data
/* memory for global registers */
Cmm.globalsig.bcPDWKVXdYZZBPSFTVVJOBXQNc:
Cmm.global_area:
.globl Cmm_stack_growth
.section .data
.align 4
Cmm_stack_growth:
.long 0xffffffff
.section .text
f:
	leal -24(%esp), %esp
	leal 24(%esp), %eax
	movl (%eax),%eax
.Linitialize_continuations_l5:
.Lproc_body_start_l4:
	movl $388380743,%ecx
	leal 24(%esp), %edx
	movl %eax,20(%esp)
	movl $-24,%eax
	addl %eax,%edx
	movl %ecx,(%edx)
	movl $9975624,%eax
	leal 24(%esp), %ecx
	movl $-20,%edx
	addl %edx,%ecx
	movl %eax,(%ecx)
	movl $32,%eax
	leal 24(%esp), %ecx
	movl $-8,%edx
	addl %edx,%ecx
	movl %eax,(%ecx)
	leal 24(%esp), %eax
	movl $-8,%ecx
	addl %ecx,%eax
	fildl (%eax)
	leal 24(%esp), %eax
	movl $-16,%ecx
	addl %ecx,%eax
	fstpl (%eax)
	call c_fun
.Lcall_successor_l9:
	leal 24(%esp), %eax
	movl $0,%ecx
	addl %ecx,%eax
	movl 20(%esp),%ecx
	movl %ecx,(%eax)
	leal 24(%esp), %esp
	ret
.section .pcmap_data
.Lstackdata_l16:
.long 0
.section .pcmap
.long .Lcall_successor_l9
.long .Lframe_l17
.section .pcmap_data
.Lframe_l17:
.long 0x80000004
.long 0xffffffe8
.long 0xfffffffc
.long .Lstackdata_l16
.long 0
.long 0
.long 0
.long 1
.long 0
.section .text
.section .text
c_fun:
	leal -40(%esp), %esp
	leal 40(%esp), %eax
	movl $4,%ecx
	addl %ecx,%eax
	leal 40(%esp), %ecx
	movl $-20,%edx
	addl %edx,%ecx
	fildq (%eax)
	fistpq (%ecx)
	leal 40(%esp), %eax
	movl $12,%ecx
	addl %ecx,%eax
	leal 40(%esp), %ecx
	movl $-12,%edx
	addl %edx,%ecx
	fildq (%eax)
	fistpq (%ecx)
	leal 40(%esp), %eax
	movl (%eax),%eax
.Linitialize_continuations_l19:
.Lproc_body_start_l18:
	leal fmt,%ecx
	leal 40(%esp), %edx
	movl %eax,36(%esp)
	movl $-40,%eax
	addl %eax,%edx
	movl %ecx,(%edx)
	leal 40(%esp), %eax
	movl $-20,%ecx
	addl %ecx,%eax
	leal 40(%esp), %ecx
	movl $-36,%edx
	addl %edx,%ecx
	fildq (%eax)
	fistpq (%ecx)
	leal 40(%esp), %eax
	movl $-12,%ecx
	addl %ecx,%eax
	leal 40(%esp), %ecx
	movl $-28,%edx
	addl %edx,%ecx
	fildq (%eax)
	fistpq (%ecx)
	call printf
.Lcall_successor_l23:
	leal 40(%esp), %eax
	movl $0,%ecx
	addl %ecx,%eax
	movl 36(%esp),%ecx
	movl %ecx,(%eax)
	leal 40(%esp), %esp
	ret
.section .pcmap_data
.Lstackdata_l30:
.long 0
.section .pcmap
.long .Lcall_successor_l23
.long .Lframe_l31
.section .pcmap_data
.Lframe_l31:
.long 0x80000004
.long 0xffffffd8
.long 0xfffffffc
.long .Lstackdata_l30
.long 0
.long 3
.long 0
.long 1
.long 0xffffffec
.long 0xfffffff4
.long 0
.long 0
.section .text
.section .text
main:
	leal -4(%esp), %esp
	leal 4(%esp), %eax
	movl $4,%ecx
	addl %ecx,%eax
	movl (%eax),%eax
	leal 4(%esp), %eax
	movl $8,%ecx
	addl %ecx,%eax
	movl (%eax),%eax
	leal 4(%esp), %eax
	movl (%eax),%eax
.Linitialize_continuations_l33:
.Lproc_body_start_l32:
	movl %eax,(%esp)
	call f
.Lcall_successor_l37:
	movl $0,%eax
	leal 4(%esp), %ecx
	movl $0,%edx
	addl %edx,%ecx
	movl (%esp),%edx
	movl %edx,(%ecx)
	leal 4(%esp), %esp
	ret
.section .pcmap_data
.Lstackdata_l44:
.long 0
.section .pcmap
.long .Lcall_successor_l37
.long .Lframe_l45
.section .pcmap_data
.Lframe_l45:
.long 0x80000004
.long 0xfffffffc
.long 0xfffffffc
.long .Lstackdata_l44
.long 0
.long 2
.long 0
.long 1
.long 0
.long 0
.long 0
.section .text
.section .data
fmt:
.byte 99
.byte 97
.byte 108
.byte 108
.byte 101
.byte 100
.byte 32
.byte 99
.byte 95
.byte 102
.byte 117
.byte 110
.byte 40
.byte 48
.byte 120
.byte 37
.byte 48
.byte 49
.byte 54
.byte 108
.byte 108
.byte 120
.byte 44
.byte 32
.byte 37
.byte 108
.byte 102
.byte 41
.byte 10
.byte 0
