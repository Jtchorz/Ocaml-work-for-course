	global main 
	section .text
main:
	sub	rsp, 104
	mov	qword [rsp + 24], 3
	mov	qword [rsp + 32], 4
	mov	r10, qword [rsp + 24]
	mov	qword [rsp + 8], r10
	mov	r10, qword [rsp + 32]
	add	qword [rsp + 8], r10
	mov	qword [rsp + 16], 5
	mov	rax, qword [rsp + 8]
	cqo
	idiv	qword [rsp + 16]
	mov	qword [rsp + 0], rdx
	mov	qword [rsp + 64], 7
	mov	qword [rsp + 72], 2
	mov	r10, qword [rsp + 64]
	mov	qword [rsp + 48], r10
	mov	r10, qword [rsp + 72]
	sub	qword [rsp + 48], r10
	mov	qword [rsp + 56], 5
	mov	rax, qword [rsp + 48]
	cqo
	idiv	qword [rsp + 56]
	mov	qword [rsp + 40], rax
	mov	r10, qword [rsp + 0]
	mov	qword [rsp + 88], r10
	mov	r10, qword [rsp + 40]
	mov	qword [rsp + 96], r10
	mov	rax, qword [rsp + 88]
	imul	qword [rsp + 96]
	mov	qword [rsp + 80], rax
	mov	rax, qword [rsp + 80]
	add	rsp, 104
	ret
