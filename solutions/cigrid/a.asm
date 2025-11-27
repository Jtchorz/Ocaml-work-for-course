	global main 
section .text
main:
	sub	rsp, 128
	mov	qword[rsp + 0], 0
	mov	qword[rsp + 8], 0
	mov	r10, qword[rsp + 0]
	mov	qword[rsp + 16], r10
	mov	qword[rsp + 24], 10
	xor	r11, r11
	mov	r10, qword[rsp + 24]
	sub	qword[rsp + 16], r10
	setl	r11b
	mov	rax, r11
	cmp	rax, 0
	jne	_while_body0
	jmp	_after_while1
_while_body0:
	mov	r10, qword[rsp + 8]
	mov	qword[rsp + 32], r10
	mov	qword[rsp + 40], 7
	xor	r11, r11
	mov	r10, qword[rsp + 40]
	sub	qword[rsp + 32], r10
	setl	r11b
	mov	rax, r11
	cmp	rax, 0
	jne	_while_body3
	jmp	_after_while4
_while_body3:
	mov	r10, qword[rsp + 8]
	mov	qword[rsp + 48], r10
	mov	qword[rsp + 56], 1
	mov	r10, qword[rsp + 48]
	mov	qword[rsp + 8], r10
	mov	r10, qword[rsp + 56]
	add	qword[rsp + 8], r10
	jmp	_while_cond5
_while_cond5:
	mov	r10, qword[rsp + 8]
	mov	qword[rsp + 64], r10
	mov	qword[rsp + 72], 7
	xor	r11, r11
	mov	r10, qword[rsp + 72]
	sub	qword[rsp + 64], r10
	setl	r11b
	mov	rax, r11
	cmp	rax, 0
	jne	_while_body3
	jmp	_after_while4
_after_while4:
	mov	r10, qword[rsp + 0]
	mov	qword[rsp + 80], r10
	mov	r10, qword[rsp + 8]
	mov	qword[rsp + 88], r10
	mov	r10, qword[rsp + 80]
	mov	qword[rsp + 0], r10
	mov	r10, qword[rsp + 88]
	add	qword[rsp + 0], r10
	jmp	_while_cond2
_while_cond2:
	mov	r10, qword[rsp + 0]
	mov	qword[rsp + 96], r10
	mov	qword[rsp + 104], 10
	xor	r11, r11
	mov	r10, qword[rsp + 104]
	sub	qword[rsp + 96], r10
	setl	r11b
	mov	rax, r11
	cmp	rax, 0
	jne	_while_body0
	jmp	_after_while1
_after_while1:
	mov	r10, qword[rsp + 0]
	mov	qword[rsp + 112], r10
	mov	r10, qword[rsp + 8]
	mov	qword[rsp + 120], r10
	mov	rax, qword[rsp + 112]
	add	rax, qword[rsp + 120]
	jmp	________
________:
	add	rsp, 128
	ret
