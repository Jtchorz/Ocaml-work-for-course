	extern	islower
	extern	tolower
	extern	putchar
	global main 
section .text
main:
	sub	rsp, 40
	mov	qword[rsp + 0], 65
	mov	qword[rsp + 8], rax
	call	$islower
	mov	rdi, qword[rsp + 24]
	mov	r10, qword[rsp + 0]
	mov	qword[rsp + 24], r10
	mov	qword[rsp + 16], 0
	xor	r11, r11
	mov	r10, qword[rsp + 16]
	sub	qword[rsp + 8], r10
	sete	r11b
	mov	rax, r11
	cmp	rax, 0
	jne	_if_true0
	jmp	_after_if1
_if_true0:
	mov	r10, qword[rsp + 0]
	mov	qword[rsp + 32], r10
	mov	rdi, qword[rsp + 32]
	call	$tolower
	mov	qword[rsp + 0], rax
	jmp	_after_if1
_after_if1:
	mov	rax, qword[rsp + 0]
	jmp	________
________:
	add	rsp, 40
	ret
