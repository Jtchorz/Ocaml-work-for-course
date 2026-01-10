	extern	malloc 
	extern	free 
	extern	printf

 	section .data
	string0:	db  "F", "i", "r", "s", "t", ":", " ", "%", "d", " ", "%", "d", " ", "%", "d", 10, 0
	string1:	db  "L", "a", "s", "t", ":", " ", "%", "d", " ", "%", "d", " ", "%", "d", " ", "%", "d", 10, 0
 
	global	main
	section .text
main:
	push	r12
	sub	rsp, 400
	jmp	_main_
_main_:
	mov	qword[rsp + 0], 0
	mov	qword[rsp + 16], 2
	mov	rdi, qword[rsp + 16]
	shl	rdi, 3
	call	$malloc
	mov	qword[rsp + 8], rax
	mov	qword[rsp + 32], 0
	xor	r12, r12
	mov	r10, qword[rsp + 8]
	mov	r11, qword[rsp + 32]
	mov	r12, qword[r10 + r11 * 8 + 0]
	mov	qword[rsp + 24], r12
	mov	qword[rsp + 48], 1
	xor	r12, r12
	mov	r10, qword[rsp + 8]
	mov	r11, qword[rsp + 48]
	mov	r12, qword[r10 + r11 * 8 + 0]
	mov	qword[rsp + 40], r12
	mov	qword[rsp + 56], 0
	mov	qword[rsp + 72], 3
	mov	rdi, qword[rsp + 72]
	shl	rdi, 3
	call	$malloc
	mov	qword[rsp + 64], rax
	mov	r11, qword[rsp + 56]
	mov	r10, qword[rsp + 8]
	xor	r12, r12
	mov	r12, qword[rsp + 64]
	mov	qword[r10 + r11 * 8 + 0], r12
	mov	qword[rsp + 80], 1
	mov	qword[rsp + 96], 4
	mov	rdi, qword[rsp + 96]
	shl	rdi, 3
	call	$malloc
	mov	qword[rsp + 88], rax
	mov	r11, qword[rsp + 80]
	mov	r10, qword[rsp + 8]
	xor	r12, r12
	mov	r12, qword[rsp + 88]
	mov	qword[r10 + r11 * 8 + 0], r12
	mov	qword[rsp + 104], 0
	xor	r12, r12
	mov	r10, qword[rsp + 8]
	mov	r11, qword[rsp + 104]
	mov	r12, qword[r10 + r11 * 8 + 0]
	mov	qword[rsp + 24], r12
	mov	qword[rsp + 112], 1
	xor	r12, r12
	mov	r10, qword[rsp + 8]
	mov	r11, qword[rsp + 112]
	mov	r12, qword[r10 + r11 * 8 + 0]
	mov	qword[rsp + 40], r12
	mov	qword[rsp + 0], 0
	mov	r10, qword[rsp + 0]
	mov	qword[rsp + 120], r10
	mov	qword[rsp + 128], 3
	xor	r11, r11
	mov	r10, qword[rsp + 128]
	sub	qword[rsp + 120], r10
	setl	r11b
	mov	rax, r11
	cmp	rax, 0
	jne	_while_body1
	jmp	_after_while2
_while_body1:
	mov	r10, qword[rsp + 0]
	mov	qword[rsp + 136], r10
	mov	r10, qword[rsp + 0]
	mov	qword[rsp + 144], r10
	mov	r11, qword[rsp + 136]
	mov	r10, qword[rsp + 24]
	xor	r12, r12
	mov	r12, qword[rsp + 144]
	mov	qword[r10 + r11 * 8 + 0], r12
	jmp	_scope4
_scope4:
	mov	r10, qword[rsp + 0]
	mov	qword[rsp + 152], r10
	mov	qword[rsp + 160], 1
	mov	r10, qword[rsp + 152]
	mov	qword[rsp + 0], r10
	mov	r10, qword[rsp + 160]
	add	qword[rsp + 0], r10
	jmp	_while_cond3
_while_cond3:
	mov	r10, qword[rsp + 0]
	mov	qword[rsp + 168], r10
	mov	qword[rsp + 176], 3
	xor	r11, r11
	mov	r10, qword[rsp + 176]
	sub	qword[rsp + 168], r10
	setl	r11b
	mov	rax, r11
	cmp	rax, 0
	jne	_while_body1
	jmp	_after_while2
_after_while2:
	jmp	_scope0
_scope0:
	mov	qword[rsp + 0], 0
	mov	r10, qword[rsp + 0]
	mov	qword[rsp + 184], r10
	mov	qword[rsp + 192], 4
	xor	r11, r11
	mov	r10, qword[rsp + 192]
	sub	qword[rsp + 184], r10
	setl	r11b
	mov	rax, r11
	cmp	rax, 0
	jne	_while_body6
	jmp	_after_while7
_while_body6:
	mov	r10, qword[rsp + 0]
	mov	qword[rsp + 200], r10
	mov	qword[rsp + 232], 2
	xor	r12, r12
	mov	r10, qword[rsp + 24]
	mov	r11, qword[rsp + 232]
	mov	r12, qword[r10 + r11 * 8 + 0]
	mov	qword[rsp + 216], r12
	mov	r10, qword[rsp + 0]
	mov	qword[rsp + 224], r10
	mov	r10, qword[rsp + 216]
	mov	qword[rsp + 208], r10
	mov	r10, qword[rsp + 224]
	add	qword[rsp + 208], r10
	mov	r11, qword[rsp + 200]
	mov	r10, qword[rsp + 40]
	xor	r12, r12
	mov	r12, qword[rsp + 208]
	mov	qword[r10 + r11 * 8 + 0], r12
	jmp	_scope9
_scope9:
	mov	r10, qword[rsp + 0]
	mov	qword[rsp + 240], r10
	mov	qword[rsp + 248], 1
	mov	r10, qword[rsp + 240]
	mov	qword[rsp + 0], r10
	mov	r10, qword[rsp + 248]
	add	qword[rsp + 0], r10
	jmp	_while_cond8
_while_cond8:
	mov	r10, qword[rsp + 0]
	mov	qword[rsp + 256], r10
	mov	qword[rsp + 264], 4
	xor	r11, r11
	mov	r10, qword[rsp + 264]
	sub	qword[rsp + 256], r10
	setl	r11b
	mov	rax, r11
	cmp	rax, 0
	jne	_while_body6
	jmp	_after_while7
_after_while7:
	jmp	_scope5
_scope5:
	mov	qword[rsp + 320], 2
	xor	r12, r12
	mov	r10, qword[rsp + 24]
	mov	r11, qword[rsp + 320]
	mov	r12, qword[r10 + r11 * 8 + 0]
	mov	qword[rsp + 312], r12
	mov	rcx, qword[rsp + 312]
	mov	qword[rsp + 304], 1
	xor	r12, r12
	mov	r10, qword[rsp + 24]
	mov	r11, qword[rsp + 304]
	mov	r12, qword[r10 + r11 * 8 + 0]
	mov	qword[rsp + 296], r12
	mov	rdx, qword[rsp + 296]
	mov	qword[rsp + 288], 0
	xor	r12, r12
	mov	r10, qword[rsp + 24]
	mov	r11, qword[rsp + 288]
	mov	r12, qword[r10 + r11 * 8 + 0]
	mov	qword[rsp + 280], r12
	mov	rsi, qword[rsp + 280]
	mov	r12, string0
	mov	qword[rsp + 272], r12
	mov	rdi, qword[rsp + 272]
	xor	rax, rax
	call	$printf
	mov	rax, rax
	mov	qword[rsp + 392], 3
	xor	r12, r12
	mov	r10, qword[rsp + 40]
	mov	r11, qword[rsp + 392]
	mov	r12, qword[r10 + r11 * 8 + 0]
	mov	qword[rsp + 384], r12
	mov	r8, qword[rsp + 384]
	mov	qword[rsp + 376], 2
	xor	r12, r12
	mov	r10, qword[rsp + 40]
	mov	r11, qword[rsp + 376]
	mov	r12, qword[r10 + r11 * 8 + 0]
	mov	qword[rsp + 368], r12
	mov	rcx, qword[rsp + 368]
	mov	qword[rsp + 360], 1
	xor	r12, r12
	mov	r10, qword[rsp + 40]
	mov	r11, qword[rsp + 360]
	mov	r12, qword[r10 + r11 * 8 + 0]
	mov	qword[rsp + 352], r12
	mov	rdx, qword[rsp + 352]
	mov	qword[rsp + 344], 0
	xor	r12, r12
	mov	r10, qword[rsp + 40]
	mov	r11, qword[rsp + 344]
	mov	r12, qword[r10 + r11 * 8 + 0]
	mov	qword[rsp + 336], r12
	mov	rsi, qword[rsp + 336]
	mov	r12, string1
	mov	qword[rsp + 328], r12
	mov	rdi, qword[rsp + 328]
	xor	rax, rax
	call	$printf
	mov	rax, rax
	mov	rdi, qword[rsp + 24]
	call	$free
	mov	rdi, qword[rsp + 40]
	call	$free
	mov	rdi, qword[rsp + 8]
	call	$free
	mov	rax, 0
	jmp	________2
________2:
	add	rsp, 400
	pop	r12
	ret
