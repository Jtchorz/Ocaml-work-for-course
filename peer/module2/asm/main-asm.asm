        global  main
        extern  puts
        extern  fflush
        extern  putchar
        extern  atoi
        extern  factorial_message

        section .text

menu_str        db      "usage: <command> <arg1> <arg2>",10,10,                   "commands:",10, "  e   Echo arguments arg1 and arg2",10,"  m   Prints out a magic number",10,"  +   Adds together arg1 and arg2",10,"  !   Prints out the factorial value of",10,"      arg1, as well as the message in arg2",10,0

; input: number in rdi
; returns: nothing
print_int:                      ; print a supplied number using putchar
        push    r15
        push    r14
        push    r13

        mov     r13,rdi         ; save parameter value in callee-saved register
        mov     r15,1000000     ; save i (denominator) in callee-saved register
        mov     r14, 0          ; save b (print indicator flag) in callee-saved register

        cmp     r13,0           ; check if value is negative by comparing with 0
        jge     _print_loop     ; and skipping this part if the value is >= 0

        neg     r13             ; if x < 0, negate it
        mov     dil,'-'         ; and print the negative sign
        call    putchar

; I check the conditions in the wrong order
; but since the condition is side effect free, it does not matter
_print_loop:                    ; begin by checking if-conditions
        cmp    r14b,0           ; the third one: is the indicator flag turned on?
        ja      _print_lbl      ; if yes, go to print, otherwise check next condition
_after_cond_one:
                                ; second one: is x==0 && i==1
        cmp    r13,0             ; is r13 (aka x) zero?
        ja     _after_cond_two    ; if no, check the next condition (short circuit AND)
                                ; otherwise fall through
_cond_check:
        cmp     r15, 1         ; is r15 (aka i) one?
        je      _print_lbl        ; if yes, go to print
                                ; otherwise check next condition
_after_cond_two:
                                ; first one: is x>=i, i.e. r13 >= r15? 
        cmp     r13, r15        ; check if r13 < r15
        jl     _post_print     ; if yes, skip the print, otherwise fall through

_print_lbl:
        xor     rdx,rdx         ; zero out the upper parts of the numerator
        mov     rax, r13        ; copy value into rax, which is the register that gets divided
        idiv    r15             ; divide by i
        ; now, rax holds the quotient and rdx the remainder
        mov     rdi,rax         ; the quotient is what should be printed
                                ; we can discard old r13 value since rdx holds the new one
        push    rdx             ; we do however need to ensure we keep that value
        add     rdi,'0'         ; turn into printable number
        call    putchar
        pop     r13             ; move the new value directly to the correct register

        mov     r14,1           ; set the print indicator

        xor     rdx,rdx         ; zero out the upper parts of the numerator
        mov     rax,r15         ; must divide i by 10
        mov     rsi,10
        idiv    rsi
        mov     r15,rax         ; move quotient back into i
        jmp     _while_check
_post_print:
        xor     rdx,rdx         ; zero out the upper parts of the numerator
        mov     rax, r13        ; copy value into rax, which is the register that gets divided
        idiv    r15             ; divide by i
        mov     r13,rdx         ; update remaining value to compute with, which is the remainder

        xor     rdx,rdx         ; zero out the upper parts of the numerator
        mov     rax,r15         ; must divide i by 10
        mov     rsi,10
        idiv    rsi
        mov     r15,rax         ; move quotient back into i

; condition not checked before the first iteration since it is always true then
; so this has become a do-while loop instead, but black-box semantics are the same
_while_check:                   ; check the while condition
        add    r15,0            ; is r15 (i) equal to zero?
        jne     _print_loop     ; if not, loop. otherwise, fall through

_end:
        mov     rdi,10          ; ensure that a new-line is written
        call    putchar         

        pop     r13             ; pop the stored registers
        pop     r14
        pop     r15
        ret
; end print_int


main:
        sub     rdi,4   ; rdi is argc. it should be atleast 4 (name+args)
        js      incorrect_args ; print menu if fewer than three args
        
        ; now, the command should be identified
        ; argv is in rsi, meaning rsi holds a pointer to an array of pointers
        ; fetching the pointer to the second argument is done by:
        mov     r12, rsi        ; move to register that we can keep
        add     r12, 8          ; change pointer to second arg (first param)
        mov     rax,[r12]       ; indexation to get string pointer
        mov     cl,[rax]        ; should hold the string value of the command
        mov     bl,cl           ; copy it to another register that we can modify
                                ; so we don't have to fetch from memory again
        xor     bl,'e'          ; check if 'e'
        jz      echo

        mov     bl,cl           ; re-copy value
        xor     bl,'m'          ; check if 'm'
        jz      magic

        mov     bl,cl
        xor     bl,'+'          ; check if '+'
        jz      add_command

        mov     bl,cl
        xor     bl,'!'          ; check if '!'
        jz      factorial
        
        jmp     incorrect_args  ; default, print menu

echo:
        add     r12, 8          ; increment parameter pointer
        mov     rdi,[r12]       ; move string pointer into argument to puts
        call    puts

        add     r12, 8          ; increment parameter pointer
        mov     rdi,[r12]       ; move string pointer into argument to puts
        call    puts

        mov     rax,0           ; set exit code
        jmp     exit

magic:
        mov     rdi, -126
        call    print_int
        
        mov     rax,0
        jmp     exit

add_command:
        add     r12, 8          ; increment parameter pointer
        mov     rdi,[r12]       ; move string pointer into argument to puts
        call    atoi            ; turn string into int
        push    rax             ; save the return value

        add     r12, 8          ; increment parameter pointer
        mov     rdi,[r12]       ; move string pointer into argument to puts
        call    atoi
        
        pop     rdi             ; pop the first value into the parameter register
        add     rdi,rax         ; add the second value
        call    print_int       ; print it

        mov     rax,0
        jmp     exit

factorial:
        add     r12, 8          ; increment parameter pointer
        mov     rdi,[r12]       ; move string pointer into argument to puts
        call    atoi
        mov     rdi,rax         ; save the return value

        add     r12, 8          ; increment parameter pointer
        mov     rsi,[r12]       ; move string pointer into argument to puts

        call    factorial_message
        mov     rdi,rax         ; move return value into parameter register
        call    print_int

        mov     rax,0
        jmp     exit


incorrect_args:                 ; print menu on error
        mov     rdi, menu_str
        call    puts

        mov     rax,1           ; and exit 1

exit:
        push    rax             ; store the supplied exit code
        mov     rdi, 0
        call    fflush          ; flush output
        pop     rax             ; pop supplied exit code
        ret

