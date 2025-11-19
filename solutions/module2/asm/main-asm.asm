        global  main
        extern  puts
        extern  putchar
        extern  atoi
        extern  factorial_message
        extern  fflush

        section .data
menu_str       db       "usage: <command> <arg1> <arg2>",10,10, \
                        "commands:",10, \
                        "  e   Echo arguments arg1 and arg2",10, \
                        "  m   Prints out a magic number",10, \
                        "  +   Adds together arg1 and arg2",10, \
                        "  !   Prints out the factorial value of",10, \
                        "      arg1, as well as the message in arg2",10, 0

        section .text

print_int:
        push    r12
        push    r13
        push    r14
                        ; x=r12
        mov     r12, rdi  ;save rdi to be able to call functions
        cmp     rdi, 0

        jge     endifprint ;if x>=0 jump
        mov     rdi, '-'
        call    putchar   ;print a minus
        imul    r12, -1   ;multply x by -1

endifprint:
        mov     r13, 1000000 ;i=r13
        mov     r14, 0 ;b=r14

whileprint:
        cmp     r13, 0 ;i!=0
        je      endwhileprint 
        cmp     r12, r13    ;x>=i
        jge     iftrue  
        cmp     r14, 0        ;b>=0
        jg      iftrue

        ;if we got here, only and matters, so if any of them is false
        ; we jump to false, otherwise proceed to true because both true
        cmp     r12, 0  ;x==0
        jne     iffalse
        cmp     r13, 1  ;i==1
        jne     iffalse
iftrue:
        xor     rdx, rdx ; clear out rdx
        mov     rax, r12 ; x / by smth so move it to rax
        div     r13      ; x/i
        mov     rdi, rax ;put result as input to putchar
        add     rdi, '0' ;add char for zero
        call    putchar
        mov     r14, 1   ; b =1
iffalse:
        ;for modulo use the fact that rdx contains the remainder
        xor     rdx, rdx ; clear out rdx
        mov     rax, r12 ; x / by smth so move it to rax
        div     r13      ; x/i
        mov     r12, rdx ;x = remainder of just happened division

        xor     rdx, rdx ; clear out rdx
        mov     rax, r13 ; i / by smth so move it to rax
        mov     rbp, 10
        div     rbp      ; i/10
        mov     r13, rax ;i = result of division

        jmp     whileprint  

endwhileprint:
        mov     rdi, 10
        call    putchar
        pop     r14
        pop     r13
        pop     r12
        ret



menu:
        mov     rdi, menu_str
        call    puts  
        ret

main:
        cmp     rdi, 4
        jge     afterif
        call    menu
        mov     rax, 1
        ret
afterif:
        push    r12   ;save these two for own usage
        push    r13
        push    r14   ; this will be the command pointer

        mov     r12, rdi  ;r12=argc
        mov     r13, rsi  ;r13=argv

        mov     r14, [r13+8] ;get the value argv points to as command
                             ;r14=command
        mov     rdi, [r14]
        cmp     dil, 'e'  ;compare command[0] with e
        jne     endif2    
        mov     rdi, [r13+8*2] ; argv[2]
        call    puts
        mov     rdi, [r13+8*3] ;argv[3]
        call    puts
        jmp     exit0
endif2:
        cmp     dil, 'm'  ;this should still be command[0] because we just jumped here
        jne     endif3
        mov     rdi, -126
        call    print_int
        jmp     exit0
endif3:
        cmp     dil, '+'  ;this should still be command[0] because we just jumped here
        jne     endif4
        push    r15     ;r15 is used only for this one so I only declare it in tis one
        mov     rdi, [r13+8*2] ;argv[2]
        call    atoi    ;atoi(argv[2])
        mov     r15, rax  ;store it so it isn't overwritten by another call
        mov     rdi, [r13+8*3] ;argv[3]
        call    atoi    ;atoi(argv[3])
        mov     rdi, rax
        add     rdi, r15   ;argument=result of first atoi+res of second
        call    print_int       
        pop     r15
        jmp     exit0
endif4:
        cmp     dil, '!'  ;this should still be command[0] because we just jumped here
        jne     endif5
        mov     rdi, [r13+8*2]  ;atoi(argv[2])
        call    atoi
        mov     rdi, rax
        mov     rsi, [r13+8*3]  ;argv[3]
        call    factorial_message
        mov     rdi, rax
        call    print_int
        jmp     exit0
endif5:
        call    menu
        pop     r14
        pop     r13
        pop     r12
        mov     rax, 1
        ret
exit0:
        pop     r14
        pop     r13
        pop     r12
        mov     rax, 0
        ret


