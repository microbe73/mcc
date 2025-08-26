    .globl _drawChar
_drawChar:
    pushq %rbp
    movq %rsp, %rbp
    movq %rdi, %rax
    pushq %rax
    movq $0, %rax
    popq %rcx
    cmpq %rax, %rcx
    movq $0, %rax
    sete %al
    cmpq $0, %rax
    je _label4
    movq $95, %rax
    movq %rax,%rdi

    call _putchar
    addq $0, %rsp
    addq $8, %rsp
    jmp _label5
_label4:
    movq %rdi, %rax
    pushq %rax
    movq $1, %rax
    popq %rcx
    cmpq %rax, %rcx
    movq $0, %rax
    sete %al
    cmpq $0, %rax
    je _label2
    movq $120, %rax
    movq %rax,%rdi

    call _putchar
    addq $0, %rsp
    addq $8, %rsp
    jmp _label3
_label2:
    movq %rdi, %rax
    pushq %rax
    movq $2, %rax
    popq %rcx
    cmpq %rax, %rcx
    movq $0, %rax
    sete %al
    cmpq $0, %rax
    je _label1
    movq $111, %rax
    movq %rax,%rdi

    call _putchar
    addq $0, %rsp
    addq $8, %rsp
_label1:
_label3:
_label5:
    movq $0, %rax
    movq %rbp, %rsp
    popq %rbp
    retq
    addq $8, %rsp
    movq %rbp, %rsp
    popq %rbp
    .globl _drawRow
_drawRow:
    pushq %rbp
    movq %rsp, %rbp
    movq $3, %rax
    pushq %rax
    movq %rdi, %rax
    popq %rcx
    and %rcx, %rax
    pushq %rax
    movq -8(%rbp), %rax
    movq %rax,%rdi

    call _drawChar
    addq $0, %rsp
    movq $3, %rax
    pushq %rax
    movq %rdi, %rax
    pushq %rax
    movq $2, %rax
    movq %rax, %rcx
    popq %rax
    shrq %cl, %rax
    popq %rcx
    and %rcx, %rax
    pushq %rax
    movq -16(%rbp), %rax
    movq %rax,%rdi

    call _drawChar
    addq $0, %rsp
    movq $3, %rax
    pushq %rax
    movq %rdi, %rax
    pushq %rax
    movq $4, %rax
    movq %rax, %rcx
    popq %rax
    shrq %cl, %rax
    popq %rcx
    and %rcx, %rax
    pushq %rax
    movq -24(%rbp), %rax
    movq %rax,%rdi

    call _drawChar
    addq $0, %rsp
    movq $0, %rax
    movq %rbp, %rsp
    popq %rbp
    retq
    addq $32, %rsp
    movq %rbp, %rsp
    popq %rbp
    .globl _drawBoard
_drawBoard:
    pushq %rbp
    movq %rsp, %rbp
    movq %rdi, %rax
    movq %rax,%rdi

    call _drawRow
    addq $0, %rsp
    movq $10, %rax
    movq %rax,%rdi

    call _putchar
    addq $0, %rsp
    movq %rsi, %rax
    movq %rax,%rdi

    call _drawRow
    addq $0, %rsp
    movq $10, %rax
    movq %rax,%rdi

    call _putchar
    addq $0, %rsp
    movq %rdx, %rax
    movq %rax,%rdi

    call _drawRow
    addq $0, %rsp
    movq $10, %rax
    movq %rax,%rdi

    call _putchar
    addq $0, %rsp
    movq $0, %rax
    movq %rbp, %rsp
    popq %rbp
    retq
    addq $24, %rsp
    movq %rbp, %rsp
    popq %rbp
    .globl _main
_main:
    pushq %rbp
    movq %rsp, %rbp
    movq $0, %rax
    movq %rax,%rdi
    movq $0, %rax
    movq %rax,%rsi
    movq $0, %rax
    movq %rax,%rdx

    call _drawBoard
    addq $0, %rsp
    pushq %rax
    movq $0, %rax
    movq %rbp, %rsp
    popq %rbp
    retq
    addq $8, %rsp
    movq %rbp, %rsp
    popq %rbp
