    .globl _putchar
    .globl _getchar

    .globl _drawChar
_drawChar:
    pushq %rbp
    movq %rsp, %rbp
    pushq %rdi
    movq -8(%rbp), %rax
    pushq %rax
    movq $0, %rax
    popq %rcx
    cmpq %rax, %rcx
    movq $0, %rax
    sete %al
    cmpq $0, %rax
    je _label5
    movq $95, %rax
    movq %rax,%rdi
    callq _putchar
    addq $0, %rsp
    addq $0, %rsp
    jmp _label6
_label5:
    movq -8(%rbp), %rax
    pushq %rax
    movq $1, %rax
    popq %rcx
    cmpq %rax, %rcx
    movq $0, %rax
    sete %al
    cmpq $0, %rax
    je _label3
    movq $120, %rax
    movq %rax,%rdi
    callq _putchar
    addq $0, %rsp
    addq $0, %rsp
    jmp _label4
_label3:
    movq -8(%rbp), %rax
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
    callq _putchar
    addq $0, %rsp
    addq $0, %rsp
    jmp _label2
_label1:
    movq $1, %rax
    neg  %rax
    movq %rbp, %rsp
    popq %rbp
    retq
    addq $0, %rsp
_label2:
_label4:
_label6:
    movq $0, %rax
    movq %rbp, %rsp
    popq %rbp
    retq
    addq $8, %rsp
    movq %rbp, %rsp
    popq %rbp
    ret
    .globl _drawRow
_drawRow:
    pushq %rbp
    movq %rsp, %rbp
    pushq %rdi
    movq $3, %rax
    pushq %rax
    movq -8(%rbp), %rax
    popq %rcx
    and %rcx, %rax
    pushq %rax
    movq -16(%rbp), %rax
    movq %rax,%rdi
    callq _drawChar
    addq $0, %rsp
    pushq %rax
    movq $3, %rax
    pushq %rax
    movq -8(%rbp), %rax
    pushq %rax
    movq $2, %rax
    movq %rax, %rcx
    popq %rax
    shrq %cl, %rax
    popq %rcx
    and %rcx, %rax
    pushq %rax
    movq -32(%rbp), %rax
    movq %rax,%rdi
    callq _drawChar
    addq $0, %rsp
    pushq %rax
    movq $3, %rax
    pushq %rax
    movq -8(%rbp), %rax
    pushq %rax
    movq $4, %rax
    movq %rax, %rcx
    popq %rax
    shrq %cl, %rax
    popq %rcx
    and %rcx, %rax
    pushq %rax
    movq -48(%rbp), %rax
    movq %rax,%rdi
    callq _drawChar
    addq $0, %rsp
    pushq %rax
    movq -24(%rbp), %rax
    pushq %rax
    movq -40(%rbp), %rax
    popq %rcx
    add %rcx, %rax
    pushq %rax
    movq -56(%rbp), %rax
    popq %rcx
    add %rcx, %rax
    movq %rbp, %rsp
    popq %rbp
    retq
    addq $56, %rsp
    movq %rbp, %rsp
    popq %rbp
    ret
    .globl _drawBoard
_drawBoard:
    pushq %rbp
    movq %rsp, %rbp
    pushq %rdi
    pushq %rsi
    pushq %rdx
    movq -8(%rbp), %rax
    movq %rax,%rdi
    callq _drawRow
    addq $0, %rsp
    pushq %rax
    movq $10, %rax
    movq %rax,%rdi
    callq _putchar
    addq $0, %rsp
    movq -16(%rbp), %rax
    movq %rax,%rdi
    callq _drawRow
    addq $0, %rsp
    pushq %rax
    movq $10, %rax
    movq %rax,%rdi
    callq _putchar
    addq $0, %rsp
    movq -24(%rbp), %rax
    movq %rax,%rdi
    callq _drawRow
    addq $0, %rsp
    pushq %rax
    movq $10, %rax
    movq %rax,%rdi
    callq _putchar
    addq $0, %rsp
    movq $10, %rax
    movq %rax,%rdi
    callq _putchar
    addq $0, %rsp
    movq -32(%rbp), %rax
    pushq %rax
    movq -40(%rbp), %rax
    popq %rcx
    add %rcx, %rax
    pushq %rax
    movq -48(%rbp), %rax
    popq %rcx
    add %rcx, %rax
    movq %rbp, %rsp
    popq %rbp
    retq
    addq $48, %rsp
    movq %rbp, %rsp
    popq %rbp
    ret
    .globl _updateRow
_updateRow:
    pushq %rbp
    movq %rsp, %rbp
    pushq %rdi
    pushq %rsi
    movq -16(%rbp), %rax
    pushq %rax
    movq $1, %rax
    popq %rcx
    cmpq %rax, %rcx
    movq $0, %rax
    sete %al
    cmpq $0, %rax
    je _label15
    movq -8(%rbp), %rax
    pushq %rax
    movq $1, %rax
    popq %rcx
    cmpq %rax, %rcx
    movq $0, %rax
    sete %al
    cmpq $0, %rax
    je _label9
    movq $1, %rax
    movq %rbp, %rsp
    popq %rbp
    retq
    addq $0, %rsp
    jmp _label10
_label9:
    movq -8(%rbp), %rax
    pushq %rax
    movq $2, %rax
    popq %rcx
    cmpq %rax, %rcx
    movq $0, %rax
    sete %al
    cmpq $0, %rax
    je _label7
    movq $4, %rax
    movq %rbp, %rsp
    popq %rbp
    retq
    addq $0, %rsp
    jmp _label8
_label7:
    movq $16, %rax
    movq %rbp, %rsp
    popq %rbp
    retq
_label8:
_label10:
    addq $0, %rsp
    jmp _label16
_label15:
    movq -8(%rbp), %rax
    pushq %rax
    movq $1, %rax
    popq %rcx
    cmpq %rax, %rcx
    movq $0, %rax
    sete %al
    cmpq $0, %rax
    je _label13
    movq $2, %rax
    movq %rbp, %rsp
    popq %rbp
    retq
    jmp _label14
_label13:
    movq -8(%rbp), %rax
    pushq %rax
    movq $2, %rax
    popq %rcx
    cmpq %rax, %rcx
    movq $0, %rax
    sete %al
    cmpq $0, %rax
    je _label11
    movq $8, %rax
    movq %rbp, %rsp
    popq %rbp
    retq
    addq $0, %rsp
    jmp _label12
_label11:
    movq $32, %rax
    movq %rbp, %rsp
    popq %rbp
    retq
_label12:
_label14:
    addq $0, %rsp
_label16:
    movq $0, %rax
    movq %rbp, %rsp
    popq %rbp
    retq
    addq $16, %rsp
    movq %rbp, %rsp
    popq %rbp
    ret
    .globl _main
_main:
    pushq %rbp
    movq %rsp, %rbp
    movq $0, %rax
    pushq %rax
    movq $0, %rax
    pushq %rax
    movq $0, %rax
    pushq %rax
    movq $0, %rax
    pushq %rax
    movq $1, %rax
    pushq %rax
_label17:
    movq -40(%rbp), %rax
    pushq %rax
    movq $10, %rax
    popq %rcx
    cmpq %rax, %rcx
    movq $0, %rax
    setl %al
    cmpq $0, %rax
    je _label18
    movq -8(%rbp), %rax
    movq %rax,%rdi
    movq -16(%rbp), %rax
    movq %rax,%rsi
    movq -24(%rbp), %rax
    movq %rax,%rdx
    callq _drawBoard
    addq $0, %rsp
    movq %rax, -32(%rbp)
    callq _getchar
    addq $0, %rsp
    pushq %rax
    movq $48, %rax
    popq %rcx
    sub %rax, %rcx
    movq %rcx, %rax
    pushq %rax
    movq -48(%rbp), %rax
    pushq %rax
    movq $3, %rax
    popq %rcx
    cmpq %rax, %rcx
    movq $0, %rax
    setle %al
    cmpq $0, %rax
    je _label22
    movq -8(%rbp), %rax
    pushq %rax
    movq -48(%rbp), %rax
    movq %rax,%rdi
    movq -40(%rbp), %rax
    pushq %rax
    movq $2, %rax
    popq %rcx
    pushq %rax
    movq %rcx, %rax
    popq %rcx
    cqo
    idiv %rcx
    movq %rdx, %rax
    movq %rax,%rsi
    callq _updateRow
    addq $0, %rsp
    popq %rcx
    add %rcx, %rax
    movq %rax, -8(%rbp)
    addq $0, %rsp
    jmp _label23
_label22:
    movq -48(%rbp), %rax
    pushq %rax
    movq $6, %rax
    popq %rcx
    cmpq %rax, %rcx
    movq $0, %rax
    setle %al
    cmpq $0, %rax
    je _label20
    movq -16(%rbp), %rax
    pushq %rax
    movq -48(%rbp), %rax
    pushq %rax
    movq $3, %rax
    popq %rcx
    sub %rax, %rcx
    movq %rcx, %rax
    movq %rax,%rdi
    movq -40(%rbp), %rax
    pushq %rax
    movq $2, %rax
    popq %rcx
    pushq %rax
    movq %rcx, %rax
    popq %rcx
    cqo
    idiv %rcx
    movq %rdx, %rax
    movq %rax,%rsi
    callq _updateRow
    addq $0, %rsp
    popq %rcx
    add %rcx, %rax
    movq %rax, -16(%rbp)
    addq $0, %rsp
    jmp _label21
_label20:
    movq -48(%rbp), %rax
    pushq %rax
    movq $9, %rax
    popq %rcx
    cmpq %rax, %rcx
    movq $0, %rax
    setle %al
    cmpq $0, %rax
    je _label19
    movq -24(%rbp), %rax
    pushq %rax
    movq -48(%rbp), %rax
    pushq %rax
    movq $6, %rax
    popq %rcx
    sub %rax, %rcx
    movq %rcx, %rax
    movq %rax,%rdi
    movq -40(%rbp), %rax
    pushq %rax
    movq $2, %rax
    popq %rcx
    pushq %rax
    movq %rcx, %rax
    popq %rcx
    cqo
    idiv %rcx
    movq %rdx, %rax
    movq %rax,%rsi
    callq _updateRow
    addq $0, %rsp
    popq %rcx
    add %rcx, %rax
    movq %rax, -24(%rbp)
    addq $0, %rsp
_label19:
_label21:
_label23:
    movq -40(%rbp), %rax
    pushq %rax
    movq $1, %rax
    popq %rcx
    add %rcx, %rax
    movq %rax, -40(%rbp)
    callq _getchar
    addq $0, %rsp
    pushq %rax
    addq $16, %rsp
    jmp _label17
_label18:
    movq -8(%rbp), %rax
    movq %rax,%rdi
    movq -16(%rbp), %rax
    movq %rax,%rsi
    movq -24(%rbp), %rax
    movq %rax,%rdx
    callq _drawBoard
    addq $0, %rsp
    movq %rax, -32(%rbp)
    movq $0, %rax
    movq %rbp, %rsp
    popq %rbp
    retq
    addq $40, %rsp
    movq %rbp, %rsp
    popq %rbp
    ret