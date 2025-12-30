    .globl _main
_main:
    pushq %rbp
    movq %rsp, %rbp
    movq $15, %rax
    pushq %rax
    movq -8(%rbp), %rax
    pushq %rax
    movq $15, %rax
    popq %rcx
    cmpq %rax, %rcx
    movq $0, %rax
    sete %al
    cmpq $0, %rax
    je _label1
    movq -8(%rbp), %rax
    pushq %rax
    movq $14, %rax
    popq %rcx
    sub %rax, %rcx
    movq %rcx, %rax
    cmpq $0, %rax
    je _label2
    movq $3, %rax
    jmp _label3
_label2:
    movq $12, %rax
_label3:
    movq %rax, -8(%rbp)
_label1:
    movq -8(%rbp), %rax
    movq %rbp, %rsp
    popq %rbp
    retq

