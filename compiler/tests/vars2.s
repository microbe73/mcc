    .globl _main
_main:
    pushq %rbp
    movq %rsp, %rbp
    movq $15, %rax
    pushq %rax
    movq $7, %rax
    pushq %rax
    pushq $0
    movq -8(%rbp), %rax
    pushq %rax
    movq -16(%rbp), %rax
    popq %rcx
    imul %rcx, %rax
    movq %rax, -24(%rbp)
    movq -24(%rbp), %rax
    pushq %rax
    movq $15, %rax
    popq %rcx
    sub %rax, %rcx
    movq %rcx, %rax
    movq %rbp, %rsp
    popq %rbp
    retq

