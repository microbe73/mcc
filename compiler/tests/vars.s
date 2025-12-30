    .globl _main
_main:
    pushq %rbp
    movq %rsp, %rbp
    movq $7, %rax
    pushq %rax
    pushq $0
    movq $17, %rax
    pushq %rax
    movq $21, %rax
    popq %rcx
    sub %rax, %rcx
    movq %rcx, %rax
    movq %rax, -16(%rbp)
    movq -8(%rbp), %rax
    pushq %rax
    movq -16(%rbp), %rax
    popq %rcx
    add %rcx, %rax
    movq %rbp, %rsp
    popq %rbp
    retq

