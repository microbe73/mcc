
    .globl _main
_main:
    pushq %rbp
    movq %rsp, %rbp
    movq $1, %rax
    pushq %rax
    movq -8(%rbp), %rax
    pushq %rax
    movq $2, %rax
    movq %rax, %rcx
    popq %rax
    shlq %cl, %rax
    pushq %rax
    movq -16(%rbp), %rax
    movq %rbp, %rsp
    popq %rbp
    ret
    addq $16, %rsp
    movq %rbp, %rsp
    popq %rbp
    ret
