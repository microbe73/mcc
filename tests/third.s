    .globl _main
_main:
    movq $1, %rax
    pushq %rax
    movq $2, %rax
    pushq %rax
    movq $7, %rax
    popq %rcx
    addq %rcx, %rax
    pushq %rax
    movq $8, %rax
    popq %rcx
    sub %rax, %rcx
    movq %rcx, %rax
    pushq %rax
    movq $3, %rax
    movq %rax, %rcx
    shlq %cl, %rax
    popq %rcx
    and %rcx, %rax
    popq %rcx
    retq
