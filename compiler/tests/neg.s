	.globl _main
_main:
    pushq %rbp
    movq %rsp, %rbp
	movq $7, %rax
	pushq %rax
    movq -8(%rbp), %rax
    movq %rbp, %rsp
    popq %rbp
    retq
    addq $8, %rsp
