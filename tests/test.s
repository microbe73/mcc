	.globl _main
_main:	
  movq $15, %rax
	pushq %rax
	movq $7, %rax
	popq %rcx
	pushq %rax
	movq %rcx, %rax
	popq %rcx
	cqo
	idiv %rcx
	retq
