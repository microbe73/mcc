	.globl	_main                           ## -- Begin function main
_main:                                  ## @main
## %bb.0:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$2, %eax
	popq	%rbp
	retq
                                        ## -- End function
.subsections_via_symbols
