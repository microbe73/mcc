	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 14, 0	sdk_version 15, 2
	.globl	_main                           ## -- Begin function main
	.p2align	4, 0x90
_main:                                  ## @main
## %bb.0:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	$0, -4(%rbp)
	movb	$0, %al
	callq	_getchar
	movl	%eax, -8(%rbp)
	movl	-8(%rbp), %ecx
	subl	$60, %ecx
	movl	$15, %eax
                                        ## kill: def $cl killed $ecx
	shll	%cl, %eax
	movl	%eax, -12(%rbp)
	movl	-12(%rbp), %eax
	addq	$16, %rsp
	popq	%rbp
	retq
                                        ## -- End function
.subsections_via_symbols
