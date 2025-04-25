	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 14, 0	sdk_version 15, 2
	.globl	_main                           ## -- Begin function main
	.p2align	4, 0x90
_main:                                  ## @main
## %bb.0:
	pushq	%rbp
	movq	%rsp, %rbp
	xorl	%eax, %eax
	callq	_getchar
	movl	%eax, %edi
	callq	_putchar
	movl	$101, %edi
	callq	_putchar
	movl	$108, %edi
	callq	_putchar
	movl	$108, %edi
	callq	_putchar
	movl	$111, %edi
	callq	_putchar
	movl	$44, %edi
	callq	_putchar
	movl	$32, %edi
	callq	_putchar
	movl	$87, %edi
	callq	_putchar
	movl	$111, %edi
	callq	_putchar
	movl	$114, %edi
	callq	_putchar
	movl	$108, %edi
	callq	_putchar
	movl	$100, %edi
	callq	_putchar
	movl	$33, %edi
	callq	_putchar
	movl	$10, %edi
	callq	_putchar
	xorl	%eax, %eax
	popq	%rbp
	retq
                                        ## -- End function
.subsections_via_symbols
