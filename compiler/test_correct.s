	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 15, 0	sdk_version 15, 5
	.globl	_drawChar                       ## -- Begin function drawChar
	.p2align	4, 0x90
_drawChar:                              ## @drawChar
	.cfi_startproc
## %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movl	%edi, -4(%rbp)
	cmpl	$0, -4(%rbp)
	jne	LBB0_2
## %bb.1:
	movl	$95, %edi
	callq	_putchar
	jmp	LBB0_8
LBB0_2:
	cmpl	$1, -4(%rbp)
	jne	LBB0_4
## %bb.3:
	movl	$120, %edi
	callq	_putchar
	jmp	LBB0_7
LBB0_4:
	cmpl	$2, -4(%rbp)
	jne	LBB0_6
## %bb.5:
	movl	$111, %edi
	callq	_putchar
LBB0_6:
	jmp	LBB0_7
LBB0_7:
	jmp	LBB0_8
LBB0_8:
	xorl	%eax, %eax
	addq	$16, %rsp
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_drawRow                        ## -- Begin function drawRow
	.p2align	4, 0x90
_drawRow:                               ## @drawRow
	.cfi_startproc
## %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	andl	$3, %eax
	movl	%eax, -8(%rbp)
	movl	-8(%rbp), %edi
	callq	_drawChar
	movl	-4(%rbp), %eax
	sarl	$2, %eax
	andl	$3, %eax
	movl	%eax, -12(%rbp)
	movl	-12(%rbp), %edi
	callq	_drawChar
	movl	-4(%rbp), %eax
	sarl	$4, %eax
	andl	$3, %eax
	movl	%eax, -16(%rbp)
	movl	-16(%rbp), %edi
	callq	_drawChar
	movl	-4(%rbp), %eax
	sarl	$6, %eax
	andl	$3, %eax
	movl	%eax, -20(%rbp)
	movl	-20(%rbp), %eax
	addq	$32, %rsp
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_drawBoard                      ## -- Begin function drawBoard
	.p2align	4, 0x90
_drawBoard:                             ## @drawBoard
	.cfi_startproc
## %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	movl	%edx, -12(%rbp)
	movl	-4(%rbp), %edi
	callq	_drawRow
	movl	$10, %edi
	callq	_putchar
	movl	-8(%rbp), %edi
	callq	_drawRow
	movl	$10, %edi
	callq	_putchar
	movl	-12(%rbp), %edi
	callq	_drawRow
	movl	$10, %edi
	callq	_putchar
	xorl	%eax, %eax
	addq	$16, %rsp
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_main                           ## -- Begin function main
	.p2align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movl	$0, -4(%rbp)
	xorl	%edx, %edx
	movl	%edx, %edi
	movl	%edx, %esi
	callq	_drawBoard
	movl	%eax, -8(%rbp)
	xorl	%eax, %eax
	addq	$16, %rsp
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
.subsections_via_symbols
