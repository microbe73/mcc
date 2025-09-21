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
	movl	%edi, -8(%rbp)
	cmpl	$0, -8(%rbp)
	jne	LBB0_2
## %bb.1:
	movl	$95, %edi
	callq	_putchar
	jmp	LBB0_9
LBB0_2:
	cmpl	$1, -8(%rbp)
	jne	LBB0_4
## %bb.3:
	movl	$120, %edi
	callq	_putchar
	jmp	LBB0_8
LBB0_4:
	cmpl	$2, -8(%rbp)
	jne	LBB0_6
## %bb.5:
	movl	$111, %edi
	callq	_putchar
	jmp	LBB0_7
LBB0_6:
	movl	$-1, -4(%rbp)
	jmp	LBB0_10
LBB0_7:
	jmp	LBB0_8
LBB0_8:
	jmp	LBB0_9
LBB0_9:
	movl	$0, -4(%rbp)
LBB0_10:
	movl	-4(%rbp), %eax
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
	movl	%eax, -12(%rbp)
	movl	-4(%rbp), %eax
	sarl	$2, %eax
	andl	$3, %eax
	movl	%eax, -16(%rbp)
	movl	-16(%rbp), %edi
	callq	_drawChar
	movl	%eax, -20(%rbp)
	movl	-4(%rbp), %eax
	sarl	$4, %eax
	andl	$3, %eax
	movl	%eax, -24(%rbp)
	movl	-24(%rbp), %edi
	callq	_drawChar
	movl	%eax, -28(%rbp)
	movl	-12(%rbp), %eax
	addl	-20(%rbp), %eax
	addl	-28(%rbp), %eax
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
	subq	$32, %rsp
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	movl	%edx, -12(%rbp)
	movl	-4(%rbp), %edi
	callq	_drawRow
	movl	%eax, -16(%rbp)
	movl	$10, %edi
	callq	_putchar
	movl	-8(%rbp), %edi
	callq	_drawRow
	movl	%eax, -20(%rbp)
	movl	$10, %edi
	callq	_putchar
	movl	-12(%rbp), %edi
	callq	_drawRow
	movl	%eax, -24(%rbp)
	movl	$10, %edi
	callq	_putchar
	movl	$10, %edi
	callq	_putchar
	movl	-16(%rbp), %eax
	addl	-20(%rbp), %eax
	addl	-24(%rbp), %eax
	addq	$32, %rsp
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_updateRow                      ## -- Begin function updateRow
	.p2align	4, 0x90
_updateRow:                             ## @updateRow
	.cfi_startproc
## %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	movl	%edi, -8(%rbp)
	movl	%esi, -12(%rbp)
	cmpl	$1, -12(%rbp)
	jne	LBB3_6
## %bb.1:
	cmpl	$1, -8(%rbp)
	jne	LBB3_3
## %bb.2:
	movl	$1, -4(%rbp)
	jmp	LBB3_11
LBB3_3:
	cmpl	$2, -8(%rbp)
	jne	LBB3_5
## %bb.4:
	movl	$4, -4(%rbp)
	jmp	LBB3_11
LBB3_5:
	movl	$16, -4(%rbp)
	jmp	LBB3_11
LBB3_6:
	cmpl	$1, -8(%rbp)
	jne	LBB3_8
## %bb.7:
	movl	$2, -4(%rbp)
	jmp	LBB3_11
LBB3_8:
	cmpl	$2, -8(%rbp)
	jne	LBB3_10
## %bb.9:
	movl	$8, -4(%rbp)
	jmp	LBB3_11
LBB3_10:
	movl	$32, -4(%rbp)
LBB3_11:
	movl	-4(%rbp), %eax
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
	subq	$64, %rsp
	movl	$0, -4(%rbp)
	movl	$0, -8(%rbp)
	movl	$0, -12(%rbp)
	movl	$0, -16(%rbp)
	movl	$0, -20(%rbp)
	movl	$1, -24(%rbp)
LBB4_1:                                 ## =>This Inner Loop Header: Depth=1
	cmpl	$10, -24(%rbp)
	jge	LBB4_14
## %bb.2:                               ##   in Loop: Header=BB4_1 Depth=1
	movl	-8(%rbp), %edi
	movl	-12(%rbp), %esi
	movl	-16(%rbp), %edx
	callq	_drawBoard
	movl	%eax, -20(%rbp)
	movb	$0, %al
	callq	_getchar
	subl	$48, %eax
	movl	%eax, -28(%rbp)
	cmpl	$1, -28(%rbp)
	jge	LBB4_4
## %bb.3:                               ##   in Loop: Header=BB4_1 Depth=1
	movl	-28(%rbp), %edi
	callq	_putchar
	movl	$69, %edi
	callq	_putchar
	movl	$10, %edi
	callq	_putchar
	movl	-24(%rbp), %eax
	subl	$1, %eax
	movl	%eax, -24(%rbp)
	movb	$0, %al
	callq	_getchar
	movl	%eax, -32(%rbp)
	jmp	LBB4_1
LBB4_4:                                 ##   in Loop: Header=BB4_1 Depth=1
	cmpl	$3, -28(%rbp)
	jg	LBB4_6
## %bb.5:                               ##   in Loop: Header=BB4_1 Depth=1
	movl	-8(%rbp), %eax
	movl	%eax, -44(%rbp)                 ## 4-byte Spill
	movl	-28(%rbp), %edi
	movl	-24(%rbp), %eax
	movl	$2, %ecx
	cltd
	idivl	%ecx
	movl	%edx, %esi
	callq	_updateRow
	movl	%eax, %ecx
	movl	-44(%rbp), %eax                 ## 4-byte Reload
	addl	%ecx, %eax
	movl	%eax, -8(%rbp)
	jmp	LBB4_13
LBB4_6:                                 ##   in Loop: Header=BB4_1 Depth=1
	cmpl	$6, -28(%rbp)
	jg	LBB4_8
## %bb.7:                               ##   in Loop: Header=BB4_1 Depth=1
	movl	-12(%rbp), %eax
	movl	%eax, -48(%rbp)                 ## 4-byte Spill
	movl	-28(%rbp), %edi
	subl	$3, %edi
	movl	-24(%rbp), %eax
	movl	$2, %ecx
	cltd
	idivl	%ecx
	movl	%edx, %esi
	callq	_updateRow
	movl	%eax, %ecx
	movl	-48(%rbp), %eax                 ## 4-byte Reload
	addl	%ecx, %eax
	movl	%eax, -12(%rbp)
	jmp	LBB4_12
LBB4_8:                                 ##   in Loop: Header=BB4_1 Depth=1
	cmpl	$9, -28(%rbp)
	jg	LBB4_10
## %bb.9:                               ##   in Loop: Header=BB4_1 Depth=1
	movl	-16(%rbp), %eax
	movl	%eax, -52(%rbp)                 ## 4-byte Spill
	movl	-28(%rbp), %edi
	subl	$6, %edi
	movl	-24(%rbp), %eax
	movl	$2, %ecx
	cltd
	idivl	%ecx
	movl	%edx, %esi
	callq	_updateRow
	movl	%eax, %ecx
	movl	-52(%rbp), %eax                 ## 4-byte Reload
	addl	%ecx, %eax
	movl	%eax, -16(%rbp)
	jmp	LBB4_11
LBB4_10:                                ##   in Loop: Header=BB4_1 Depth=1
	movl	-28(%rbp), %edi
	callq	_putchar
	movl	$69, %edi
	callq	_putchar
	movl	$10, %edi
	callq	_putchar
	movl	-24(%rbp), %eax
	subl	$1, %eax
	movl	%eax, -24(%rbp)
	movb	$0, %al
	callq	_getchar
	movl	%eax, -36(%rbp)
	jmp	LBB4_1
LBB4_11:                                ##   in Loop: Header=BB4_1 Depth=1
	jmp	LBB4_12
LBB4_12:                                ##   in Loop: Header=BB4_1 Depth=1
	jmp	LBB4_13
LBB4_13:                                ##   in Loop: Header=BB4_1 Depth=1
	movl	-24(%rbp), %eax
	addl	$1, %eax
	movl	%eax, -24(%rbp)
	movb	$0, %al
	callq	_getchar
	movl	%eax, -40(%rbp)
	jmp	LBB4_1
LBB4_14:
	movl	-8(%rbp), %edi
	movl	-12(%rbp), %esi
	movl	-16(%rbp), %edx
	callq	_drawBoard
	movl	%eax, -20(%rbp)
	xorl	%eax, %eax
	addq	$64, %rsp
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
.subsections_via_symbols
