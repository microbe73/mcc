	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 14, 0	sdk_version 15, 2
	.globl	_main                           ## -- Begin function main
	.p2align	4, 0x90
_main:                                  ## @main
## %bb.0:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, -4(%rbp)
  movq  $2, %rcx
  movq	$125, %rax                    ## imm = 0x2B80
  shrq  %cl, %rax
	popq	%rbp
	retq
                                        ## -- End function
.subsections_via_symbols
