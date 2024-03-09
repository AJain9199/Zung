	.text
	.file	"test.c"
	.globl	flr                             # -- Begin function flr
	.p2align	4, 0x90
	.type	flr,@function
flr:                                    # @flr
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	cmpl	$0, -8(%rbp)
	je	.LBB0_2
# %bb.1:
	jmp	.LBB0_3
.LBB0_2:
	leaq	.L.str(%rip), %rdi
	leaq	.L.str.1(%rip), %rsi
	movl	$5, %edx
	leaq	.L__PRETTY_FUNCTION__.flr(%rip), %rcx
	callq	__assert_fail@PLT
.LBB0_3:
	movl	-4(%rbp), %edi
	movl	-8(%rbp), %esi
	callq	div@PLT
	movq	%rax, -16(%rbp)
	cmpl	$0, -12(%rbp)
	je	.LBB0_6
# %bb.4:
	cmpl	$0, -4(%rbp)
	setl	%al
	andb	$1, %al
	movzbl	%al, %eax
	cmpl	$0, -8(%rbp)
	setl	%cl
	andb	$1, %cl
	movzbl	%cl, %ecx
	xorl	%ecx, %eax
	cmpl	$0, %eax
	je	.LBB0_6
# %bb.5:
	movl	-16(%rbp), %eax
	addl	$-1, %eax
	movl	%eax, -16(%rbp)
.LBB0_6:
	movl	-16(%rbp), %eax
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end0:
	.size	flr, .Lfunc_end0-flr
	.cfi_endproc
                                        # -- End function
	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"b != 0"
	.size	.L.str, 7

	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"test.c"
	.size	.L.str.1, 7

	.type	.L__PRETTY_FUNCTION__.flr,@object # @__PRETTY_FUNCTION__.flr
.L__PRETTY_FUNCTION__.flr:
	.asciz	"int flr(int, int)"
	.size	.L__PRETTY_FUNCTION__.flr, 18

	.ident	"clang version 19.0.0git (https://github.com/llvm/llvm-project.git 512a8a78a7f3bffc41cd7c00788eb099519f4625)"
	.section	".note.GNU-stack","",@progbits
	.addrsig
	.addrsig_sym __assert_fail
	.addrsig_sym div
