	.text
	.file	"test.c"
	.globl	pow                             # -- Begin function pow
	.p2align	4, 0x90
	.type	pow,@function
pow:                                    # @pow
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	pushq	%rbx
	.cfi_offset %rbx, -24
	movsd	%xmm0, -16(%rbp)
	movsd	%xmm1, -24(%rbp)
	movl	$2687, -28(%rbp)                # imm = 0xA7F
	movl	$0, -32(%rbp)
	#APP
	fnstcw	-32(%rbp)
	fldcw	-28(%rbp)
	fldl	-24(%rbp)
	fldl	-16(%rbp)
	fyl2x
	fld	%st(0)
	frndint
	fldz
	fcomi	%st(1), %st
	ja	NEGATIVE
POSITIVE:
	faddp	%st, %st(1)
	movl	$0, %eax
	jmp	FRACTIONSTEPS
NEGATIVE:
	faddp	%st, %st(1)
	movl	$1, %eax
	jmp	FRACTIONSTEPS
FRACTIONSTEPS:
	fsub	%st(1), %st
	fldz
	fcomi	%st(1), %st
	je	NOREMAINDER
	jmp	REMAINDER
REMAINDER:
	faddp	%st, %st(1)
	fld1
	fsubp	%st, %st(1)
	f2xm1
	fld1
	faddp	%st, %st(1)
	fscale
	testl	%eax, %eax
	je	FINAL
	jmp	DIVIDEBYTWO
NOREMAINDER:
	faddp	%st, %st(1)
	fxch	%st(1)
	fld1
	fscale
	jmp	FINAL
DIVIDEBYTWO:
	fld1
	fld1
	faddp	%st, %st(1)
	fxch	%st(1)
	fdivp	%st, %st(1)
	jmp	FINAL
FINAL:
	fstpl	-16(%rbp)
	fldcw	-32(%rbp)

	#NO_APP
	movsd	-16(%rbp), %xmm0                # xmm0 = mem[0],zero
	popq	%rbx
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end0:
	.size	pow, .Lfunc_end0-pow
	.cfi_endproc
                                        # -- End function
	.ident	"clang version 19.0.0git (https://github.com/llvm/llvm-project.git 512a8a78a7f3bffc41cd7c00788eb099519f4625)"
	.section	".note.GNU-stack","",@progbits
	.addrsig
