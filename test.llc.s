	.text
	.file	"test.llc"
	.globl	main                            # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movl	%edi, 4(%rsp)
	movl	$72, %edi
	callq	puts@PLT
	movl	$101, %edi
	callq	puts@PLT
	movl	$108, %edi
	callq	puts@PLT
	movl	$108, %edi
	callq	puts@PLT
	movl	$111, %edi
	callq	puts@PLT
	movl	$32, %edi
	callq	puts@PLT
	movl	$87, %edi
	callq	puts@PLT
	movl	$111, %edi
	callq	puts@PLT
	movl	$114, %edi
	callq	puts@PLT
	movl	$108, %edi
	callq	puts@PLT
	movl	$100, %edi
	callq	puts@PLT
	movl	$10, %edi
	callq	puts@PLT
	popq	%rax
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
	.section	".note.GNU-stack","",@progbits
