	.globl main
main:
	pushq	%rbp
	pushq	%rax
	movq	%rsp,	%rbp
	subq	$0,	%rsp
	movq	$10,	%rdx
	addq	$32,	%rdx
	movq	%rdx,	%rax
	movq	%rax,	%rdi
	callq	print_int
	addq	$0,	%rsp
	popq	%rax
	and	$0,	%rax
	popq	%rbp
	retq


