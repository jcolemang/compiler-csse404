	.globl main
main:
	pushq	%rbp
	pushq	%rax
	movq	%rsp,	%rbp
	subq	$0,	%rsp
	movq	$5,	%rcx
	neg	%rcx
	movq	$47,	%rdx
	addq	%rcx,	%rdx
	movq	%rdx,	%rax
	movq	%rax,	%rdi
	callq	print_int
	addq	$0,	%rsp
	popq	%rax
	and	$0,	%rax
	popq	%rbp
	retq


