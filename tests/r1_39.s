	.globl main
main:
	pushq	%rbp
	pushq	%rax
	movq	%rsp,	%rbp
	subq	$0,	%rsp
	movq	$7,	%rdx
	addq	$3,	%rdx
	movq	%rdx,	%rcx
	neg	%rcx
	movq	$52,	%rdx
	addq	%rcx,	%rdx
	movq	%rdx,	%rax
	movq	%rax,	%rdi
	callq	print_int
	addq	$0,	%rsp
	popq	%rax
	and	$0,	%rax
	popq	%rbp
	retq


