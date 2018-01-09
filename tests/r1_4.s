	.globl main
main:
	pushq	%rbp
	pushq	%rax
	movq	%rsp,	%rbp
	subq	$0,	%rsp
	movq	$10,	%rsi
	addq	$11,	%rsi
	movq	$4,	%rdx
	neg	%rdx
	movq	$25,	%rcx
	addq	%rdx,	%rcx
	movq	%rsi,	%rdx
	addq	%rcx,	%rdx
	movq	%rdx,	%rax
	movq	%rax,	%rdi
	callq	print_int
	addq	$0,	%rsp
	popq	%rax
	and	$0,	%rax
	popq	%rbp
	retq


