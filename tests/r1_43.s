	.globl main
main:
	pushq	%rbp
	pushq	%rax
	movq	%rsp,	%rbp
	subq	$0,	%rsp
	movq	$1,	%rdx
	movq	$46,	%rsi
	addq	$7,	%rdx
	movq	$4,	%rcx
	addq	%rdx,	%rcx
	addq	%rsi,	%rdx
	neg	%rcx
	addq	%rcx,	%rdx
	movq	%rdx,	%rax
	movq	%rax,	%rdi
	callq	print_int
	addq	$0,	%rsp
	popq	%rax
	and	$0,	%rax
	popq	%rbp
	retq


