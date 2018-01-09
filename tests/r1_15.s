	.globl main
main:
	pushq	%rbp
	pushq	%rax
	movq	%rsp,	%rbp
	subq	$0,	%rsp
	callq	read_int
	movq	%rax,	%rdx
	pushq	%rdx
	callq	read_int
	popq	%rdx
	movq	%rax,	%rcx
	addq	%rcx,	%rdx
	movq	%rdx,	%rax
	movq	%rax,	%rdi
	callq	print_int
	addq	$0,	%rsp
	popq	%rax
	and	$0,	%rax
	popq	%rbp
	retq


