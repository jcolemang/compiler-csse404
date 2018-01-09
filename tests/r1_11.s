	.globl main
main:
	pushq	%rbp
	pushq	%rax
	movq	%rsp,	%rbp
	subq	$0,	%rsp
	movq	$1,	%rdx
	addq	$1,	%rdx
	movq	$1,	%rcx
	addq	%rdx,	%rcx
	movq	$1,	%rdx
	addq	%rcx,	%rdx
	movq	$1,	%rcx
	addq	%rdx,	%rcx
	movq	$1,	%rdx
	addq	%rcx,	%rdx
	movq	$1,	%rcx
	addq	%rdx,	%rcx
	movq	$1,	%rdx
	addq	%rcx,	%rdx
	movq	$1,	%rcx
	addq	%rdx,	%rcx
	movq	$1,	%rdx
	addq	%rcx,	%rdx
	movq	$1,	%rcx
	addq	%rdx,	%rcx
	movq	$1,	%rdx
	addq	%rcx,	%rdx
	movq	$1,	%rcx
	addq	%rdx,	%rcx
	movq	$1,	%rdx
	addq	%rcx,	%rdx
	movq	$1,	%rcx
	addq	%rdx,	%rcx
	movq	$1,	%rdx
	addq	%rcx,	%rdx
	movq	$1,	%rcx
	addq	%rdx,	%rcx
	movq	$1,	%rdx
	addq	%rcx,	%rdx
	movq	$1,	%rcx
	addq	%rdx,	%rcx
	movq	$1,	%rdx
	addq	%rcx,	%rdx
	movq	$1,	%rcx
	addq	%rdx,	%rcx
	movq	$1,	%rdx
	addq	%rcx,	%rdx
	movq	$1,	%rcx
	addq	%rdx,	%rcx
	movq	$1,	%rdx
	addq	%rcx,	%rdx
	movq	$1,	%rcx
	addq	%rdx,	%rcx
	movq	$1,	%rdx
	addq	%rcx,	%rdx
	movq	$1,	%rcx
	addq	%rdx,	%rcx
	movq	$1,	%rdx
	addq	%rcx,	%rdx
	movq	$1,	%rcx
	addq	%rdx,	%rcx
	movq	$1,	%rdx
	addq	%rcx,	%rdx
	movq	$1,	%rcx
	addq	%rdx,	%rcx
	movq	$1,	%rdx
	addq	%rcx,	%rdx
	movq	$1,	%rcx
	addq	%rdx,	%rcx
	movq	$1,	%rdx
	addq	%rcx,	%rdx
	movq	$1,	%rcx
	addq	%rdx,	%rcx
	movq	$1,	%rdx
	addq	%rcx,	%rdx
	movq	$1,	%rcx
	addq	%rdx,	%rcx
	movq	$1,	%rdx
	addq	%rcx,	%rdx
	movq	$1,	%rcx
	addq	%rdx,	%rcx
	movq	$1,	%rdx
	addq	%rcx,	%rdx
	movq	$1,	%rcx
	addq	%rdx,	%rcx
	movq	$1,	%rdx
	addq	%rcx,	%rdx
	movq	%rdx,	%rax
	movq	%rax,	%rdi
	callq	print_int
	addq	$0,	%rsp
	popq	%rax
	and	$0,	%rax
	popq	%rbp
	retq


