	.globl main
main:
	pushq	%rbp
	pushq	%rax
	movq	%rsp,	%rbp
	subq	$0,	%rsp
	movq	$1,	%rcx
	addq	$1,	%rcx
	movq	$1,	%rdx
	addq	$1,	%rdx
	movq	%rcx,	%rsi
	addq	%rdx,	%rsi
	movq	$1,	%rdx
	addq	$1,	%rdx
	movq	$1,	%rcx
	addq	$1,	%rcx
	addq	%rcx,	%rdx
	addq	%rdx,	%rsi
	movq	$1,	%rcx
	addq	$1,	%rcx
	movq	$1,	%rdx
	addq	$1,	%rdx
	movq	%rcx,	%rdi
	addq	%rdx,	%rdi
	movq	$1,	%rcx
	addq	$1,	%rcx
	movq	$1,	%rdx
	addq	$1,	%rdx
	addq	%rdx,	%rcx
	movq	%rdi,	%rdx
	addq	%rcx,	%rdx
	addq	%rdx,	%rsi
	movq	$1,	%rcx
	addq	$1,	%rcx
	movq	$1,	%rdx
	addq	$1,	%rdx
	movq	%rcx,	%rdi
	addq	%rdx,	%rdi
	movq	$1,	%rdx
	addq	$1,	%rdx
	movq	$1,	%rcx
	addq	$1,	%rcx
	addq	%rcx,	%rdx
	addq	%rdx,	%rdi
	movq	$1,	%rcx
	addq	$1,	%rcx
	movq	$1,	%rdx
	addq	$1,	%rdx
	movq	%rcx,	%r8
	addq	%rdx,	%r8
	movq	$1,	%rdx
	addq	$1,	%rdx
	movq	$1,	%rcx
	addq	$1,	%rcx
	addq	%rcx,	%rdx
	movq	%r8,	%rcx
	addq	%rdx,	%rcx
	movq	%rdi,	%rdx
	addq	%rcx,	%rdx
	movq	%rsi,	%rcx
	addq	%rdx,	%rcx
	movq	$10,	%rdx
	addq	%rcx,	%rdx
	movq	%rdx,	%rax
	movq	%rax,	%rdi
	callq	print_int
	addq	$0,	%rsp
	popq	%rax
	and	$0,	%rax
	popq	%rbp
	retq


