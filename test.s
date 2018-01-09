	.globl main
main:
	pushq	%rbp
	pushq	%rax
	movq	%rsp,	%rbp
	subq	$8,	%rsp
	callq	read_int
	movq	%rax,	%rdx
	pushq	%rdx
	callq	read_int
	popq	%rdx
	movq	%rax,	0(%rsp)
	movq	0(%rsp),	%rax
	movq	%rax,	0(%rsp)
	movq	0(%rsp),	%rax
	movq	%rax,	0(%rsp)
	movq	0(%rsp),	%rax
	neg	%rax
	movq	%rax,	0(%rsp)
	addq	0(%rsp),	%rdx
	movq	%rdx,	%rax
	movq	%rax,	%rdi
	callq	print_int
	addq	$8,	%rsp
	popq	%rax
	and	$0,	%rax
	popq	%rbp
	retq

