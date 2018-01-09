	.globl main
main:
	pushq	%rbp
	pushq	%rax
	movq	%rsp,	%rbp
	subq	$64,	%rsp
	callq	read_int
	movq	%rax,	%rdx
	movq	%rdx,	8(%rsp)
	callq	read_int
	movq	%rax,	%rdx
	movq	%rdx,	%r13
	callq	read_int
	movq	%rax,	%rdx
	movq	%rdx,	%rdi
	pushq	%rdi
	callq	read_int
	popq	%rdi
	movq	%rax,	%rdx
	movq	%rdx,	16(%rsp)
	pushq	%rdi
	callq	read_int
	popq	%rdi
	movq	%rax,	%rdx
	movq	%rdx,	%r14
	pushq	%rdi
	callq	read_int
	popq	%rdi
	movq	%rax,	%rdx
	movq	%rdx,	0(%rsp)
	pushq	%rdi
	callq	read_int
	popq	%rdi
	movq	%rax,	%rdx
	movq	%rdx,	%rsi
	pushq	%rsi
	pushq	%rdi
	callq	read_int
	popq	%rdi
	popq	%rsi
	movq	%rax,	%rdx
	movq	%rdx,	%r15
	pushq	%rdi
	pushq	%rsi
	callq	read_int
	popq	%rsi
	popq	%rdi
	movq	%rax,	%rdx
	movq	%rdx,	%r11
	pushq	%r11
	pushq	%rsi
	pushq	%rdi
	callq	read_int
	popq	%rdi
	popq	%rsi
	popq	%r11
	movq	%rax,	%rdx
	movq	%rdx,	%r12
	pushq	%rdi
	pushq	%rsi
	pushq	%r11
	callq	read_int
	popq	%r11
	popq	%rsi
	popq	%rdi
	movq	%rax,	%rdx
	pushq	%rdx
	pushq	%r11
	pushq	%rsi
	pushq	%rdi
	callq	read_int
	popq	%rdi
	popq	%rsi
	popq	%r11
	popq	%rdx
	movq	%rax,	%rcx
	movq	%rcx,	%rbx
	pushq	%rdi
	pushq	%rsi
	pushq	%r11
	pushq	%rdx
	callq	read_int
	popq	%rdx
	popq	%r11
	popq	%rsi
	popq	%rdi
	movq	%rax,	%rcx
	movq	%rcx,	%r8
	pushq	%r8
	pushq	%rdx
	pushq	%r11
	pushq	%rsi
	pushq	%rdi
	callq	read_int
	popq	%rdi
	popq	%rsi
	popq	%r11
	popq	%rdx
	popq	%r8
	movq	%rax,	%rcx
	movq	%rcx,	%r10
	pushq	%rdi
	pushq	%rsi
	pushq	%r11
	pushq	%rdx
	pushq	%r10
	pushq	%r8
	callq	read_int
	popq	%r8
	popq	%r10
	popq	%rdx
	popq	%r11
	popq	%rsi
	popq	%rdi
	movq	%rax,	%rcx
	pushq	%rcx
	pushq	%r8
	pushq	%r10
	pushq	%rdx
	pushq	%r11
	pushq	%rsi
	pushq	%rdi
	callq	read_int
	popq	%rdi
	popq	%rsi
	popq	%r11
	popq	%rdx
	popq	%r10
	popq	%r8
	popq	%rcx
	movq	%rax,	%r9
	neg	%r13
	movq	8(%rsp),	%rax
	movq	%rax,	8(%rsp)
	addq	%r13,	8(%rsp)
	movq	16(%rsp),	%r13
	neg	%r13
	addq	%r13,	%rdi
	movq	8(%rsp),	%r13
	addq	%rdi,	%r13
	movq	0(%rsp),	%rdi
	neg	%rdi
	addq	%rdi,	%r14
	movq	%r15,	%rdi
	neg	%rdi
	addq	%rdi,	%rsi
	movq	%r14,	%rdi
	addq	%rsi,	%rdi
	movq	%r13,	%rsi
	addq	%rdi,	%rsi
	movq	%r12,	%rdi
	neg	%rdi
	addq	%rdi,	%r11
	movq	%rbx,	%rdi
	neg	%rdi
	addq	%rdi,	%rdx
	movq	%r11,	%rdi
	addq	%rdx,	%rdi
	movq	%r10,	%rdx
	neg	%rdx
	addq	%rdx,	%r8
	movq	%r9,	%rdx
	neg	%rdx
	addq	%rdx,	%rcx
	movq	%r8,	%rdx
	addq	%rcx,	%rdx
	movq	%rdi,	%rcx
	addq	%rdx,	%rcx
	movq	%rsi,	%rdx
	addq	%rcx,	%rdx
	addq	$42,	%rdx
	movq	%rdx,	%rax
	movq	%rax,	%rdi
	callq	print_int
	addq	$64,	%rsp
	popq	%rax
	and	$0,	%rax
	popq	%rbp
	retq


