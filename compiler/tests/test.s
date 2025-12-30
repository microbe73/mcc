    .globl _main
_main:
    movq $1, %rax
    cmpq $0, %rax
    je _label1
    movq $1, %rax
    jmp _label2
_label1:
    movq $2, %rax
    pushq %rax
    movq $4, %rax
    popq %rcx
    and %rcx, %rax
    pushq %rax
    movq $9, %rax
    pushq %rax
    movq $3, %rax
    movq %rax, %rcx
    popq %rax
    shrq %cl, %rax
    pushq %rax
    movq $1, %rax
    movq %rax, %rcx
    popq %rax
    shlq %cl, %rax
    popq %rcx
    xor %rcx, %rax
    cmpq $0, %rax
    jne _label3
    jmp _label4
_label3:
    movq $6, %rax
    pushq %rax
    movq $5, %rax
    pushq %rax
    movq $6, %rax
    popq %rcx
    cmpq %rax, %rcx
    movq $0, %rax
    setle %al
    pushq %rax
    movq $7, %rax
    popq %rcx
    cmpq %rax, %rcx
    movq $0, %rax
    setg %al
    pushq %rax
    movq $8, %rax
    popq %rcx
    cmpq %rax, %rcx
    movq $0, %rax
    setl %al
    pushq %rax
    movq $9, %rax
    popq %rcx
    cmpq %rax, %rcx
    movq $0, %rax
    setge %al
    popq %rcx
    cmpq %rax, %rcx
    movq $0, %rax
    sete %al
    pushq %rax
    movq $10, %rax
    pushq %rax
    movq $11, %rax
    popq %rcx
    pushq %rax
    movq %rcx, %rax
    popq %rcx
    cqo
    idiv %rcx
    movq %rdx, %rax
    popq %rcx
    or %rcx, %rax
    cmpq $0, %rax
    movq $0, %rax
    setne %al
_label4:
    cmpq $0, %rax
    movq $0, %rax
    setne %al
_label2:
    retq