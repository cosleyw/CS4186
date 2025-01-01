EXTERN exit


GLOBAL _Add
_Add:
	push rbp
	mov rbp, rsp	

	mov eax, [rbp + 8*(2 + 0)]
	mov edx, [rbp + 8*(2 + 1)]

	add eax, edx

	mov rsp, rbp
	pop rbp
	ret

GLOBAL _Sub
GLOBAL _EQ
_Sub:
_EQ:
	push rbp
	mov rbp, rsp	

	mov eax, [rbp + 8*(2 + 0)]
	mov edx, [rbp + 8*(2 + 1)]

	sub eax, edx

	mov rsp, rbp
	pop rbp
	ret

GLOBAL _Not
_Not:
	push rbp
	mov rbp, rsp	

	mov eax, [rbp + 8*(2 + 0)]
	cmp eax, 0
	sete al

	mov rsp, rbp
	pop rbp
	ret


GLOBAL _Mul
_Mul:
	push rbp
	mov rbp, rsp	

	mov eax, [rbp + 8*(2 + 0)]
	mov edx, [rbp + 8*(2 + 1)]

	imul eax, edx

	mov rsp, rbp
	pop rbp
	ret

GLOBAL _Div
_Div:
	push rbp
	mov rbp, rsp	

	mov eax, [rbp + 8*(2 + 0)]
	mov ecx, [rbp + 8*(2 + 1)]

	cdq
	idiv ecx

	mov rsp, rbp
	pop rbp
	ret
