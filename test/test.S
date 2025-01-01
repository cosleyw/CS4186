%include"my_crt.S"
GLOBAL _start
_start:
jmp _Ffac_end
fac:
push rbp
mov rbp, rsp
MOV eax, 0
PUSH rax
MOV eax, [rbp + 8*(2 + 0)]
PUSH rax
CALL _EQ
add rsp, 16
test eax, eax
jne .else0
MOV eax, 1
mov rsp, rbp
pop rbp
ret
.else0:
MOV eax, [rbp + 8*(2 + 0)]
PUSH rax
MOV eax, 1
PUSH rax
MOV eax, [rbp + 8*(2 + 0)]
PUSH rax
CALL _Sub
add rsp, 16
PUSH rax
CALL fac
add rsp, 8
PUSH rax
CALL _Mul
add rsp, 16
mov rsp, rbp
pop rbp
ret
mov rsp, rbp
pop rbp
ret
_Ffac_end:
jmp _Fmain_end
main:
push rbp
mov rbp, rsp
MOV eax, 5
PUSH rax
CALL fac
add rsp, 8
mov rsp, rbp
pop rbp
ret
mov rsp, rbp
pop rbp
ret
_Fmain_end:
MOV eax, 1
PUSH rax
CALL main
MOV edi, eax
CALL exit
