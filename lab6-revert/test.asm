	.file	"local_test/bellatrix-dfs.l4"
	.globl	_c0_DFS
_c0_DFS:
	pushq %rbp
	movq %rsp, %rbp
	pushq %rbx
	pushq %r12
	pushq %r13
	pushq %r14
	pushq %r15
	subq $264, %rsp
	movq %rdi, -96(%rbp)
	movq %rsi, -88(%rbp)
	movq %rdx, %r15
	movq %rcx, %r14
	movq -88(%rbp), %r11
	mov %r11, -112(%rbp)
	movq %r15, -104(%rbp)
	movq -112(%rbp), %r11
	cmpl -104(%rbp), %r11d
	je label232
	jmp label233
label232:
	movq $1, -104(%rbp)
	jmp label234
label233:
	movq $0, -104(%rbp)
	jmp label234
label234:
	movq -104(%rbp), %r11
	cmpl $0, %r11d
	jne label0
jmp label1
label0:
	movq $1, %rax
	addq $264, %rsp
	popq %r15
	popq %r14
	popq %r13
	popq %r12
	popq %rbx
	popq %rbp
	ret
	jmp label2
label1:
	jmp label2
label2:
	movq %r14, -104(%rbp)
	movq -104(%rbp), %r11
	cmpl $0, %r11d
	jne label228
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label228:
	movq (%r11), %r10
	movq %r10, -112(%rbp)
	movq -88(%rbp), %r11
	mov %r11, -104(%rbp)
	movq -112(%rbp), %r11
	movq $0, %r10
	cmpq %r11, %r10
	jle label227
	jmp label226
label227:
	movq -8(%r11), %r10
	movq -104(%rbp), %rdi
	cmpl %edi, %r10d
	jle label226
	jmp label225
label226:
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label225:
	cmpl $0, %edi
	jl label226
	imulq $4, %rdi
	addl %edi,%r11d
	movq %r11, -104(%rbp)
	movq $1, -112(%rbp)
	movq -104(%rbp), %r11
	movq -112(%rbp), %r10
	cmpl $0, %r11d
	jne label224
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label224:
	movl %r10d, (%r11)
	movq $0, %r13
label12:
	movq %r13, %r12
	movq $6, %rbx
	cmpl %ebx, %r12d
	jl label238
	jmp label239
label238:
	movq $1, -104(%rbp)
	jmp label240
label239:
	movq $0, -104(%rbp)
	jmp label240
label240:
	movq -104(%rbp), %r11
	cmpl $0, %r11d
	jne label13
jmp label14
label13:
	movq %r14, -104(%rbp)
	movq -104(%rbp), %r11
	cmpl $0, %r11d
	jne label223
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label223:
	movq (%r11), %r10
	movq %r10, -112(%rbp)
	movq %r13, -104(%rbp)
	movq -112(%rbp), %r11
	movq $0, %r10
	cmpq %r11, %r10
	jne label222
	jmp label221
label222:
	movq -8(%r11), %r10
	movq -104(%rbp), %rdi
	cmpl %edi, %r10d
	jle label221
	jmp label220
label221:
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label220:
	cmpq $0, %rdi
	jl label221
	imulq $4, %rdi
	addl %edi,%r11d
	movl (%r11), %r10d
	movq %r10, -104(%rbp)
	movq -104(%rbp), %r11
	xorq $1, %r11
	movq %r11, -104(%rbp)
	movq -104(%rbp), %r11
	cmpl $0, %r11d
	jne label9
jmp label10
label9:
	movq -96(%rbp), %r11
	mov %r11, -112(%rbp)
	movq -88(%rbp), %r11
	mov %r11, -104(%rbp)
	movq -112(%rbp), %r11
	movq $0, %r10
	cmpq %r11, %r10
	jne label219
	jmp label218
label219:
	movq -8(%r11), %r10
	movq -104(%rbp), %rdi
	cmpl %edi, %r10d
	jle label218
	jmp label217
label218:
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label217:
	cmpq $0, %rdi
	jl label218
	imulq $8, %rdi
	addl %edi,%r11d
	movq (%r11), %r10
	movq %r10, -112(%rbp)
	movq %r13, -104(%rbp)
	movq -112(%rbp), %r11
	movq $0, %r10
	cmpq %r11, %r10
	jne label216
	jmp label215
label216:
	movq -8(%r11), %r10
	movq -104(%rbp), %rdi
	cmpl %edi, %r10d
	jle label215
	jmp label214
label215:
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label214:
	cmpq $0, %rdi
	jl label215
	imulq $4, %rdi
	addl %edi,%r11d
	movl (%r11), %r10d
	movq %r10, -104(%rbp)
	movq -104(%rbp), %r11
	cmpl $0, %r11d
	jne label6
jmp label7
label6:
	movq -96(%rbp), %r11
	mov %r11, -128(%rbp)
	movq %r13, -120(%rbp)
	movq %r15, -112(%rbp)
	movq %r14, -104(%rbp)
	pushq %rdi
	movq -128(%rbp), %rdi
	pushq %rsi
	movq -120(%rbp), %rsi
	pushq %rdx
	movq -112(%rbp), %rdx
	pushq %rcx
	movq -104(%rbp), %rcx
	subq $16, %rsp
	callq _c0_DFS
	addq $16, %rsp
	popq %rcx
	popq %rdx
	popq %rsi
	popq %rdi
	movq %rax, -104(%rbp)
	movq -104(%rbp), %r11
	cmpl $0, %r11d
	jne label3
jmp label4
label3:
	movq $1, %rax
	addq $264, %rsp
	popq %r15
	popq %r14
	popq %r13
	popq %r12
	popq %rbx
	popq %rbp
	ret
	jmp label5
label4:
	jmp label5
label5:
	jmp label8
label7:
	jmp label8
label8:
	jmp label11
label10:
	jmp label11
label11:
	movq %r13, -112(%rbp)
	movq $1, -104(%rbp)
	movq -112(%rbp), %r11
	addq -104(%rbp), %r11
	movq %r11, %r13
	jmp label12
label14:
	movq $0, %rax
	addq $264, %rsp
	popq %r15
	popq %r14
	popq %r13
	popq %r12
	popq %rbx
	popq %rbp
	ret
	.globl	_c0_main
_c0_main:
	pushq %rbp
	movq %rsp, %rbp
	pushq %rbx
	pushq %r12
	pushq %r13
	pushq %r14
	pushq %r15
	subq $1696, %rsp
	movq $6, %rbx
	movq %rbx, %r11
	cmpl $0, %r11d
	jge label213
	movq $12,%rdi
	subq $16, %rsp
	callq raise
label213:
	imulq $4, %r11
	addq $8, %r11
	pushq %rdi
	movq %r11, %rdi
	subq $16, %rsp
	callq malloc
	addq $16, %rsp
	popq %rdi
	movq %rbx, %r11
	movq %r11,(%rax)
	addq $8, %rax
	movq %rax, -200(%rbp)
	movq -200(%rbp), %r11
	mov %r11, -216(%rbp)
	movq $0, -208(%rbp)
	movq -216(%rbp), %r11
	movq $0, %r10
	cmpq %r11, %r10
	jle label212
	jmp label211
label212:
	movq -8(%r11), %r10
	movq -208(%rbp), %rdi
	cmpl %edi, %r10d
	jle label211
	jmp label210
label211:
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label210:
	cmpl $0, %edi
	jl label211
	imulq $4, %rdi
	addl %edi,%r11d
	movq %r11, -208(%rbp)
	movq $0, -216(%rbp)
	movq -208(%rbp), %r11
	movq -216(%rbp), %r10
	cmpl $0, %r11d
	jne label209
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label209:
	movl %r10d, (%r11)
	movq -200(%rbp), %r11
	mov %r11, -216(%rbp)
	movq $1, -208(%rbp)
	movq -216(%rbp), %r11
	movq $0, %r10
	cmpq %r11, %r10
	jle label208
	jmp label207
label208:
	movq -8(%r11), %r10
	movq -208(%rbp), %rdi
	cmpl %edi, %r10d
	jle label207
	jmp label206
label207:
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label206:
	cmpl $0, %edi
	jl label207
	imulq $4, %rdi
	addl %edi,%r11d
	movq %r11, -208(%rbp)
	movq $0, -216(%rbp)
	movq -208(%rbp), %r11
	movq -216(%rbp), %r10
	cmpl $0, %r11d
	jne label205
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label205:
	movl %r10d, (%r11)
	movq -200(%rbp), %r11
	mov %r11, -216(%rbp)
	movq $2, -208(%rbp)
	movq -216(%rbp), %r11
	movq $0, %r10
	cmpq %r11, %r10
	jle label204
	jmp label203
label204:
	movq -8(%r11), %r10
	movq -208(%rbp), %rdi
	cmpl %edi, %r10d
	jle label203
	jmp label202
label203:
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label202:
	cmpl $0, %edi
	jl label203
	imulq $4, %rdi
	addl %edi,%r11d
	movq %r11, -208(%rbp)
	movq $1, -216(%rbp)
	movq -208(%rbp), %r11
	movq -216(%rbp), %r10
	cmpl $0, %r11d
	jne label201
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label201:
	movl %r10d, (%r11)
	movq -200(%rbp), %r11
	mov %r11, -216(%rbp)
	movq $3, -208(%rbp)
	movq -216(%rbp), %r11
	movq $0, %r10
	cmpq %r11, %r10
	jle label200
	jmp label199
label200:
	movq -8(%r11), %r10
	movq -208(%rbp), %rdi
	cmpl %edi, %r10d
	jle label199
	jmp label198
label199:
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label198:
	cmpl $0, %edi
	jl label199
	imulq $4, %rdi
	addl %edi,%r11d
	movq %r11, -208(%rbp)
	movq $1, -216(%rbp)
	movq -208(%rbp), %r11
	movq -216(%rbp), %r10
	cmpl $0, %r11d
	jne label197
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label197:
	movl %r10d, (%r11)
	movq -200(%rbp), %r11
	mov %r11, -216(%rbp)
	movq $4, -208(%rbp)
	movq -216(%rbp), %r11
	movq $0, %r10
	cmpq %r11, %r10
	jle label196
	jmp label195
label196:
	movq -8(%r11), %r10
	movq -208(%rbp), %rdi
	cmpl %edi, %r10d
	jle label195
	jmp label194
label195:
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label194:
	cmpl $0, %edi
	jl label195
	imulq $4, %rdi
	addl %edi,%r11d
	movq %r11, -208(%rbp)
	movq $1, -216(%rbp)
	movq -208(%rbp), %r11
	movq -216(%rbp), %r10
	cmpl $0, %r11d
	jne label193
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label193:
	movl %r10d, (%r11)
	movq -200(%rbp), %r11
	mov %r11, -216(%rbp)
	movq $5, -208(%rbp)
	movq -216(%rbp), %r11
	movq $0, %r10
	cmpq %r11, %r10
	jle label192
	jmp label191
label192:
	movq -8(%r11), %r10
	movq -208(%rbp), %rdi
	cmpl %edi, %r10d
	jle label191
	jmp label190
label191:
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label190:
	cmpl $0, %edi
	jl label191
	imulq $4, %rdi
	addl %edi,%r11d
	movq %r11, -208(%rbp)
	movq $0, -216(%rbp)
	movq -208(%rbp), %r11
	movq -216(%rbp), %r10
	cmpl $0, %r11d
	jne label189
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label189:
	movl %r10d, (%r11)
	movq $6, -208(%rbp)
	movq -208(%rbp), %r11
	cmpl $0, %r11d
	jge label188
	movq $12,%rdi
	subq $16, %rsp
	callq raise
label188:
	imulq $4, %r11
	addq $8, %r11
	pushq %rdi
	movq %r11, %rdi
	subq $16, %rsp
	callq malloc
	addq $16, %rsp
	popq %rdi
	movq -208(%rbp), %r11
	movq %r11,(%rax)
	addq $8, %rax
	movq %rax, -184(%rbp)
	movq -200(%rbp), %r11
	mov %r11, -216(%rbp)
	movq $0, -208(%rbp)
	movq -216(%rbp), %r11
	movq $0, %r10
	cmpq %r11, %r10
	jle label187
	jmp label186
label187:
	movq -8(%r11), %r10
	movq -208(%rbp), %rdi
	cmpl %edi, %r10d
	jle label186
	jmp label185
label186:
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label185:
	cmpl $0, %edi
	jl label186
	imulq $4, %rdi
	addl %edi,%r11d
	movq %r11, -208(%rbp)
	movq $0, -216(%rbp)
	movq -208(%rbp), %r11
	movq -216(%rbp), %r10
	cmpl $0, %r11d
	jne label184
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label184:
	movl %r10d, (%r11)
	movq -200(%rbp), %r11
	mov %r11, -216(%rbp)
	movq $1, -208(%rbp)
	movq -216(%rbp), %r11
	movq $0, %r10
	cmpq %r11, %r10
	jle label183
	jmp label182
label183:
	movq -8(%r11), %r10
	movq -208(%rbp), %rdi
	cmpl %edi, %r10d
	jle label182
	jmp label181
label182:
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label181:
	cmpl $0, %edi
	jl label182
	imulq $4, %rdi
	addl %edi,%r11d
	movq %r11, -208(%rbp)
	movq $0, -216(%rbp)
	movq -208(%rbp), %r11
	movq -216(%rbp), %r10
	cmpl $0, %r11d
	jne label180
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label180:
	movl %r10d, (%r11)
	movq -184(%rbp), %r11
	mov %r11, -216(%rbp)
	movq $2, -208(%rbp)
	movq -216(%rbp), %r11
	movq $0, %r10
	cmpq %r11, %r10
	jle label179
	jmp label178
label179:
	movq -8(%r11), %r10
	movq -208(%rbp), %rdi
	cmpl %edi, %r10d
	jle label178
	jmp label177
label178:
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label177:
	cmpl $0, %edi
	jl label178
	imulq $4, %rdi
	addl %edi,%r11d
	movq %r11, -208(%rbp)
	movq $1, -216(%rbp)
	movq -208(%rbp), %r11
	movq -216(%rbp), %r10
	cmpl $0, %r11d
	jne label176
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label176:
	movl %r10d, (%r11)
	movq -184(%rbp), %r11
	mov %r11, -216(%rbp)
	movq $3, -208(%rbp)
	movq -216(%rbp), %r11
	movq $0, %r10
	cmpq %r11, %r10
	jle label175
	jmp label174
label175:
	movq -8(%r11), %r10
	movq -208(%rbp), %rdi
	cmpl %edi, %r10d
	jle label174
	jmp label173
label174:
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label173:
	cmpl $0, %edi
	jl label174
	imulq $4, %rdi
	addl %edi,%r11d
	movq %r11, -208(%rbp)
	movq $0, -216(%rbp)
	movq -208(%rbp), %r11
	movq -216(%rbp), %r10
	cmpl $0, %r11d
	jne label172
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label172:
	movl %r10d, (%r11)
	movq -184(%rbp), %r11
	mov %r11, -216(%rbp)
	movq $4, -208(%rbp)
	movq -216(%rbp), %r11
	movq $0, %r10
	cmpq %r11, %r10
	jle label171
	jmp label170
label171:
	movq -8(%r11), %r10
	movq -208(%rbp), %rdi
	cmpl %edi, %r10d
	jle label170
	jmp label169
label170:
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label169:
	cmpl $0, %edi
	jl label170
	imulq $4, %rdi
	addl %edi,%r11d
	movq %r11, -208(%rbp)
	movq $1, -216(%rbp)
	movq -208(%rbp), %r11
	movq -216(%rbp), %r10
	cmpl $0, %r11d
	jne label168
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label168:
	movl %r10d, (%r11)
	movq -184(%rbp), %r11
	mov %r11, -216(%rbp)
	movq $5, -208(%rbp)
	movq -216(%rbp), %r11
	movq $0, %r10
	cmpq %r11, %r10
	jle label167
	jmp label166
label167:
	movq -8(%r11), %r10
	movq -208(%rbp), %rdi
	cmpl %edi, %r10d
	jle label166
	jmp label165
label166:
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label165:
	cmpl $0, %edi
	jl label166
	imulq $4, %rdi
	addl %edi,%r11d
	movq %r11, -208(%rbp)
	movq $0, -216(%rbp)
	movq -208(%rbp), %r11
	movq -216(%rbp), %r10
	cmpl $0, %r11d
	jne label164
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label164:
	movl %r10d, (%r11)
	movq $6, -208(%rbp)
	movq -208(%rbp), %r11
	cmpl $0, %r11d
	jge label163
	movq $12,%rdi
	subq $16, %rsp
	callq raise
label163:
	imulq $4, %r11
	addq $8, %r11
	pushq %rdi
	movq %r11, %rdi
	subq $16, %rsp
	callq malloc
	addq $16, %rsp
	popq %rdi
	movq -208(%rbp), %r11
	movq %r11,(%rax)
	addq $8, %rax
	movq %rax, -168(%rbp)
	movq -168(%rbp), %r11
	mov %r11, -216(%rbp)
	movq $0, -208(%rbp)
	movq -216(%rbp), %r11
	movq $0, %r10
	cmpq %r11, %r10
	jle label162
	jmp label161
label162:
	movq -8(%r11), %r10
	movq -208(%rbp), %rdi
	cmpl %edi, %r10d
	jle label161
	jmp label160
label161:
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label160:
	cmpl $0, %edi
	jl label161
	imulq $4, %rdi
	addl %edi,%r11d
	movq %r11, -208(%rbp)
	movq $1, -216(%rbp)
	movq -208(%rbp), %r11
	movq -216(%rbp), %r10
	cmpl $0, %r11d
	jne label159
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label159:
	movl %r10d, (%r11)
	movq -168(%rbp), %r11
	mov %r11, -216(%rbp)
	movq $1, -208(%rbp)
	movq -216(%rbp), %r11
	movq $0, %r10
	cmpq %r11, %r10
	jle label158
	jmp label157
label158:
	movq -8(%r11), %r10
	movq -208(%rbp), %rdi
	cmpl %edi, %r10d
	jle label157
	jmp label156
label157:
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label156:
	cmpl $0, %edi
	jl label157
	imulq $4, %rdi
	addl %edi,%r11d
	movq %r11, -208(%rbp)
	movq $1, -216(%rbp)
	movq -208(%rbp), %r11
	movq -216(%rbp), %r10
	cmpl $0, %r11d
	jne label155
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label155:
	movl %r10d, (%r11)
	movq -168(%rbp), %r11
	mov %r11, -216(%rbp)
	movq $3, -208(%rbp)
	movq -216(%rbp), %r11
	movq $0, %r10
	cmpq %r11, %r10
	jle label154
	jmp label153
label154:
	movq -8(%r11), %r10
	movq -208(%rbp), %rdi
	cmpl %edi, %r10d
	jle label153
	jmp label152
label153:
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label152:
	cmpl $0, %edi
	jl label153
	imulq $4, %rdi
	addl %edi,%r11d
	movq %r11, -208(%rbp)
	movq $0, -216(%rbp)
	movq -208(%rbp), %r11
	movq -216(%rbp), %r10
	cmpl $0, %r11d
	jne label151
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label151:
	movl %r10d, (%r11)
	movq -168(%rbp), %r11
	mov %r11, -216(%rbp)
	movq $4, -208(%rbp)
	movq -216(%rbp), %r11
	movq $0, %r10
	cmpq %r11, %r10
	jle label150
	jmp label149
label150:
	movq -8(%r11), %r10
	movq -208(%rbp), %rdi
	cmpl %edi, %r10d
	jle label149
	jmp label148
label149:
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label148:
	cmpl $0, %edi
	jl label149
	imulq $4, %rdi
	addl %edi,%r11d
	movq %r11, -208(%rbp)
	movq $0, -216(%rbp)
	movq -208(%rbp), %r11
	movq -216(%rbp), %r10
	cmpl $0, %r11d
	jne label147
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label147:
	movl %r10d, (%r11)
	movq -168(%rbp), %r11
	mov %r11, -216(%rbp)
	movq $5, -208(%rbp)
	movq -216(%rbp), %r11
	movq $0, %r10
	cmpq %r11, %r10
	jle label146
	jmp label145
label146:
	movq -8(%r11), %r10
	movq -208(%rbp), %rdi
	cmpl %edi, %r10d
	jle label145
	jmp label144
label145:
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label144:
	cmpl $0, %edi
	jl label145
	imulq $4, %rdi
	addl %edi,%r11d
	movq %r11, -208(%rbp)
	movq $0, -216(%rbp)
	movq -208(%rbp), %r11
	movq -216(%rbp), %r10
	cmpl $0, %r11d
	jne label143
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label143:
	movl %r10d, (%r11)
	movq $6, -208(%rbp)
	movq -208(%rbp), %r11
	cmpl $0, %r11d
	jge label142
	movq $12,%rdi
	subq $16, %rsp
	callq raise
label142:
	imulq $4, %r11
	addq $8, %r11
	pushq %rdi
	movq %r11, %rdi
	subq $16, %rsp
	callq malloc
	addq $16, %rsp
	popq %rdi
	movq -208(%rbp), %r11
	movq %r11,(%rax)
	addq $8, %rax
	movq %rax, -152(%rbp)
	movq -152(%rbp), %r11
	mov %r11, -216(%rbp)
	movq $0, -208(%rbp)
	movq -216(%rbp), %r11
	movq $0, %r10
	cmpq %r11, %r10
	jle label141
	jmp label140
label141:
	movq -8(%r11), %r10
	movq -208(%rbp), %rdi
	cmpl %edi, %r10d
	jle label140
	jmp label139
label140:
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label139:
	cmpl $0, %edi
	jl label140
	imulq $4, %rdi
	addl %edi,%r11d
	movq %r11, -208(%rbp)
	movq $1, -216(%rbp)
	movq -208(%rbp), %r11
	movq -216(%rbp), %r10
	cmpl $0, %r11d
	jne label138
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label138:
	movl %r10d, (%r11)
	movq -152(%rbp), %r11
	mov %r11, -216(%rbp)
	movq $1, -208(%rbp)
	movq -216(%rbp), %r11
	movq $0, %r10
	cmpq %r11, %r10
	jle label137
	jmp label136
label137:
	movq -8(%r11), %r10
	movq -208(%rbp), %rdi
	cmpl %edi, %r10d
	jle label136
	jmp label135
label136:
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label135:
	cmpl $0, %edi
	jl label136
	imulq $4, %rdi
	addl %edi,%r11d
	movq %r11, -208(%rbp)
	movq $0, -216(%rbp)
	movq -208(%rbp), %r11
	movq -216(%rbp), %r10
	cmpl $0, %r11d
	jne label134
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label134:
	movl %r10d, (%r11)
	movq -152(%rbp), %r11
	mov %r11, -216(%rbp)
	movq $2, -208(%rbp)
	movq -216(%rbp), %r11
	movq $0, %r10
	cmpq %r11, %r10
	jle label133
	jmp label132
label133:
	movq -8(%r11), %r10
	movq -208(%rbp), %rdi
	cmpl %edi, %r10d
	jle label132
	jmp label131
label132:
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label131:
	cmpl $0, %edi
	jl label132
	imulq $4, %rdi
	addl %edi,%r11d
	movq %r11, -208(%rbp)
	movq $0, -216(%rbp)
	movq -208(%rbp), %r11
	movq -216(%rbp), %r10
	cmpl $0, %r11d
	jne label130
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label130:
	movl %r10d, (%r11)
	movq -152(%rbp), %r11
	mov %r11, -216(%rbp)
	movq $3, -208(%rbp)
	movq -216(%rbp), %r11
	movq $0, %r10
	cmpq %r11, %r10
	jle label129
	jmp label128
label129:
	movq -8(%r11), %r10
	movq -208(%rbp), %rdi
	cmpl %edi, %r10d
	jle label128
	jmp label127
label128:
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label127:
	cmpl $0, %edi
	jl label128
	imulq $4, %rdi
	addl %edi,%r11d
	movq %r11, -208(%rbp)
	movq $0, -216(%rbp)
	movq -208(%rbp), %r11
	movq -216(%rbp), %r10
	cmpl $0, %r11d
	jne label126
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label126:
	movl %r10d, (%r11)
	movq -152(%rbp), %r11
	mov %r11, -216(%rbp)
	movq $4, -208(%rbp)
	movq -216(%rbp), %r11
	movq $0, %r10
	cmpq %r11, %r10
	jle label125
	jmp label124
label125:
	movq -8(%r11), %r10
	movq -208(%rbp), %rdi
	cmpl %edi, %r10d
	jle label124
	jmp label123
label124:
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label123:
	cmpl $0, %edi
	jl label124
	imulq $4, %rdi
	addl %edi,%r11d
	movq %r11, -208(%rbp)
	movq $0, -216(%rbp)
	movq -208(%rbp), %r11
	movq -216(%rbp), %r10
	cmpl $0, %r11d
	jne label122
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label122:
	movl %r10d, (%r11)
	movq -152(%rbp), %r11
	mov %r11, -216(%rbp)
	movq $5, -208(%rbp)
	movq -216(%rbp), %r11
	movq $0, %r10
	cmpq %r11, %r10
	jle label121
	jmp label120
label121:
	movq -8(%r11), %r10
	movq -208(%rbp), %rdi
	cmpl %edi, %r10d
	jle label120
	jmp label119
label120:
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label119:
	cmpl $0, %edi
	jl label120
	imulq $4, %rdi
	addl %edi,%r11d
	movq %r11, -208(%rbp)
	movq $0, -216(%rbp)
	movq -208(%rbp), %r11
	movq -216(%rbp), %r10
	cmpl $0, %r11d
	jne label118
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label118:
	movl %r10d, (%r11)
	movq $6, -208(%rbp)
	movq -208(%rbp), %r11
	cmpl $0, %r11d
	jge label117
	movq $12,%rdi
	subq $16, %rsp
	callq raise
label117:
	imulq $4, %r11
	addq $8, %r11
	pushq %rdi
	movq %r11, %rdi
	subq $16, %rsp
	callq malloc
	addq $16, %rsp
	popq %rdi
	movq -208(%rbp), %r11
	movq %r11,(%rax)
	addq $8, %rax
	movq %rax, -136(%rbp)
	movq -136(%rbp), %r11
	mov %r11, -216(%rbp)
	movq $0, -208(%rbp)
	movq -216(%rbp), %r11
	movq $0, %r10
	cmpq %r11, %r10
	jle label116
	jmp label115
label116:
	movq -8(%r11), %r10
	movq -208(%rbp), %rdi
	cmpl %edi, %r10d
	jle label115
	jmp label114
label115:
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label114:
	cmpl $0, %edi
	jl label115
	imulq $4, %rdi
	addl %edi,%r11d
	movq %r11, -208(%rbp)
	movq $0, -216(%rbp)
	movq -208(%rbp), %r11
	movq -216(%rbp), %r10
	cmpl $0, %r11d
	jne label113
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label113:
	movl %r10d, (%r11)
	movq -136(%rbp), %r11
	mov %r11, -216(%rbp)
	movq $1, -208(%rbp)
	movq -216(%rbp), %r11
	movq $0, %r10
	cmpq %r11, %r10
	jle label112
	jmp label111
label112:
	movq -8(%r11), %r10
	movq -208(%rbp), %rdi
	cmpl %edi, %r10d
	jle label111
	jmp label110
label111:
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label110:
	cmpl $0, %edi
	jl label111
	imulq $4, %rdi
	addl %edi,%r11d
	movq %r11, -208(%rbp)
	movq $0, -216(%rbp)
	movq -208(%rbp), %r11
	movq -216(%rbp), %r10
	cmpl $0, %r11d
	jne label109
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label109:
	movl %r10d, (%r11)
	movq -136(%rbp), %r11
	mov %r11, -216(%rbp)
	movq $2, -208(%rbp)
	movq -216(%rbp), %r11
	movq $0, %r10
	cmpq %r11, %r10
	jle label108
	jmp label107
label108:
	movq -8(%r11), %r10
	movq -208(%rbp), %rdi
	cmpl %edi, %r10d
	jle label107
	jmp label106
label107:
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label106:
	cmpl $0, %edi
	jl label107
	imulq $4, %rdi
	addl %edi,%r11d
	movq %r11, -208(%rbp)
	movq $0, -216(%rbp)
	movq -208(%rbp), %r11
	movq -216(%rbp), %r10
	cmpl $0, %r11d
	jne label105
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label105:
	movl %r10d, (%r11)
	movq -136(%rbp), %r11
	mov %r11, -216(%rbp)
	movq $3, -208(%rbp)
	movq -216(%rbp), %r11
	movq $0, %r10
	cmpq %r11, %r10
	jle label104
	jmp label103
label104:
	movq -8(%r11), %r10
	movq -208(%rbp), %rdi
	cmpl %edi, %r10d
	jle label103
	jmp label102
label103:
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label102:
	cmpl $0, %edi
	jl label103
	imulq $4, %rdi
	addl %edi,%r11d
	movq %r11, -208(%rbp)
	movq $0, -216(%rbp)
	movq -208(%rbp), %r11
	movq -216(%rbp), %r10
	cmpl $0, %r11d
	jne label101
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label101:
	movl %r10d, (%r11)
	movq -136(%rbp), %r11
	mov %r11, -216(%rbp)
	movq $4, -208(%rbp)
	movq -216(%rbp), %r11
	movq $0, %r10
	cmpq %r11, %r10
	jle label100
	jmp label99
label100:
	movq -8(%r11), %r10
	movq -208(%rbp), %rdi
	cmpl %edi, %r10d
	jle label99
	jmp label98
label99:
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label98:
	cmpl $0, %edi
	jl label99
	imulq $4, %rdi
	addl %edi,%r11d
	movq %r11, -208(%rbp)
	movq $0, -216(%rbp)
	movq -208(%rbp), %r11
	movq -216(%rbp), %r10
	cmpl $0, %r11d
	jne label97
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label97:
	movl %r10d, (%r11)
	movq -136(%rbp), %r11
	mov %r11, -216(%rbp)
	movq $1, -208(%rbp)
	movq -216(%rbp), %r11
	movq $0, %r10
	cmpq %r11, %r10
	jle label96
	jmp label95
label96:
	movq -8(%r11), %r10
	movq -208(%rbp), %rdi
	cmpl %edi, %r10d
	jle label95
	jmp label94
label95:
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label94:
	cmpl $0, %edi
	jl label95
	imulq $4, %rdi
	addl %edi,%r11d
	movq %r11, -208(%rbp)
	movq $1, -216(%rbp)
	movq -208(%rbp), %r11
	movq -216(%rbp), %r10
	cmpl $0, %r11d
	jne label93
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label93:
	movl %r10d, (%r11)
	movq $6, -208(%rbp)
	movq -208(%rbp), %r11
	cmpl $0, %r11d
	jge label92
	movq $12,%rdi
	subq $16, %rsp
	callq raise
label92:
	imulq $4, %r11
	addq $8, %r11
	pushq %rdi
	movq %r11, %rdi
	subq $16, %rsp
	callq malloc
	addq $16, %rsp
	popq %rdi
	movq -208(%rbp), %r11
	movq %r11,(%rax)
	addq $8, %rax
	movq %rax, -120(%rbp)
	movq -120(%rbp), %r11
	mov %r11, -216(%rbp)
	movq $0, -208(%rbp)
	movq -216(%rbp), %r11
	movq $0, %r10
	cmpq %r11, %r10
	jle label91
	jmp label90
label91:
	movq -8(%r11), %r10
	movq -208(%rbp), %rdi
	cmpl %edi, %r10d
	jle label90
	jmp label89
label90:
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label89:
	cmpl $0, %edi
	jl label90
	imulq $4, %rdi
	addl %edi,%r11d
	movq %r11, -208(%rbp)
	movq $0, -216(%rbp)
	movq -208(%rbp), %r11
	movq -216(%rbp), %r10
	cmpl $0, %r11d
	jne label88
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label88:
	movl %r10d, (%r11)
	movq -120(%rbp), %r11
	mov %r11, -216(%rbp)
	movq $1, -208(%rbp)
	movq -216(%rbp), %r11
	movq $0, %r10
	cmpq %r11, %r10
	jle label87
	jmp label86
label87:
	movq -8(%r11), %r10
	movq -208(%rbp), %rdi
	cmpl %edi, %r10d
	jle label86
	jmp label85
label86:
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label85:
	cmpl $0, %edi
	jl label86
	imulq $4, %rdi
	addl %edi,%r11d
	movq %r11, -208(%rbp)
	movq $0, -216(%rbp)
	movq -208(%rbp), %r11
	movq -216(%rbp), %r10
	cmpl $0, %r11d
	jne label84
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label84:
	movl %r10d, (%r11)
	movq -120(%rbp), %r11
	mov %r11, -216(%rbp)
	movq $2, -208(%rbp)
	movq -216(%rbp), %r11
	movq $0, %r10
	cmpq %r11, %r10
	jle label83
	jmp label82
label83:
	movq -8(%r11), %r10
	movq -208(%rbp), %rdi
	cmpl %edi, %r10d
	jle label82
	jmp label81
label82:
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label81:
	cmpl $0, %edi
	jl label82
	imulq $4, %rdi
	addl %edi,%r11d
	movq %r11, -208(%rbp)
	movq $0, -216(%rbp)
	movq -208(%rbp), %r11
	movq -216(%rbp), %r10
	cmpl $0, %r11d
	jne label80
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label80:
	movl %r10d, (%r11)
	movq -120(%rbp), %r11
	mov %r11, -216(%rbp)
	movq $3, -208(%rbp)
	movq -216(%rbp), %r11
	movq $0, %r10
	cmpq %r11, %r10
	jle label79
	jmp label78
label79:
	movq -8(%r11), %r10
	movq -208(%rbp), %rdi
	cmpl %edi, %r10d
	jle label78
	jmp label77
label78:
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label77:
	cmpl $0, %edi
	jl label78
	imulq $4, %rdi
	addl %edi,%r11d
	movq %r11, -208(%rbp)
	movq $0, -216(%rbp)
	movq -208(%rbp), %r11
	movq -216(%rbp), %r10
	cmpl $0, %r11d
	jne label76
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label76:
	movl %r10d, (%r11)
	movq -120(%rbp), %r11
	mov %r11, -216(%rbp)
	movq $4, -208(%rbp)
	movq -216(%rbp), %r11
	movq $0, %r10
	cmpq %r11, %r10
	jle label75
	jmp label74
label75:
	movq -8(%r11), %r10
	movq -208(%rbp), %rdi
	cmpl %edi, %r10d
	jle label74
	jmp label73
label74:
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label73:
	cmpl $0, %edi
	jl label74
	imulq $4, %rdi
	addl %edi,%r11d
	movq %r11, -208(%rbp)
	movq $0, -216(%rbp)
	movq -208(%rbp), %r11
	movq -216(%rbp), %r10
	cmpl $0, %r11d
	jne label72
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label72:
	movl %r10d, (%r11)
	movq -120(%rbp), %r11
	mov %r11, -216(%rbp)
	movq $1, -208(%rbp)
	movq -216(%rbp), %r11
	movq $0, %r10
	cmpq %r11, %r10
	jle label71
	jmp label70
label71:
	movq -8(%r11), %r10
	movq -208(%rbp), %rdi
	cmpl %edi, %r10d
	jle label70
	jmp label69
label70:
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label69:
	cmpl $0, %edi
	jl label70
	imulq $4, %rdi
	addl %edi,%r11d
	movq %r11, -208(%rbp)
	movq $0, -216(%rbp)
	movq -208(%rbp), %r11
	movq -216(%rbp), %r10
	cmpl $0, %r11d
	jne label68
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label68:
	movl %r10d, (%r11)
	movq $6, -208(%rbp)
	movq -208(%rbp), %r11
	cmpl $0, %r11d
	jge label67
	movq $12,%rdi
	subq $16, %rsp
	callq raise
label67:
	imulq $8, %r11
	addq $8, %r11
	pushq %rdi
	movq %r11, %rdi
	subq $16, %rsp
	callq malloc
	addq $16, %rsp
	popq %rdi
	movq -208(%rbp), %r11
	movq %r11,(%rax)
	addq $8, %rax
	movq %rax, -104(%rbp)
	movq -104(%rbp), %r11
	mov %r11, -216(%rbp)
	movq $0, -208(%rbp)
	movq -216(%rbp), %r11
	movq $0, %r10
	cmpq %r11, %r10
	jle label66
	jmp label65
label66:
	movq -8(%r11), %r10
	movq -208(%rbp), %rdi
	cmpl %edi, %r10d
	jle label65
	jmp label64
label65:
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label64:
	cmpl $0, %edi
	jl label65
	imulq $8, %rdi
	addl %edi,%r11d
	movq %r11, -192(%rbp)
	movq -200(%rbp), %r11
	mov %r11, -200(%rbp)
	movq -192(%rbp), %r11
	movq -200(%rbp), %r10
	cmpl $0, %r11d
	jne label63
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label63:
	movq %r10, (%r11)
	movq -104(%rbp), %r11
	mov %r11, -200(%rbp)
	movq $1, -192(%rbp)
	movq -200(%rbp), %r11
	movq $0, %r10
	cmpq %r11, %r10
	jle label62
	jmp label61
label62:
	movq -8(%r11), %r10
	movq -192(%rbp), %rdi
	cmpl %edi, %r10d
	jle label61
	jmp label60
label61:
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label60:
	cmpl $0, %edi
	jl label61
	imulq $8, %rdi
	addl %edi,%r11d
	movq %r11, -176(%rbp)
	movq -184(%rbp), %r11
	mov %r11, -184(%rbp)
	movq -176(%rbp), %r11
	movq -184(%rbp), %r10
	cmpl $0, %r11d
	jne label59
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label59:
	movq %r10, (%r11)
	movq -104(%rbp), %r11
	mov %r11, -184(%rbp)
	movq $2, -176(%rbp)
	movq -184(%rbp), %r11
	movq $0, %r10
	cmpq %r11, %r10
	jle label58
	jmp label57
label58:
	movq -8(%r11), %r10
	movq -176(%rbp), %rdi
	cmpl %edi, %r10d
	jle label57
	jmp label56
label57:
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label56:
	cmpl $0, %edi
	jl label57
	imulq $8, %rdi
	addl %edi,%r11d
	movq %r11, -160(%rbp)
	movq -168(%rbp), %r11
	mov %r11, -168(%rbp)
	movq -160(%rbp), %r11
	movq -168(%rbp), %r10
	cmpl $0, %r11d
	jne label55
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label55:
	movq %r10, (%r11)
	movq -104(%rbp), %r11
	mov %r11, -168(%rbp)
	movq $3, -160(%rbp)
	movq -168(%rbp), %r11
	movq $0, %r10
	cmpq %r11, %r10
	jle label54
	jmp label53
label54:
	movq -8(%r11), %r10
	movq -160(%rbp), %rdi
	cmpl %edi, %r10d
	jle label53
	jmp label52
label53:
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label52:
	cmpl $0, %edi
	jl label53
	imulq $8, %rdi
	addl %edi,%r11d
	movq %r11, -144(%rbp)
	movq -152(%rbp), %r11
	mov %r11, -152(%rbp)
	movq -144(%rbp), %r11
	movq -152(%rbp), %r10
	cmpl $0, %r11d
	jne label51
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label51:
	movq %r10, (%r11)
	movq -104(%rbp), %r11
	mov %r11, -152(%rbp)
	movq $4, -144(%rbp)
	movq -152(%rbp), %r11
	movq $0, %r10
	cmpq %r11, %r10
	jle label50
	jmp label49
label50:
	movq -8(%r11), %r10
	movq -144(%rbp), %rdi
	cmpl %edi, %r10d
	jle label49
	jmp label48
label49:
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label48:
	cmpl $0, %edi
	jl label49
	imulq $8, %rdi
	addl %edi,%r11d
	movq %r11, -128(%rbp)
	movq -136(%rbp), %r11
	mov %r11, -136(%rbp)
	movq -128(%rbp), %r11
	movq -136(%rbp), %r10
	cmpl $0, %r11d
	jne label47
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label47:
	movq %r10, (%r11)
	movq -104(%rbp), %r11
	mov %r11, -136(%rbp)
	movq $5, -128(%rbp)
	movq -136(%rbp), %r11
	movq $0, %r10
	cmpq %r11, %r10
	jle label46
	jmp label45
label46:
	movq -8(%r11), %r10
	movq -128(%rbp), %rdi
	cmpl %edi, %r10d
	jle label45
	jmp label44
label45:
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label44:
	cmpl $0, %edi
	jl label45
	imulq $8, %rdi
	addl %edi,%r11d
	movq %r11, -112(%rbp)
	movq -120(%rbp), %r11
	mov %r11, -120(%rbp)
	movq -112(%rbp), %r11
	movq -120(%rbp), %r10
	cmpl $0, %r11d
	jne label43
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label43:
	movq %r10, (%r11)
	pushq %rdi
	movq $8, %rdi
	subq $16, %rsp
	callq malloc
	addq $16, %rsp
	popq %rdi
	movq %rax, %r15
	movq $6, -112(%rbp)
	movq -112(%rbp), %r11
	cmpl $0, %r11d
	jge label42
	movq $12,%rdi
	subq $16, %rsp
	callq raise
label42:
	imulq $4, %r11
	addq $8, %r11
	pushq %rdi
	movq %r11, %rdi
	subq $16, %rsp
	callq malloc
	addq $16, %rsp
	popq %rdi
	movq -112(%rbp), %r11
	movq %r11,(%rax)
	addq $8, %rax
	movq %rax, -112(%rbp)
	movq -112(%rbp), %r11
	mov %r11, -128(%rbp)
	movq $0, -120(%rbp)
	movq -128(%rbp), %r11
	movq $0, %r10
	cmpq %r11, %r10
	jle label41
	jmp label40
label41:
	movq -8(%r11), %r10
	movq -120(%rbp), %rdi
	cmpl %edi, %r10d
	jle label40
	jmp label39
label40:
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label39:
	cmpl $0, %edi
	jl label40
	imulq $4, %rdi
	addl %edi,%r11d
	movq %r11, -120(%rbp)
	movq $0, -128(%rbp)
	movq -120(%rbp), %r11
	movq -128(%rbp), %r10
	cmpl $0, %r11d
	jne label38
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label38:
	movl %r10d, (%r11)
	movq -112(%rbp), %r11
	mov %r11, -128(%rbp)
	movq $1, -120(%rbp)
	movq -128(%rbp), %r11
	movq $0, %r10
	cmpq %r11, %r10
	jle label37
	jmp label36
label37:
	movq -8(%r11), %r10
	movq -120(%rbp), %rdi
	cmpl %edi, %r10d
	jle label36
	jmp label35
label36:
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label35:
	cmpl $0, %edi
	jl label36
	imulq $4, %rdi
	addl %edi,%r11d
	movq %r11, -120(%rbp)
	movq $0, -128(%rbp)
	movq -120(%rbp), %r11
	movq -128(%rbp), %r10
	cmpl $0, %r11d
	jne label34
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label34:
	movl %r10d, (%r11)
	movq -112(%rbp), %r11
	mov %r11, -128(%rbp)
	movq $2, -120(%rbp)
	movq -128(%rbp), %r11
	movq $0, %r10
	cmpq %r11, %r10
	jle label33
	jmp label32
label33:
	movq -8(%r11), %r10
	movq -120(%rbp), %rdi
	cmpl %edi, %r10d
	jle label32
	jmp label31
label32:
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label31:
	cmpl $0, %edi
	jl label32
	imulq $4, %rdi
	addl %edi,%r11d
	movq %r11, -120(%rbp)
	movq $0, -128(%rbp)
	movq -120(%rbp), %r11
	movq -128(%rbp), %r10
	cmpl $0, %r11d
	jne label30
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label30:
	movl %r10d, (%r11)
	movq -112(%rbp), %r11
	mov %r11, -128(%rbp)
	movq $3, -120(%rbp)
	movq -128(%rbp), %r11
	movq $0, %r10
	cmpq %r11, %r10
	jle label29
	jmp label28
label29:
	movq -8(%r11), %r10
	movq -120(%rbp), %rdi
	cmpl %edi, %r10d
	jle label28
	jmp label27
label28:
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label27:
	cmpl $0, %edi
	jl label28
	imulq $4, %rdi
	addl %edi,%r11d
	movq %r11, -120(%rbp)
	movq $0, -128(%rbp)
	movq -120(%rbp), %r11
	movq -128(%rbp), %r10
	cmpl $0, %r11d
	jne label26
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label26:
	movl %r10d, (%r11)
	movq -112(%rbp), %r11
	mov %r11, -128(%rbp)
	movq $4, -120(%rbp)
	movq -128(%rbp), %r11
	movq $0, %r10
	cmpq %r11, %r10
	jle label25
	jmp label24
label25:
	movq -8(%r11), %r10
	movq -120(%rbp), %rdi
	cmpl %edi, %r10d
	jle label24
	jmp label23
label24:
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label23:
	cmpl $0, %edi
	jl label24
	imulq $4, %rdi
	addl %edi,%r11d
	movq %r11, -120(%rbp)
	movq $0, -128(%rbp)
	movq -120(%rbp), %r11
	movq -128(%rbp), %r10
	cmpl $0, %r11d
	jne label22
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label22:
	movl %r10d, (%r11)
	movq -112(%rbp), %r11
	mov %r11, -128(%rbp)
	movq $5, -120(%rbp)
	movq -128(%rbp), %r11
	movq $0, %r10
	cmpq %r11, %r10
	jle label21
	jmp label20
label21:
	movq -8(%r11), %r10
	movq -120(%rbp), %rdi
	cmpl %edi, %r10d
	jle label20
	jmp label19
label20:
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label19:
	cmpl $0, %edi
	jl label20
	imulq $4, %rdi
	addl %edi,%r11d
	movq %r11, -120(%rbp)
	movq $0, -128(%rbp)
	movq -120(%rbp), %r11
	movq -128(%rbp), %r10
	cmpl $0, %r11d
	jne label18
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label18:
	movl %r10d, (%r11)
	movq %r15, -96(%rbp)
	movq -112(%rbp), %r11
	mov %r11, -88(%rbp)
	movq -96(%rbp), %r11
	movq -88(%rbp), %r10
	cmpl $0, %r11d
	jne label17
	movq $12,%rdi
	subq $16, %rsp
	callq raise
	addq $16, %rsp
label17:
	movq %r10, (%r11)
	movq -104(%rbp), %r14
	movq $0, %r13
	movq $5, %r12
	movq %r15, %rbx
	pushq %rdi
	movq %r14, %rdi
	pushq %rsi
	movq %r13, %rsi
	pushq %rdx
	movq %r12, %rdx
	pushq %rcx
	movq %rbx, %rcx
	subq $16, %rsp
	callq _c0_DFS
	addq $16, %rsp
	popq %rcx
	popq %rdx
	popq %rsi
	popq %rdi
	movq %rax, %rbx
	cmpl $0, %ebx
	jne label15
jmp label16
label16:
	subq $16,%rsp
	callq abort
	addq $16,%rsp
	popq %rbp
	ret
label15:
	movq $0, %rax
	addq $1696, %rsp
	popq %r15
	popq %r14
	popq %r13
	popq %r12
	popq %rbx
	popq %rbp
	ret
	.ident	"15-411 L1 reference compiler"
