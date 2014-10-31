.data
	
L1:
	.asciz "NULL return"
	
L2:
	.asciz "Not NULL return"
	
L3:
	.asciz "\nSum of 5 numbers:"
	
L4:
	.asciz "%i"
	
L5:
	.asciz "\nSum of other 5 numbers:"
	
L6:
	.asciz "%i"
	.text
	.global main
	
Compute_1:
	stmfd sp!,{fp,lr,v1,v2,v3,v4,v5}
	add fp,sp,#24
	sub sp,fp,#28
	mov v5,#0
	mov v4,v5
	mov a1,v4
	b .L3exit
	
.L3exit:
	sub sp,fp,#24
	ldmfd sp!,{fp,pc,v1,v2,v3,v4,v5}
	
Compute_0:
	stmfd sp!,{fp,lr,v1,v2,v3,v4,v5}
	add fp,sp,#24
	sub sp,fp,#28
	add v5,a2,a3
	add v3,v5,a4
	ldr v1,[fp,#4]
	add v1,v3,v1
	ldr v4,[fp,#8]
	rsb v2,v4,#0
	sub v4,v1,v2
	mov a1,v4
	b .L2exit
	
.L2exit:
	sub sp,fp,#24
	ldmfd sp!,{fp,pc,v1,v2,v3,v4,v5}
	
main:
	stmfd sp!,{fp,lr,v1,v2,v3,v4,v5}
	add fp,sp,#24
	sub sp,fp,#40
	mov v5,#0
	mov a4,v5
	cmp a4,#0
	moveq a3,#1
	movne a3,#0
	orr a2,a3,#0
	cmp a2,#0
	moveq a2,#0
	movne a2,#1
	and a4,a2,a4
	cmp a4,#0
	moveq a4,#0
	movne a4,#1
	ldr a1,[fp,#-32]
	bl Compute_1(PLT)
	mov v1,a1
	cmp v1,#0
	moveq v5,#1
	movne v5,#0
	cmp v5,#0
	beq .1
	ldr a1,=L1
	bl printf(PLT)
	b .2
	
.1:
	ldr a1,=L2
	bl printf(PLT)
	
.2:
	mov a1,#12
	bl _Znwj(PLT)
	mov v2,a1
	sub sp,sp,#8
	mov a1,v2
	mov a2,#1
	mov a3,#2
	mov a4,#3
	mov v5,#4
	str v5,[sp,#0]
	mov v5,#5
	str v5,[sp,#4]
	bl Compute_0(PLT)
	add sp,sp,#8
	mov v3,a1
	ldr a1,=L3
	bl printf(PLT)
	ldr a1,=L4
	mov a2,v3
	bl printf(PLT)
	mov v5,#5
	rsb v4,v5,#0
	sub sp,sp,#8
	mov a1,v2
	mov a2,v4
	mov a3,#6
	mov a4,#7
	mov v5,#8
	str v5,[sp,#0]
	mov v5,#9
	str v5,[sp,#4]
	bl Compute_0(PLT)
	add sp,sp,#8
	mov v3,a1
	ldr a1,=L5
	bl printf(PLT)
	ldr a1,=L6
	mov a2,v3
	bl printf(PLT)
	
.L1exit:
	mov a4,#0
	mov a1,a4
	sub sp,fp,#24
	ldmfd sp!,{fp,pc,v1,v2,v3,v4,v5}
