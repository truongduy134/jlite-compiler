.data
	
L1:
	.asciz "%i"
	.text
	.global main
	
FieldAccess_0:
	stmfd sp!,{fp,lr,v1,v2,v3,v4,v5}
	add fp,sp,#24
	sub sp,fp,#28
	mov a3,#6
	add v1,a3,a2
	mov a1,v1
	b .L2exit
	
.L2exit:
	sub sp,fp,#24
	ldmfd sp!,{fp,pc,v1,v2,v3,v4,v5}
	
main:
	stmfd sp!,{fp,lr,v1,v2,v3,v4,v5}
	add fp,sp,#24
	sub sp,fp,#44
	mov a1,#4
	bl _Znwj(PLT)
	mov v5,a1
	mov a1,v5
	mov a2,#5
	bl FieldAccess_0(PLT)
	mov v2,a1
	mov a3,#7
	mov a3,a3
	str a3,[v5,#0]
	ldr a2,[v5,#0]
	add a3,a2,v2
	mov a3,#2
	mul v4,a3,a3
	ldr a1,=L1
	mov a2,v4
	bl printf(PLT)
	
.L1exit:
	mov a4,#0
	mov a1,a4
	sub sp,fp,#24
	ldmfd sp!,{fp,pc,v1,v2,v3,v4,v5}
