ASSUME CS:OVL1, DS:OVL1, SS:NOTHING, ES:NOTHING
OVL1 SEGMENT
;---------------------------------------------------------------
MAIN1 PROC FAR 
	push ds
	push dx
	push di
	push ax
	mov ax,cs
	mov ds,ax
	mov bx, offset ForPrint
	add bx, 24h			
	mov di, bx		
	mov ax, cs			
	call WRD_TO_HEX
	mov dx, offset ForPrint	
	call OUTPT_PROC
	pop ax
	pop di
	pop dx	
	pop ds
	retf
MAIN1 ENDP
;---------------------------------------------------------------
OUTPT_PROC PROC NEAR 
	push ax
	mov ah, 09h
	int 21h
	pop ax
	ret
OUTPT_PROC ENDP
;--------------------------------------------------------------------------------
TETR_TO_HEX		PROC near 
		and		al, 0Fh 
		cmp		al, 09 
		jbe		NEXT 
		add		al, 07 
	NEXT:	add		al, 30h 
		ret
TETR_TO_HEX		ENDP
;--------------------------------------------------------------------------------
BYTE_TO_HEX		PROC near 
		push	cx
		mov		ah, al 
		call	TETR_TO_HEX 
		xchg	al, ah 
		mov		cl, 4 
		shr		al, cl 
		call	TETR_TO_HEX 
		pop		cx 			
		ret
BYTE_TO_HEX		ENDP
;--------------------------------------------------------------------------------
WRD_TO_HEX		PROC	near 
		push	bx
		mov		bh, ah
		call	BYTE_TO_HEX 
		mov		[di], ah 
		dec		di 
		mov		[di], al 
		dec		di
		mov		al, bh 
		xor		ah, ah 
		call	BYTE_TO_HEX
		mov		[di], ah
		dec		di
		mov		[di], al
		pop		bx
		ret
WRD_TO_HEX		ENDP
;--------------------------------------------------------------------------------
ForPrint  DB 0DH,0AH, 'First overlay segment address:       ',0DH,0AH,'$'
;--------------------------------------------------------------------------------
OVL1 ENDS
END