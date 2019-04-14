; .COM
TesTPC 	SEGMENT
		ASSUME CS:TesTPC, DS:TesTPC, es:NOTHING, SS:NOTHING
		ORG 100H
START: JMP BEGIN
; ДАННЫЕ

AVL_MEM 		db "Available memory:        bytes", 0dh, 0ah, '$'
FREE_MEM 		db "Free memory:        bytes", 0dh, 0ah, '$'
ENDLINE 		db 0dh, 0ah, '$'

EXT_MEM			db "Extended memory:         kb", 0dh, 0ah, '$'


MCB_CHAIN 		db 'MCB chain: ', 0dh, 0ah, '$'
MCB_HEADER 		db 'Adress   Owner PSP     Size     Name      Type', 0dh, 0ah, '$'
MCB 			db '                                              ', 0dh, 0ah, '$'



; ПРОЦЕДУРЫ
;-----------------------------------------------------
TETR_TO_HEX PROC near 
	and AL,0Fh
	cmp AL,09
	jbe NEXT
	add AL,07
NEXT:	add AL,30h
	ret
TETR_TO_HEX ENDP

BYTE_TO_HEX PROC near
	push cx
	mov AH,AL
	call TETR_TO_HEX
	xchg AL,AH
	mov CL,4
	shr AL,CL
	call TETR_TO_HEX 
	pop cx
	ret
BYTE_TO_HEX ENDP
;-------------------------------
WRD_TO_HEX PROC near
	push bx
	mov BH,AH
	call BYTE_TO_HEX
	mov [di],AH
	dec di
	mov [di],AL
	dec di
	mov AL,BH
	call BYTE_TO_HEX
	mov [di],AH
	dec di
	mov [di],AL
	pop bx
	ret
WRD_TO_HEX ENDP

WRD_TO_DEC PROC near
		push cx
		push dx
		
		mov cx,10
	_b: div cx
		or DL, 30h		; 30h = '0'
		mov [SI],DL
		dec SI
		xor dx,dx
		cmp ax,10
		jae _b
		cmp AL,00h
		je endl
		or AL,30h
		mov [SI],AL
	endl:
		pop dx
		pop cx
		ret
WRD_TO_DEC ENDP
;--------------------------------------------------

BYTE_TO_DEC PROC near
		push cx
		push dx
		xor AH,AH
		xor dx,dx
		mov cx,10
loop_bd: div cx
		or DL,30h
		mov [SI],DL
		dec SI
		xor dx,dx
		cmp ax,10
		jae loop_bd
		cmp AL,00h
		je end_l
		or AL,30h
		mov [SI],AL
end_l: 	pop dx
		pop cx
		ret
BYTE_TO_DEC ENDP 
;--------------------------------------------------
OUTPUT_PROC PROC NEAR ;Вывод на экран сообщения
		push ax
		mov ah, 09h
	    int 21h
	    pop ax
	    ret
OUTPUT_PROC ENDP

AVL_MEM_PROC PROC NEAR 
		push AX
		push BX
		push DX
		push si
	
		sub ax, ax
		mov ah, 04Ah
		mov bx, 0FFFFh
		int 21h
		mov ax, 10h
		mul bx
		mov si, offset AVL_MEM
		add si, 017h
		call WRD_TO_DEC
		mov dx, offset AVL_MEM

		call OUTPUT_PROC

		pop si
		pop DX
		pop BX
		pop AX
		ret

AVL_MEM_PROC ENDP

EXTENDED_MEM_PROC PROC NEAR
		push ax
		push si
		push dx
		
		mov al, 31h
		out 70h, al
		in al, 71h
		mov ah, al			; get upper byte to ah
		
		mov al, 30h
		out 70h, al
		in al, 71h
		mov bl, al			; get lower byte to al
		
		lea si, EXT_MEM
		add si, 23
		xor dx, dx
		call WRD_TO_DEC		; write extended memory quantity to EXT_MEM string
		
		LEA dx, EXT_MEM
		call OUTPUT_PROC
		
		
		pop dx
		pop si
		pop ax
		ret
EXTENDED_MEM_PROC ENDP

PRINT_MCB_TABLE PROC near
		push ax
		push bx
		push cx
		push dx
		push es
		push si
		push di
		
		lea dx, MCB_CHAIN
		call OUTPUT_PROC
		lea dx,  MCB_HEADER
		call OUTPUT_PROC

		mov AH, 52h
		int 21h
		mov es, es:[bx-2]		; get first MCB seg. adress

	
	MCB_loop:
		mov ax, es				; get current MCB adress
		lea di, MCB
		add di, 4
		call WRD_TO_HEX			; write current MCB adress 

		mov ax, es:[01h]
		lea di, MCB
		add di, 14
		call WRD_TO_HEX			; write current MCB owner PSP adress

		mov ax, es:[03h]		; get current MCB size, paragraphs
		mov bx, 10h
		mul bx					; get current MCB size, bytes
		lea si, MCB
		add si, 25
		call WRD_TO_DEC			; write current MCB size
		
		lea di, MCB
		add di, 32
		mov cx, 8
		mov bx, 0
		name_loop:
			mov al, es:[08h + bx]
			mov [di + bx], al
			inc bx
		loop name_loop			; write current MCB name
		
		
		mov al, es:[00h]
		lea di, MCB
		add di, 43
		call BYTE_TO_HEX		; write current MCB type
		mov [di], al
		inc di
		mov [di], ah			; write current MCB type
		
		lea dx, MCB
		call OUTPUT_PROC		; output MCB info
		
		mov ax, es
		add ax, es:[03h]
		inc ax
		mov BL, es:[00h]	
		mov es, ax				; get next MCB seg. adress
		
		cmp BL, 4Dh
		je MCB_loop				; if current MCB is not the last

		pop di
		pop si
		pop es
		pop dx
		pop cx
		pop bx
		pop ax
		ret
PRINT_MCB_TABLE ENDP

;--------------------------------------------------
BEGIN:
		
		call AVL_MEM_PROC
		call EXTENDED_MEM_PROC
		call PRINT_MCB_TABLE

		xor AL,AL
		mov AH,4Ch
		int 21H
		END_OF_PROGRAMM db 0
TesTPC 	ENDS
		END START ;

;------------------------------- 
