AStack SEGMENT STACK
	dw 16 dup (?)
AStack ENDS

CODE SEGMENT
	ASSUME CS:CODE, DS:DATA, SS:AStack
; start of resident program
		; resident program data:
		SIGNATURE		DB 'Losev$'
		COUNT 			DW 0
		COUNT_STRING	DB '000000$'
		KEEP_CS 		DW 0
		KEEP_IP 		DW 0
		KEEP_PSP		dw 0
		KEEP_SP			dw 0
		KEEP_SS			dw 0
		COUNTER			dw 0
		COUNT_MESSAGE	db 'ROUT CALLED:    $'
		ROUT_STACK 		dw 64 dup (?)			;Стек для резидента
		stack_ptr		=$

		
TETR_TO_HEX PROC near
		and AL,0Fh
		cmp AL,09
		jbe NEXT
		add AL,07
NEXT:	add AL,30h
		ret
TETR_TO_HEX ENDP

BYTE_TO_HEX PROC near
		push CX
		mov AH,AL
		call TETR_TO_HEX
		xchg AL,AH
		mov CL,4
		shr AL,CL
		call TETR_TO_HEX
		pop CX
		ret
BYTE_TO_HEX ENDP

WRD_TO_HEX PROC near
		push BX
		mov BH,AH
		call BYTE_TO_HEX
		mov [DI],AH
		dec DI
		mov [DI],AL
		dec DI
		mov AL,BH
		call BYTE_TO_HEX
		mov [DI],AH
		dec DI
		mov [DI],AL
		pop BX
		ret
WRD_TO_HEX ENDP

outputBP proc		;  es:bp
		push ax
		push bx
		push cx
		push dx

		mov ah, 13h	
		mov al, 0		;sub function code
		mov cx, 12h
		mov bx, 36
		int 10h

		pop dx
		pop cx
		pop bx
		pop ax
		ret
outputBP endp

getCurs proc
		push ax
		push bx
		
		mov AH,03h
		mov BH,00h
		int 10h
		
		pop bx
		pop ax
		
		ret
getCurs endp

setCurs proc
		push ax
		push bx
		push cx
		push dx
		
		mov ah, 02h
		mov bh, 0
		int 10h
		
		pop dx
		pop cx
		pop bx
		pop ax
		ret
setCurs endp

ROUT PROC FAR		; resident interuption handler
		mov KEEP_SP,SP
		mov KEEP_SS,SS
		mov AX, seg ROUT_STACK
		mov SS, AX
		mov SP, offset stack_ptr
	
		push AX
		push BX
		push CX
		push DX


		call getCurs
		push DX		
		
		mov DX,013Dh
		call setCurs
		
		push DS
		mov AX,seg COUNTER
		mov DS,AX
		mov AX,COUNTER
		inc AX
		mov COUNTER,AX
		
		push DI
		mov DI,offset COUNT_MESSAGE
		add DI,17
		call WRD_TO_HEX
		pop DI
		pop DS
		
		push ES
		push BP
		mov AX,seg COUNT_MESSAGE
		mov ES,AX
		mov BP,offset COUNT_MESSAGE
		call outputBP
		
		pop BP
		pop ES
		pop DX
		call setCurs

		;Восстановление всех регистров:
		pop DX
		pop CX
		pop BX
		pop AX
		mov SP, KEEP_SP
		mov SS, KEEP_SS
		
		
		mov AL,20h
		out 20h,AL
		
		iret
ROUT ENDP

END_OF_RESIDENT db 0
; end of resident program

OUTPUT_PROC PROC NEAR ;Вывод на экран сообщения
		push ax
		mov ah, 09h
	    int 21h
	    pop ax
	    ret
OUTPUT_PROC ENDP

KEEP_HANDLER PROC NEAR
		push ax
		push bx
		push es
		
		mov ah, 35h
		mov al, 1ch
		int 21h
		mov KEEP_CS, es
		mov KEEP_IP, bx

		pop es
		pop bx
		pop ax
		
		ret
KEEP_HANDLER ENDP

RECOVER_HANDLER PROC NEAR
		push ax
		push dx
		push ds
		
		mov DX,ES:KEEP_IP
		mov AX,ES:KEEP_CS
		mov ds, ax
		call SET_HANDLER

		pop ds
		pop dx
		pop ax
		
		ret
RECOVER_HANDLER ENDP

SET_HANDLER PROC NEAR		; (dx = offset, ds = seg)
		push ax
		mov AX, 251Ch
		int 21h
		pop ax
		ret
SET_HANDLER ENDP

KEEP PROC NEAR 
		mov dx, offset END_OF_RESIDENT
		mov cl, 4
		shr dx, cl
		add dx, 14h		; 10h paragraphs for PSP and 4 for stack
		inc dx			; round up
		mov ah, 31h
		mov al, 0h
		int 21h
		
	    ret
KEEP ENDP


check_handler proc near
		mov ah, 35h
		mov al, 1ch
		int 21h				; es:bx (load to es segment of interruption handler)
		
		cld                				; straight direction
		mov cx, 6     					; signature lenght
		lea si, SIGNATURE_CHECK			; ds:si = data seg : offset SIGNATURE_CHECK
		mov	di, 0h	 					; es:di = handler segment : 0h
		repe cmpsb              		; compare strings
		jne check_handler_exit			; if not ZF       		
	check_handler_exit:
		ret
check_handler endp

check_tail proc near 
		mov CL, ES:[80h]
		cmp CL, 4						; lenght of ' /un'
		jne check_tail_exit
		
		cld                				; straight direction
		mov cx, 2     					; command lenght
		lea si, UNLOAD_COMMAND			; ds : si = data seg : offset UNLOAD_COMMAND
		mov	di, 81h	 					; es : di = es : 81h, command line adress
		repe cmpsb              		; compare strings
		jne check_tail_exit						; if not ZF       		
check_tail_exit:				
		ret
check_tail endp

unload_handler proc near
		mov ah, 35h
		mov al, 1ch
		int 21h			; es:bx = custom handler
		cli
		

		call RECOVER_HANDLER	; RECOVER_HANDLER default handler
		;Освобождение памяти резидента:
		mov SI, offset KEEP_PSP
		mov AX, ES:[SI]
		mov ES,AX
		
		
		PUSH ES
		MOV ES, ES:2CH
		MOV AH, 49H
		INT 21H ; освобождение блока памяти
		POP ES
		MOV AH, 49H
		INT 21H ; освобождение блока памяти

		sti

		;Завершение программы:
		mov DX,offset UNLOAD_MESSAGE
		call OUTPUT_PROC
		mov AX,4C00h
		int 21h
			
		
		ret
unload_handler endp

MAIN PROC FAR
		push DS
		mov AX, seg DATA
		mov DS, AX

		call check_tail
		je unload 				; if ZF = 1, tail is ' /un'
		
		call check_handler
		jne load						; if handler havent been set
		lea dx, HANDLER_IS_ALREADY_SET_MESSAGE
		call OUTPUT_PROC				; print message
		jmp exit_main					; exit
		
unload:
		call check_handler
		jne unloaded
		call unload_handler
		unloaded:
		lea dx, UNLOAD_MESSAGE
		call OUTPUT_PROC				; print message
		jmp exit_main

load:
		mov ax, ss
		sub ax, 16
		mov KEEP_PSP, ax
		call KEEP_HANDLER
		mov DX,	offset ROUT
		mov AX,	seg ROUT
		push ds
		mov DS,	AX
		call SET_HANDLER
		pop ds
		lea dx, HANDLER_HAD_BEEN_SET_MESSAGE
		call OUTPUT_PROC
		call KEEP						; exit
exit_main:
		mov ah,4Ch
		int 21h
		ret
MAIN ENDP
CODE ENDS

DATA SEGMENT
		HANDLER_IS_ALREADY_SET_MESSAGE	db 'Custom interrupt handler is already set', '$'
		HANDLER_HAD_BEEN_SET_MESSAGE	db 'Custom interrupt handler had been set', '$'
		UNLOAD_MESSAGE			db 'Handler is unloaded', '$'
		SIGNATURE_CHECK			db 'Losev$'
		UNLOAD_COMMAND			db ' /un'
		SEG_STR					db '0000$'
DATA ENDS 

END MAIN