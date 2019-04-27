CODE SEGMENT
	ASSUME CS:CODE, DS:DATA, SS:AStack
; start of resident program
		; resident program data:
		SIGNATURE		DB 'Losev_lab5$'
		int9_vect		dd 0
		COUNT 			DW 0
		COUNT_STRING	DB '000000$'
		KEEP_AX			dw 0
		KEEP_CS 		DW 0
		KEEP_IP 		DW 0
		KEEP_PSP		dw 0
		KEEP_SP			dw 0
		KEEP_SS			dw 0
		COUNTER			dw 0
		COUNT_MESSAGE	db 'keyboard_handler CALLED:    $'
		
		; specified xt codes:
		Scancode_1 		db 01Eh						; press A
		Scancode_2		db 09Eh						; release A
		Scancode_3 		db 030h						; press B
		Scancode_4 		db 0B0h						; release B
		Scancode_5		db 02Eh						; press C
		Scancode_6		db 0AEh						; release C
		
		keyboard_handler_STACK 		dw 64 dup (?)
		stack_top		=$
;-----------------------------------------------
; define 2 macroses for ease:
push_registers macro
		mov KEEP_AX, ax
		mov KEEP_SP, sp
		mov KEEP_SS, ss
		mov AX, seg keyboard_handler_STACK
		mov SS, AX
		mov SP, offset stack_top
		push AX
		push BX
		push CX
		push DX
		push ds
endm

pop_registers macro
		pop ds
		pop DX
		pop CX
		pop BX
		pop AX
		mov ax, KEEP_AX
		mov SP, KEEP_SP
		mov SS, KEEP_SS
endm
;-------------------------------------------------
; functions that can be called by the resident:
fix_letter_case proc near		; cl = letter
		push 	es
		push 	si
		push	ax
		push 	cx
		
		mov		ax, 0040h
		mov 	es, ax
		mov 	si, 0017h
		mov 	ax, es:[si] 	; get the state bytes
				
		mov 	ch, al			; the last bit is right shift flag
		shr 	al, 1			; the prelast bit is left shift flag
		or 	ch, al				; now the lower bit of ch is shift flag
		
		shr 	ah, 6			; the second bit is CapsLock flag	
		xor 	ah, ch			; the lower bit of ah is now lower case flag
		
		and		ah, 00000001b	
		cmp		ah, 0
		jne 	fix_letter_case_exit; no changes
		pop		cx
		add 	cl, 20h				; 'A' -> 'a'
		push 	cx
	fix_letter_case_exit:
		pop		cx
		pop		ax
		pop		si
		pop		es
		ret
fix_letter_case endp

OUTPUT_PROC PROC NEAR ;Вывод на экран сообщения
		push ax
		mov ah, 09h
	    int 21h
	    pop ax
	    ret
OUTPUT_PROC ENDP

write_char_to_buffer proc near		; write character is cl
		push ax
		push bx
		push cx
		push dx 
	try:
		mov ah, 05h
		;mov cl, 'D'
		mov ch, 00h
		int 16h
		or al, al
		jnz clear_buffer
		jmp write_char_to_buffer_exit
	clear_buffer:				; clear the buffer
		push 	es
		push 	si
		
		mov 	ax, 0040h		; segment of keyboard buffer
		mov 	es, ax
		mov 	si, 001ah		; the beginning of the buffer
		mov 	ax, es:[si] 
		mov 	si, 001ch		; the end of the buffer
		mov 	es:[si], ax		; now end = beginning
		
		pop		si
		pop		es
		
		jmp try					; try again
	write_char_to_buffer_exit:
		pop dx
		pop cx
		pop bx
		pop ax
		
		ret
write_char_to_buffer endp
; resident interuption handler:
keyboard_handler PROC FAR		
		push_registers			; save registrs state
		in		al, 60h			; get scan-code
		
		push ax
		mov ax, cs
		mov ds, ax
		pop ax					; set ds = cs
		
	check_scancode1:
		cmp al, Scancode_1
		jne check_scancode2

		mov cl, 'A'
		call fix_letter_case
		call write_char_to_buffer
		jmp keyboard_handler_exit
	check_scancode2:
		cmp al, Scancode_2
		jne check_scancode3
		jmp keyboard_handler_exit
		
	check_scancode3:
		cmp al, Scancode_3
		jne check_scancode4
		mov cl, 'B'
		call fix_letter_case
		call write_char_to_buffer
		jmp keyboard_handler_exit
	check_scancode4:
		cmp al, Scancode_4
		jne check_scancode5
		jmp keyboard_handler_exit
		
	check_scancode5:
		cmp al, Scancode_5
		jne check_scancode6
		mov cl, 'C'
		call fix_letter_case
		call write_char_to_buffer
		jmp keyboard_handler_exit
	check_scancode6:
		cmp al, Scancode_6
		jne call_int9
		jmp keyboard_handler_exit

	call_int9:					; call default handler
		pop_registers			; recover registrs state
		jmp		cs:[int9_vect]

	keyboard_handler_exit:
		pop_registers
		mov 	al, 20h
		out 	20h, al
		iret
keyboard_handler ENDP

END_OF_RESIDENT db 0
; end of resident program



KEEP_HANDLER PROC NEAR
		push ax
		push bx
		push es
		
		mov ah, 35h
		mov al, 09h
		int 21h
		mov KEEP_CS, es
		mov KEEP_IP, bx
		mov word ptr int9_vect, bx
		mov word ptr int9_vect+2, es

		pop es
		pop bx
		pop ax
		
		ret
KEEP_HANDLER ENDP

RECOVER_HANDLER PROC NEAR
		push ax
		push dx
		push ds
		
		mov DX, ES:KEEP_IP
		mov AX, ES:KEEP_CS
		mov ds, ax
		call SET_HANDLER

		pop ds
		pop dx
		pop ax
		
		ret
RECOVER_HANDLER ENDP

SET_HANDLER PROC NEAR		; (dx = offset, ds = seg)
		push ax
		mov AX, 2509h
		int 21h
		pop ax
		ret
SET_HANDLER ENDP

KEEP PROC NEAR 
		mov dx, offset END_OF_RESIDENT
		mov cl, 4
		shr dx, cl
		add dx, 10h		; 10h paragraphs (256 bytes) for PSP
		inc dx			; round up
		mov ah, 31h
		mov al, 0h
		int 21h
	    ret
KEEP ENDP

check_signature proc near
		mov ah, 35h
		mov al, 09h
		int 21h				; es:bx (load to es segment of interruption handler)
		
		cld                				; straight direction
		mov cx, 11     					; signature lenght 
		lea si, SIGNATURE_CHECK			; ds:si = data seg : offset SIGNATURE_CHECK
		mov	di, 0h	 					; es:di = handler segment : 0h
		repe cmpsb              		; compare strings
		jne check_signature_exit			; if not ZF       		
	check_signature_exit:
		ret
check_signature endp

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
		mov al, 09h
		int 21h			; es:bx = custom handler

		call RECOVER_HANDLER	; RECOVER_HANDLER default handler
		
		mov SI, offset KEEP_PSP
		mov ES, ES:[SI]
		
		PUSH ES
		MOV ES, ES:[2Ch]
		MOV AH, 49H
		INT 21H 				; free environment variable scope
		POP ES
		MOV AH, 49H
		INT 21H 				; environment resident scope

		ret
unload_handler endp

MAIN PROC FAR
		push DS
		mov AX, seg DATA
		mov DS, AX

		call check_tail
		je unload 				; if ZF = 1, tail is ' /un'
		
		call check_signature
		jne load						; if handler havent been set
		lea dx, HANDLER_IS_ALREADY_SET_MESSAGE
		call OUTPUT_PROC				; print message
		jmp exit_main					; exit
		
unload:
		call check_signature
		jne not_loaded
		call unload_handler
		mov DX,offset UNLOADED_MESSAGE
		call OUTPUT_PROC
		jmp exit_main
		
not_loaded:
		lea dx, NOT_LOADED_MESSAGE
		call OUTPUT_PROC				; print message
		jmp exit_main

load:
		mov ax, cs
		sub ax, 010h 					; psp
		mov KEEP_PSP, ax
		call KEEP_HANDLER
		mov DX,	offset keyboard_handler
		mov AX,	seg keyboard_handler
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

AStack SEGMENT STACK
		dw 16 dup (?)
AStack ENDS

DATA SEGMENT
		HANDLER_IS_ALREADY_SET_MESSAGE	db 'Custom interrupt handler is already set', '$'
		HANDLER_HAD_BEEN_SET_MESSAGE	db 'Custom interrupt handler had been set', '$'
		UNLOADED_MESSAGE					db 'Handler is unloaded', '$'
		NOT_LOADED_MESSAGE				db 'Handler is not loaded', '$'
		SIGNATURE_CHECK					db 'Losev_lab5$'
		UNLOAD_COMMAND					db ' /un'
		SEG_STR							db '0000$'
DATA ENDS 

END MAIN