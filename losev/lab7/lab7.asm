AStack SEGMENT STACK
		dw 16 dup (?)
AStack ENDS

DATA SEGMENT
		DTA						db 43 dup (?)
		overlay1_name			db 'overlay1.ovl', 0		; 0 = eol
		overlay2_name			db 'overlay2.ovl', 0
		path					db '00000000000000000000', '$'
				
		error_message1			db 'File not found or route not found',  0dh, 0ah, '$'
		overlay_segment			dw 0
		
		CALL_ADDR         		dd 0
		
		load_overlay_error_message00		db 'Load overlay error:     ',0Dh,0Ah,'$'
		load_overlay_error_message_1 		db 'Wrong function number.',0Dh,0Ah,'$'	
		load_overlay_error_message_2		db 'File is not found.',0Dh,0Ah,'$'
		load_overlay_error_message_3		db 'Path is not found.',0Dh,0Ah,'$'
		load_overlay_error_message_4		db 'Too many opened files.',0Dh,0Ah,'$'
		load_overlay_error_message_5		db 'Permission denied.',0Dh,0Ah,'$'
		load_overlay_error_message_8		db 'Memory is not enough.',0Dh,0Ah,'$'
		load_overlay_error_message_10		db 'Wrong environment.',0Dh,0Ah,'$'
		load_overlay_error_message_other	db 'Unknown error.',0Dh,0Ah,'$'
		
		;SEG_STR				db '0000', 0dh, 0ah, '$'
DATA ENDS 

CODE SEGMENT
	ASSUME CS:CODE, DS:DATA, SS:AStack
	
	; need it to be in code segment cause overlay can change ds, ss, sp
	KEEP_SS		dw 	0
	KEEP_SP		dw	0
	KEEP_DS		dw	0
	

OUTPUT_PROC PROC NEAR
		push ax
		mov ah, 09h
	    int 21h
	    pop ax
	    ret
OUTPUT_PROC ENDP

free_mem proc near
		push ax
		push bx
	
		mov ax, offset END_OF_PROGRAMM		; get program size, bytes
		mov bx, ax
		and bx, 0Fh
		cmp bx, 0h
		je size_multiple_of_10h				; if round up is not needed
		add ax, 0fh							; for round up
	size_multiple_of_10h:
		mov bl, 10h
		div bl								; get program size, paragraphs				
		mov bl, al
		mov bh, 0h
		mov ah, 4ah
		add bx, 100h						; for psp
		int 21h
		
		pop bx
		pop ax
		ret
free_mem endp

strcpy proc near ; copy string from ds:si to es:di
		; not saving registers di, si
		push ax
	strcpy_loop:
		mov al, ds:[si]
		mov es:[di], al
		inc si
		inc di
		cmp al, 00h			; cmp goes after copying so 0 is copied too
		je exit_strcpy
		jmp strcpy_loop
	exit_strcpy:
		pop ax
	ret
strcpy endp

get_overlay_size proc near 
		; input: ds:dx = overlays path
		; output: bx = overlays size in paragraphs
		push ax
		push cx
		push dx
		push di
		
		push dx
		mov ah, 1Ah
		lea dx, DTA
		int 21h
		pop dx
		
		mov cx, 0h
		mov ah, 4Eh
		int 21h
		jc get_overlay_size_error
		
		lea di, DTA
		add di, 1Ah
		mov bx, [di]
		add bx, 0Fh
		shr bx, 04h		; *= 2^4 = 16
		add di, 2
		mov ax, [di]
		sal ax, 0Ch		; /= 2^12 = 16^3
		add bx, ax
		inc bx
		jmp exit_get_overlay_size
		
	get_overlay_size_error:
		lea dx, error_message1
		call OUTPUT_PROC
	
	exit_get_overlay_size:
		pop di
		pop dx
		pop cx
		pop ax
		ret
get_overlay_size endp

allocate_memory proc near
		; input: bx - size of new block, paragraph
		; output: block = blocks seg adr
		mov ah, 48h
		int 21h
		mov overlay_segment, ax
		
		ret
allocate_memory endp

prepare_path proc near
		;input: ds:si = overlay name, last byte is 0
		push es
		push ax
		push di
		push ds
		push si
		
	; find first byte of path:
		mov es, es:[2Ch]	; get env segment
		xor si, si
	path_loop:
		mov al, es:[si]
		inc si
		cmp al, 0
		jne path_loop		; one zero in a row
		mov al, es:[si]
		inc si
		cmp al, 0
		jne path_loop		; one zero in a row
		add si, 2			; skip two bytes after env. and before 
	
	; prerare segments for strcpy:
		push ds			
		mov ax, es
		mov ds, ax			; ds:si = env. var. seg. : path start offset
		pop es
		push es
		lea di, path		; es:di = data seg. : string named path offset
		call strcpy			; copy string from ds:si to es:di
		pop ds				; recover ds = data segment
		
	; find the last slash if the path:
	slash_loop:
		dec di
		mov al, es:[di]
		cmp al, '\'
		jne slash_loop
		inc di
		
		pop si
		pop ds
	; prerare segments for strcpy:
	
		;lea si, overlay1_name	; ds:si = overlay name (seg : offset)
		mov ax, ds
		mov es, ax
		call strcpy				; copy string from ds:si to es:di
		mov es:[di], al
		
		pop di
		pop ax
		pop es
		
		ret
prepare_path endp

exec_ovl proc near
		push ax
		push bx
		push cx
		push dx
		push bp
		push es
		push di
		push si
		mov KEEP_SP, sp
		mov KEEP_SS, ss
		mov KEEP_DS, ds
		
		
	; load overlay:
		mov ax, seg DATA
		mov ds, ax
		lea dx, path
		mov es, ax
		lea bx, overlay_segment
		mov AX, 4B03h
		int 21h
		jc load_overlay_error_1
		
	; execute overlay:
		mov ax, overlay_segment
		mov word ptr CALL_ADDR + 2, ax
		call CALL_ADDR 
	; free memory:
		mov ax, overlay_segment
		mov es, ax
		mov ax, 4900h 
		int 21h
	; recover ds, ss, sp 
		mov ax, KEEP_SP
		mov sp, ax
		mov ax, KEEP_SS
		mov ss, ax
		mov ax, KEEP_DS
		mov ds, ax
		
		jmp exit_exec_ovl
		
		
	load_overlay_error_1:
		cmp AX, 1
		jne load_overlay_error_2
		mov DX,offset load_overlay_error_message_1
		jmp load_overlay_error_print
	load_overlay_error_2:
		cmp ax, 2
		jne load_overlay_error_3
		mov DX,offset load_overlay_error_message_2
		jmp load_overlay_error_print
	load_overlay_error_3:
		cmp ax, 3
		jne load_overlay_error_4
		mov DX,offset load_overlay_error_message_3
		jmp load_overlay_error_print
	load_overlay_error_4:
		cmp ax, 4
		jne load_overlay_error_5
		mov DX,offset load_overlay_error_message_4
		jmp load_overlay_error_print
	load_overlay_error_5:
		cmp ax, 5
		jne load_overlay_error_8
		mov DX,offset load_overlay_error_message_5
		jmp load_overlay_error_print
	load_overlay_error_8:
		cmp ax, 8
		jne load_overlay_error_10
		mov DX,offset load_overlay_error_message_8
		jmp load_overlay_error_print
	load_overlay_error_10:
		cmp ax, 10
		jne load_overlay_error_other
		mov DX,offset load_overlay_error_message_10
		jmp load_overlay_error_print
	load_overlay_error_other:
		mov DX,offset load_overlay_error_message_other
		jmp load_overlay_error_print	
		
	load_overlay_error_print:
		mov DX, offset load_overlay_error_message00
		call OUTPUT_PROC

	exit_exec_ovl:
		pop si
		pop di
		pop es
		pop bp
		pop dx
		pop cx
		pop bx
		pop ax
		ret
exec_ovl endp

MAIN PROC FAR
		push DS
		mov ax, seg DATA
		mov ds, ax
		
		call free_mem
		
		lea si, overlay1_name
		call prepare_path
		lea dx, path
		call get_overlay_size
		call allocate_memory
		call exec_ovl
		
		lea si, overlay2_name
		call prepare_path
		lea dx, path
		call get_overlay_size
		call allocate_memory
		call exec_ovl

exit_main:
		mov ah,4Ch
		int 21h
		ret
MAIN ENDP
END_OF_PROGRAMM:
CODE ENDS
END MAIN