; .COM
TESTPC 	SEGMENT
		ASSUME CS:TESTPC, DS:TESTPC, ES:NOTHING, SS:NOTHING
		ORG 100H
START: JMP BEGIN
; ДАННЫЕ
UNAVAL_FIRST_LA DB 'FIRST UNAVALIBLE BYTE: '
UNAVAL_FIRST_AD DB '    ', 0DH, 0AH, '$'
ENVIROM_SEG_LA 	DB 'ENVIROMENT SEGMENT: '
ENVIROM_SEG_AD 	DB '    ', 0DH, 0AH, '$'
ENVOIRM_CONTENT DB 'ENVOIRMENT ADRESS: ', 0DH, 0AH, '$'
NO_LAIL_LA 		DB 'COMMAND LINE TAIL MISSING', '$'
TAIL_LA 		DB 'COMMAND LINE TAIL:',  '$'
PATH_LA			DB 'PATH: ', '$'

EOL				DB 0DH, 0AH, '$'


; ПРОЦЕДУРЫ
;-----------------------------------------------------
TETR_TO_HEX   PROC  near 
			and      AL,0Fh
			cmp      AL,09
			jbe      NEXT
			add      AL,07 
NEXT:  	    add      AL,30h
            ret 
TETR_TO_HEX   ENDP 
;-------------------------------------------------- 
BYTE_TO_HEX   PROC  near
            push     CX
            mov      AH,AL
            call     TETR_TO_HEX
            xchg     AL,AH
            mov      CL,4
            shr      AL,CL
            call     TETR_TO_HEX ; AL &
			pop      CX          ; AH &
            ret 
BYTE_TO_HEX  ENDP 
;-------------------------------------------------- 
WRD_TO_HEX   PROC  near ;  16 / 16-   ;  AX - , DI -
            push     BX
            mov      BH,AH
            call     BYTE_TO_HEX
            mov      [DI],AH
            dec      DI
            mov      [DI],AL
            dec      DI
            mov      AL,BH
            call     BYTE_TO_HEX
            mov      [DI],AH
            dec      DI
            mov      [DI],AL
            pop      BX
            ret 
WRD_TO_HEX ENDP 
;-------------------------------------------------- 
PRINT PROC near
	mov AH, 09h
	int 21h
	ret
PRINT ENDP
;-------------------------------------------------- 


OUTPUT_PROC PROC NEAR ; вывод сообщения
		push ax
		MOV ah, 09h
	    INT 21h
	    pop ax
	    ret
OUTPUT_PROC ENDP
;--------------------------------------------------

PRINT_UNAVAL_MEM PROC NEAR 		; первый байт недоступной памяти
		MOV AX, ES:[02H]			
		LEA DI, UNAVAL_FIRST_AD
		ADD DI, 03H
		CALL 	WRD_TO_HEX
		LEA DX, UNAVAL_FIRST_LA
		CALL	OUTPUT_PROC
		RET
PRINT_UNAVAL_MEM ENDP

PRINT_SEG_ENV PROC NEAR  		;сегментный адрес среды
		MOV AX, ES:[2CH]			;
		LEA DI, ENVIROM_SEG_AD		; 
		ADD DI, 03H
		CALL 	WRD_TO_HEX
		LEA DX, ENVIROM_SEG_LA
		CALL	OUTPUT_PROC
		RET
PRINT_SEG_ENV ENDP

PRINT_TAIL PROC NEAR 			; хвост командной строки
		XOR CH, CH					
		MOV CL, ES:[80h] 
		CMP CL, 0H
		JE PRINT_NO_TAIL
		LEA DX, TAIL_LA
		CALL OUTPUT_PROC
		MOV SI, 81H				; 81H - СМЕЩЕНИЕ НАЧАЛА ХВОСТА
		MOV AH, 02H
	PRINT_CHAR:
			MOV DL, ES:[SI]
			INT 21H
			INC SI
			LOOP PRINT_CHAR
			JMP PRINT_TAIL_END
	PRINT_NO_TAIL:
		LEA DX, NO_LAIL_LA
		CALL OUTPUT_PROC
	PRINT_TAIL_END:
		LEA DX, EOL
		CALL OUTPUT_PROC
		RET
PRINT_TAIL ENDP


PRINT_ENV_CONT PROC NEAR 		; 
		LEA DX, ENVOIRM_CONTENT
		CALL OUTPUT_PROC

		MOV BX, ES:[2Ch]
		MOV ES, BX
		MOV AH, 02h
		XOR SI, SI
		
	ENV_LOOP:
		MOV DL, ES:[SI]
		INT 21h
		INC SI
		
		CMP WORD PTR ES:[SI], 0000h
		je PRINT_ENV_CONT_END
		
		CMP BYTE PTR ES:[SI], 00h
		JNE ENV_LOOP				
			LEA DX, EOL
			CALL OUTPUT_PROC	
			INC SI		
		JMP ENV_LOOP
	
	PRINT_ENV_CONT_END:
		LEA DX, EOL
		CALL OUTPUT_PROC
		RET
PRINT_ENV_CONT ENDP

PRINT_PATH PROC NEAR 	
		LEA DX, PATH_LA
		CALL OUTPUT_PROC
							; при входе в процедуру в SI смещение конца содержимого области среды
		ADD SI, 4			; пропускаем 0001h
		MOV AH, 02h			; номер функции вывода символа
		
		PATH_LOOP:
			MOV DL, ES:[SI]
			INT 21h
			INC SI
			CMP BYTE PTR ES:[SI], 00h
			JNE PATH_LOOP
		
		RET
PRINT_PATH ENDP

input_char proc near 
		mov ah, 01h
		int 21h

		ret
input_char endp

BEGIN:
		CALL PRINT_UNAVAL_MEM
		CALL PRINT_SEG_ENV
		CALL PRINT_TAIL
		CALL PRINT_ENV_CONT
		CALL PRINT_PATH
		
		lea dx, EOL
		call OUTPUT_PROC
		call input_char
		lea dx, EOL
		call OUTPUT_PROC
		
; ВЫХОД В DOS
		MOV AH, 4Ch
		INT 21H
TESTPC 	ENDS
		END START ;

;------------------------------- 
