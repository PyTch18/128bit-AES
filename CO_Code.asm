SUB_BYTES MACRO DATA_PTR, SBOX_PTR
    
    LEA SI, DATA_PTR      ; SI points to DATA1
    LEA DX, SBOX_PTR      ; DX points to SBOX
    MOV CX, 16         ; We will process 16 bytes
    local SubByteLoop
    SubByteLoop:
       
    ; Load the byte from DATA1
    MOV AL, [SI]                  ; Load current byte into AL
    
    ; Extract row (high nibble) and column (low nibble)
    MOV BL, AL                    ; Copy AL to BL
    SHR AL, 4                     ; Get the row (high nibble) in AL
    AND BL, 0Fh                   ; Get the column (low nibble) in BL
    
    ; Calculate S-Box index: (row * 16) + column
    MOV AH, 0                     ; Clear AH for 16-bit calculations
    SHL AL, 4                     ; Multiply row by 16
    ADD AL, BL                    ; Add column to row*16
    MOV BX, AX                    ; BX now holds the index into SBOX
    
    ; Perform S-Box substitution
    PUSH SI                       ; Save SI (preserve pointer to DATA)
    MOV SI, DX                    ; Base address of SBOX
    ADD SI, BX                    ; Add the index to SI
    MOV AL, [SI]                  ; Fetch the substituted value from SBOX
    
    ; Store the result in SUBBYTES_RESULT
    POP SI                        ; Restore SI (points to DATA1) 
    MOV [SI], Al
    ; Increment pointers
    inc SI
    LOOP SubByteLoop              ; Repeat for all 16 bytes 
ENDM

SHIFT_ROWS MACRO INPUT_PTR 
    
    LEA SI, INPUT_PTR 
    
    ; Row 0: No Shift 
    ; Shifting is done in place, so no need for doing the fitsy row
   

    ; Row 1: Shift left by 1
    MOV AH, [SI+4]
    MOV AL, [SI+5]    ; b5
    MOV [SI+4], AL
    MOV AL, [SI+6]    ; b6
    MOV [SI+5], AL
    MOV AL, [SI+7]    ; b7
    MOV [SI+6], AL
    MOV AL, AH        ; b4
    MOV [SI+7], AL

    ; Row 2: Shift left by 2
    MOV AH, [SI+8]
    MOV AL, [SI+10]   ; b10
    MOV [SI+8], AL
    MOV BL, [SI+9]
    MOV AL, [SI+11]   ; b11
    MOV [SI+9], AL
    MOV AL, AH        ; b8
    MOV [SI+10], AL
    MOV AL, BL        ; b9
    MOV [SI+11], AL

    ; Row 3: Shift left by 3
    MOV AH, [SI+12]
    MOV AL, [SI+15]   ; b15
    MOV [SI+12], AL
    MOV BL, [SI+13]
    MOV AL, AH        ; b12
    MOV [SI+13], AL
    MOV AL, BL        ; b13
    MOV AH, [Si+14]
    MOV [SI+14], AL
    MOV AL, AH        ; b14
    MOV [SI+15], AL
ENDM 

ADD_ROUND_KEY MACRO INPUT_PTR, ROUND_KEY_PTR
    LEA SI, INPUT_PTR
    LEA DI, ROUND_KEY_PTR
    MOV CX, 16
    local ADDROUNDKEY 
    ADDROUNDKEY:
    MOV AL, [SI]
    XOR AL, [DI]
    MOV [SI], AL
    INC SI 
    INC DI       
    LOOP ADDROUNDKEY
ENDM       

mixcolumn macro DATA_PTR, MATRIX_PTR, TMP_PTR
    local process_column, next_column, inter_processing, inner, increment, skip_line
    lea si, DATA_PTR         ; Load source data pointer
    lea di, TMP_PTR
    mov bh, 0h
    mov cx, 0           

process_column: 
    push cx   
    lea dx, MATRIX_PTR       ; Load matrix pointer 

    mov cx, 4
    inter_processing:                  ; Save loop counter

    ; Initialize the result for the current column 
    
    
    mov al, [si]             ; Load first byte of the column
    mult_macro dx            ; Multiply by the first matrix element
    mov ah, al               ; Store partial result

    ; Process next three rows of the matrix
    mov al, [si + 4]         ; Load second byte of the column
    inc dx                   ; Move to next matrix element
    mult_macro dx
    xor ah, al               ; Accumulate result (GF addition)

    mov al, [si + 8]         ; Load third byte of the column
    inc dx                   ; Move to next matrix element
    mult_macro dx
    xor ah, al               ; Accumulate result (GF addition)

    mov al, [si + 12]        ; Load fourth byte of the column
    inc dx                   ; Move to next matrix element
    mult_macro dx
    xor ah, al               ; Accumulate result (GF addition)

    ; Store the result back 
    inc dx
    
    mov [di], ah
    inc di
    inc di
    inc di 
    cmp cx, 0
    jz skip_line
    inc di 
    skip_line:
    
    loop inter_processing
    
    continue:
    ; Reset DX pointer for the next column
    lea di, TMP_PTR
    inc bh 

    inc si
    pop cx
    inc cx
    push cx
    increment: 
    inc di
    loop increment
    pop cx
    cmp cx, 4                  ; Restore loop counter
    jnz process_column      ; Repeat for all columns 
     
     
     
    lea si, DATA_PTR        
    lea di, TMP_PTR      
    mov cx, 16
    returnToOriginal:
        mov bh,[di]
        mov [si], bh
        inc si
        inc di
    loop returnToOriginal
endm

mult_macro macro mat_ele
    local mult2, mult3, end_mult 
    
    push si
    mov si, mat_ele
    cmp [si], 02h
    jz mult2
    cmp [si], 03h
    jz mult3
    cmp [si], 01h
    jz end_mult

mult2:
    mult2_macro              ; Multiply by 2
    jmp end_mult

mult3:
    mult3_macro              ; Multiply by 3
end_mult:
    pop si
endm

; Macro for GF(2^8) multiplication by 2
mult2_macro macro
    LOCAL no_xor_mult2      ; Declare no_xor_mult2 as a local label
    ; AL already contains the byte to be multiplied by 2
    test al, 80h ; Check the most significant bit
    shl al, 1    ; Multiply by 2
    jnc no_xor_mult2  ; If no carry, skip the XOR
    xor al, 1bh  ; XOR with 1B if carry is set
no_xor_mult2: 
endm


; Macro for GF(2^8) multiplication by 3
mult3_macro macro
    mov bl, al
    mult2_macro    ; Multiply by 2 (result is in AL)
    xor al, bl    ; Add the original value (equivalent to multiplying by 3)
endm  

NEWLINE MACRO
    mov ah, 2         ; DOS interrupt for output
    mov dl, 13        ; Carriage return
    int 21h
    mov ah, 2         ; DOS interrupt for output
    mov dl, 10        ; Line feed
    int 21h
ENDM


.MODEL SMALL 
.STACK 100H
.data segment
    
                ;   0     1     2     3     4     5     6     7     8     9     a     b     c     d     e     f
    SBOX       db 063h, 07Ch, 077h, 07Bh, 0F2h, 06Bh, 06Fh, 0C5h, 030h, 001h, 067h, 02Bh, 0FEh, 0D7h, 0ABh, 076h  ;0
               db 0CAh, 082h, 0C9h, 07Dh, 0FAh, 059h, 047h, 0F0h, 0ADh, 0D4h, 0A2h, 0AFh, 09Ch, 0A4h, 072h, 0C0h  ;1
               db 0B7h, 0FDh, 093h, 026h, 036h, 03Fh, 0F7h, 0CCh, 034h, 0A5h, 0E5h, 0F1h, 071h, 0D8h, 031h, 015h  ;2
               db 004h, 0C7h, 023h, 0C3h, 018h, 096h, 005h, 09Ah, 007h, 012h, 080h, 0E2h, 0EBh, 027h, 0B2h, 075h  ;3
               db 009h, 083h, 02Ch, 01Ah, 01Bh, 06Eh, 05Ah, 0A0h, 052h, 03Bh, 0D6h, 0B3h, 029h, 0E3h, 02Fh, 084h  ;4
               db 053h, 0D1h, 000h, 0EDh, 020h, 0FCh, 0B1h, 05Bh, 06Ah, 0CBh, 0Beh, 039h, 04Ah, 04Ch, 058h, 0CFh  ;5
               db 0D0h, 0EFh, 0AAh, 0FBh, 043h, 04Dh, 033h, 085h, 045h, 0F9h, 002h, 07Fh, 050h, 03Ch, 09Fh, 0A8h  ;6
               db 051h, 0A3h, 040h, 08Fh, 092h, 09Dh, 038h, 0F5h, 0BCh, 0B6h, 0DAh, 021h, 010h, 0FFh, 0F3h, 0D2h  ;7
               db 0CDh, 00Ch, 013h, 0ECh, 05Fh, 097h, 044h, 017h, 0C4h, 0A7h, 07Eh, 03Dh, 064h, 05Dh, 019h, 073h  ;8
               db 060h, 081h, 04Fh, 0DCh, 022h, 02Ah, 090h, 088h, 046h, 0EEh, 0B8h, 014h, 0DEh, 05Eh, 00Bh, 0DBh  ;9
               db 0E0h, 032h, 03Ah, 00Ah, 049h, 006h, 024h, 05Ch, 0C2h, 0D3h, 0ACh, 062h, 091h, 095h, 0E4h, 079h  ;a
               db 0E7h, 0C8h, 037h, 06Dh, 08Dh, 0D5h, 04Eh, 0A9h, 06Ch, 056h, 0F4h, 0EAh, 065h, 07Ah, 0AEh, 008h  ;b
               db 0BAh, 078h, 025h, 02Eh, 01Ch, 0A6h, 0B4h, 0C6h, 0E8h, 0DDh, 074h, 01Fh, 04Bh, 0BDh, 08Bh, 08Ah  ;c
               db 070h, 03Eh, 0B5h, 066h, 048h, 003h, 0F6h, 00Eh, 061h, 035h, 057h, 0B9h, 086h, 0C1h, 01Dh, 09Eh  ;d
               db 0E1h, 0F8h, 098h, 011h, 069h, 0D9h, 08Eh, 094h, 09Bh, 01Eh, 087h, 0E9h, 0CEh, 055h, 028h, 0DFh  ;e
               db 08Ch, 0A1h, 089h, 00Dh, 0BFh, 0E6h, 042h, 068h, 041h, 099h, 02Dh, 00Fh, 0B0h, 054h, 0BBh, 016h  ;f
    
    MATRIX     db 02, 03, 01, 01
               db 01, 02, 03, 01
               db 01, 01, 02, 03
               db 03, 01, 01, 02  

    ROUND_KEY  db 0FFh, 0FFh, 0FFh, 0FFh
               db 0FFh, 0FFh, 0FFh, 0FFh
               db 0FFh, 0FFh, 0FFh, 0FFh
               db 0FFh, 0FFh, 0FFh, 0FFh
    
    tmp DB 16 DUP(0)
    ;DATA3 db 019h,0a0h,09ah,0e9h,03dh,0f4h,0c6h,0f8h,0e3h,0e2h,08dh,048h,0beh,02bh,02ah,008h 
    ;DATA3 DB 00h, 01h, 02h, 03h, 04h, 05h, 06h, 07h, 08h, 09h, 0ah, 0bh, 0ch, 0dh, 0eh, 0fh
    DATA3 DB 16 DUP(0)          ; Storage for 16 bytes of input
    String DB 34, ?, 34 DUP(0)  ; Input buffer (32 hex chars + newline)
    PROMPT_MSG DB 'Enter 16 hex pairs (32 characters): $'
    ERROR_MSG DB 'Invalid input! Please try again.$'
    
.code segment
    MOV AX, @data
    MOV DS, AX
    
    CALL READ_INPUT  
    NEWLINE   
    
    
    ADD_ROUND_KEY DATA3 , ROUND_KEY
    MOV CX , 9  
    
    AES:
    PUSH CX
    SUB_BYTES DATA3, SBOX
    SHIFT_ROWS DATA3
    MIXCOLUMN DATA3, MATRIX , tmp
    ADD_ROUND_KEY DATA3 , ROUND_KEY
    POP CX 
    LOOP AES  
    
    SUB_BYTES DATA3, SBOX
    SHIFT_ROWS DATA3 
    ADD_ROUND_KEY DATA3 , ROUND_KEY
    
    ; Print the RESULT
    CALL DISPLAY
    hlt
 ; Procedure to read 16 hexadecimal byte pairs at once and store them in DATA3
READ_INPUT PROC
    LEA DI, DATA3       ; Destination for input bytes
    MOV CX, 16          ; Number of bytes to read

    ; Prompt user for input
    MOV DX, OFFSET PROMPT_MSG
    MOV AH, 09h
    INT 21H

    ; Read the entire input string (32 hex characters)
    MOV BYTE PTR [String], 34 ; Allow up to 32 hex characters + newline
    LEA DX, String
    MOV AH, 0Ah         ; DOS interrupt for input
    INT 21H             ; Read input

    ; Validate the length of the input
    MOV AL, [String + 1] ; Get the length of input
    CMP AL, 32          ; Should be exactly 32 characters (16 pairs)
    JNE INPUT_ERROR     ; Retry if not

    ; Process the input string
    LEA SI, [String + 2] ; Point to the actual input (skip size byte)

    ; Convert hex pairs to bytes
    CALL ConvertToBytes

    RET                 ; Done

INPUT_ERROR:
    MOV DX, OFFSET ERROR_MSG
    MOV AH, 09H
    INT 21H
    JMP READ_INPUT      ; Retry on error

READ_INPUT ENDP

; Convert a single hex character to its nibble value
HexCharToNibble PROC
    CMP AL, '0'
    JAE ValidHex        ; Check if >= '0'
    JMP HexCharError    ; Invalid input
ValidHex:
    CMP AL, '9'
    JBE DigitChar       ; Is a decimal digit
    CMP AL, 'A'
    JAE LetterChar      ; Check if >= 'A'
    JMP HexCharError    ; Invalid input
DigitChar:
    SUB AL, '0'         ; Convert ASCII '0'-'9' to 0-9
    RET
LetterChar:
    SUB AL, 'A'         ; Convert ASCII 'A'-'F' to 10-15
    ADD AL, 0ah
    RET
HexCharError:
    MOV AL, 0           ; Return 0 on error (optional handling)
    RET
HexCharToNibble ENDP

; Procedure to convert 16 hex pairs from the input string to bytes
ConvertToBytes PROC
    LEA DI, DATA3       ; Destination for bytes
    LEA SI, String      ; Source is the string with hex pairs

    INPUT_LOOP:
        MOV AL, [SI]    ; Load first character
        CALL HexCharToNibble ; Convert to high nibble
        SHL AL, 4       ; Shift left to make room for low nibble
        INC SI          ; Move to next character
        MOV BL, [SI]    ; Load second character
        CALL HexCharToNibble ; Convert to low nibble
        OR AL, BL       ; Combine high and low nibbles
        MOV [DI], AL    ; Store the byte in DATA3
        INC DI          ; Move to next byte
        INC SI          ; Move to next hex pair
        LOOP INPUT_LOOP ; Repeat for all 16 bytes

    RET
ConvertToBytes ENDP



DISPLAY PROC
    LEA SI, DATA3
    MOV CX, 16
PRINTLOOP:
    MOV AL, [SI]
    CALL ByteToHex
    lea DX, String
    MOV AH, 09H
    INT 21H
    INC SI
    LOOP PRINTLOOP
    RET
DISPLAY ENDP

; Convert byte to hexadecimal string
ByteToHex PROC
    PUSH AX
    PUSH BX
    MOV BL, AL          ; Copy byte to BL
    SHR BL, 4           ; Get high nibble
    AND BL, 0FH         ; Mask high nibble
    ADD BL, 30H         ; Convert to ASCII (0-9)
    CMP BL, 39H         ; Check if greater than '9'
    JBE StoreHigh
    ADD BL, 07H         ; Adjust for A-F
StoreHigh:
    MOV [String], BL    ; Store high nibble
    MOV BL, AL          ; Copy byte again
    AND BL, 0FH         ; Mask low nibble
    ADD BL, 30H         ; Convert to ASCII (0-9)
    CMP BL, 39H         ; Check if greater than '9'
    JBE StoreLow
    ADD BL, 07H         ; Adjust for A-F
StoreLow:
    MOV [String + 1], BL ; Store low nibble
    MOV [String + 2], '$' ; String terminator for DOS
    POP BX
    POP AX
    RET
ByteToHex ENDP


ret 