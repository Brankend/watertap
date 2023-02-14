 CS EQU P2.5 		;Chip Select 
RDP EQU P2.2           	;Read signal 
WRP EQU P2.3           	;Write signal 
INTR EQU P2.4         	;INTR signal 
;LED_PORT EQU P1 
ADC_PORT EQU P1       ;ADC data pins P3
ADC_VAL EQU 30H       ;ADC read value stored in address 30H
    ORG 00H
    RS BIT P2.6
    RW BIT P2.7
    E  BIT P0.7
    CLR P2.0
    CLR P2.1
ACALL LCD_INITIALIZATION		
;PRINTING A CHARACTER
		;MOV A, #'A'
		;ACALL DATAWRT	

ROUTINE:
    ;ACALL DELAY
    ACALL CONV 
    ACALL READ            ;Read converted value
    ACALL REPEAT
    ACALL CONV 
    ACALL READ 
    MOV A,ADC_VAL
    MOV B,#50
        DIV AB
        MOV R0,B
        MOV B ,#29
        MUL AB
        MOV R1,A
        MOV A,R0
        MOV B,#3
        MUL AB
        MOV B,#5
        DIV AB
        ADD A,R1
        ADD A,#1       ; move the contents of register A to R6
        MOV B,#100
        DIV AB
        JZ BV
        ADD A,#48
        ACALL DATAWRT	
   BV:  MOV A,B
        MOV B,#10
        DIV AB
        ADD A,#48
        ACALL DATAWRT
        MOV A,#00H
        MOV A,B
       ; ACALL DATAWRT 
        ADD A,#48
        ;ACALL DATAWRT
        ACALL DATAWRT
        MOV A,#223
        MOV A,#00H
        ADD A,#223
        ACALL DATAWRT
        MOV A,#67
        MOV A,#00H
        ADD A,#67
        ACALL DATAWRT
        ;X / 1.72 = X * 29/50
    ;ADD A,#26H

    ;MOV LED_PORT,ADC_VAL  ;Move the value to Port 1 (LED Port)
    ;SJMP ROUTINE          ;Do it again

                    ;Reading done


				
;DELAY SUBROUTINE

 
loo :
MOV A,#00H
         ;Set P2.1 as input
     ;MOV A,P2
     ;ANL A,#00000100b
     
     JB P2.1,BUTTON_PRESSED
    ;Button is pressed

BUTTON_NOT_PRESSED:
       CLR P2.0
       JMP loo
BUTTON_PRESSED:
    ;Button is pressed, perform the necessary actions
        SETB P2.0
	MOV TMOD, #00010000B ; TIMER 1 IS SET FOR TIMER MODE 1, Soft controlled by TR1.
	ACALL REPEAT

    JMP loo
;COMMAND SUB-ROUTINE FOR LCD CONTROL
COMMANDWRT:

    MOV P3, A ;SEND DATA TO P1
	CLR RS	;RS=0 FOR COMMAND
	CLR RW	;R/W=0 FOR WRITE
	SETB E	;E=1 FOR HIGH PULSE
	ACALL DELAY ;SOME DELAY
	CLR E	;E=0 FOR H-L PULSE
	RET

;SUBROUTINE FOR DATA LACTCHING TO LCD
DATAWRT:

	MOV P3, A
	SETB RS	;;RS=1 FOR DATA
	CLR RW
	SETB E
	ACALL DELAY
	CLR E
	RET
	
DELAY1:
    	MOV R0, #10 ;DELAY. HIGHER VALUE FOR FASTER CPUS
Y:	MOV R1, #255
	DJNZ R1, $
	DJNZ R0, Y
RET
LCD_INITIALIZATION :
		MOV A, #38H	; INITIATE LCD
		ACALL COMMANDWRT
		ACALL DELAY

		MOV A, #0FH	; DISPLAY ON CURSOR ON
		ACALL COMMANDWRT
		ACALL DELAY
		
		MOV A, #01H	; CLEAR LCD
		ACALL COMMANDWRT
		ACALL DELAY
		RET
FORCE_CURSOR_TO_2ND_LINE:
		MOV A, #0C0H	
		ACALL COMMANDWRT
		ACALL DELAY
		RET
PRINTING_A_STRING:
		MOV DPTR, #STRINGDATA
STRING:		CLR A
		MOVC A, @A+DPTR
		ACALL DATAWRT
		ACALL DELAY
		INC DPTR
		JZ HLT
		SJMP STRING
HLT:		RET
 
 CONV:                      ;Start of Conversion
    CLR CS             ;Make CS low
    ACALL DELAY
    CLR WRP            ;Make WR Low
    SETB CS            ;Make CS High. Disable ADC to allow conversion
        JB INTR,$       ;Wait for INTR signal
    SETB INTR          ;Conversion done. Set INTR to get accept upcoming conversions
    RET
 
READ:                         ;Read ADC value
    CLR CS
    SETB RDP
    ACALL DELAY
    CLR RDP               ;Make RD Low
    ACALL DELAY          ;Some delay to allow the stored values to be placed on the ADC port
    MOV A,ADC_PORT        ;Read the converted value
    MOV ADC_VAL,A         ;Store it in local variable
    RET
    DELAY: MOV R4, #10
       DJNZ R4, $
       RET
REPEAT: MOV R5, #0AFH
	;Timer Clk = 11.0592/12*1 = 0.9216 MHz (1 cycle = 1.085.... µs)
	;   50000 uS = (65536 - Count) * (1 / 0.9216). Count = 19456 => 4C00H
	;OR 50000 uS / (1 / 0.9216) uS = 19456 (65536 - 19456 = 19456 => 4C00H)
LOOP: 	MOV TH1, #04H ; TIMER 1 HIGH BYTE IS LOADED
	MOV TL1, #00H ; TIMER 1 LOW BYTE IS LOADED
	SETB TR1 ; START TIMER 1
	
	JNB TF1, $ ; LOOP AROUND UNTIL TIMER 1 OVERFLOWS
	CLR TR1 ; STOP TIMER 1
	CLR TF1 ; CLEAR OVERFLOW FLAG

	DJNZ R5, LOOP

	;CJNE A, #0AH, REPEAT
RET

            ;Loop back to the beginning
            ORG 300H
STRINGDATA:	DB	"HELLO" ,0 ;STRING AND NULL

END
