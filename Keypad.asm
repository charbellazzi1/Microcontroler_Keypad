#include "p16f877a.inc"
__config 0x1f72 ; two underscores 

D_COL1	EQU 0x20
D_COL2	EQU 0x21
D_COL3	EQU 0x22
D_COL4	EQU 0x23
D_COL5	EQU 0x24
TEST	EQU 0X25
KEYVAL	EQU 0X26
COL_NUM	EQU 0X27
COL_CHOICE 	EQU 0x28
KEYVAL_INIT	EQU 0x29
COUNT1	EQU 0x30
COUNT2	EQU 0x31
TEMP	EQU 0x32
SHIFT EQU 1
TMR0_Start	EQU 0x62	;0X62 gonna put 0xfe just for debugging


org 0x00
	GOTO MAIN
org 0x04
	GOTO INT


DELAY_3.34ms
	CLRF	COUNT2	; 1 cycle
	MOVLW	0x03	; 1 cycle
	MOVWF	COUNT1	; 1 cycle
LOOP1	
	DECF	COUNT1	; value in Count1* cycles
	BTFSC	STATUS, Z ; 2 x (value in Count1-1) +1
	RETURN			;2 cycles
LOOP2
	DECF	COUNT2		; 256 x value in count1 cycles
	BTFSC	STATUS, Z	; 255 x 2 x value in Count1 cycle +(value in Count1)
	GOTO	LOOP1		;value in count1 cycles
	GOTO	LOOP2		;255 x 2 x value in Count1 cycle

	
ROTATE_TEMP
	ANDLW	0xC0
	MOVWF	TEMP
	RRF	TEMP, 1
	RRF	TEMP, 1
	RRF	TEMP, 1
	RRF	TEMP, 1
	RRF	TEMP, 1
	RRF	TEMP, 0
	MOVWF	PORTE
	RETURN

INT
	BCF	INTCON, GIE
	BTFSS	INTCON, TMR0IF
	GOTO CONTINUE
	BTFSS	TEST, 0 	;CHECK IF THE TMR0IF =1 RESULTED FORM INTERRUPT
	GOTO	CONTINUE
	BCF	TEST, 0
	BSF		INTCON, RBIE
	BCF	INTCON, TMR0IF
	BCF	INTCON, TMR0IE
	BTFSC	PORTB, 7
	BTFSS	PORTB, 6
	GOTO	KEY_DOWN
	BTFSC	PORTB, 5
	BTFSS	PORTB, 4
	GOTO	KEY_DOWN
	GOTO	ENDINT
	
KEY_DOWN
	BCF	TEST , SHIFT
	MOVLW	0x7E	;0111 1110
	MOVWF	PORTD
	BTFSS	PORTB,4
	BSF	TEST, SHIFT
	MOVLW	0xFF
	MOVWF	PORTD
	MOVFW	KEYVAL_INIT
	MOVWF	KEYVAL
	DECF	KEYVAL
	
	MOVLW 	0xFE	; column 0 set to 0
	MOVWF	PORTD	;
	MOVWF	COL_CHOICE
ROW_CHECK
	MOVFW	KEYVAL	; STORE KEYVAL FOR LATER ADDITION IF FOUND
	BTFSS	PORTB, 7 	;CHECK ROW 0
	GOTO	ADD1
	BTFSS	PORTB, 6	;CHECK ROW 1
	GOTO	ADD2
	BTFSS	PORTB, 5	;CHECK ROW 2
	GOTO	ADD3
	BTFSS	PORTD, 0	; CHECK IF COL0
	GOTO	CHANGE_COL
	BTFSS	PORTB, 4	;Check Row 3
	GOTO 	ADD4
CHANGE_COL	
	MOVLW 	0xFF
	XORWF	COL_CHOICE,1 ;exp 11111110 ->00000001  
	RLF	COL_CHOICE,0 ;00000001 -> 00000010
	XORLW	0xFF 		; 00000010 -> 11111101
	MOVWF 	COL_CHOICE
	MOVWF	PORTD
	DECF	KEYVAL
	BTFSC	STATUS, C	;CHECK IF IT WAS LAST COLUMN
	GOTO	ENDINT
	GOTO	ROW_CHECK
	
ADD1
	ADDWF	KEYVAL_INIT, 0
ADD2
	ADDWF	KEYVAL_INIT, 0
ADD3
	ADDWF	KEYVAL_INIT, 0
ADD4
	MOVWF	KEYVAL
LOOKUP_TABLES
	CALL ASCII_LOOKUP
	BTFSS	PIR1, TXIF	;TXIF=1 => TXREG is empty	
	GOTO 	$-1
	MOVWF	TXREG
	MOVFW	KEYVAL
	CALL	LOOKUP_COL1
	MOVWF	D_COL1
	MOVFW	KEYVAL
	CALL	LOOKUP_COL2
	MOVWF	D_COL2
	MOVFW	KEYVAL
	CALL 	LOOKUP_COL3
	MOVWF	D_COL3
	MOVFW	KEYVAL
	CALL	LOOKUP_COL4
	MOVWF	D_COL4
	MOVFW	KEYVAL
	CALL	LOOKUP_COL5
	MOVFW	D_COL5
	MOVLW	0XFF
	MOVWF	PORTD
	GOTO ENDINT

CONTINUE
	BTFSS	INTCON, RBIF
	GOTO	ENDINT
	BCF		INTCON, RBIE
	BCF		INTCON, RBIF
	MOVLW	TMR0_Start
	MOVWF	TMR0
	BSF	TEST,0
	BCF	INTCON, TMR0IF
	BSF	INTCON,	TMR0IE

ENDINT
	BSF INTCON,GIE
	RETFIE
	
	
	
ASCII_LOOKUP
	BTFSS	TEST, SHIFT
	ADDLW	0x20 ;32
	ADDWF	PCL
	RETLW	0x00
	RETLW	0x2D 	;-
	RETLW	0x2B	;+
	RETLW	0x2A	;*
	RETLW	0x2F	;/
	RETLW	0x50	;P
	RETLW	0x4F	;O
	RETLW 	0x00	;SHIFT NO NEED FOR ASCII
	RETLW	0x4C	;L
	RETLW	0x4D	;M
	RETLW	0x4E	;N
	RETLW	0x42	;B
	RETLW	0x56	;V
	RETLW	0x43	;C
	RETLW	0x58	;X
	RETLW	0x5A	;Z
	RETLW	0x4B	;K
	RETLW	0x4A	;J
	RETLW	0x48	;H
	RETLW	0x47	;G
	RETLW	0x46	;F
	RETLW	0x44	;D
	RETLW	0x53	;S
	RETLW	0x41	;A
	RETLW	0x49	;I
	RETLW	0x55	;U
	RETLW	0x59	;Y
	RETLW	0x54	;T
	RETLW	0x52	;R
	RETLW	0x45	;E
	RETLW	0x57	;W
	RETLW	0x51	;Q
	RETLW	0x00
	RETLW	0x2D 	;-
	RETLW	0x2B	;+
	RETLW	0x2A	;*
	RETLW	0x2F	;/
	RETLW	0x70	;p
	RETLW	0x6F	;o
	RETLW 	0x00	;SHIFT NO NEED FOR ASCII 
	RETLW	0x6C	;l
	RETLW	0x6D	;m
	RETLW	0x6E	;n
	RETLW	0x62	;b
	RETLW	0x76	;v
	RETLW	0x63	;c
	RETLW	0x78	;x
	RETLW	0x7A	;z
	RETLW	0x6B	;k
	RETLW	0x6A	;j
	RETLW	0x68	;h
	RETLW	0x67	;g
	RETLW	0x66	;f
	RETLW	0x64	;d
	RETLW	0x73	;s
	RETLW	0x61	;a
	RETLW	0x69	;i
	RETLW	0x75	;u
	RETLW	0x79	;y
	RETLW	0x74	;t
	RETLW	0x72	;r
	RETLW	0x65	;e
	RETLW	0x77	;w
	RETLW	0x71	;q
	


LOOKUP_COL1
	BTFSC	TEST, SHIFT
	ADDLW	0x20     
	ADDWF PCL
	RETLW 0x00  ; for SHIFT
	RETLW 0x08  ; ('-')
	RETLW 0x08  ; ('+')
	RETLW 0x14  ; ('*')
	RETLW 0x20  ; ('/')
	RETLW 0x7c  ; ('p')
	RETLW 0x38  ; ('o')
	RETLW 0x00  ; for SHIFT
	RETLW 0x00  ; ('l')
	RETLW 0x7c  ; ('m')
	RETLW 0x7c  ; ('n')
	RETLW 0x7f  ; ('b')
	RETLW 0x1c  ; ('v')
	RETLW 0x38  ; ('c')
	RETLW 0x44  ; ('x')
	RETLW 0x44  ; ('z')
	RETLW 0x7f  ; ('k')
	RETLW 0x20  ; ('j')
	RETLW 0x7f  ; ('h')
	RETLW 0x0c  ; ('g')
	RETLW 0x08  ; ('f')
	RETLW 0x30  ; ('d')
	RETLW 0x48  ; ('s')
	RETLW 0x20  ; ('a')
	RETLW 0x00  ; ('i')
	RETLW 0x3c  ; ('u')
	RETLW 0x0c  ; ('y')
	RETLW 0x04  ; ('t')
	RETLW 0x7c  ; ('r')
	RETLW 0x38  ; ('e')
	RETLW 0x3c  ; ('w')
	RETLW 0x08  ; ('q')
	RETLW 0x00  ; for SHIFT
	RETLW 0x08  ; ('-')
	RETLW 0x08  ; ('+')
	RETLW 0x14  ; ('*')
	RETLW 0x20  ; ('/')
	RETLW 0x7f  ; ('P')
	RETLW 0x3e  ; ('O')
	RETLW 0x00  ; for SHIFT
	RETLW 0x7f  ; ('L')
	RETLW 0x7f  ; ('M')
	RETLW 0x7f  ; ('N')
	RETLW 0x7f  ; ('B')
	RETLW 0x1f  ; ('V')
	RETLW 0x3e  ; ('C')
	RETLW 0x63  ; ('X')
	RETLW 0x61  ; ('Z')
	RETLW 0x7f  ; ('K')
	RETLW 0x20  ; ('J')
	RETLW 0x7f  ; ('H')
	RETLW 0x3e  ; ('G')
	RETLW 0x7f  ; ('F')
	RETLW 0x7f  ; ('D')
	RETLW 0x26  ; ('S')
	RETLW 0x7c  ; ('A')
	RETLW 0x00  ; ('I')
	RETLW 0x3f  ; ('U')
	RETLW 0x07  ; ('Y')
	RETLW 0x01  ; ('T')
	RETLW 0x7f  ; ('E')
	RETLW 0x7f  ; ('R')
	RETLW 0x3f  ; ('W')
	RETLW 0x3e  ; ('Q')
	  


LOOKUP_COL2
	BTFSC	TEST, SHIFT
	ADDLW	0x20
	ADDWF PCL       
	RETLW 0x00  ; for SHIFT
	RETLW 0x08  ; ('-')
	RETLW 0x08  ; ('+')
	RETLW 0x08  ; ('*')
	RETLW 0x10  ; ('/')
	RETLW 0x14  ; ('p')
	RETLW 0x44  ; ('o')
	RETLW 0x00  ; for SHIFT
	RETLW 0x41  ; ('l')
	RETLW 0x04  ; ('m')
	RETLW 0x04  ; ('n')
	RETLW 0x50  ; ('b')
	RETLW 0x20  ; ('v')
	RETLW 0x44  ; ('c')
	RETLW 0x28  ; ('x')
	RETLW 0x64  ; ('z')
	RETLW 0x10  ; ('k')
	RETLW 0x40  ; ('j')
	RETLW 0x08  ; ('h')
	RETLW 0x52  ; ('g')
	RETLW 0x7e  ; ('f')
	RETLW 0x48  ; ('d')
	RETLW 0x54  ; ('s')
	RETLW 0x54  ; ('a')
	RETLW 0x44  ; ('i')
	RETLW 0x40  ; ('u')
	RETLW 0x50  ; ('y')
	RETLW 0x3f  ; ('t')
	RETLW 0x08  ; ('r')
	RETLW 0x54  ; ('e')
	RETLW 0x40  ; ('w')
	RETLW 0x14  ; ('q')
	RETLW 0x00  ; for SHIFT
	RETLW 0x08  ; ('-')
	RETLW 0x08  ; ('+')
	RETLW 0x08  ; ('*')
	RETLW 0x10  ; ('/')
	RETLW 0x09  ; ('P')
	RETLW 0x41  ; ('O')
	RETLW 0x00  ; for SHIFT
	RETLW 0x40  ; ('L')
	RETLW 0x02  ; ('M')
	RETLW 0x02  ; ('N')
	RETLW 0x49  ; ('B')
	RETLW 0x20  ; ('V')
	RETLW 0x41  ; ('C')
	RETLW 0x14  ; ('X')
	RETLW 0x51  ; ('Z')
	RETLW 0x08  ; ('K')
	RETLW 0x40  ; ('J')
	RETLW 0x08  ; ('H')
	RETLW 0x41  ; ('G')
	RETLW 0x09  ; ('F')
	RETLW 0x41  ; ('D')
	RETLW 0x49  ; ('S')
	RETLW 0x12  ; ('A')
	RETLW 0x41  ; ('I')
	RETLW 0x40  ; ('U')
	RETLW 0x08  ; ('Y')
	RETLW 0x01  ; ('T')
	RETLW 0x49  ; ('E')
	RETLW 0x09  ; ('R')
	RETLW 0x40  ; ('W')
	RETLW 0x41  ; ('Q')

LOOKUP_COL3

	BTFSC	TEST, SHIFT
	ADDLW	0x20     
	ADDWF PCL    
	RETLW 0x00  ; for SHIFT
	RETLW 0x08  ; ('-')
	RETLW 0x3e  ; ('+')
	RETLW 0x3e  ; ('*')
	RETLW 0x08  ; ('/')
	RETLW 0x14  ; ('p')
	RETLW 0x44  ; ('o')
	RETLW 0x00  ; for SHIFT
	RETLW 0x7f  ; ('l')
	RETLW 0x18  ; ('m')
	RETLW 0x04  ; ('n')
	RETLW 0x48  ; ('b')
	RETLW 0x40  ; ('v')
	RETLW 0x44  ; ('c')
	RETLW 0x10  ; ('x')
	RETLW 0x54  ; ('z')
	RETLW 0x28  ; ('k')
	RETLW 0x44  ; ('j')
	RETLW 0x04  ; ('h')
	RETLW 0x52  ; ('g')
	RETLW 0x09  ; ('f')
	RETLW 0x48  ; ('d')
	RETLW 0x54  ; ('s')
	RETLW 0x54  ; ('a')
	RETLW 0x7d  ; ('i')
	RETLW 0x40  ; ('u')
	RETLW 0x50  ; ('y')
	RETLW 0x44  ; ('t')
	RETLW 0x04  ; ('r')
	RETLW 0x54  ; ('e')
	RETLW 0x30  ; ('w')
	RETLW 0x14  ; ('q')
	RETLW 0x00  ; for SHIFT
	RETLW 0x08  ; ('-')
	RETLW 0x3e  ; ('+')
	RETLW 0x3e  ; ('*')
	RETLW 0x08  ; ('/')
	RETLW 0x09  ; ('P')
	RETLW 0x41  ; ('O')
	RETLW 0x00  ; for SHIFT
	RETLW 0x40  ; ('L')
	RETLW 0x0c  ; ('M')
	RETLW 0x04  ; ('N')
	RETLW 0x49  ; ('B')
	RETLW 0x40  ; ('V')
	RETLW 0x41  ; ('C')
	RETLW 0x08  ; ('X')
	RETLW 0x49  ; ('Z')
	RETLW 0x14  ; ('K')
	RETLW 0x41  ; ('J')
	RETLW 0x08  ; ('H')
	RETLW 0x49  ; ('G')
	RETLW 0x09  ; ('F')
	RETLW 0x41  ; ('D')
	RETLW 0x49  ; ('S')
	RETLW 0x11  ; ('A')
	RETLW 0x7f  ; ('I')
	RETLW 0x40  ; ('U')
	RETLW 0x70  ; ('Y')
	RETLW 0x7f  ; ('T')
	RETLW 0x49  ; ('E')
	RETLW 0x19  ; ('R')
	RETLW 0x38  ; ('W')
	RETLW 0x51  ; ('Q')

LOOKUP_COL4
	BTFSC	TEST, SHIFT
	ADDLW	0x20
	ADDWF PCL        
	RETLW 0x00  ; for SHIFT
	RETLW 0x08  ; ('-')
	RETLW 0x08  ; ('+')
	RETLW 0x08  ; ('*')
	RETLW 0x04  ; ('/')
	RETLW 0x14  ; ('p')
	RETLW 0x44  ; ('o')
	RETLW 0x00  ; for SHIFT
	RETLW 0x40  ; ('l')
	RETLW 0x04  ; ('m')
	RETLW 0x04  ; ('n')
	RETLW 0x48  ; ('b')
	RETLW 0x20  ; ('v')
	RETLW 0x44  ; ('c')
	RETLW 0x28  ; ('x')
	RETLW 0x4c  ; ('z')
	RETLW 0x44  ; ('k')
	RETLW 0x3d  ; ('j')
	RETLW 0x04  ; ('h')
	RETLW 0x52  ; ('g')
	RETLW 0x01  ; ('f')
	RETLW 0x50  ; ('d')
	RETLW 0x54  ; ('s')
	RETLW 0x54  ; ('a')
	RETLW 0x40  ; ('i')
	RETLW 0x20  ; ('u')
	RETLW 0x50  ; ('y')
	RETLW 0x04  ; ('t')
	RETLW 0x04  ; ('r')
	RETLW 0x54  ; ('e')
	RETLW 0x40  ; ('w')
	RETLW 0x14  ; ('q')
	RETLW 0x00  ; for SHIFT
	RETLW 0x08  ; ('-')
	RETLW 0x08  ; ('+')
	RETLW 0x08  ; ('*')
	RETLW 0x04  ; ('/')
	RETLW 0x09  ; ('P')
	RETLW 0x41  ; ('O')
	RETLW 0x00  ; for SHIFT
	RETLW 0x40  ; ('L')
	RETLW 0x02  ; ('M')
	RETLW 0x08  ; ('N')
	RETLW 0x49  ; ('B')
	RETLW 0x20  ; ('V')
	RETLW 0x41  ; ('C')
	RETLW 0x14  ; ('X')
	RETLW 0x45  ; ('Z')
	RETLW 0x22  ; ('K')
	RETLW 0x3f  ; ('J')
	RETLW 0x08  ; ('H')
	RETLW 0x49  ; ('G')
	RETLW 0x09  ; ('F')
	RETLW 0x22  ; ('D')
	RETLW 0x49  ; ('S')
	RETLW 0x12  ; ('A')
	RETLW 0x41  ; ('I')
	RETLW 0x40  ; ('U')
	RETLW 0x08  ; ('Y')
	RETLW 0x01  ; ('T')
	RETLW 0x49  ; ('E')
	RETLW 0x29  ; ('R')
	RETLW 0x40  ; ('W')
	RETLW 0x21  ; ('Q')

LOOKUP_COL5
	BTFSC	TEST, SHIFT
	ADDLW	0x20
	ADDWF PCL       
	RETLW 0x00  ; for SHIFT
	RETLW 0x08  ; ('-')
	RETLW 0x08  ; ('+')
	RETLW 0x14  ; ('*')
	RETLW 0x02  ; ('/')
	RETLW 0x08  ; ('p')
	RETLW 0x38  ; ('o')
	RETLW 0x00  ; for SHIFT
	RETLW 0x00  ; ('l')
	RETLW 0x78  ; ('m')
	RETLW 0x78  ; ('n')
	RETLW 0x30  ; ('b')
	RETLW 0x1c  ; ('v')
	RETLW 0x20  ; ('c')
	RETLW 0x44  ; ('x')
	RETLW 0x44  ; ('z')
	RETLW 0x00  ; ('k')
	RETLW 0x00  ; ('j')
	RETLW 0x78  ; ('h')
	RETLW 0x3e  ; ('g')
	RETLW 0x02  ; ('f')
	RETLW 0x7f  ; ('d')
	RETLW 0x20  ; ('s')
	RETLW 0x78  ; ('a')
	RETLW 0x00  ; ('i')
	RETLW 0x7c  ; ('u')
	RETLW 0x3c  ; ('y')
	RETLW 0x00  ; ('t')
	RETLW 0x08  ; ('r')
	RETLW 0x18  ; ('e')
	RETLW 0x3c  ; ('w')
	RETLW 0x7c  ; ('q')
	RETLW 0x00  ; for SHIFT
	RETLW 0x08  ; ('-')
	RETLW 0x08  ; ('+')
	RETLW 0x14  ; ('*')
	RETLW 0x02  ; ('/')
	RETLW 0x06  ; ('P')
	RETLW 0x3e  ; ('O')
	RETLW 0x00  ; for SHIFT
	RETLW 0x40  ; ('L')
	RETLW 0x7f  ; ('M')
	RETLW 0x7f  ; ('N')
	RETLW 0x36  ; ('B')
	RETLW 0x1f  ; ('V')
	RETLW 0x22  ; ('C')
	RETLW 0x63  ; ('X')
	RETLW 0x43  ; ('Z')
	RETLW 0x41  ; ('K')
	RETLW 0x01  ; ('J')
	RETLW 0x7f  ; ('H')
	RETLW 0x7a  ; ('G')
	RETLW 0x01  ; ('F')
	RETLW 0x1c  ; ('D')
	RETLW 0x32  ; ('S')
	RETLW 0x7c  ; ('A')
	RETLW 0x00  ; ('I')
	RETLW 0x3f  ; ('U')
	RETLW 0x07  ; ('Y')
	RETLW 0x01  ; ('T')
	RETLW 0x41  ; ('E')
	RETLW 0x46  ; ('R')
	RETLW 0x3f  ; ('W')
	RETLW 0x5e  ; ('Q')




MAIN
	BCF 	INTCON, GIE
	BCF 	STATUS, RP1
	BSF 	STATUS, RP0 	; BANK1
	MOVLW	0xFF 		; set inputs for rows
	MOVWF	TRISB 
	CLRF	TRISD  		; output for keypad columns
	CLRF 	TRISC 		; outputs for LED Matrix columns and TX pin
	CLRF	TRISA 		;outputs for ROWS in LED Matrix
	CLRF	TRISE		;OUTPUTS
	MOVLW	0X04		;0000 0100
	MOVWF	OPTION_REG 	; enable port B pullup resistors ( RBPU bar = 0) and configure TMR0
	MOVLW	0x20		;0010 0000
	MOVWF	TXSTA		;enable transmition
	MOVLW	0x19		;9.6 BAUD Rate with 6.99% error	( now we just need to put a value to TXREG to transmit)
	MOVWF	SPBRG
	BSF 	INTCON,RBIE	; Set interrupt on change on Port B <4:7>

	BCF	STATUS, RP0	;BANK0
	CLRF	TXREG
	CLRF	PORTB
	CLRF	PORTD
	CLRF	PORTA
	CLRF	PORTE
	MOVLW	0x1F
	MOVWF	PORTC
	CLRF	TEST
	MOVLW	0X08	
	MOVWF	KEYVAL_INIT
	MOVLW	0XFF
	MOVWF	D_COL1
	MOVWF	D_COL2
	MOVWF	D_COL3
	MOVWF	D_COL4
	MOVWF	D_COL5
	BSF		RCSTA,SPEN	;ENABLE SERIAL PORT
	BCF	INTCON, RBIF
	BSF	INTCON, GIE
	BSF	INTCON, PEIE
	LOOP
		MOVFW	D_COL1
		MOVWF	PORTA
		CALL ROTATE_TEMP
		BCF	PORTC, 0
		CALL DELAY_3.34ms

		MOVFW	D_COL2
		BSF	PORTC, 0
		MOVWF	PORTA
		CALL ROTATE_TEMP
		BCF	PORTC, 1
		CALL DELAY_3.34ms

		MOVFW	D_COL3
		BSF	PORTC, 1
		MOVWF	PORTA
		CALL ROTATE_TEMP
		BCF	PORTC, 2
		CALL DELAY_3.34ms

		MOVFW	D_COL4
		BSF	PORTC, 2
		MOVWF	PORTA
		CALL ROTATE_TEMP
		BCF	PORTC, 3
		CALL DELAY_3.34ms

		MOVFW	D_COL5
		BSF	PORTC, 3
		MOVWF	PORTA
		CALL ROTATE_TEMP
		BCF	PORTC, 4
		CALL DELAY_3.34ms
		BSF	PORTC, 4
		GOTO LOOP
		END