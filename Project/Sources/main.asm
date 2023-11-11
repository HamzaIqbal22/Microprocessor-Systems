; COE 538 Project: Robot Guidance Program                                                


; export symbols
            XDEF Entry, _Startup                ; export 'Entry' symbol
            ABSENTRY Entry                      ; for absolute assembly: mark this as application entry point

; Include derivative-specific definitions 
		        INCLUDE 'derivative.inc' 


; Equates Section                                                 

LCD_DAT         EQU   PORTB                     ; LCD data port, bits - PB7,...,PB0
LCD_CNTR        EQU   PTJ                       ; LCD control port, bits - PJ6(RS),PJ7(E)
LCD_E           EQU   $80                       ; LCD E-signal pin
LCD_RS          EQU   $40                       ; LCD RS-signal pin

INC_DIS         EQU   300                       ; Utilized for increment distance
FWD_DIS         EQU   2000                      ; Utilized for forward distance
REV_DIS         EQU   1000                      ; Utilized for reverse distance
UTRN_DIS        EQU   15000                     ; Utilized for u-turn distance
STR_DIS         EQU   1000                      ; Utilized for straight distance
TRN_DIS         EQU   13000                     ; Utilized for turn distance

PRIMARY_PATH_INT     EQU   0                    ; PRIMARY path value
SECONDARY_PATH_INT   EQU   1                    ; Secondary path value

START           EQU   0                         
FWD             EQU   1                       
REV             EQU   2                        
RT_TRN          EQU   3                         
LT_TRN          EQU   4                         
BACK_TRK        EQU   5                         
STANDBY         EQU   6 

PATH_A_INT      EQU   $C0                       ; Threshold Detection for path
PATH_B_INT      EQU   $CA                       ; ""
PATH_C_INT      EQU   $CA                       ; ""
PATH_D_INT      EQU   $CA                       ; ""                                              
PATH_E_INT      EQU   $60                       ; Shift robot to the right If SENSOR_LINE < PATH_E_INT
PATH_F_INT      EQU   $B4                       ; Shift robot to the left If SENSOR_LINE > PATH_F_INT
                        


; Variable Section                                                                         


                ORG   $3000

COUNT1          DC.W  0                         ; Initializing to $0000
COUNT2          DC.W  0                         ; Initializing to $0000'
                
CRNT_STATE      DC.B  6                         ; Current state register

TEN_THOUS       DS.B  1                         ; 10,000 digit
THOUSANDS       DS.B  1                         ; 1,000 digit
HUNDREDS        DS.B  1                         ; 100 digit
TENS            DS.B  1                         ; 10 digit
UNITS           DS.B  1                         ; 1 digit
BCD_SPARE       DS.B  10
NO_BLANK        DS.B  1                         ; Used in ’leading zero’ blanking by BCD2ASC

RETURN          DC.B  0                         ; 0 if False and 1 if True
NEXT_D          DC.B  1                         ; Next instruction regarding direction

DETECTION_A     DC.B  0                         ; Detection for Sensor A (No Path = 0, Path = 1)
DETECTION_B     DC.B  0                         ; Detection for Sensor B (No Path = 0, Path = 1)
DETECTION_C     DC.B  0                         ; Detection for Sensor C (No Path = 0, Path = 1)
DETECTION_D     DC.B  0                         ; Detection for Sensor D (No Path = 0, Path = 1)
DETECTION_E     DC.B  0                         ; Detection for Sensor E (No Path = 0, Path = 1)
DETECTION_F     DC.B  0                         ; Detection for Sensor F (No Path = 0, Path = 1)

SENSOR_LINE     DC.B  $0                        ; Storage for guider sensor readings
SENSOR_BOW      DC.B  $0                        ; 
SENSOR_PORT     DC.B  $0                        ; 
SENSOR_MID      DC.B  $0                        ; 
SENSOR_STBD     DC.B  $0                        ; 
SENSOR_NUM      DS.B  1                         ; Sensor that is currently selected
TEMP            DS.B  1                         ; Temporary location




; Code Section                                                                             


                ORG   $4000                     ; Where the code starts 
Entry:                                          ;                                          
_Startup:                                       ;                                           
                LDS   #$4000                    ; Initialize the stack pointer              
                                                ;                                          
                JSR   initPORTS                 ;                                                                          ;                                           
                JSR   initAD                    ; Initialize ATD converter                                                            
                JSR   initLCD                   ; Initialize the LCD                        
                JSR   clrLCD                    ; Clear LCD & home cursor                                                  ;                                           
                JSR   initTCNT                  ; Initialize the TCNT                       
                                                ;                                           
                CLI                             ; Enable interrupts                         
                                                ;                                           
                LDX   #msg1                     ; Display msg1                              
                JSR   putsLCD                   ; ""                                        
                                                ;                                           
                LDAA  #$8F                      ; Move LCD cursor to the end of msg1        
                JSR   cmd2LCD                   ; ""                                        
                LDX   #msg2                     ; Display msg2                              
                JSR   putsLCD                   ; ""                                        
                                                ;                                           
                LDAA  #$C0                      ; Move LCD cursor to the 2nd row            
                JSR   cmd2LCD                   ; ""                                       
                LDX   #msg3                     ; Display msg3                              
                JSR   putsLCD                   ; ""                                        
                                                ;                                           
                LDAA  #$C7                      ; Move LCD cursor to the end of msg3        
                JSR   cmd2LCD                   ; ""                                        
                LDX   #msg4                     ; Display msg4                              
                JSR   putsLCD                   ; ""                                        
                                                             
                                                
          MAIN: JSR   UPDT_READING                                                        
                JSR   UPDT_DISPL                                                           
                LDAA  CRNT_STATE                                                           
                JSR   DISPATCHER                                                           
                BRA   MAIN                                                                
                                                


; Data Section                                                                             


          msg1: dc.b  "S:",0                    
          msg2: dc.b  "R:",0                    
          msg3: dc.b  "V:",0                    
          msg4: dc.b  "B:",0                    
          
           tab: dc.b  "START  ",0               
                dc.b  "FWD    ",0               
                dc.b  "REV    ",0              
                dc.b  "RT_TRN ",0               
                dc.b  "LT_TRN ",0               
                dc.b  "RETURN ",0               
                dc.b  "STANDBY",0               
                

; Subroutine for Intitialization                                                         


initPORTS       BCLR  DDRAD,$FF                 ; Set PORTAD as input
                BSET  DDRA, $FF                 ; Set PORTA as output
                BSET  DDRT, $30                 ; Set channels 4 & 5 of PORTT as output
                RTS
        
initAD          MOVB  #$C0,ATDCTL2              ; power up AD, select fast flag clear
                JSR   del_50us                  ; wait for 50 us
                MOVB  #$00,ATDCTL3              ; 8 conversions in a sequence
                MOVB  #$85,ATDCTL4              ; res=8, conv-clks=2, prescal=12
                BSET  ATDDIEN,$0C               ; configure pins AN03,AN02 as digital inputs
                RTS   

initLCD         BSET  DDRB,%11111111            ; configure pins PB7,...,PB0 for output
                BSET  DDRJ,%11000000            ; configure pins PJ7(E), PJ6(RS) for output
                LDY   #2000                     ; wait for LCD to be ready
                JSR   del_50us                  ; -"-
                LDAA  #$28                      ; set 4-bit data, 2-line display
                JSR   cmd2LCD                   ; -"-
                LDAA  #$0C                      ; display on, cursor off, blinking off
                JSR   cmd2LCD                   ; -"-
                LDAA  #$06                      ; move cursor right after entering a character
                JSR   cmd2LCD                   ; -"-
                RTS

clrLCD          LDAA  #$01                      ; clear cursor and return to home position
                JSR   cmd2LCD                   ; -"-
                LDY   #40                       ; wait until "clear cursor" command is complete
                JSR   del_50us                  ; -"-
                RTS

initTCNT        MOVB  #$80,TSCR1                ; enable TCNT
                MOVB  #$00,TSCR2                ; disable TCNT OVF interrupt, set prescaler to 1
                MOVB  #$FC,TIOS                 ; channels PT1/IC1,PT0/IC0 are input captures
                MOVB  #$05,TCTL4                ; capture on rising edges of IC1,IC0 signals
                MOVB  #$03,TFLG1                ; clear the C1F,C0F input capture flags
                MOVB  #$03,TIE                  ; enable interrupts for channels IC1,IC0
                RTS


; Subroutine for Utility                                                         


del_50us:   
            PSHX                    ;2 E-clk
eloop:      LDX     #30             ;2 E-clk -
iloop:      PSHA                    ;2 E-clk |
            PULA                    ;3 E-clk |
            PSHA                    ;2 E-clk |
            PULA                    ;3 E-clk | 
            PSHA                    ;2 E-clk | 
            PULA                    ;3 E-clk | 
            PSHA                    ;2 E-clk |                                                                                 
            PULA                    ;3 E-clk |                                                                     
            PSHA                    ;2 E-clk | 
            PULA                    ;3 E-clk |
            PSHA                    ;2 E-clk |
            PULA                    ;3 E-clk |                                    
            PSHA                    ;2 E-clk |  
            PULA                    ;3 E-clk |  
            NOP                     ;1 E-clk |
            NOP                     ;1 E-clk |
            DBNE    X,iloop         ;3 E-clk -
            DBNE    Y,eloop         ;3 E-clk
            PULX                    ;3 E-clk
            RTS                     ;5 E-clk


; This function sends a command in accumulator A to the LCD


cmd2LCD:        BCLR  LCD_CNTR,LCD_RS           ; select the LCD Instruction Register (IR)
                JSR   dataMov                   ; send data to IR
      	        RTS

;This function outputs a NULL-terminated string pointed to by X                        

putsLCD         LDAA  1,X+                      ; get one character from the string
                BEQ   donePS                    ; reach NULL character?
                JSR   putcLCD
                BRA   putsLCD
donePS 	        RTS


;This function outputs the character in accumulator in A to LCD                     


putcLCD         BSET  LCD_CNTR,LCD_RS           ; select the LCD Data register (DR)
                JSR   dataMov                   ; send data to DR
                RTS


; This function sends data to the LCD IR or DR depending on RS                             


dataMov         BSET  LCD_CNTR,LCD_E            ; pull the LCD E-sigal high
                STAA  LCD_DAT                   ; send the upper 4 bits of data to LCD
                BCLR  LCD_CNTR,LCD_E            ; pull the LCD E-signal low to complete the write oper.
                LSLA                            ; match the lower 4 bits with the LCD data pins
                LSLA                            ; -"-
                LSLA                            ; -"-
                LSLA                            ; -"-
                BSET  LCD_CNTR,LCD_E            ; pull the LCD E signal high
                STAA  LCD_DAT                   ; send the lower 4 bits of data to LCD
                BCLR  LCD_CNTR,LCD_E            ; pull the LCD E-signal low to complete the write oper.
                LDY   #1                        ; adding this delay will complete the internal
                JSR   del_50us                  ; operation for most instructions
                RTS
                
;**********************************************************************
;*        Integer to BCD Conversion Routine                           *
;* This routine converts a 16 bit binary number in .D into            *
;* BCD digits in BCD_BUFFER.                                          *
;* Peter Hiscocks                                                     *
;* Algorithm:                                                         *
;* Because the IDIV (Integer Division) instruction is available on    *
;* the HCS12, we can determine the decimal digits by repeatedly       *
;* dividing the binary number by ten: the remainder each time is      *
;* a decimal digit. Conceptually, what we are doing is shifting       *
;* the decimal number one place to the right past the decimal         *
;* point with each divide operation. The remainder must be            *
;* a decimal digit between 0 and 9, because we divided by 10.         *
;* The algorithm terminates when the quotient has become zero.        *
;* Bug note: XGDX does not set any condition codes, so test for       *
;* quotient zero must be done explicitly with CPX.                    *
;**********************************************************************

int2BCD         XGDX                            ; Save the binary number into .X
                LDAA  #0                        ; Clear the BCD_BUFFER
                STAA  TEN_THOUS
                STAA  THOUSANDS
                STAA  HUNDREDS
                STAA  TENS
                STAA  UNITS
                STAA  BCD_SPARE
                STAA  BCD_SPARE+1

                CPX   #0                        ; Check for a zero input
                BEQ   CON_EXIT                  ; and if so, exit

                XGDX                            ; Not zero, get the binary number back to .D as dividend
                LDX   #10                       ; Setup 10 (Decimal!) as the divisor
                IDIV                            ; Divide: Quotient is now in .X, remainder in .D
                STAB  UNITS                     ; Store remainder
                CPX   #0                        ; If quotient is zero,
                BEQ   CON_EXIT                  ; then exit

                XGDX                            ; else swap first quotient back into .D
                LDX   #10                       ; and setup for another divide by 10
                IDIV
                STAB  TENS
                CPX   #0
                BEQ   CON_EXIT

                XGDX                            ; Swap quotient back into .D
                LDX   #10                       ; and setup for another divide by 10
                IDIV
                STAB  HUNDREDS
                CPX   #0
                BEQ   CON_EXIT

                XGDX                            ; Swap quotient back into .D
                LDX   #10                       ; and setup for another divide by 10
                IDIV
                STAB  THOUSANDS
                CPX   #0
                BEQ   CON_EXIT

                XGDX                            ; Swap quotient back into .D
                LDX   #10                       ; and setup for another divide by 10
                IDIV
                STAB  TEN_THOUS

      CON_EXIT: RTS                             ; We’re done the conversion
      
;******************************************************************
;*            BCD to ASCII Conversion Routine                     *
;* This routine converts the BCD number in the BCD_BUFFER         *
;* into ascii format, with leading zero suppression.              *
;* Leading zeros are converted into space characters.             *
;* The flag ’NO_BLANK’ starts cleared and is set once a non-zero  *
;* digit has been detected.                                       *
;* The ’units’ digit is never blanked, even if it and all the     *
;* preceding digits are 0                                         *
;* Peter Hiscocks                                                 *
;****************************************************************** 


BCD2ASC         LDAA  #$0                       ; Initialize the blanking flag
                STAA  NO_BLANK

       C_TTHOU: LDAA  TEN_THOUS                 ; Check the ’ten_thousands’ digit
                ORAA  NO_BLANK
                BNE   NOT_BLANK1

      ISBLANK1: LDAA  #$20                      ; It’s blank
                STAA  TEN_THOUS                 ; so store a space
                BRA   C_THOU                    ; and check the ’thousands’ digit

    NOT_BLANK1: LDAA  TEN_THOUS                 ; Get the ’ten_thousands’ digit
                ORAA  #$30                      ; Convert to ascii
                STAA  TEN_THOUS
                LDAA  #$1                       ; Signal that we have seen a ’non-blank’ digit
                STAA  NO_BLANK

        C_THOU: LDAA  THOUSANDS                 ; Check the thousands digit for blankness
                ORAA  NO_BLANK                  ; If it’s blank and ’no-blank’ is still zero
                BNE   NOT_BLANK2
                     
      ISBLANK2: LDAA  #$30                      ; Thousands digit is blank
                STAA  THOUSANDS                 ; so store a space
                BRA   C_HUNS                    ; and check the hundreds digit

    NOT_BLANK2: LDAA  THOUSANDS                 ; (similar to ’ten_thousands’ case)
                ORAA  #$30
                STAA  THOUSANDS
                LDAA  #$1
                STAA  NO_BLANK

        C_HUNS: LDAA  HUNDREDS                  ; Check the hundreds digit for blankness
                ORAA  NO_BLANK                  ; If it’s blank and ’no-blank’ is still zero
                BNE   NOT_BLANK3

      ISBLANK3: LDAA  #$20                      ; Hundreds digit is blank
                STAA  HUNDREDS                  ; so store a space
                BRA   C_TENS                    ; and check the tens digit
                     
    NOT_BLANK3: LDAA  HUNDREDS                  ; (similar to ’ten_thousands’ case)
                ORAA  #$30
                STAA  HUNDREDS
                LDAA  #$1
                STAA  NO_BLANK

        C_TENS: LDAA  TENS                      ; Check the tens digit for blankness
                ORAA  NO_BLANK                  ; If it’s blank and ’no-blank’ is still zero
                BNE   NOT_BLANK4
                     
      ISBLANK4: LDAA  #$20                      ; Tens digit is blank
                STAA  TENS                      ; so store a space
                BRA   C_UNITS                   ; and check the units digit

    NOT_BLANK4: LDAA  TENS                      ; (similar to ’ten_thousands’ case)
                ORAA  #$30
                STAA  TENS

       C_UNITS: LDAA  UNITS                     ; No blank check necessary, convert to ascii.
                ORAA  #$30
                STAA  UNITS

                RTS                             ; We’re done
                

; Motor Control Subroutine 
          
                                                                
; Starboard commands --> right motor                         


; Right Motor ON                                                               
STARON          BSET  PTT,%00100000
                RTS


; Right Motor OFF                                                              
STAROFF         BCLR  PTT,%00100000
                RTS


; Right Motor FWD                                                              
STARFWD         BCLR  PORTA,%00000010
                RTS


; Right Motor REV                                                              
STARREV         BSET  PORTA,%00000010
                RTS

; Port commands --> left motor                


; Left Motor ON                                                                     
PORTON          BSET  PTT,%00010000
                RTS


; Left Motor OFF                                                                    
PORTOFF         BCLR  PTT,%00010000
                RTS


; Left Motor FWD                                                                   
PORTFWD         BCLR  PORTA,%00000001
                RTS


; Left Motor REV                                                                    
PORTREV         BSET  PORTA,%00000001
                RTS
                
           

; States Section                                                                         


DISPATCHER      CMPA  #START                    ; If it’s the START state ------------------+
                BNE   NOT_START                 ;                                           |
                JSR   START_ST                  ; then call START_ST routine                D
                RTS                             ; and exit                                  I
                                                ;                                           S
NOT_START       CMPA  #FWD                      ; Else if it’s the FORWARD state            P
                BNE   NOT_FORWARD               ;                                           A
                JMP   FWD_ST                    ; then call the FWD_ST routine              T
                                                ;                                           C
NOT_FORWARD     CMPA  #RT_TRN                   ; Else if it’s the RIGHT_TURN state         H
                BNE   NOT_RT_TRN                ;                                           E
                JSR   RT_TRN_ST                 ; then call the RT_TRN_ST routine           R
                RTS                             ; and exit                                  |
                                                ;                                           |
NOT_REVERSE     CMPA  #BACK_TRK                 ; Else if it’s the BACKTRACK state          |
                BNE   NOT_BACK_TRK              ;                                           |
                JMP   BACK_TRK_ST               ; then call the BK_TRK_ST routine           |
                                                                                            
NOT_LT_TRN      CMPA  #REV                      ; Else if it’s the REVERSE state            |
                BNE   NOT_REVERSE               ;                                           |
                JSR   REV_ST                    ; then call the REV_ST routine              |
                RTS                             ; and exit                                                                                              
NOT_RT_TRN      CMPA  #LT_TRN                   ; Else if it’s the LEFT_TURN state          |
                BNE   NOT_LT_TRN                ;                                           |
                JSR   LT_TRN_ST                 ; then call LT_TRN_ST routine               |
                RTS                             ; and exit                                  |
                                                ;                                           |   
                                                ;                                           |
                                                ;                                           |
NOT_BACK_TRK    CMPA  #STANDBY                  ; Else if it’s the STANDBY state            |
                BNE   NOT_STANDBY               ;                                           |
                JSR   STANDBY_ST                ; then call the SBY_ST routine              |
                RTS                             ; and exit                                  |
                                                ;                                           |
NOT_STANDBY     NOP                             ; Else the CRNT_STATE is not defined, so    |
DISP_EXIT       RTS                             ; Exit from the state dispatcher -----------+



START_ST        BRCLR PORTAD0,$04,NO_FWD        ; If "NOT" FWD_BUMP
                JSR   INIT_FWD                  ; Initialize the FORWARD state and
                MOVB  #FWD,CRNT_STATE           ; Set CRNT_STATE to FWD and exit
                BRA   START_EXIT                
                                                
NO_FWD          NOP                              
START_EXIT      RTS                           	; Else return to the MAIN routine

FWD_ST          PULD                            
                BRSET PORTAD0,$04,NO_FWD_BUMP   ; If FWD_BUMP then
                LDAA  SECONDARY_PATH_INT        ; Correct NEXT_D value
                STAA  NEXT_D                    
                JSR   INIT_REV                  ; Initialize the REVERSE routine
                MOVB  #REV,CRNT_STATE           ; Set CRNT_STATE to REV
                JMP   FWD_EXIT                  ; and return
                
NO_REV_BUMP     LDAA  DETECTION_D               ; Else if DETECTION_D = 1 then
                BEQ   NO_RT_INTXN               ; Robot make a RIGHT turn
                LDAA  NEXT_D                    ; Push direction for the previous 
                PSHA                            ; Intersection to the stack
                LDAA  PRIMARY_PATH_INT          ; Then store direction taken to NEXT_D
                STAA  NEXT_D                    
                JSR   INIT_RT_TRN               ; Initialize the RT_TRN state
                MOVB  #RT_TRN,CRNT_STATE        ; Set CRNT_STATE to RT_TRN
                JMP   FWD_EXIT                  ; Then exit
              
NO_FWD_BUMP     BRSET PORTAD0,$08,NO_REV_BUMP   ; Else if REV_BUMP, then 
                JMP   INIT_BACK_TRK             ; Initialize the BACKTRACK state
                MOVB  #BACK_TRK,CRNT_STATE      ; Set CRNT_STATE to BACK_TRK
                JMP   FWD_EXIT                  ; and return

NO_LT_INTXN     LDAA  DETECTION_F               ; Else if DETECTION_F = 1
                BEQ   NO_SHFT_RT                ; Robot shifts RIGHT
                JSR   PORTON                    ; and turn on the LEFT motor

NO_RT_INTXN     LDAA  DETECTION_B               ; Else if DETECTION_B = 1
                BEQ   NO_LT_INTXN               ; Check if DETECTION_A = 1
                LDAA  DETECTION_A               ; If DETECTION_A = 1 a FORWARD path exists
                BEQ   LT_TURN                   ; Robot continues forward
                LDAA  NEXT_D                    ; Push direction for the previous
                PSHA                            ; Intersection to the stack
                LDAA  PRIMARY_PATH_INT          ; Then store direction taken to NEXT_D
                STAA  NEXT_D                    ; ""
                BRA   NO_SHFT_LT                ; Else if DETECTION_A = 0
LT_TURN         LDAA  NEXT_D                    ; Push direction for the previous
                PSHA                            ; Intersection to the stack
                LDAA  SECONDARY_PATH_INT        ; Then store direction taken to NEXT_D
                STAA  NEXT_D                    ; ""
                JSR   INIT_LT_TRN               ; Robot takes a LEFT turn
                MOVB  #LT_TRN,CRNT_STATE        ; Initialize the LT_TRN state
                JMP   FWD_EXIT                  ; Set CRNT_STATE to LT_TRN and exit

NO_SHFT_RT      LDAA  DETECTION_E               ; Else if DETECTION_E = 1
                BEQ   NO_SHFT_LT                ; Robot shift LEFT
                JSR   STARON                    ; and turn on the RIGHT motor                
  
FWD_EXIT        JMP   MAIN                      ; return to the MAIN routine

REV_ST          LDD   COUNT1                    ; If Dc>Drev then
                CPD   #REV_DIS                  ; Robot make a U TURN
                BLO   REV_ST                    ; so
                JSR   STARFWD                   ; Set STBD Motor to FWD direction
                LDD   #0                        ; Reset timer
                STD   COUNT1                    
                
NO_SHFT_LT      JSR   STARON                    ; Turn motors on
                JSR   PORTON                                                    
                
REV_U_TRN       LDD   COUNT1                    ; If Dc>Dutrn then
                CPD   #UTRN_DIS                 ; Robot stop
                BLO   REV_U_TRN                 ; so
                JSR   INIT_FWD                  ; Initialize the FWD state
                LDAA  RETURN                    ; If RETURN = 1 
                BNE   BACK_TRK_REV              
                MOVB  #FWD,CRNT_STATE           ; Then set state to FWD
                BRA   REV_EXIT                  ; and exit
BACK_TRK_REV    JSR   INIT_FWD                  
                MOVB  #BACK_TRK,CRNT_STATE      ; Else set CRNT_STATE to BACK_TRK
               
REV_EXIT        RTS                             ; return to the MAIN routine

RT_TRN_ST       LDD   COUNT2                    ; If Dc>Dfwd then
                CPD   #STR_DIS                  ; Robot make a TURN
                BLO   RT_TRN_ST                 ; so
                JSR   STAROFF                   ; Set STBD Motor to OFF
                LDD   #0                        ; Reset timer
                STD   COUNT2                    
                
RT_TURN_LOOP    LDD   COUNT2                    ; If Dc>Dfwdturn then
                CPD   #TRN_DIS                  ; Robot stop
                BLO   RT_TURN_LOOP              ; so
                JSR   INIT_FWD                  ; Initialize the FWD state
                LDAA  RETURN                    ; If RETURN = 1 
                BNE   BACK_TRK_RT_TRN             
                MOVB  #FWD,CRNT_STATE           ; Then set state to FWD and exit
                BRA   RT_TRN_EXIT               ; 
BACK_TRK_RT_TRN MOVB  #BACK_TRK,CRNT_STATE      ; Else set state to BACK_TRK
            
RT_TRN_EXIT     RTS                             ; return to the MAIN routine
                
LT_TURN_LOOP    LDD   COUNT1                    ; If Dc>Dfwdturn then
                CPD   #TRN_DIS                  ; Robot will stop
                BLO   LT_TURN_LOOP              
                JSR   INIT_FWD                  ; Initialize the FWD state
                LDAA  RETURN                    ; If RETURN = 1 
                BNE   BACK_TRK_LT_TRN             
                MOVB  #FWD,CRNT_STATE           ; Then set state to FWD and exit
                BRA   LT_TRN_EXIT                
BACK_TRK_LT_TRN MOVB  #BACK_TRK,CRNT_STATE      ; Else set state to BACK_TRK

LT_TRN_ST       LDD   COUNT1                    ; If Dc>Dfwd then
                CPD   #STR_DIS                  ; Robot will take a TURN
                BLO   LT_TRN_ST                  
                JSR   PORTOFF                   ; Set PORT Motor to OFF
                LDD   #0                        ; Reset timer
                STD   COUNT1                    
                
LT_TRN_EXIT     RTS                             ; return to the MAIN routine

NO_BACK_BUMP    LDAA  NEXT_D                    ; If NEXT_D = 0
                BEQ   REG_PATHING               ; Use regular pathing mode
                BNE   IRREG_PATHING             ; Else use irregular pathing mode

BACK_TRK_ST     PULD                            
                BRSET PORTAD0,$08,NO_BACK_BUMP  ; If REV_BUMP, then we should stop
                JSR   INIT_STANDBY              ; Initialize the STANDBY state
                MOVB  #STANDBY,CRNT_STATE       ; set the state to STANDBY and exit
                JMP   BACK_TRK_EXIT             

IRREG_PATHING   LDAA  DETECTION_B               ; If DETECTION_B = 1
                BEQ   NO_LT_TRN                 ; Robot will take a LEFT turn
                PULA                            ; Grab the next direction value from the stack
                STAA  NEXT_D                    ; and store it in NEXT_D
                JSR   INIT_LT_TRN               ; Initialize the LT_TRN state
                MOVB  #LT_TRN,CRNT_STATE        ; Set CRNT_STATE to LT_TRN and exit
                JMP   BACK_TRK_EXIT              

NO_LT_TRN       LDAA  DETECTION_D               ; If DETECTION_D = 1
                BEQ   RT_LINE_S                 ; Check if DETECTION_A = 1
                LDAA  DETECTION_A               ; If DETECTION_A = 1 a FORWARD path exists and robot continues forward
                BEQ   RIGHT_TURN                
                PULA                            ; Pull the next direction value from the stack
                STAA  NEXT_D                    ; and store it in NEXT_D
                BRA   NO_LINE_S                 ; Else if DETECTION_A = 0
RIGHT_TURN      PULA                            ; Robot will take a RIGHT turn
                STAA  NEXT_D                    ; Grab the next direction value from the stack
                JSR   INIT_RT_TRN               ; Initialize the RT_TRN state
                MOVB  #RT_TRN,CRNT_STATE        ; Set CRNT_STATE to RT_TRN then exit
                JMP   BACK_TRK_EXIT              

REG_PATHING     LDAA  DETECTION_D               ; If DETECTION_D = 1
                BEQ   NO_RT_TRN                 ; Robot will take a RIGHT turn
                PULA                            ; Grab the next direction value from the stack
                PULA                            ; and store it in NEXT_D
                STAA  NEXT_D                    
                JSR   INIT_RT_TRN               ; Initialize the RT_TRN state
                MOVB  #RT_TRN,CRNT_STATE        ; Set CRNT_STATE to RT_TRN then exit
                JMP   BACK_TRK_EXIT              

NO_RT_TRN       LDAA  DETECTION_B               ; If DETECTION_B = 1
                BEQ   RT_LINE_S                 ; Check if DETECTION_A = 1
                LDAA  DETECTION_A               ; If DETECTION_A = 1 a FORWARD path exists
                BEQ   LEFT_TURN                 ; Robot proceeds forward
                PULA                            ; Grab the next direction value from the stack
                PULA                            ; and store it in NEXT_D
                STAA  NEXT_D                   
                BRA   NO_LINE_S                 ; Else if DETECTION_A = 0
LEFT_TURN       PULA                            ; Robot will take a LEFT turn
                PULA                            ; Grab the next direction value from the stack
                STAA  NEXT_D                    ; and store it in NEXT_D
                JSR   INIT_LT_TRN               ; Initialize the LT_TRN state
                MOVB  #LT_TRN,CRNT_STATE        ; Set CRNT_STATE to LT_TRN and exit
                JMP   BACK_TRK_EXIT             ; 

RT_LINE_S       LDAA  DETECTION_F               ; Else if DETECTION_F = 1 the robot will shift right and the left motor turns on
                BEQ   LT_LINE_S                  
                JSR   PORTON                                             
                
NO_LINE_S       JSR   STARON                    ; Turn motors on
                JSR   PORTON                                       

LT_LINE_S       LDAA  DETECTION_E               ; Else if DETECTION_F = 1 Robot shift RIGHT
                BEQ   NO_LINE_S                 ; and turn on the LEFT motor
                JSR   STARON                                                
                
BACK_TRK_EXIT   JMP   MAIN                      ; return to the MAIN routine

STANDBY_ST      BRSET PORTAD0,$04,NO_START      ; If FWD_BUMP
                BCLR  PTT,%00110000             ; Initialize the START state
                MOVB  #START,CRNT_STATE         ; Set CRNT_STATE to START
                BRA   STANDBY_EXIT              ; Then exit
                                                
NO_START        NOP                             ; Else
STANDBY_EXIT    RTS                             ; return to the MAIN routine



; Initializing State


INIT_FWD        BCLR  PTT,%00110000             ; Turn OFF the drive motors
                LDD   #0                        ; Reset timer
                STD   COUNT1                    
                STD   COUNT2                    
                BCLR  PORTA,%00000011           ; Set FWD direction for both motors
                RTS

INIT_REV        BSET  PORTA,%00000011           ; Set REV direction for both motors
                LDD   #0                        ; Reset timer
                STD   COUNT1                    ; ""
                BSET  PTT,%00110000             ; Turn ON the drive motors
                RTS                             

INIT_LT_TRN     BCLR  PORTA,%00000011           ; Set FWD direction for both motors
                LDD   #0                        ; Reset timer
                STD   COUNT1                    ; ""
                BSET  PTT,%00110000             ; Turn ON the drive motors
                RTS

INIT_RT_TRN     BCLR  PORTA,%00000011           ; Set FWD direction for both motors
                LDD   #0                        ; Reset timer
                STD   COUNT2                    ; ""
                BSET  PTT,%00110000             ; Turn ON the drive motors
                RTS

INIT_BACK_TRK   INC   RETURN                    ; Change RETURN value to 1
                PULA                            ; Pull the next direction value from the stack
                STAA  NEXT_D                    ; and store it in NEXT_D
                JSR   INIT_REV                  ; Initialize the REVERSE routine
                JSR   REV_ST                    ; Jump to REV_ST
                JMP   MAIN

INIT_STANDBY    BCLR  PTT,%00110000             ; Turn off the drive motors
                RTS
                


; Subroutine for Sensor                                                          


UPDT_READING    JSR   G_LEDS_ON                 ; Turn ON LEDS
                JSR   READ_SENSORS              ; Take readings from sensors
                JSR   G_LEDS_OFF                ; Turn OFF LEDS
                
                LDAA  #0                        ; Setting sensor A detection value to 0
                STAA  DETECTION_A                
                STAA  DETECTION_B                
                STAA  DETECTION_C                
                STAA  DETECTION_D                
                STAA  DETECTION_E                
                STAA  DETECTION_F                
                
CHECK_A         LDAA  SENSOR_BOW                ; If SENSOR_BOW is larger than
                CMPA  #PATH_A_INT               ; PATH_A_INT
                BLO   CHECK_B                   
                INC   DETECTION_A               ; Setting DETECTION_A = 1

CHECK_B         LDAA  SENSOR_PORT               ; If SENSOR_PORT is larger than
                CMPA  #PATH_B_INT               ; PATH_B_INT
                BLO   CHECK_C                   
                INC   DETECTION_B               ; Setting DETECTION_B = 1

CHECK_C         LDAA  SENSOR_MID                ; If SENSOR_MID is larger than
                CMPA  #PATH_C_INT               ; PATH_C_INT
                BLO   CHECK_D                   
                INC   DETECTION_C               ; Setting DETECTION_C = 1
                
CHECK_D         LDAA  SENSOR_STBD               ; If SENSOR_STBD is larger than
                CMPA  #PATH_D_INT               ; PATH_D_INT
                BLO   CHECK_E                   
                INC   DETECTION_D               ; Setting DETECTION_D = 1

CHECK_E         LDAA  SENSOR_LINE               ; If SENSOR_LINE is lower than
                CMPA  #PATH_E_INT               ; PATH_E_INT
                BHI   CHECK_F                   
                INC   DETECTION_E               ; Setting DETECTION_E = 1
                
CHECK_F         LDAA  SENSOR_LINE               ; If SENSOR_LINE is larger than
                CMPA  #PATH_F_INT               ; PATH_F_INT
                BLO   UPDT_DONE                 
                INC   DETECTION_F               ; Setting DETECTION_F = 1
                
UPDT_DONE       RTS

G_LEDS_ON       BSET  PORTA,%00100000            
                RTS

G_LEDS_OFF      BCLR  PORTA,%00100000           
                RTS

READ_SENSORS    CLR   SENSOR_NUM                ; Select sensor number 0
                LDX   #SENSOR_LINE              ; Point at the start of the sensor array
  RS_MAIN_LOOP: LDAA  SENSOR_NUM                ; Select the correct sensor input
                JSR   SELECT_SENSOR             ; on the hardware
                LDY   #400                      ; 20 ms delay to allow the
                JSR   del_50us                  ; sensor to stabilize
                LDAA  #%10000001                ; Start A/D conversion on AN1
                STAA  ATDCTL5
                BRCLR ATDSTAT0,$80,*            ; Repeat until A/D signals done
                LDAA  ATDDR0L                   ; A/D conversion is complete in ATDDR0L
                STAA  0,X                       ; so copy it to the sensor register
                CPX   #SENSOR_STBD              ; If this is the last reading
                BEQ   RS_EXIT                   ; Then exit
                INC   SENSOR_NUM                ; Else, increment the sensor number
                INX                             ; and the pointer into the sensor array
                BRA   RS_MAIN_LOOP              ; and do it again
       RS_EXIT: RTS

SELECT_SENSOR   PSHA                            ; 
                LDAA  PORTA                     ; Clear the sensor selection bits to zeros
                ANDA  #%11100011                
                STAA  TEMP                      ; Storing within TEMP
                PULA                            ; Retrieving sensor number
                ASLA                            ; Selection number shifted twice to the left
                ASLA
                ANDA  #%00011100                ; Will clear irrelevant bit positions
                ORAA  TEMP                      ; OR it into the sensor bit positions and update hardware
                STAA  PORTA                      
                RTS
                

; Update Display (Current State + Bumper Switches + Battery Voltage + Sensor Readings) 


UPDT_DISPL      LDAA  #$82                      ; Move LCD cursor to the end of msg1
                JSR   cmd2LCD                   
                
                LDAB  CRNT_STATE                ; Display current state
                LSLB                           
                LSLB                          
                LSLB                            
                LDX   #tab                      
                ABX                            
                JSR   putsLCD                   
             
                LDAA  #$8F                      ; Move LCD cursor to the end of msg2
                JSR   cmd2LCD                   
                LDAA  SENSOR_BOW                ; Convert value from SENSOR_BOW to a
                JSR   BIN2ASC                   ; Two digit hexidecimal value
                JSR   putcLCD                   
                EXG   A,B                       
                JSR   putcLCD                   

                LDAA  #$92                      ; Move LCD cursor to Line position 
                JSR   cmd2LCD                   
                LDAA  SENSOR_LINE               ; Convert value from SENSOR_BOW to a
                JSR   BIN2ASC                   ; Two digit hexidecimal value
                JSR   putcLCD                   
                EXG   A,B                        
                JSR   putcLCD                    

                LDAA  #$CC                      ; Move LCD cursor to Port position on 2nd row 
                JSR   cmd2LCD                    
                LDAA  SENSOR_PORT               ; Convert value from SENSOR_BOW to a
                JSR   BIN2ASC                   ; Two digit hexidecimal value
                JSR   putcLCD                    
                EXG   A,B                       
                JSR   putcLCD                   

                LDAA  #$CF                      ; Move LCD cursor to Mid position on 2nd row 
                JSR   cmd2LCD                    
                LDAA  SENSOR_MID                ; Convert value from SENSOR_BOW to a
                JSR   BIN2ASC                   ; Two digit hexidecimal value
                JSR   putcLCD                   
                EXG   A,B                       
                JSR   putcLCD                   

                LDAA  #$D2                      ; Move LCD cursor to Starboard position on 2nd row 
                JSR   cmd2LCD                    
                LDAA  SENSOR_STBD               ; Convert value from SENSOR_BOW to a
                JSR   BIN2ASC                   ; Two digit hexidecimal value
                JSR   putcLCD                    
                EXG   A,B                        
                JSR   putcLCD                    
           
                MOVB  #$90,ATDCTL5              ; R-just., uns., sing. conv., mult., ch=0, start
                BRCLR ATDSTAT0,$80,*            ; Wait until the conver. seq. is complete
                LDAA  ATDDR0L                   ; Load the ch0 result - battery volt - into A
                LDAB  #39                       ; AccB = 39
                MUL                             ; AccD = 1st result x 39
                ADDD  #600                      ; AccD = 1st result x 39 + 600
                JSR   int2BCD
                JSR   BCD2ASC
                LDAA  #$C2                      ; move LCD cursor to the end of msg3
                JSR   cmd2LCD                                   
                LDAA  TEN_THOUS                 ; output the TEN_THOUS ASCII character
                JSR   putcLCD                   
                LDAA  THOUSANDS                 ; output the THOUSANDS ASCII character
                JSR   putcLCD                   
                LDAA  #$2E                      ; output the HUNDREDS ASCII character
                JSR   putcLCD                   
                LDAA  HUNDREDS                  ; output the HUNDREDS ASCII character
                JSR   putcLCD                              

                LDAA  #$C9                      ; Move LCD cursor to the end of msg4
                JSR   cmd2LCD
                
                BRCLR PORTAD0,#%00000100,bowON  ; If FWD_BUMP, then
                LDAA  #$20                      
                JSR   putcLCD                   
                BRA   stern_bump                ; Display 'B' on LCD
         bowON: LDAA  #$42                      
                JSR   putcLCD                   
          
    stern_bump: BRCLR PORTAD0,#%00001000,sternON; If REV_BUMP, then
                LDAA  #$20                      
                JSR   putcLCD                   
                BRA   UPDT_DISPL_EXIT           ; Display 'S' on LCD
       sternON: LDAA  #$53                      
                JSR   putcLCD                   
UPDT_DISPL_EXIT RTS                             ; and exit

;*********************************************************************************************
; Binary to ASCII                                                                            *
; Converts an 8 bit binary value in ACCA to the equivalent ASCII character 2                 *
; character string in accumulator D                                                          *
; Uses a table-driven method rather than various tricks.                                     *
; Passed: Binary value in ACCA                                                               *
; Returns: ASCII Character string in D                                                       *
; Side Fx: ACCB is destroyed                                                                 *
;*********************************************************************************************

HEX_TABLE       FCC '0123456789ABCDEF'          ; Table for converting values
BIN2ASC         PSHA                            ; Save a copy of the input number on the stack
                TAB                             ; and copy it into ACCB
                ANDB #%00001111                 ; Strip off the upper nibble of ACCB
                CLRA                            ; D now contains 000n where n is the LSnibble
                ADDD #HEX_TABLE                 ; Set up for indexed load
                XGDX                
                LDAA 0,X                        ; Get the Least Significant nibble character
                PULB                            ; Retrieve the input number into ACCB
                PSHA                            ; and push the Least Significant nibble character in its place
                RORB                            ; Move the upper nibble of the input number
                RORB                            ; into the lower nibble position.
                RORB
                RORB 
                ANDB #%00001111                 ; Strip off the upper nibble
                CLRA                            ; D now contains 000n where n is the Most Significant nibble 
                ADDD #HEX_TABLE                 ; Set up for indexed load
                XGDX                                                               
                LDAA 0,X                        ; Retrieve the Most Signficant nibble character into ACCA
                PULB                            ; Retrieve the Least Significant nibble character into ACCB
                RTS
                                
ISR1            MOVB  #$01,TFLG1                ; clear the C0F input capture flag
                INC   COUNT1                    ; increment COUNT1
                RTI

ISR2            MOVB  #$02,TFLG1                ; clear the C1F input capture flag
                INC   COUNT2                    ; increment COUNT2 
                RTI

; Interrupt Vectors                                                                      

                ORG   $FFFE
                DC.W  Entry                     ; Reset Vector

                ORG   $FFEE
                DC.W  ISR1                      ; COUNT1 INT

                ORG   $FFEC
                DC.W  ISR2                      ; COUNT2 INT

