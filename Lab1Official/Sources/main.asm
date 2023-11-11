********************************************************************
* Demonstration Program *
* *
* This program illustrates how to use the assembler. *
* It adds together two 8 bit numbers and leaves the result *
* in the ‘SUM’ location. *
* Author: Peter Hiscocks *
********************************************************************
; export symbols
            XDEF Entry, _Startup ; export ‘Entry’ symbol
            ABSENTRY Entry       ; for absolute assembly: mark
                                 ; this as applicat. entry point
; Include derivative-specific definitions
            INCLUDE 'derivative.inc'
********************************************************************
* Code section *
********************************************************************
           ORG $3000

FIRSTNUM   FCB 05 ; First Number to be multiplied (multiplicand)
SECNUM     FCB 03 ; Second Number to be multiplied (multiplier)
PRODUCT    RMB 2 ; Result of multiplication
********************************************************************
* The actual program starts here *
********************************************************************
           ORG $4000
Entry:
_Startup:

          LDAA FIRSTNUM ; Loading first number into Accumulator A Register
          lDAB SECNUM ;   Loading second number into Accumulator B Register
          MUL         ; Multiply A * B together
          STD PRODUCT ; and store the product into Accumulator D register
          SWI ; break to the monitor
********************************************************************
* Interrupt Vectors *
********************************************************************
          ORG $FFFE
          FDB Entry ; Reset Vector    