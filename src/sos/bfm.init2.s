; Update bfm.init2.s
; - This is not called any more, we just include padding to keep the kernel the
;   same size. This was the copy protection decode routine
;
; - Use this free space to add support to print loaded drivers if the 'ctrl' key is held down on boot.
;   Will pause the boot while the key is pressed, and continue once you release it.
;   I've had this idea for a while, just not sure on how to implement it. I noticed when looking
;   through the desktopmanager code, its driver uses an event to run after they are loaded to do its init.
;   I wrote a driver using this method to print them, but then realised i can put it in here, so don't
;   need the event stuff.
;   Prints:
;    - which drivers are loaded
;    - prints the loaded address and bank for debugging
;   (this area will get overwritten after the interp is started)
;
; Updates by Robert Justice
; 

;SBTL "SOS 1.1  BFM.INIT2"
;.RELOC
             .SEGMENT  "CODE"
             .INCLUDE  "SOSORG"
             .ORG      ORGBFMI
ZZORG:
;MSB OFF
;***************************************************************************************************
;          COPYRIGHT (C) APPLE COMPUTER INC.  1980
;                    ALL RIGHTS RESERVED
;***************************************************************************************************
;
; BLOCK FILE MANAGER INIT2
;
;  SECONDARY INITIALIZATION ROUTINE FOR BLOCK FILE MANAGER
;
; MODIFIED: 03/25/81 TO UTILIZE NEW
;   DISK DRIVER'S SEEKDSK3 ROUTINE.
;  CHANGES MARKED BY 'D3RRA81084'
;
; MODIFIED: 08/19/81 TO WORK WITH NEW               
;   SOSLDR MODULE.
;***************************************************************************************************
;

             .EXPORT PRTDRIV

             .IMPORT QUEEVENT
             .IMPORT MAX_DNUM
             .IMPORT SDT_DIBL
             .IMPORT SDT_DIBH
             .IMPORT SDT_ADRL
             .IMPORT SDT_ADRH
             .IMPORT SDT_BANK
             .IMPORT SDT_UNIT

;
; SOS EQUATES
;

ENVREG       = $FFDF          ; ENVIRONMENT REGISTER
BANK_REG     = $FFEF          ; BANK REGISTER
KEYBD        = $C008          ; KEYBOARD B REG
EXTPG        = $1601          ; EXTENDED BANK ADDRESS OFFSET (we are in the interpreter environment)
SYSERR       = $1928          ; REPORT ERROR TO SYSTEM

E_IFR        = $FFED          ; VIA E INTERRUPT FLAG REGISTER
E_IER        = $FFEE          ; VIA E INTERRUPT ENABLE REGISTER


POINTER      = $D0
TEMPY        = $D2

;
; SOS ERROR CODES
;
XDNFERR      = $10            ; DEVICE NOT FOUND
XIOERROR     = $27            ; I/O ERROR
;
; SOS CALLS
;
SOS_OPEN     = $C8
SOS_WRITE    = $CB
SOS_CLOSE    = $CC

;
; LOCAL STORAGE LOCATIONS
;
DEVICE:      .BYTE   0
NAMELEN:     .BYTE   0

; OPEN CONSOLE PARAM LIST
OPENCON:     .BYTE   4
             .WORD   NAME
CONSREF:     .BYTE   0
             .WORD   0
             .BYTE   0

NAME:        .BYTE   8
             .BYTE   ".CONSOLE"

; INIT CONSOLE PARAM LIST
INITCON:     .BYTE   3
INITREF:     .BYTE   0
             .WORD   INITSCR
             .WORD   ENDINIT-INITSCR

INITSCR:     .BYTE   16                ; SET TEXT MODE
             .BYTE   3                 ; 80X24
             .BYTE   28                ; CLEAR VIEWPORT 
             .BYTE   24                ; SET CURSOR XPOS
             .BYTE   1                 ; COLUMN 4
             .BYTE   25                ; SET CURSOR YPOS
             .BYTE   1                 ; ROW 1
             .BYTE   " DRIVERNAME     ACTIVE SLOT  UNIT  TYPE  SUBT  BLKS  MFGR  VERS  ADDR  BANK"
ENDINIT      =       *

; WRITE CONSOLE PARAM LIST
WRITECON:    .BYTE   3
WRITEREF:    .BYTE   0
             .WORD   WRITEBUF
REQCOUNT:    .WORD   0

WRITEBUF:    .BYTE   24                ; SET CURSOR XPOS
             .BYTE   1                 ; COLUMN 1
             .BYTE   25                ; SET CURSOR YPOS
ROW:         .BYTE   3                 ; ROW 3
LINE:        .RES    75,$A0

; CLEAR CONSOLE PARAM LIST
CLEARCON:    .BYTE   3
CLEARREF:    .BYTE   0
             .WORD   CLRSCR
             .WORD   1

CLRSCR:      .BYTE   28                ; CLEAR VIEWPORT 

; CLOSE CONSOLE PARAM LIST
CLOSECON:    .BYTE   1
CLOSEREF:    .BYTE   0

;
; LOOK FOR 'CTRL' KEY DOWN, THEN PRINT ALL DRIVERS
;
PRTDRIV:     LDA     ENVREG             ;TURN ON CXXX I/O
             PHA                        ;SAVE CURRENT STATE
             ORA     #$40 
             STA     ENVREG

             LDA     KEYBD              ;CHECK IF CTRL KEY IS DOWN
             AND     #04
             BEQ     PRINT              ;YES, THEN LETS PRINT THEM
             PLA                        ;NO, JUST RETURN AND DO NOTHING
             STA     ENVREG             ;RESTORE ENVIRONMENT
             RTS

;
; PRINT THE LOADED DRIVERS
;
PRINT:       PLA                        ;RESTORE ENVIRONMENT
             STA     ENVREG
             BRK                        ;OPEN CONSOLE
             .BYTE   SOS_OPEN
             .WORD   OPENCON
             BEQ     @OK1     
             LDA     #XDNFERR           ;DEVICE NOT FOUND
             JSR     SYSERR

@OK1:        LDA     CONSREF            ;UPDATE REFERENCE NUMBERS
             STA     INITREF
             STA     WRITEREF
             STA     CLEARREF
             STA     CLOSEREF
             BRK                        ;INIT CONSOLE
             .BYTE   SOS_WRITE
             .WORD   INITCON
             BEQ     @OK2
             LDA     #XIOERROR          ;I/O ERROR
             JSR     SYSERR

; LETS RUN THROUGH EACH DEVICE DRIVER
@OK2:        LDA     BANK_REG           ;SAVE CURRENT BANK
             PHA
             LDA     #0                 ;DISABLE EXTENDED ADDRESSING
             STA     POINTER+EXTPG
             LDX     #1                 ;START AT DEV 1
             STX     DEVICE

LOOP:        LDA     SDT_DIBL,X         ;GET POINTER TO DIB FROM SOS DEV TABLE
             STA     POINTER
             LDA     SDT_DIBH,X
             STA     POINTER+1
             LDA     SDT_BANK,X
             STA     BANK_REG
; GRAB THE NAME OF THE DRIVER
             LDY     #0
             LDA     (POINTER),Y        ;GET NAME LENGTH FROM DIB
             STA     NAMELEN
             TAX
@L1:         INY
             LDA     (POINTER),Y        ;GET THE NAME
             STA     LINE,Y             ;STORE NAME IN CONSOLE WRITE BUFFER
             DEX
             BNE     @L1

             LDA     #16                ;ALLOW 16 CHARS FOR NAME
             CLC
             SBC     NAMELEN            ;SUBTRACT THE ACTUAL NAME LENGTH
             TAX
             LDA     #$A0               ;SO WE CAN PAD THE REST OF THE 16CHARS WITH SPACES
@L2:         INY
             STA     LINE,Y
             DEX
             BPL     @L2

; GRAB THE OTHER INFO FROM THE DIB AND PRINT AS HEX BYTES
             LDX     #17                ;START IN LINE BUFFER FOR THIS
             LDY     #16                ;DIB OFFSET FOR 'ACTIVE FLAG' (AFTER NAME)
             JSR     NEXTBYTE           ;GET ACTIVE FLAG
             JSR     NEXTBYTE           ;GET SLOT
             JSR     NEXTBYTE           ;GET UNIT
             JSR     NEXTBYTE           ;GET TYPE
             JSR     NEXTBYTE           ;GET SUBT
             INY                        ;SKIP FILLER BYTE
             JSR     NEXTWORD           ;GET BLKS
             JSR     NEXTWORD           ;GET MFGR
             JSR     NEXTWORD           ;GET VERS

; GRAB THE ADDRESS OF THE DRIVER AND BANK (USEFUL FOR DEBUGGING)
             LDY     DEVICE
             LDA     SDT_ADRL,Y         ;GET DRIVER ADDRESS LSB FROM SOS DEV TABLE
             CLC
             ADC     #1                 ;ADDRESS IS STORED -1, ADD 1
             PHA                        ;SAVE
             LDA     SDT_ADRH,Y         ;GET DRIVER ADDRESS MSB FROM SOS DEV TABLE
             ADC     #0                 ;ADD CARRY IF NEEDED
             JSR     PRBYTE             ;OUTPUT MSB AS HEX INTO LINE BUFFER
             PLA
             JSR     PRBYTE             ;OUTPUT LSB AS HEX INTO LINE BUFFER
             LDA     #2                 ;PAD WITH 2 SPACES
             JSR     ADDSPACE           ;ADD SPACE BETWEEN FIELDS

             LDA     SDT_BANK,Y         ;GET DRIVER BANK
             JSR     PRBYTE             ;OUTPUT AS HEX INTO LINE BUFFER

             LDA     #79
             STA     REQCOUNT           ;SET LENGTH OF OUTPUT TO CONSOLE
            
             BRK                        ;OUTPUT TO CONSOLE
             .BYTE   SOS_WRITE
             .WORD   WRITECON

             BEQ     @OK3
             LDA     #XIOERROR          ;I/O ERROR
             JSR     SYSERR

@OK3:        INC     ROW
             INC     DEVICE
             LDX     DEVICE
             CPX     MAX_DNUM           ;ARE WE AT THE LAST ONE
             BCS     DONE               ;EXIT IF >= MAX_DNUM
             JMP     LOOP


DONE:        PLA                        ;RESTORE BANK_REG
             STA     BANK_REG
             LDA     ENVREG             ;TURN ON CXXX I/O
             PHA                        ;SAVE CURRENT STATE
             ORA     #$40 
             STA     ENVREG

WAIT:        LDA     KEYBD              ;CHECK IF CTRL KEY IS STILL DOWN
             AND     #04
             BEQ     WAIT               ;YES, LOOP UNTIL ITS RELEASED

             PLA                        ;RESTORE ENVREG
             STA     ENVREG


             BRK                        ;CLEAR SCREEN
             .BYTE   SOS_WRITE
             .WORD   CLEARCON

             BRK                        ;CLOSE CONSOLE
             .BYTE   SOS_CLOSE
             .WORD   CLOSECON
            
             RTS

;
; SUBROUTINES
;
;
; EXTRACT AND PRINT NEXT DIB BYTE INTO LINE BUFFER
; 6 CHARS PER FIELD
;
NEXTBYTE:    JSR     EXTRACT            ;EXTRACT THE BYTE AND PRINT INTO LINE BUF
             LDA     #4                 ;PAD WITH 4 SPACES
             JSR     ADDSPACE           ;ADD SPACE BETWEEN FIELDS
             RTS

;
; EXTRACT AND PRINT NEXT DIB WORD INTO LINE BUFFER
; 6 CHARS PER FIELD
;
NEXTWORD:    INY                        ;MSB FIRST
             JSR     EXTRACT            ;EXTRACT THE BYTE AND PRINT INTO LINE BUF
             DEY                        ;THEN LSB
             DEY
             JSR     EXTRACT
             INY                        ;ONE MORE TO GET TO NEXT BYTE
             LDA     #2                 ;PAD WITH 4 SPACES
             JSR     ADDSPACE           ;ADD SPACE BETWEEN FIELDS
             RTS

;
; EXTRACT THE BYTE FROM THE DIB AND THEN PRINT AS HEX INTO
; THE LINE BUFFER
; X=INDEX IN LINE BUF, Y=INDEX INTO DIB
;
EXTRACT:     LDA     (POINTER),Y        ;GET ACTIVE FLAG
             JSR     PRBYTE             ;OUTPUT AS HEX INTO LINE BUFFER
             INY                        ;INC Y FOR NEXT EXTRACT
             RTS
;
; SUBROUTINE TO PRINT A BYTE IN A IN HEX FORM (DESTRUCTIVE)
; PUTS IT INTO 'LINE' BUFFER AT INDEX X
;
PRBYTE:      PHA                        ;SAVE A FOR LSD
             LSR                        ;MSD TO LSD POSITION
             LSR
             LSR
             LSR
             JSR    PRHEX               ;OUTPUT HEX DIGIT
             PLA                        ;RESTORE A
; FALL THROUGH TO PRINT HEX ROUTINE

PRHEX:       AND    #$0F                ;MASK LSD FOR HEX PRINT
             ORA    #'0'                ;ADD "0"
             CMP    #'9'+1              ;IS IT A DECIMAL DIGIT?
             BCC    STORE               ;YES! OUTPUT IT
             ADC    #6                  ;ADD OFFSET FOR LETTER A-F
; FALL THROUGH TO PRINT ROUTINE

STORE:       STA    LINE,X
             INX
             RTS

;
; PRINT SPACES INTO LINE BUFFER
; A=NUMBER SPACES,X=INDEX INTO LINE BUFFER
;
ADDSPACE:    STY     TEMPY           ;SAVE Y
             TAY
             LDA     #$A0
@S1:         STA     LINE,X
             INX
             DEY
             BNE     @S1
             LDY     TEMPY           ;RESTORE Y
             RTS


ZZEND:
ZZLEN        =         ZZEND-ZZORG
; ADD PADDING TO MAKE $400 BYTES LONG
PADDING      =         $400-ZZLEN
             .RES      PADDING
             .IF       $400-LENBFMI
             .FATAL    "SOSORG FILE IS INCORRECT FOR BFM.INIT2"
             .ENDIF

