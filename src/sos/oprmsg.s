;SBTL "SOS 1.1  OPERATOR MESSAGE/REPLY"
;.RELOC
            .SEGMENT  "CODE"
            .INCLUDE  "SOSORG"
            .ORG      ORGOMSG
ZZORG:
;MSB OFF
;***************************************************************************************************
;
;          COPYRIGHT (C) APPLE COMPUTER INC. 1981
;                   ALL RIGHTS RESERVED
;
;***************************************************************************************************
;
;  THIS MODULE CONTAINS THE BLOCK FILE MANAGERS'S OPERATOR
;  INTERFACE.  IT DISPLAYS A MESSAGE IN A FOUR LINE BY
;  FOURTY COLUMN WINDOW, THEN WAITS FOR THE USER TO TOGGLE
;  THE ALPHA-LOCK KEY BEFORE RETURNING.
;
;  THE VERTICAL BLANKING FLAGS AND COMPOSITE BLANKING
;  TIMER ARE USED TO MAINTAIN THE DISPLAY.  MEMORY PAGE
;  $02 IS USED FOR TEMPORARY STORAGE.  ON EXIT, ALL
;  RESOURCES ARE RESTORED TO THEIR PREVIOUS STATES.
;
;  ENTRY POINT:  OPMSGRPLY
;
;  PARAMETERS:  X -- MESSAGE ADDRESS (LOW BYTE)
;               Y -- MESSAGE ADDRESS (HIGH BYTE)
;     (THE MESSAGE MUST RESIDE IN THE CURRENT BANK)
;
;  RESULT:  A -- RESPONSE KEYSTROKE
;           X, Y -- UNDEFINED
;
;***************************************************************************************************
;
;
            .EXPORT   OPMSGRPLY: ABSOLUTE
;
            .IMPORT   SCRNMODE
;PAGE
;
;  HARDWARE EQUATES
;
Z_REG       =         $FFD0
E_REG       =         $FFDF
;
KBPORT      =         $C008
;
BELL        =         $C040
;
VM0         =         $C050
VM1         =         $C052
VM2         =         $C054
VM3         =         $C056
;
E_T2        =         $FFE8
E_ACR       =         $FFEB
E_PCR       =         $FFEC
E_IFR       =         $FFED
E_IER       =         $FFEE
;
;  ZERO PAGE DECLARATIONS
;

; DSECT
ZPBASE      =         $200                                   ; modified for ca65
ZPSTART     =         0                                      ;ZERO PAGE DECLARATIONS
MSGPTR      =         ZPSTART                                ;RES 2  MESSAGE POINTER
MSGIDX      =         ZPSTART+2                              ;RES 1
;
SCRNIDX     =         ZPSTART+3                              ;RES 1
SCRNPTR     =         ZPSTART+4                              ;RES 2
DATAPTR     =         ZPSTART+6                              ;RES 2
DATABUF     =         ZPSTART+8                              ;RES 160
;
SV_ZREG     =         ZPSTART+168                            ;RES 1
SV_EREG     =         ZPSTART+169                            ;RES 1
SV_SMODE    =         ZPSTART+170                            ;RES 1
SV_EACR     =         ZPSTART+171                            ;RES 1
SV_EPCR     =         ZPSTART+172                            ;RES 1
SV_EIER     =         ZPSTART+173                            ;RES 1
;
FLAG        =         ZPSTART+174                            ;RES 1
; DEND
;PAGE
OPMSGRPLY:
;
;
;  SAVE CURRENT VALUES AND SET UP ZERO PAGE,
;  ENVIRONMENT, SCREEN MODE, AND E.6522 REGISTERS.
;
            PHP
            SEI
            LDA       Z_REG
            STA       ZPBASE+SV_ZREG                         ;SAVE ZERO PAGE
            LDA       #>ZPBASE
            STA       Z_REG
            STX       MSGPTR                                 ;SAVE MESSAGE ADDRESS
            STY       MSGPTR+1
            LDA       E_REG
            STA       SV_EREG                                ;SAVE ENVIRONMENT
            AND       #$5F
            ORA       #$40
            STA       E_REG                                  ;SCREEN OFF, I/O SPACE ON
            LDA       SCRNMODE
            STA       SV_SMODE                               ;SAVE SCREEN MODE
            LDA       #$00
            STA       SCRNMODE
            BIT       VM0                                    ;SET 40 COLUMN
            BIT       VM1                                    ;  BLACK & WHITE TEXT
            BIT       VM2
            BIT       VM3
            LDX       E_ACR
            TXA
            AND       #$20
            STA       SV_EACR                                ;SAVE AUXILIARY CONTROL REG
            TXA
            ORA       #$20
            STA       E_ACR                                  ;SET UP BL TIMER
            LDX       E_PCR
            TXA
            AND       #$F0
            STA       SV_EPCR                                ;SAVE PERIPHERAL CONTROL REG
            TXA
            AND       #$0F
            ORA       #$60
            STA       E_PCR                                  ;SET UP VBL FLAGS
            LDA       E_IER
            AND       #$38
            STA       E_IER                                  ;MASK VBL & BL INTERRUPTS
            STA       SV_EIER                                ;SAVE INTERRUPT MASKS
            PLP
;
;
;  SAVE SCREEN DATA AND CLEAR MESSAGE WINDOW
;
            LDX       #3
OPR010:     JSR       SETPTRS
            LDY       #39
OPR020:     LDA       (SCRNPTR),Y                            ;SAVE SCREEN DATA
            STA       (DATAPTR),Y
            LDA       #$A0
            STA       (SCRNPTR),Y                            ;BLANK SCREEN
            DEY
            BPL       OPR020
            DEX
            BPL       OPR010
;
;
;  MOVE MESSAGE TO WINDOW
;
            BIT       BELL
            LDX       #$00
            STX       MSGIDX
OPR100:     JSR       SETPTRS
            LDY       #$00
            STY       SCRNIDX
OPR110:     LDY       MSGIDX
            INC       MSGIDX
            LDA       (MSGPTR),Y                             ;SET UP MESSAGE
            BEQ       OPR110
            BMI       OPR200
            CMP       #$0D
            BEQ       OPR120
            LDY       SCRNIDX
            INC       SCRNIDX
            ORA       #$80
            STA       (SCRNPTR),Y
            CPY       #39
            BCC       OPR110
OPR120:
            INX
            CPX       #4
            BCC       OPR100
;
;
;  DISPLAY MESSAGE UNTIL ALPHA-LOCK KEY TOGGLES
;
OPR200:     LDY       #2
            LDA       KBPORT
            AND       #$08
            STA       FLAG
OPR210:     JSR       VIDEO
            LDA       KBPORT
            AND       #$08
            CMP       FLAG
            BEQ       OPR210
            STA       FLAG
            DEY
            BNE       OPR210
;
;
;  RESTORE PREVIOUS CONTENTS OF WINDOW
;
            LDX       #3
OPR400:     JSR       SETPTRS
            LDY       #39
OPR410:     LDA       (DATAPTR),Y
            STA       (SCRNPTR),Y
            DEY
            BPL       OPR410
            DEX
            BPL       OPR400
;
;
;  RESTORE E.6522, SCREEN MODE, ENVIRONMENT, & ZERO PAGE
;  THEN RETURN TO CALLER
;
            PHP
            SEI
            LDA       E_ACR
            AND       #$DF
            ORA       SV_EACR                                ;RESTORE AUXILIARY CONTROL REG
            STA       E_ACR
            LDA       E_PCR
            AND       #$0F
            ORA       SV_EPCR                                ;RESTORE PERIPHERAL CONTROL REG
            STA       E_PCR
            LDA       SV_EIER                                ;RESTORE INTERRUPT ENABLE REG
            ORA       #$80
            STA       E_IER
            LDA       SV_SMODE                               ;RESTORE SCREEN MODE
            STA       SCRNMODE
            LSR       A
            BCC       OPR500
            BIT       VM0+1                                  ;RESTORE VIDEO MODE
OPR500:     LSR       A
            BCC       OPR510
            BIT       VM1+1
OPR510:     LSR       A
            BCC       OPR520
            BIT       VM2+1
OPR520:     BIT       SCRNMODE
            BVC       OPR530
            BIT       VM3+1
OPR530:     LDA       SV_EREG                                ;RESTORE ENVIRONMENT
            STA       E_REG
            LDA       SV_ZREG                                ;RESTORE ZERO PAGE
            STA       Z_REG
            PLP
            RTS
;PAGE
;***************************************************************************************************
;
;  SUBROUTINE VIDEO
;
;  THIS SUBROUTINE POLLS THE VERTICAL-BLANKING AND
;  COMPOSITE-BLANKING-TIMER FLAGS AND TURNS THE SCREEN
;  OFF AND ON SO THAT ONLY THE MESSAGE WINDOW WILL BE
;  DISPLAYED.
;
;  THE E.6522 MUST BE INITIALIZED SO THAT E.CB2 FLAGS THE
;  POSITIVE EDGE OF VBL AND E.T2 COUNTS BL PULSES.  THE
;  INTERRUPTS MUST BE MASKED AND THE PROPER COUNT MUST
;  ALREADY BE STORED IN THE LOW ORDER BYTE OF E.T2.
;
;  ENTRY:  VIDEO
;
;  PARAMETERS:  INTERRUPT SYSTEM DISABLED
;
;  EXIT:  A -- UNDEFINED
;         X, Y -- PRESERVED
;
;***************************************************************************************************
;
VIDEO:
            LDA       E_IFR
            AND       #$28                                   ;GET VBL & BL FLAGS
            BEQ       VID030
            STA       E_IFR                                  ;CLEAR FLAGS
            AND       #$20                                   ;WHICH FLAG?
            BNE       VID010                                 ;  BL
;
            LDA       #$1F
            STA       E_T2                                   ;SET UP BL TIMER
            LDA       #$00
            STA       E_T2+1
            LDA       E_REG
            ORA       #$20                                   ;SET UP FOR SCREEN ON
            SEC
            BCS       VID020
;
VID010:     LDA       E_REG
            AND       #$DF                                   ;SET UP FOR SCREEN OFF
            CLC
;
VID020:     STA       E_REG
            LDA       #$00
            ROR       A
            STA       SCRNMODE
VID030:
            RTS
;PAGE
;***************************************************************************************************
;
;  SUBROUTINE SETPTRS
;
;  THIS SUBROUTINE SETS UP THE POINTERS TO THE MESSAGE
;  WINDOW AND DATA SAVE AREA.
;
;  ENTRY:  SETPTRS
;
;  PARAMETERS:  X -- LINE NUMBER [0..3]
;
;  EXIT:  A -- UNDEFINED
;         X, Y -- PRESERVED
;
;***************************************************************************************************
;
SETPTRS:
            TXA
            LSR       A
            ORA       #$04
            STA       SCRNPTR+1
            LDA       #$00
            ROR       A
            STA       SCRNPTR
            LDA       #00                                    ;<DATABUF
            STA       DATAPTR+1
            LDA       DBUFADR,X
            STA       DATAPTR
            RTS
;
DBUFADR:
            .BYTE     0*40+DATABUF
            .BYTE     1*40+DATABUF
            .BYTE     2*40+DATABUF
            .BYTE     3*40+DATABUF
; LST ON
ZZEND:
ZZLEN       =         ZZEND-ZZORG
            .IF       ZZLEN-LENOMSG
            .FATAL    "SOSORG FILE IS INCORRECT FOR OPRMSG"
            .ENDIF

