;SBTL "SOS 1_1 SYSTEM ERROR ROUTINES"
;.RELOC
            .SEGMENT  "CODE"
            .INCLUDE  "SOSORG"
            .ORG      ORGSERR
ZZORG       =         *
;MSB OFF
;***************************************************************************************************
;          COPYRIGHT (C) APPLE COMPUTER INC. 1980
;                    ALL RIGHTS RESERVED
;***************************************************************************************************
;
; SYSTEM ERROR ROUTINES  (VERSION = 1.1O   )
;                        (DATE    = 12/02/81)
;
; THIS MODULE CONTAINS THE SYSTEM ERROR AND SYSTEM FAILURE ROUTINES.
;
;***************************************************************************************************
;
            .EXPORT   SYSERR
            .EXPORT   SYSDEATH
;
            .IMPORT   SERR
            .IMPORT   SDEATH_REGS
            .IMPORT   SCRNMODE
;PAGE
;***************************************************************************************************
;
; DATA DECLARATIONS
;
;***************************************************************************************************
;
E_REG       =         $FFDF
Z_REG       =         $FFD0
B_REG       =         $FFEF
;
S_SAVE      =         $09                                    ; REGISTER SAVE AREA
PCH_SAVE    =         $08
PCL_SAVE    =         $07
P_SAVE      =         $06
A_SAVE      =         $05
X_SAVE      =         $04
Y_SAVE      =         $03
E_SAVE      =         $02
Z_SAVE      =         $01
B_SAVE      =         $00
;
NMI_VECTOR  =         $FFFA
;
TXT_CLR     =         $C050
MIX_CLR     =         $C052
HIRES_CLR   =         $C056
;
PG2_CLR     =         $C054
;
MSGBASE     =         $7E4
MSGBASE2    =         $BE4
MSG:        .BYTE     " SYSTEM FAILURE = $"
MSGLEN      =         *-MSG
;PAGE
;***************************************************************************************************
;
; SYSTEM ERROR ROUTINE
;
; THIS ROUTINE IS CALLED WHEN AN ERROR CONDITION HAS BEEN
; ENCOUNTERED.  THE ERROR NUMBER IS PASSED IN THE A REG
; AND THE CALL TO THIS ROUTINE MUST ALWAYS BE A JSR.
;
;***************************************************************************************************
SYSERR      =         *
;
            STA       SERR
            PLA
            STA       SDEATH_REGS+PCL_SAVE
            PLA
            STA       SDEATH_REGS+PCH_SAVE
            SEC
            LDA       SERR
            BNE       SERR_EXIT
            CLC
SERR_EXIT:  RTS                                              ; RETURNS ONE LEVEL BEYOND CALLER
;PAGE
;***************************************************************************************************
;
; SYSTEM DEATH ROUTINE
;
; CALLED TO IMMEDIATELY TERMINATE EXECUTION OF THE MACHINE
; BECAUSE A FATAL ERROR HAS BEEN DETECTED BY THE OPERATING
; SYSTEM.  THE ERROR CODE IS PASSED IN THE A REG.  THE
; CALL TO THIS ROUTINE MUST ALWAYS BE A JSR.
;
;***************************************************************************************************
SYSDEATH    =         *
;
            STA       SDEATH_REGS+A_SAVE                     ; SAVE REGISTERS
            STX       SDEATH_REGS+X_SAVE
            STY       SDEATH_REGS+Y_SAVE
            PHP
            PLA
            STA       SDEATH_REGS+P_SAVE
            TSX
            STX       SDEATH_REGS+S_SAVE
            LDA       E_REG
            STA       SDEATH_REGS+E_SAVE
            LDA       Z_REG
            STA       SDEATH_REGS+Z_SAVE
            LDA       B_REG
            STA       SDEATH_REGS+B_SAVE
            PLA
            STA       SDEATH_REGS+PCL_SAVE
            PLA
            STA       SDEATH_REGS+PCH_SAVE
;
            SEI                                              ; TURN OFF INTERRUPTS
            CLD
;
            LDX       #0                                     ; SAVE SYSTEM STACK PAGE IN PAGE $17
SD005:      LDA       $100,X
            STA       $1700,X
            DEX
            BNE       SD005
;
            LDA       $C059                                  ; ENSURE SILENTYPE PORT SHUT DOWN
            LDA       $C0DD
            LDA       $C0DF
            LDA       $C05F
            LDA       $C05A
;
            LDA       $C040                                  ; SOUND BELL
;
            LDA       #$74                                   ; ENSURE RESET LOCK OFF & RAM SWITCHED IN_
            STA       E_REG
;
            LDA       TXT_CLR                                ; SWITCH TO 40 COL B&W DISPLAY MODE
            LDA       MIX_CLR
            LDA       HIRES_CLR
            LDA       PG2_CLR                                ; & SELECT PAGE 1
;
            LDA       #$02
            BIT       SCRNMODE
            BVS       SD015                                  ; IF GRAPHICS MODE THEN KEEP 40 COL MODE
            BEQ       SD015                                  ; IF 40 COL MODE THEN KEEP
            LDA       MIX_CLR+1                              ; ELSE SWITCH TO 80 COL DISPLAY MODE
;
            LDX       #MSGLEN+1                              ; ENSURE BKGRND SET TO INVERSE SPACES
            LDA       #$20                                   ; SPACE CHAR W/INVERSE
SD010:      STA       MSGBASE2-1,X
            DEX
            BPL       SD010
;
SD015:      LDX       #0                                     ; MOVE MSG TO TEXT SCREEN
SD020:      LDA       MSG,X
            STA       MSGBASE-1,X
            INX
            CPX       #MSGLEN
            BNE       SD020
;
            LDA       SDEATH_REGS+A_SAVE                     ; DISPLAY ERROR CODE (2 HEX DIGITS)
            CLC
            LSR       A
            LSR       A
            LSR       A
            LSR       A
            JSR       PRINT                                  ; FIRST DIGIT
            INX
            LDA       SDEATH_REGS+A_SAVE
            AND       #$0F
            JSR       PRINT                                  ; SECOND DIGIT
;
            LDA       #<SD100
            STA       NMI_VECTOR
            LDA       #>SD100
            STA       NMI_VECTOR+1
;
;
            JMP       *                                      ; HANG UNTIL REBOOT (CTRL/RESET)
;***************************************************************************************************
SD100:      RTI                                              ; NMI VECTOR POINT HERE TO MASK THEM OUT
;
;
; PRINT SUBROUTINE
;
PRINT       =         *
            CMP       #$A
            BCS       PRNT100
            ADC       #$30                                   ; "0"-"9"
            BCC       PRNT110                                ; ALWAYS TAKEN
PRNT100:    ADC       #$36                                   ; "A"-"F"
PRNT110:    STA       MSGBASE-1,X
            RTS
;
;LST ON
ZZEND       =         *
ZZLEN       =         ZZEND-ZZORG
            .IF       ZZLEN-LENSERR
            .FATAL    "SOSORG FILE IS INCORRECT FOR SYSERR"
            .ENDIF

