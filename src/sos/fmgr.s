;SBTL "SOS 1_1  FILE MANAGER"
;.RELOC
            .SEGMENT   "CODE"
            .INCLUDE   "SOSORG"
            .ORG       ORGFMGR
ZZORG       =          *
;MSB OFF
;***************************************************************************************************
;          COPYRIGHT (C) APPLE COMPUTER INC. 1980
;                    ALL RIGHTS RESERVED
;***************************************************************************************************
;
; FILE MANAGER (VERSION = 1.10   )
;              (DATE    = 8/04/81)
;
; THIS MODULE IS ENTERED FROM THE SYSTEM CALL MANAGER, AND
; IS RESPONSIBLE FOR SWITCHING TO EITHER THE BLOCK FILE
; MANAGER, OR THE CHARACTER FILE MANAGER.
;
;***************************************************************************************************
;
            .EXPORT    FMGR
            .EXPORT    LEVEL
;
            .IMPORT    BFMGR
            .IMPORT    CFMGR
            .IMPORT    SYSERR
            .IMPORT    SERR
            .IMPORT    BADPATH
            .IMPORTZP  FNFERR
            .IMPORTZP  LVLERR
;
F_TPARMX    =          $A0                                  ; LOC OF FILE SYSTEM CALL PARMS
OPEN        =          $8
CLOSE       =          $C
SETLEVEL    =          $12
GETLEVEL    =          $13
F_REQCODE   =          F_TPARMX
F_LEVEL     =          F_TPARMX+$1
PATHNAME    =          F_TPARMX+$1
REFNUM      =          F_TPARMX+$1
PERIOD      =          $2E
LEVEL:      .BYTE      $1
;PAGE
;***************************************************************************************************
;
; FILE MANAGER
;
;***************************************************************************************************
FMGR        =          *
;
            LDA        F_REQCODE
            CMP        #OPEN
            BCC        FMGR010
            BEQ        FMGR020
            CMP        #CLOSE
            BCC        FMGR030
            BEQ        FMGR040
            CMP        #SETLEVEL
            BEQ        SLEVEL
            CMP        #GETLEVEL
            BEQ        GLEVEL
;
FMGR010:    JMP        BFMGR                                ; EXIT
;
FMGR020:    LDY        #1
            LDA        (PATHNAME),Y
            CMP        #PERIOD
            BNE        FMGR010
            JSR        CFMGR
            BCC        FMGR024
            LDA        SERR
            CMP        #FNFERR
            BEQ        FMGR026
FMGR024:    RTS                                             ; EXIT
;
FMGR026:    LDA        #0
            STA        SERR
            JMP        BFMGR                                ; EXIT
;
FMGR030:    LDA        REFNUM
FMGR031:    BPL        FMGR010
            JMP        CFMGR                                ; EXIT
;
FMGR040:    LDA        REFNUM
            BNE        FMGR031
            JSR        BFMGR                                ; CLOSE (0)
            JMP        CFMGR                                ; EXIT
;
SLEVEL:     LDA        F_LEVEL
            BEQ        LVL_ERR
            CMP        #$40            ;(S)  was #4
            BCS        LVL_ERR
            STA        LEVEL
            RTS
LVL_ERR:    LDA        #LVLERR
            JSR        SYSERR
;
GLEVEL:     LDY        #0
            LDA        LEVEL
            STA        (F_LEVEL),Y
            RTS
;
;LST ON
ZZEND       =          *
ZZLEN       =          ZZEND-ZZORG
            .IF        ZZLEN-LENFMGR
            .FATAL     "SOSORG FILE IS INCORRECT FOR FMGR"
            .ENDIF

