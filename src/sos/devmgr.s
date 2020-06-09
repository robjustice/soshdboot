;SBTL "SOS 1.1 DEVICE MANAGER" 

;.RELOC
            .INCLUDE   "SOSORG"
            .ORG       ORGDMGR
ZZORG       =          *
;MSB OFF
;***************************************************************************************************
;          COPYRIGHT (C) APPLE COMPUTER INC. 1980
;                    ALL RIGHTS RESERVED
;***************************************************************************************************
;
; DEVICE MANAGER (VERSION = 1.1O   ) 
;                (DATE    = 8/04/81)
;
; THIS MODULE IS RESPONSIBLE FOR CALLING THE CORRECT DEVICE
; DRIVER WHEN A D.READ...D.INIT SYSTEM CALL IS MADE. 
; (NOTE:  D.OPEN,D.CLOSE AND D.INIT ARE ONLY CALLABLE FROM
; INSIDE THE OPERATING SYSTEM).  D.INFO AND GET.DNUM CALLS
; ARE HANDLED INSIDE THIS MODULE. REPEAT.IO BYPASSES THIS MODULE. 
;***************************************************************************************************
;
            .EXPORT    DMGR
;
            .EXPORT    MAX_DNUM
            .EXPORTZP  SDT_SIZE
            .EXPORT    SDT_DIBL
            .EXPORT    SDT_DIBH
            .EXPORT    SDT_ADRL
            .EXPORT    SDT_ADRH
            .EXPORT    SDT_BANK
            .EXPORT    SDT_UNIT
            .EXPORTZP  BLKD_SIZE
            .EXPORT    BLKDLST
;
            .IMPORT    SYSERR
            .IMPORT    SERR
            .IMPORTZP  NODNAME
            .IMPORTZP  BADDNUM
            .IMPORT    SYSDEATH
            .IMPORTZP  BADSYSCALL
;
            .IMPORT    SXPAGE
;
E_REG       =          $FFDF                                  ; ENVIRONMENT REGISTER
B_REG       =          $FFEF                                  ; BANK REGISTER
;PAGE 
;***************************************************************************************************
;
; SYSTEM DEVICE TABLE (SDT)
;
; CONTAINS THE ADDRESS OF EACH DRIVER'S DIB (SDT.DIB), THE
; ADDRESS OF EACH DRIVER'S ENTRY POINT (SDT.ADR), AND THE
; UNIT # OF EACH DRIVER (SDT.UNIT).  THE TABLE IS INDEXED 
; BY DEVICE NUMBER.  ENTRY 0 IS RESERVED FOR FUTURE USE. 
;
;***************************************************************************************************
;
SDT_SIZE    =          25
;
MAX_DNUM:   .RES       1                                      ;MAX DEV NUMBER IN SYSTEM+1 
SDT_DIBL:   .RES       SDT_SIZE                               ;ADR OF DEVICE INFORMATION BLOCK 
SDT_DIBH:   .RES       SDT_SIZE
;
SDT_ADRL:   .RES       SDT_SIZE                               ;ADR OF ENTRY POINT 
SDT_ADRH:   .RES       SDT_SIZE
;
SDT_BANK:   .RES       SDT_SIZE                               ;BANK # OF DEVICE
;
SDT_UNIT:   .RES       SDT_SIZE                               ;UNIT  # OF DRIVER
;
;***************************************************************************************************
; BLOCK DEVICE LIST TABLE 
;
BLKD_SIZE   =          13
BLKDLST:    .BYTE      $00
            .RES       BLKD_SIZE-1
;PAGE
;***************************************************************************************************
;
; DATA DECLARATIONS
;
;***************************************************************************************************
;
D_TPARMX    =          $C0
REQCODE     =          D_TPARMX
;
; D.READ/WRITE/CTRL/STATUS/OPEN/CLOSE/INIT/REPEAT PARMS 
;
DNUM        =          D_TPARMX+1
;
; D_INFO PARMS
;
I_DNUM      =          D_TPARMX+1
I_DNAME     =          D_TPARMX+2
I_DLIST     =          D_TPARMX+4
I_LENGTH    =          D_TPARMX+6
;
; GET_DEV_NUM PARMS
;
G_DNAME     =          D_TPARMX+1
G_DNUM      =          D_TPARMX+3
;
; SDT ENTRY (=DIB) FIELDS 
;
DIB_SLOT    =          $11                                    ;DIB'S DEVICE SLOT FIELD 
DIB_DTYPE   =          $13                                    ;DIB'S DEVICE TYPE FIELD 
;
SDTP        =          D_TPARMX+$10                           ; PTR TO CURRENT SDT ENTRY 
;PAGE
;*************************************************************************************************** 
;
; DEVICE MANAGER (MAIN ENTRY POINT) 
;
;***************************************************************************************************
DMGR        =          *
;
            LDA        REQCODE
            CMP        #4
            BCC        DRIVER                                 ; D.READ/WRITE/CTRL/STATUS CALL 
            BNE        DM000
            JMP        GET_DNUM                               ; GET_DEV_NUM CALL
DM000:      CMP        #5
            BEQ        D_INFO                                 ; D.INFO CALL    
            CMP        #$A
            BCC        DRIVER                                 ; D.OPEN/CLOSE/INIT 
            LDA        #BADSYSCALL                            ; ELSE FATAL ERROR
            JSR        SYSDEATH                               ; EXIT
;PAGE 
;***************************************************************************************************
; D_READ/WRITE/CTRL/STATUS/OPEN/CLOSE/INIT CALLS 
; "JSR" TO DEVICE DRIVER
;*************************************************************************************************** 
DRIVER      =          *
;
            LDX        DNUM                                   ; GET DNUM SYSCALL PARM
            BEQ        DM005                                  ; WITHIN BOUNDS?
            CPX        MAX_DNUM                               ;      " 
            BCC        DM010
;
; DNUM TOO LARGE
;
DM005:      LDA        #BADDNUM                               ; INVALID DEVICE NUMBER 
            JSR        SYSERR                                 ;   ERROR EXIT  
;
; MAP DEV# TO UNIT#
;
DM010:      LDA        SDT_UNIT,X
            STA        DNUM
;
; "JSR" TO DEVICE DRIVER VIA JMP TABLE  
;
            LDA        B_REG                                  ; STACK B_REG
            PHA
            LDA        #>(DM_RTN-1)                           ; STACK RETURN ADDRESS 
            PHA
            LDA        #<(DM_RTN-1)
            PHA
;
            LDA        SDT_BANK,X                             ; SELECT RAM BANK
            STA        B_REG
            LDA        SDT_ADRH,X                             ; STACK DRIVER ENTRY POINT ADDRESS 
            PHA
            LDA        SDT_ADRL,X
            PHA
;
            LDA        E_REG                                  ; SWITCH IN I/O BANK
            ORA        #$40
            STA        E_REG
            RTS                                               ; AND, "JSR" TO DEVICE DRIVER 
;
DM_RTN:     LDA        E_REG                                  ; SWITCH OUT I/O BANK
            AND        #$BF
            STA        E_REG
            PLA                                               ; RESTORE B_REG
            STA        B_REG
            SEC
            LDA        SERR                                   ; RETRIEVE ERROR CODE
            BNE        DM017                                  ; ENSURE CARRY CLEARED IF NO ERROR 
            CLC
DM017:      RTS                                               ; AND, EXIT TO CALLER
;PAGE 
;*************************************************************************************************** 
; D_INFO(IN_DNUM, OUT_DNAME, OUT_DEVLIST, IN_LENGTH) SYSTEM CALL 
;*************************************************************************************************** 
D_INFO      =          *
;
            LDX        I_DNUM                                 ; GET DNUM PARM
            BEQ        DM020                                  ; WITHIN BOUNDS?
            CPX        MAX_DNUM                               ;      " 
            BCC        DM030
DM020:      LDA        #BADDNUM                               ; NO, DNUM TOO LARGE
            JSR        SYSERR                                 ; ERROR EXIT
;
; MOVE PARMS FM SDT ENTRY (DEV INFO BLOCK) TO CALLER'S 
; PARM LIST
;
DM030:      JSR        SETUP_SDT                              ; SET UP ZPAGE PTR TO SDT ENTRY 
;
; OUPUT DNAME PARM
;
            LDA        (SDTP),Y                               ; LOAD PARM'S BYTE COUNT 
            TAY
DM040:      LDA        (SDTP),Y
            STA        (I_DNAME),Y
            DEY
            BPL        DM040
;
; OUTPUT DEVINFO PARM (SLOT,UNIT,DEVID,PRODCODE) 
;
            LDA        #DIB_SLOT
            CLC                                               ; ADVANCE SDTP TO 2ND PARM IN SDT
            ADC        SDTP
            STA        SDTP
            BCC        DM045
            INC        SDTP+1
DM045:      LDY        I_LENGTH                               ; LOAD BYTE COUNT 
            BEQ        DM_EXIT                                ; IF 0 THEN DONE

DEY
            CPY        #$B
            BCC        DM050
            LDY        #$A
DM050:      LDA        (SDTP),Y
            STA        (I_DLIST),Y
            DEY
            BPL        DM050
;
DM_EXIT:
            CLC
            RTS                                               ; NORMAL EXIT 
;PAGE 
;***************************************************************************************************
; GET_DEV_NUM(IN_DNAME; OUT_DNUM) SYSTEM CALL 
;***************************************************************************************************
;
GET_DNUM    =          *
;
            LDX        #1                                     ; SETUP PTR TO 1ST SDT ENTRY
;
DM070:      JSR        SETUP_SDT                              ; SET UP ZPAGE PTR TO SDT ENTRY 
;
            LDA        (SDTP),Y                               ; COMPARE DNAME LENGTHS 
            CMP        (G_DNAME),Y
            BNE        NXTSDT
;
            TAY                                               ; LENGTHS MATCH, NOW COMPARE CHARS
DM080:      LDA        (G_DNAME),Y
            CMP        #$60
            BCC        DM090
            AND        #$DF                                   ; UPSHIFT
DM090:      CMP        (SDTP),Y
            BNE        NXTSDT
            DEY
            BNE        DM080
;
            TXA                                               ; CHARS MATCH
            LDY        #0
            STA        (G_DNUM),Y                             ; OUTPUT DEV NUM PARM
            LDY        #DIB_DTYPE                             ; SET "N" FLAG IN STATUS REG. 
            LDA        (SDTP),Y                               ; N=1(BLOCK DEVICE) N=0(CHAR DEVICE) 
            CLC
            RTS                                               ; NORMAL EXIT 
;
NXTSDT:     INX                                               ; LAST SDT ENTRY? 
            CPX        MAX_DNUM
            BCC        DM070
;
            LDA        #NODNAME                               ; ERROR, DNAME NOT FOUND IN SDT
            JSR        SYSERR                                 ;  RETURN TO CALLER 
;PAGE
;***************************************************************************************************
; SETUP_SDT(IN_X=DNUM, OUT_SDTP, B_REG, Y=0)   X="UNCHANGED" 
;***************************************************************************************************
SETUP_SDT   =          *
            LDA        SDT_DIBL,X                             ; SET UP ZPAGE PTR TO SDT ENTRY 
            STA        SDTP                                   ; (POINTS TO DNAME FIELD) 
            LDA        SDT_DIBH,X
            STA        SDTP+1
            LDA        SDT_BANK,X
            STA        B_REG
            LDY        #0
            STY        SXPAGE+SDTP+1
            RTS
;
;LST ON
ZZEND       =          *
ZZLEN       =          ZZEND-ZZORG
            .IF        ZZLEN-LENDMGR
            .FATAL     "SOSORG FILE IS INCORRECT FOR DEVMGR"
            .ENDIF

