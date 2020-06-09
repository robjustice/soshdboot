;SBTL "SOS 1_1  CHARACTER FILE MANAGER"
;.RELOC
             .SEGMENT   "CODE"
             .INCLUDE   "SOSORG"
             .ORG       ORGCFM
ZZORG        =          *
;MSB OFF
;***************************************************************************************************
;          COPYRIGHT (C) APPLE COMPUTER INC. 1980
;                    ALL RIGHTS RESERVED
;***************************************************************************************************
;
; CHARACTER FILE MANAGER (VERSION = 1.1O   )
;                        (DATE    = 8/04/81)
;
; THIS MODULE TRANSFORMS CHARACTER FILE SYSTEM CALLS INTO
; DEVICE CALLS TO THE APPROPRIATE DEVICE HANDLER.  ONLY
; OPEN, NEWLINE, READ, WRITE AND CLOSE CALLS ARE PERMITTED
; ON CHARACTER FILES.
;
;***************************************************************************************************
;
             .EXPORT    CFMGR
;
             .EXPORT    CFCB_MAX
             .EXPORT    CFCB_DEV
;
             .IMPORT    DMGR
             .IMPORT    LEVEL
             .IMPORT    MAX_DNUM
             .IMPORT    SXPAGE
;
             .IMPORT    SYSERR
             .IMPORT    SERR
             .IMPORTZP  BADSCNUM
             .IMPORTZP  CFCBFULL
             .IMPORTZP  BADREFNUM
             .IMPORTZP  FNFERR
;PAGE
;***************************************************************************************************
;
; DATA DECLARATIONS
;
;***************************************************************************************************
;
; FILE CALL PARM LOCATIONS ON SOS ZPAGE
;
F_TPARMX     =          $A0
REQCODE      =          F_TPARMX
O_PATH       =          F_TPARMX+1                            ; OPEN'S PATHNAME LOC
O_REFNUM     =          F_TPARMX+3                            ; OPEN'S REFNUM LOC
REFNUM       =          F_TPARMX+1                            ; REFNUM'S LOC IN OTHER CALLS
NL_ISNL      =          F_TPARMX+2                            ; NEWLINE'S ISNEWLINE LOC
NL_NLCHR     =          F_TPARMX+3                            ; NEWLINE'S NEWLINECHAR LOC
RW_BUF       =          F_TPARMX+2                            ; READ/WRITE'S BUF LOC
RW_BYTES     =          F_TPARMX+4                            ; READ/WRITE'S BYTES LOC
RD_BYTESRD   =          F_TPARMX+6                            ; READ'S BYTESREAD LOC
;
; FILE REQUEST CODE VALUES
;
OPEN         =          8
NEWLINE      =          9
READ         =          $A
WRITE        =          $B
CLOSE        =          $C
;PAGE
; DEVICE CALL PARM LOCATIONS ON SOS ZPAGE
;
D_TPARMX     =          $C0
D_SCNUM      =          D_TPARMX                              ; DEVICE SYS CALL # LOC
GDN_DNAME    =          D_TPARMX+1                            ; GETDEVNUM DNAME LOC
GDN_DNUM     =          D_TPARMX+3                            ; GETDEVNUM DNUM LOC
D_DNUM       =          D_TPARMX+1                            ; OPN/CLOSE/RD/WR/CTRL'S DNUM LOC
DRW_BUF      =          D_TPARMX+2                            ; RD/WR'S BUF LOC
DRW_BYTES    =          D_TPARMX+4                            ; RD/WR'S BYTES LOC
DRD_BYTESRD  =          D_TPARMX+8                            ; RD/WR'S BYTESREAD LOC
DC_CCODE     =          D_TPARMX+2                            ; DCTRL'S CTRLCODE LOC
DC_CLIST     =          D_TPARMX+3                            ; DCTRL'S CTRLLIST LOC
;
; DEVICE REQUEST CODE VALUES
;
DREAD        =          $0
DWRITE       =          $1
DCTRL        =          $3
GETDEVNUM    =          $4
DOPEN        =          $6
DCLOSE       =          $7
;
CTRL_LIST:   .RES       2                                     ; CONTAINER FOR NEWLINE DCTRL CALL
NEWLINECC    =          2                                     ; NEWLINE CTRL CODE
;
; GETDNUM VARS
;
DNUM_TEMP:   .RES       1
;
; CLOSEALL VARS
;
DCLOSE_ERR   =          F_TPARMX+$F
DCLOSE_TBL   =          $200
TRUE         =          $80
FALSE        =          $0
;
;
;***************************************************************************************************
;
; CHARACTER FILE CONTROL BLOCK TABLE
; (ENTRY 0 IS NOT USED)
;
;***************************************************************************************************
CFCB_MAX     =          17
CFCB_DEV:    .RES       CFCB_MAX
CFCB_LVL:    .RES       CFCB_MAX
;PAGE
;***************************************************************************************************
;
; CHARACTER FILE MANAGER - MAIN ENTRY POINT
;
;***************************************************************************************************
CFMGR        =          *
;
; SWITCH, BASED ON REQUEST CODE
;
             LDA        REQCODE
             CMP        #OPEN
             BEQ        CFOPEN                                ; "OPEN"
             CMP        #NEWLINE
             BEQ        CFNEWLINE                             ; "NEWLINE"
             CMP        #READ
             BEQ        CFREAD                                ; "READ"
             CMP        #WRITE
             BNE        CFM010
             JMP        CFWRITE                               ; "WRITE"
CFM010:      CMP        #CLOSE
             BNE        CFM020
             JMP        CFCLOSE                               ; "CLOSE"
CFM020:      LDA        #BADSCNUM
             JSR        SYSERR                                ; ERR EXIT
;PAGE
;***************************************************************************************************
; OPEN(IN.PATHNAME; OUT.REFNUM; IN.OPENLIST,LENGTH) SYSTEM CALL
;***************************************************************************************************
CFOPEN       =          *                                     ; BUILD "D_OPEN" CALL
             JSR        GETDNUM                               ; MAP PATH TO DEV#
             BCS        CFOP_ERR1                             ; ERR - FILE NOT FOUND
             STA        D_DNUM
;
             JSR        REQ_CFCB                              ; BUILD NEW CFCB ENTRY
             BCS        CFOP_ERR1                             ; ERR - CFCB FULL
             LDX        #0
             STA        (O_REFNUM,X)                          ; RETURN REFNUM TO CALLER
             CPY        #1
             BNE        CFOP_EXIT                             ; DEVICE ALREADY OPEN
;
             LDA        #DOPEN
             STA        D_SCNUM
             JSR        DMGR                                  ; DOPEN CALL
             BCS        CFOP_ERR
CFOP_EXIT:   RTS                                              ; NORMAL EXIT
;
CFOP_ERR:    LDA        SERR                                  ;KLUDGE - 1_0 DRIVERS DON'T SUPPORT CARRY ERR PROTOCOL
             BEQ        CFOP_EXIT                             ;NO ERROR
             LDX        #0                                    ; RELEASE CFCB ENTRY
             LDA        (O_REFNUM,X)
             JSR        REL_CFCB
CFOP_ERR1:   RTS                                              ; ERR EXIT
;PAGE
;***************************************************************************************************
; NEWLINE(IN.REFNUM,IS	.NEWLINE,NEWLINE.CHAR) SYSTEM CALL
;***************************************************************************************************
CFNEWLINE    =          *                                     ; BUILD "D_CONTROL" CALL
             LDA        #DCTRL
             STA        D_SCNUM
             LDA        REFNUM
             JSR        GET_CFCB                              ; MAP REFNUM TO DEV #
             BCS        CFNL_ERR                              ; ERR - BAD REFNUM
;
             STA        D_DNUM
             LDA        #NEWLINECC
             STA        DC_CCODE
;
             LDA        #<CTRL_LIST
             STA        DC_CLIST
             LDA        #>CTRL_LIST
             STA        DC_CLIST+1
             LDA        #0
             STA        SXPAGE+DC_CLIST+1
;
             LDA        NL_ISNL
             STA        CTRL_LIST
             LDA        NL_NLCHR
             STA        CTRL_LIST+1
;
             JSR        DMGR                                  ; DCONTROL CALL
             RTS                                              ; NORMAL EXIT
;
CFNL_ERR:    RTS                                              ; ERR EXIT
;PAGE
;***************************************************************************************************
; READ(IN.REFNUM,BUF,BYTES,BYTESREAD) SYSTEM CALL
;***************************************************************************************************
CFREAD       =          *                                     ; BUILD "D_READ" CALL
             LDA        #DREAD
             STA        D_SCNUM
             LDA        REFNUM
             JSR        GET_CFCB                              ; MAP REFNUM TO DEV #
             BCS        CFRD_ERR                              ; ERR - BAD REFNUM
;
             STA        D_DNUM
             LDX        #3
CFRD010:     LDA        RW_BUF,X
             STA        DRW_BUF,X
             DEX
             BPL        CFRD010
;
             LDA        RD_BYTESRD
             STA        DRD_BYTESRD
             LDA        RD_BYTESRD+1
             STA        DRD_BYTESRD+1
;
             LDA        SXPAGE+RW_BUF+1
             STA        SXPAGE+DRW_BUF+1
             LDA        SXPAGE+RW_BYTES+1
             STA        SXPAGE+DRW_BYTES+1
             LDA        SXPAGE+RD_BYTESRD+1
             STA        SXPAGE+DRD_BYTESRD+1
;
             JSR        DMGR                                  ; DREAD CALL
             RTS                                              ; NORMAL EXIT
;
CFRD_ERR:    RTS                                              ; ERR EXIT
;PAGE
;***************************************************************************************************
; WRITE(IN.REFNUM,BUF,BYTES) SYSTEM CALL
;***************************************************************************************************
CFWRITE      =          *                                     ; BUILD "D_WRITE" CALL
             LDA        #DWRITE
             STA        D_SCNUM
             LDA        REFNUM
             JSR        GET_CFCB                              ; MAP REFNUM TO DEV #
             BCS        CFWR_ERR                              ; ERR - BAD REFNUM
             STA        D_DNUM
             LDX        #3
CFWR010:     LDA        RW_BUF,X
             STA        DRW_BUF,X
             DEX
             BPL        CFWR010
             LDA        SXPAGE+RW_BUF+1
             STA        SXPAGE+DRW_BUF+1
             LDA        SXPAGE+RW_BYTES+1
             STA        SXPAGE+DRW_BYTES+1
;
             JSR        DMGR                                  ; DWRITE CALL
             RTS                                              ; NORMAL EXIT
;
CFWR_ERR:    RTS                                              ; ERR EXIT
;PAGE
;***************************************************************************************************
; CLOSE(IN.REFNUM) SYSTEM CALL
;***************************************************************************************************
CFCLOSE      =          *                                     ; BUILD "D_CLOSE" CALL
             LDA        #DCLOSE
             STA        D_SCNUM
             LDA        REFNUM
             BEQ        CLOSEALL
;
             JSR        REL_CFCB                              ; RELEASE CFCB ENTRY
             BCS        CFCL010
             STA        D_DNUM
             TYA
             BNE        CFCL010
             JSR        DMGR                                  ; DCLOSE CALL
CFCL010:     RTS                                              ; NORMAL EXIT
;
;PAGE
;***************************************************************************************************
;
; CLOSE ALL CHARACTER FILES W/LEVELS >= TO CURRENT SYSTEM FILE LEVEL.
;
;***************************************************************************************************
;
CLOSEALL     =          *
             LDA        #FALSE                                ; SET ENTRIES IN DEV CLOSE TBL TO FALSE
             LDX        MAX_DNUM
CFCL020:     STA        DCLOSE_TBL,X
             DEX
             BPL        CFCL020
;
             LDX        #CFCB_MAX-1                           ; CLOSE ALL DEVICES >= TO CURRENT LEVEL
CFCL030:     LDA        CFCB_DEV,X                            ; AND MARK TRUE IN DEV CLOSE TBL
             TAY
             BMI        CFCL050
             LDA        CFCB_LVL,X
             CMP        LEVEL
             BCC        CFCL050
             LDA        #TRUE
             STA        DCLOSE_TBL,Y
             SEC
             ROR        CFCB_DEV,X
CFCL050:     DEX
             BNE        CFCL030
;
             LDX        #CFCB_MAX-1                           ; DON'T CLOSE DEVICES < CURRENT LEVEL
CFCL060:     LDA        CFCB_DEV,X
             TAY
             BMI        CFCL070
             LDA        #FALSE
             STA        DCLOSE_TBL,Y
CFCL070:     DEX
             BNE        CFCL060
;
             LDA        #0
             STA        DCLOSE_ERR
             LDX        MAX_DNUM                              ; ISSUE D'CLOSE CALLS TO ALL DEVICES MARKED AS TRUE
CFCL080:     LDA        DCLOSE_TBL,X                          ; IN DEV CLOSE TABLE
             BPL        CFCL090
             TXA
             PHA
             STX        D_DNUM
             JSR        DMGR
             PLA
             TAX
             LDA        SERR
             BEQ        CFCL090                               ; IF ERROR,
             STA        DCLOSE_ERR                            ; THEN SAVE IT
CFCL090:     DEX
             BNE        CFCL080
;
             LDA        DCLOSE_ERR                            ; IF $0 THEN NO ERRORS FROM D_CLOSE CALLS
             BNE        CFCL_ERR
             RTS                                              ; NORMAL EXIT
CFCL_ERR:    JSR        SYSERR                                ; RETURN LAST D_CLOSE ERROR REPORTED
;PAGE
;***************************************************************************************************
;
; GET DEVICE NUMBER
;
; INPUT:  CPATH
; OUTPUT: DEVICE NUMBER (A)
; ERROR:  CARRY SET ("FILE NOT FOUND")
;
; GETDNUM FIRST CALLS THE DMGR (GETDEVNUM) MAP THE PATHNAME
; TO A DEVICE #.  GETDNUM THEN ENSURES THAT THE PATHNAME
; IS NOT A BLOCK DEVICE BY CHECKING THE DBLKLST TABLE.
;
;***************************************************************************************************
;
GETDNUM      =          *
             LDA        #GETDEVNUM
             STA        D_SCNUM
;
             LDA        O_PATH
             STA        GDN_DNAME
             LDA        O_PATH+1
             STA        GDN_DNAME+1
;
             LDA        #<DNUM_TEMP
             STA        GDN_DNUM
             LDA        #>DNUM_TEMP
             STA        GDN_DNUM+1
;
             LDA        SXPAGE+O_PATH+1
             STA        SXPAGE+GDN_DNAME+1
             LDA        #0
             STA        SXPAGE+GDN_DNUM+1
;
             JSR        DMGR
             BCS        GETD_ERR                              ; D_NAME NOT FOUND
             BMI        GETD_ERR                              ; BLOCK DEVICE FOUND
             LDA        DNUM_TEMP
             RTS
;
GETD_ERR:    LDA        #FNFERR
             JSR        SYSERR
;PAGE
;***************************************************************************************************
; REQUEST FCB ENTRY
;
; INPUT: DNUM (A)
; OUTPUT: REFNUM (A), OPENCT (Y)
; ERROR:  CARRY SET ("CFCB FULL")
;
; REQ.CFCB FIRST SEARCHES THE CFCB TABLE USING THE DEV#
; AS A KEY.  IF FOUND THE OPENCT IS INCREMENTED, OTHERWISE,
; REQ.CFCB FINDS A FREE ENTRY AND STORES THE DEV# AND LEVEL #.
;
;***************************************************************************************************
;
REQ_CFCB     =          *
             LDX        #CFCB_MAX-1
             TAY
REQ010:      LDA        CFCB_DEV,X
             BMI        REQ020
             DEX
             BNE        REQ010
             LDA        #CFCBFULL
             JSR        SYSERR
REQ020:      TYA
             STA        CFCB_DEV,X
             LDA        LEVEL
             STA        CFCB_LVL,X
             TXA
             PHA
             TYA
             JSR        OPENCOUNT
             PLA
             ORA        #$80
             CLC
             RTS                                              ; NORMAL EXIT
;PAGE
;***************************************************************************************************
;
; RELEASE FCB ENTRY
;
; INPUT:  REFNUM (A)
; OUTPUT:  DNUM (A), OPENCT (Y)
; ERROR:   CARRY SET ("INVALID REFNUM")
;
; USES REFNUM AS AN CFCB TABLE INDEX TO RELEASE A CFCB ENTRY.
;
;***************************************************************************************************
REL_CFCB     =          *
             AND        #$7F
             CMP        #CFCB_MAX
             BCS        REL_ERR
             TAX
             LDA        CFCB_DEV,X
             BMI        REL_ERR
             SEC                                              ; MARK ENTRY FREE
             ROR        CFCB_DEV,X
             JSR        OPENCOUNT
             CLC
             RTS                                              ; NORMAL EXIT
;
REL_ERR:     LDA        #BADREFNUM
             JSR        SYSERR
;***************************************************************************************************
;
; OPENCOUNT SUBROUTINE
;
; INPUT:   DEVNUM (A)
; OUTPUT:  DEVNUM (A), OPENCTR (Y)
;
; OPENCTR:=COUNT OF ALL CFCB ENTRIES W/CFCB.DEV=DEVNUM
;
;***************************************************************************************************
OPENCOUNT    =          *
             LDY        #0
             LDX        #CFCB_MAX-1
OPNCT010:    CMP        CFCB_DEV,X
             BNE        OPNCT020
             INY
OPNCT020:    DEX
             BNE        OPNCT010
             RTS
;PAGE
;***************************************************************************************************
;
; GET FCB ENTRY
;
; INPUT:   REFNUM (A)
; OUTPUT:  DNUM (A)
; ERROR:   CARRY SET ("INVALID REFNUM")
;
; USES REFNUM AS AN INDEX TO RETURN THE CORRESPONDING DEVICE #.
; IF THE ENTRY INDICATED BY REFNUM IS A FREE ENTRY, THEN AN
; ERROR, "INVALID REF NUM" IS RETURNED.
;
;***************************************************************************************************
GET_CFCB     =          *
             AND        #$7F
             CMP        #CFCB_MAX
             BCS        GET_ERR
             TAX
             LDA        CFCB_DEV,X
             BMI        GET_ERR
             CLC
             RTS                                              ; NORMAL EXIT
;
GET_ERR:     LDA        #BADREFNUM
             JSR        SYSERR                                ; ERR EXIT
;
;LST ON
ZZEND        =          *
ZZLEN        =          ZZEND-ZZORG
             .IF        ZZLEN-LENCFM
             .FATAL     "SOSORG FILE IS INCORRECT FOR CFMGR"
             .ENDIF

