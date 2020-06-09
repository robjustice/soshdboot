;SBTL "SOS 1_1  BUFFER MANAGER"
;.RELOC
             .SEGMENT   "CODE"
             .INCLUDE   "SOSORG"
;ORGBUFMG EQU $F552
;LENBUFMG EQU $31C
             .ORG       ORGBUFMG
ZZORG        =          *
;MSB OFF
;***************************************************************************************************
;          COPYRIGHT (C) APPLE COMPUTER INC. 1980
;                    ALL RIGHTS RESERVED
;***************************************************************************************************
;
; BUFFER MANAGER (VERSION = 1.10   )
;                (DATE    = 8/04/81)
;
; THIS MODULE IS RESPONSIBLE FOR CREATING AND RELEASING BUFFERS
; FOR BOTH THE BLOCK FILE MANAGER AND, LATER, DEVICE HANDLERS
; THE BUFFER MANAGER CREATES BUFFERS BY REQUESTING MEMORY
; SEGMENTS FROM THE MEMORY MANAGER, AND RELEASES THEM VIA SAME.
; THE PRIMARY DATA STRUCTURE IN THIS MODULE IS THE BUFFER TABLE.
;
;***************************************************************************************************
;
             .EXPORT    REQBUF
             .EXPORT    REQFXBUF
             .EXPORT    GETBUFADR
             .EXPORT    CHKBUF
             .EXPORT    RELBUF
;
             .IMPORT    MMGR
             .IMPORT    SXPAGE
             .IMPORT    CZPAGE
             .IMPORT    CXPAGE
;
             .IMPORT    SYSERR
             .IMPORT    SERR
             .IMPORTZP  OUTOFMEM
             .IMPORTZP  BUFTBLFULL
             .IMPORTZP  BADSYSBUF
;
             .IMPORT    SYSDEATH
             .IMPORTZP  BADBUFNUM
             .IMPORTZP  BADBUFSIZ
;
             .EXPORT    BUF_CNT
             .EXPORT    PGCT_T
             .EXPORT    XBYTE_T
             .EXPORT    BUFREF
;PAGE
;***************************************************************************************************
;
; DATA DECLARATIONS
;
;***************************************************************************************************
;
Z_REG        =          $FFD0
;
; MEMORY MGMT CALL PARM LOCATIONS ON SOS ZPAGE
;
M_TPARMX     =          $60                                    ; FIRST ADR OF MEM SYS CALL PARMS ON SOS ZPAGE
REQCODE      =          M_TPARMX+$0
;
FINDSEG      =          $1
SRCHMODE     =          M_TPARMX+$1
F_ID         =          M_TPARMX+$2
F_PGCT       =          M_TPARMX+$3
F_PGCTX:     .RES       2                                      ; TEMP LOC FOR F_PGCT PARM
F_BASE       =          M_TPARMX+$5
F_BASEX:     .RES       2                                      ; TEMP LOC FOR F_BASE PARM
F_LIM        =          M_TPARMX+$7
F_LIMX:      .RES       2                                      ; TEMP LOC FOR F_LIM PARM
F_NUM        =          M_TPARMX+$9
F_NUMX:      .RES       1                                      ; TEMP LOC FOR F_NUM PARM
;
RELSEG       =          $5
RLS_NUM      =          M_TPARMX+$1
;
; REQBUF DATA DECLARATIONS
;
RQB_PGCT:    .RES       1                                      ; REQUESTED PAGE COUNT
RQB_BNUM:    .RES       1                                      ; BUFFER NUMBER (FM GETFREE CALL)
;
; REQFXBUF DATA DECLARATIONS
;
RQFB_PGCT:   .RES       1                                      ; REQUESTED PAGE COUNT
RQFB_BNUM:   .RES       1                                      ; BUFFER NUMBER (FM GETFREE CALL)
MAXPGCT      =          64                                     ; MAX BUFSIZE=16K
F_TPARMX     =          $A0                                    ; FIRST ADR OF FILE SYS CALL PARMS ON SOS ZPAGE
OPEN_LIST    =          F_TPARMX+$5                            ; LOC OF OPEN_LIST PARM (OPEN SYS CALL)
;
; BUFCOMPACT DATA DECLARATIONS (SOURCE ALSO USED BY CHKBUF)
;
BUFC_BNUM:   .RES       1                                      ; BUF# OF LOWEST BUFFER IN BUF_TBL
SOURCE       =          M_TPARMX+$10                           ; & $11
DEST         =          M_TPARMX+$12                           ; & $13
;PAGE
;***************************************************************************************************
;
; BUFFER TABLE
;
; THE BUFFER TABLE CONSISTS OF "CNT"-1 ENTRIES (1 TO "CNT"-1).
; EACH ENTRY IS "SIZ" BYTES IN LENGTH.  THE "PGCT" FIELD
; CONTAINS 3 SUBFIELDS.  BIT 7 IS THE "FREE" FLAG (0=ACTIVE,1=FREE)
; BIT 6 IS THE "FIXED" FLAG (0=FLOATING BUFFER,1=FIXED BUFFER)
; BITS 5 THRU 0 CONTAIN THE PAGE COUNT OF AN "ACTIVE" ENTRY
; (0=>1 PAGE,63=>64 PAGES DECIMAL).  THE "XBYTE" FIELD CONTAINS
; THE PROPER XBYTE OF AN "ACTIVE" ENTRY.  THE "ADRH" FIELD
; CONTAINS THE HIGH BYTE OF THE BUFFER ADDRESS.  IF THE
; BUFFER ENTRY IS "FLOATING", THEN THE "SEG" FIELD CONTAINS THE
; SEGMENT NUMBER AND THE LOW BYTE OF THE BUFFER ADDRESS IS
; ASSUMMED TO BE ZERO.
;
; THUS, THE FOLLOWING RESTRICTIONS APPLY TO BUFFERS:
;
; (1) MAXIMUM BUFFER LENGTH IS 64 PAGES (16K)
; (2) "FLOATING" BUFFERS ALWAYS BEGIN ON A PAGE BOUNDARY
;     "FIXED" BUFFERS DO NOT.
; (3) BUFFERS ARE ALWAYS AN INTEGRAL NUMBER OF PAGES IN LENGTH
; (4) BUFFERS ALWAYS RESIDE IN THE 32K BANK MEMORY REGION,
;     A LIMITATION OF FIND.SEG (MEMORY MANAGER)
; (5) MAXIMUM NUMBER OF BUFFERS = 16; ENTRY 0 IS NOT USED.
;
;***************************************************************************************************
;
; BUFFER TABLE
;
BUF_SIZ      =          5
BUF_CNT      =          17
BUF_TBL:     .RES       BUF_SIZ*BUF_CNT
PGCT_T       =          BUF_TBL
XBYTE_T      =          PGCT_T+BUF_CNT
ADRH_T       =          XBYTE_T+BUF_CNT
SEG_T        =          ADRH_T+BUF_CNT
ADRL_T       =          SEG_T
CHK_T        =          ADRL_T+BUF_CNT
ISFIXED      =          $40
ISFREE       =          $80
;
; BUFFER REFERENCE TABLE
;
; FIRST BYTE IS COUNT, FOLLOWED BY "COUNT" BUFFER #S.
; THIS TABLE IS A LIST OF ALL BUFFERS REFERENCED DURING ONE
; SOS SYSTEM CALL.  BUFFER #S ARE ADDED TO THIS LIST BY
; GETBUFADR AND REMOVED BY CHKSUM.
;
BUFREF_CNT   =          17
BUFREF:      .RES       BUFREF_CNT
ZPAGEX:      .RES       1
;PAGE
;***************************************************************************************************
;
; REQBUF
;
; INPUT:  PAGE.CNT (A)
; OUTPUT: BUFNUM   (A)
; ERROR:  "BUFFER TABLE FULL" - SYSERR
;         "OUT OF MEMORY"     - SYSERR
;         "BAD BUFFER SIZE"   - SYSDEATH
;
; THIS ROUTINE FINDS A FREE ENTRY IN THE BUFFER TABLE
; AND THEN CALLS FIND.SEG (MMGR) TO OBTAIN MEMORY FOR IT.
; IF MEMORY IS FOUND THEN THE BUFFER ENTRY IS MARKED "ACTIVE"
; AND THE BUFFER INFO IS INSERTED INTO THE ENTRY
;
;***************************************************************************************************
;
REQBUF       =          *
;
; IF REQUESTED PGCT OUT OF BOUNDS THEN FATAL ERR
;
             TAY
             BEQ        RQB_ERR2                               ; FATAL ERR, INVALID BUFFER SIZE
             CPY        #MAXPGCT+1
             BCS        RQB_ERR2                               ; FATAL ERR, INVALID BUFFER SIZE
             STY        RQB_PGCT                               ; SAVE PAGE COUNT
;
; FIND FREE ENTRY IN BUF.TBL
;
             JSR        GETFREE
             BCS        RQB_ERR                                ; ERR, BUFFER TABLE FULL
             STX        RQB_BNUM
;
; FIND PGCT*256 BYTES OF FREE MEMORY
;
             LDA        RQB_PGCT
             JSR        FSEG
             BCS        RQB_ERR1                               ; ERR, OUT OF MEMORY
;
; INSERT PGCT, XBYTE, ADRH, SEG#, CHK BYTE IN BUF.TBL(BUF#)
;
             LDX        RQB_BNUM
             DEC        RQB_PGCT                               ; PAGE COUNT FIELD
             LDA        RQB_PGCT
             STA        PGCT_T,X
;
             LDX        F_BASEX                                ; XBYTE & ADRH FIELDS
             LDY        F_BASEX+1
             JSR        CNVRT_ADR
             CPX        #$8F
             BNE        RQB010
             LDX        #$7F                                   ; IF XBYTE=$8F THEN XBYTE:=$7F
RQB010:      TXA
             LDX        RQB_BNUM
             STA        XBYTE_T,X
             TYA
             STA        ADRH_T,X
;
             LDA        F_NUMX                                 ; SEG# FIELD
             STA        SEG_T,X
;
             LDA        #0                                     ; INIT CHECK BYTE TO NULL
             STA        CHK_T,X
;
             TXA                                               ; RETURN BUF#
             CLC
             RTS                                               ; NORMAL EXIT
;
;
RQB_ERR:     LDA        #BUFTBLFULL
             JSR        SYSERR
;
RQB_ERR1:    LDA        #OUTOFMEM
             JSR        SYSERR
;
RQB_ERR2:    LDA        #BADBUFSIZ
             JSR        SYSDEATH
;PAGE
;***************************************************************************************************
;
; REQFXBUF
;
; INPUT:  PAGE.CNT (A)
; OUTPUT: BUFNUM   (A)
; ERROR:  "BUFFER TABLE FULL"            - SYSERR
;         "BAD SYSTEM.BUF PARM ADDRESS"  - SYSERR
;         "BAD BUFFER SIZE"              - SYSDEATH
;
; THIS ROUTINE COMPUTES THE ACTUAL BUFFER ADDRESS IN THE OPEN
; CALL (PARM "OPEN.LIST"), AND ALLOCATES A BUFFER ENTRY FOR IT.
; NOTE:  THE SYSBUF PARAMETER MUST BE AN EXTENDED INDIRECT PTR!!
;
;***************************************************************************************************
;
REQFXBUF     =          *
;
; IF REQUESTED PGCT OUT OF BOUNDS THEN FATAL ERR
;
             TAY
             BEQ        RQFB_ERR2                              ; FATAL ERR, BAD BUFFER SIZE
             CPY        #MAXPGCT+1
             BCS        RQFB_ERR2                              ; FATAL ERR, BAD BUFFER SIZE
;
             STY        RQFB_PGCT                              ; SAVE PAGE COUNT
;
; GET A FREE BUFFER ENTRY
;
             JSR        GETFREE
             BCS        RQFB_ERR                               ; ERR, BUFFER TABLE FULL
             STX        RQFB_BNUM                              ; SAVE BUF#
;
; FETCH SYSTEM.BUF PARAMETER IN OPEN SYSTEM CALL
;
             LDY        #3
             LDA        (OPEN_LIST),Y
             BNE        RQFB_ERR1                              ; ERR, SYSBUF ADR
             DEY
             LDA        (OPEN_LIST),Y
             TAY
             LDA        CXPAGE+1,Y
             BPL        RQFB_ERR1                              ; ERR, SYSBUF ADR
             CMP        #$8F
             BCS        RQFB_ERR1                              ; ERR, SYSBUF ADR
;
; INSERT XBYTE, ADRH, ADRL, PGCT, CHK BYTE INTO BUF.TBL(BUF#)
;
             LDX        RQFB_BNUM
             STA        XBYTE_T,X
;
             LDA        CZPAGE+1,Y
             BEQ        RQFB_ERR1                              ; ERR SYSBUF ADR
             CMP        #$81                                   ; CHECK FOR ADDRESS COMPENSATION
             BCC        RQFB010
             INC        XBYTE_T,X
             AND        #$7F
RQFB010:     STA        ADRH_T,X
;
             LDA        CZPAGE,Y
             STA        ADRL_T,X
;
             DEC        RQFB_PGCT
             LDA        RQFB_PGCT
             ORA        #ISFIXED
             STA        PGCT_T,X                               ; BUFFER ENTRY NOW "ACTIVE"
;
             LDA        #0                                     ; INIT CHECK BYTE TO NULL
             STA        CHK_T,X
;
             TXA                                               ; RETURN BUF#
             CLC
             RTS                                               ; NORMAL EXIT
;
RQFB_ERR:    LDA        #BUFTBLFULL
             JSR        SYSERR
;
RQFB_ERR1:   LDA        #BADSYSBUF
             JSR        SYSERR
;
RQFB_ERR2:   LDA        #BADBUFSIZ
             JSR        SYSDEATH
;PAGE
;***************************************************************************************************
;
; GETBUFADR
;
; INPUT:  BUFNUM   (A)
;         ZPAGELOC (X)
; OUTPUT: BUF ADR AT: X,X+1 & SXPAGE+1,X
;         PAGE.CNT (A)
;         BUFNUM   (Y)
;
; ERROR:  "BADBUFNUM" SYSDEATH
;
;***************************************************************************************************
;
GETBUFADR    =          *
;
; IF BUF# OUT OF RANGE OR BUF.TBL(BUF#)=FREE
; THEN FATAL ERR
;
             TAY
             BEQ        GTBF_ERR                               ; BUF#=0, FATAL ERR
             CPY        #BUF_CNT
             BCS        GTBF_ERR                               ; BUF# > MAX BUF TABLE ENTRY, FATAL ERR
             LDA        PGCT_T,Y
             BMI        GTBF_ERR                               ; BUF ENTRY MARKED "FREE", FATAL ERR
;
; OTHERWISE, CONSTRUCT BUFFER PTR ON SOS ZPAGE
;
             JSR        GETBUFADR1
;
; IF BUFFER NOT PREVIOUSLY REFERENCED ON THIS SOS CALL AND CHECK BYTE <> 0
;    THEN COMPARE FIRST BYTE OF BUFFER WITH CHECK BYTE IN BUFFER TABLE.
;         IF NO MATCH THEN KILL SYSTEM.
;
             STX        ZPAGEX
             TYA
             LDX        BUFREF
             BEQ        GTBF020                                ; BUFREF EMPTY
;
GTBF010:     CMP        BUFREF,X                               ; SEARCH FOR PREVIOUS REFERENCE
             BEQ        GTBF030                                ; MATCH FOUND
             DEX
             BNE        GTBF010
;
GTBF020:     INC        BUFREF                                 ; LOG BUF # IN BUFREF TABLE
             LDX        BUFREF
             CPX        #BUFREF_CNT
             BCS        GTBF_ERR                               ; BUFREF TABLE OVFLOW, KILL SYSTEM
             STA        BUFREF,X
;
             LDA        CHK_T,Y
             BEQ        GTBF030                                ; NO CHECK BYTE, SKIP CHECK
             LDX        ZPAGEX
             LDA        ($0,X)                                 ; COMPARE FIRST BYTE OF BUFFER
             CMP        CHK_T,Y                                ; WITH CHECK BYTE IN BUF TABLE
             BNE        GTBF_ERR                               ; NO MATCH, PULL THE PLUG
;
; RETURN PAGE.CNT TO CALLER
;
GTBF030:     LDA        PGCT_T,Y
             AND        #$3F                                   ; STRIP OFF FREE,FIXED FLAGS
             CLC
             ADC        #1
;
             CLC
             RTS
;
;
GTBF_ERR:    LDA        #BADBUFNUM
             JSR        SYSDEATH
;
;
;***************************************************************************************************
;
; GETBUFADR1
;
; INPUT: PGCT.T(BUF#)  (A)
;        ZPAGELOC      (X)
;        BUF#          (Y)
; ERROR: NONE.
;
; EXTRACTS THE BUFFER POINTER FROM THE BUFFER TABLE AND
; PLACES IT ON ZERO PAGE AT X, X+1 & SXPAGE+1,X
;
;***************************************************************************************************
;
GETBUFADR1   =          *
             AND        #$40
             BNE        GTB1010
             LDA        #0                                     ; "FIXED" BUFFER
             BEQ        GTB1020                                ; ALWAYS TAKEN
GTB1010:     LDA        ADRL_T,Y                               ; "FLOATING" BUFFER
GTB1020:     STA        0,X
             LDA        ADRH_T,Y
             STA        1,X
             LDA        XBYTE_T,Y
             ORA        #$80                                   ; ENSURE $7F->$8F
             STA        SXPAGE+1,X
             RTS
;PAGE
;***************************************************************************************************
;
; CHKBUF
;
; CHECK BUFFER.  FETCHES THE FIRST BYTE OF EACH BUFFER
; REFERENCED DURING THE CURRENT SYSTEM CALL AND PLACES IT
; IN CHK.T(BUF#).
;
; INPUT:  BUFREF TABLE
;         BUFFER TABLE
; OUTPUT: EMPTY BUFREF TABLE
;         BUFFER TABLE'S CHECK BYTES UPDATED
;         Z REG:=$18
; ERROR:  NONE.
;
;***************************************************************************************************
;
CHKBUF       =          *
             LDY        BUFREF                                 ; PICK UP COUNT
             BEQ        CHKB_EXIT                              ; EXIT IF BUFREF EMPTY
;
             LDA        #$18                                   ; ENSURE SOS ZPAGE SWITCHED IN
             STA        Z_REG
;
; UPDATE THE CHECK BYTE OF EACH BUF# IN THE BUFREF TABLE
;
CHKB010:     LDX        #<SOURCE
             LDA        BUFREF,Y
             TAY
             LDA        PGCT_T,Y
             JSR        GETBUFADR1                             ; PUT BUF#S ADR ON ZPAGE
             LDA        ($0,X)
             STA        CHK_T,Y
             DEC        BUFREF
             LDY        BUFREF
             BNE        CHKB010                                ; IF COUNT<>0 THEN PROCESS NEXT BUF# IN BUFREF TABLE
;
CHKB_EXIT:   RTS                                               ; BUFREF TABLE IS EMPTY (COUNT=0)
;PAGE
;***************************************************************************************************
;
; RELBUF
;
; INPUT:  BUFNUM   (A)
; OUTPUT: NONE.
; ERROR:  "BADBUFNUM" SYSDEATH
;
; THIS ROUTINE RELEASES THE BUFFER ENTRY, CALLS FIND.SEG TO
; RELEASE THE CORRESPONDING MEMORY SEGMENT, AND CALLS
; BUFCOMPACT TO PERFORM BUFFER COMPACTION.
;
;***************************************************************************************************
;
RELBUF       =          *
;
; IF BUF# OUT OF RANGE OR BUF.TBL(BUF#)=FREE
; THEN FATAL ERR
;
             TAY
             BEQ        RLBF_ERR
             CPY        #BUF_CNT
             BCS        RLBF_ERR
             LDA        PGCT_T,Y
             BMI        RLBF_ERR
;
; MARK BUF.TBL(BUF#)=FREE
;
             ORA        #ISFREE
             STA        PGCT_T,Y
;
; IF BUF.TBL(BUF#)=FIXED THEN EXIT
;
             AND        #ISFIXED
             BNE        RLBF_EXIT
;
; OTHERWISE CALL MEMORY MGR TO RELEASE BUFFER'S MEMORY SEG
;
             LDA        #RELSEG
             STA        REQCODE
;
             LDA        SEG_T,Y
             STA        RLS_NUM
;
             JSR        MMGR
             BCS        RLBF_ERR                               ; ANY ERR IS FATAL
;
; AND COMPACT BUFFERS
;
             JSR        BUFCOMPACT
;
RLBF_EXIT:   CLC
             RTS
;
RLBF_ERR:    LDA        #BADBUFNUM
             JSR        SYSDEATH
;PAGE
;***************************************************************************************************
;
; BUFCOMPACT
;
; THIS ROUTINE IS RESPONSIBLE FOR PACKING ALL SOS BUFFERS UP
; AGAINST THE HIGHEST AVAILABLE FREE MEMORY.  COULD IMPROVE THE
; EFFICIENCY OF THIS COMPACTION CYCLE BY NOT RELEASING THE "RELEASED" BUFFER
; UNTIL IT IS KNOWN THAT ANOTHER BUFFER WILL NOT BE MOVED INTO ITS LOC.
;
;***************************************************************************************************
;
BUFCOMPACT   =          *
;
; FIND THE FLOATING BUFFER IN BUF.TBL WITH THE LOWEST ADDRESS.
;
BUFC010:     LDY        #0
             LDX        #BUF_CNT-1
;
BUFC020:     LDA        PGCT_T,X
             AND        #$C0                                   ; STRIP OUT PAGE COUNT BITS
             BNE        BUFC030
;
             LDA        ADRH_T,X
             CMP        ADRH_T,Y
             LDA        XBYTE_T,X
             SBC        XBYTE_T,Y
             BCS        BUFC030
;
             TXA                                               ; SMALLER BUFFER FOUND, SAVE IN Y
             TAY
;
BUFC030:     DEX
             BNE        BUFC020
;
; IF NO BUFFER FOUND THEN DONE
;
             TYA
             BNE        BUFC040
             JMP        BUFC_EXIT
BUFC040:     STY        BUFC_BNUM                              ; OTHERWISE SAVE BUF# IN Y REG_
;
; CALL FIND.SEG:  FINDS HIGHEST AVAILABLE FREE MEMORY
;
             LDA        PGCT_T,Y
             AND        #$3F                                   ; STRIP OUT "FREE","FIXED" FLAGS
             CLC
             ADC        #1
             JSR        FSEG
             BCS        BUFC_EXIT                              ; DONE IF NO FREE SEG FOUND
;
; CONVERT BASE.BKPG TO BUFFER ADR
;
             LDX        F_BASEX                                ; BASE BANK
             LDY        F_BASEX+1                              ; BASE PAGE
             JSR        CNVRT_ADR
             STX        F_BASEX                                ; XBYTE
             STY        F_BASEX+1                              ; ADRH
;
; IF NEW SEG'S BASE < CURRENT BUFFER'S BASE ADR THEN DONE
;
             LDY        BUFC_BNUM
             LDA        ADRH_T,Y
             STA        SOURCE+1
             CMP        F_BASEX+1
             LDA        XBYTE_T,Y
             STA        SXPAGE+SOURCE+1
             SBC        F_BASEX
             BCS        BUFC_EXIT1
;
; MOVE DATA FROM CURRENT BUFFER TO NEW BUFFER
;
             LDX        F_BASEX
             STX        SXPAGE+DEST+1
             LDY        F_BASEX+1
             STY        DEST+1
             LDA        #0
             STA        SOURCE
             STA        DEST
;
             TAY
             LDX        F_PGCTX
BUFC200:     LDA        (SOURCE),Y                             ; MOVE LOOP
             STA        (DEST),Y
             DEY
             BNE        BUFC200
             INC        SOURCE+1
             INC        DEST+1
             DEX
             BNE        BUFC200
;
; UPDATE BUF.TBL(BUF#)
;
             LDY        BUFC_BNUM
             LDA        F_BASEX
             STA        XBYTE_T,Y
             LDA        F_BASEX+1
             STA        ADRH_T,Y
;
             LDX        SEG_T,Y
             LDA        F_NUMX
             STA        SEG_T,Y
;
; AND RELEASE OLD MEMORY SEGMENT
;
             STX        RLS_NUM
             LDA        #RELSEG
             STA        REQCODE
             JSR        MMGR
             BCS        BUFC_ERR
;
             JMP        BUFC010                                ; REPEAT COMPACTION CYCLE
;
;
BUFC_EXIT1:  LDX        F_NUMX                                 ; DONE,
             STX        RLS_NUM                                ; RELEASE SEG BEFORE EXIT
             LDA        #RELSEG
             STA        REQCODE
             JSR        MMGR
             BCS        BUFC_ERR
;
BUFC_EXIT:   LDA        #0
             STA        SERR                                   ; MASK OUT ANY ERROR FROM MEMORY MGR
             CLC
             RTS                                               ; NORMAL EXIT
;
;
BUFC_ERR:    LDA        #BADBUFNUM
             JSR        SYSDEATH
;PAGE
;***************************************************************************************************
;
; FSEG
;
; INPUT:  PAGE.CNT (A)
; OUTPUT: PAGE.CNT (A) UNCHANGED IF FIND.SEG SUCCESSFUL
; ERROR:  CARRY SET "UNABLE TO FIND MEMORY SEG OF PAGE.CNT*256 BYTES"
;
; THIS ROUTINE BUILDS THE PARAMETERS FOR A FIND.SEG SYSTEM CALL
; AND THEN CALLS THE MEMORY MANAGER.
;
;***************************************************************************************************
;
FSEG         =          *
;
; SETUP INPUT PARAMETERS FOR FIND.SEG CALL
;
             STA        F_PGCTX
             LDA        #FINDSEG
             STA        REQCODE
             LDA        #2
             STA        SRCHMODE
             LDA        #4
             STA        F_ID
;
; SETUP OUTPUT PARAMETER ADRESSES
;
             LDA        #<F_PGCTX
             STA        F_PGCT
             LDA        #>F_PGCTX
             STA        F_PGCT+1
             LDA        #<F_BASEX
             STA        F_BASE
             LDA        #>F_BASEX
             STA        F_BASE+1
             LDA        #<F_LIMX
             STA        F_LIM
             LDA        #>F_LIMX
             STA        F_LIM+1
             LDA        #<F_NUMX
             STA        F_NUM
             LDA        #>F_NUMX
             STA        F_NUM+1
;
             LDA        #0
             STA        F_PGCTX+1
             STA        SXPAGE+F_PGCT+1
             STA        SXPAGE+F_BASE+1
             STA        SXPAGE+F_LIM+1
             STA        SXPAGE+F_NUM+1
;
             JSR        MMGR
             LDA        F_PGCTX
;
             RTS                                               ; EXIT_  CARRY SET->ERR
;PAGE
;***************************************************************************************************
;
; GETFREE
;
; INPUT:  NONE
; OUTPUT: BUF# (X)
; ERROR:  "BUFTBLFULL" SYSERR
;
; THIS ROUTINE SEARCHES THE BUFFER TABLE, LOOKING FOR A FREE
; ENTRY.  IF FOUND, IT RETURNS THE BUFFER NUMBER, ELSE ERROR.
;
;***************************************************************************************************
;
GETFREE      =          *
             LDX        #BUF_CNT-1
GFR010:      LDA        PGCT_T,X
             BMI        GFR_EXIT                               ; FREE ENTRY FOUND
             DEX
             BNE        GFR010
;
             LDA        #BUFTBLFULL
             JSR        SYSERR                                 ; ERR EXIT
;
GFR_EXIT:    CLC
             RTS                                               ; NORMAL EXIT
;PAGE
;***************************************************************************************************
;
; CNVRT.ADR
;
; INPUT:  BANK VALUE (X)
;         PAGE VALUE (Y)
; OUTPUT: XBYTE (X)
;         ADRH  (Y)
; ERROR:  NONE.
;
; THIS ROUTINE CONVERTS A BASE.BKPG PARM (MMGR) INTO A
; VIRTUAL POINTER
;
;***************************************************************************************************
;
CNVRT_ADR    =          *
;
; IF PAGE <> $20 THEN GOTO L2
;
             CPY        #$20
             BNE        CNVA020
;
; IF BANK <> 0 THEN GOTO L1
;
             TXA
             BNE        CNVA010
;
; XBYTE=$8F
; ADRH:=PAGE
;
             LDX        #$8F
             BMI        CNVA_EXIT
;
; L1: XBYTE:=(BANK-1) ORA #$80
;     ADRH:=#$80
;
CNVA010:     ORA        #$80
             TAX
             DEX
             LDY        #$80
             BMI        CNVA_EXIT
;
; L2: XBYTE:=BANK ORA #$80
;     ADRH:=ADRH-#$20
;
CNVA020:     TXA
             ORA        #$80
             TAX
             SEC
             TYA
             SBC        #$20
             TAY
;
CNVA_EXIT:   RTS
;
;LST ON
ZZEND        =          *
ZZLEN        =          ZZEND-ZZORG
             .IF        ZZLEN-LENBUFMG
             .FATAL     "SOSORG FILE IS INCORRECT FOR BUFMGR"
             .ENDIF

