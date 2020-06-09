;SBTL "SOS 1_1  MEMORY MANAGER"
;.RELOC
              .SEGMENT   "CODE"
              .INCLUDE   "SOSORG"
              .ORG       ORGMEMMG
ZZORG         =          *
;MSB OFF
;***************************************************************************************************
;          COPYRIGHT (C) APPLE COMPUTER INC. 1980
;                    ALL RIGHTS RESERVED
;***************************************************************************************************
;
; MEMORY MANAGER (VERSION = 1.1O   )
;                (DATE    = 8/04/81)
;
; THIS MODULE CONTAINS ALL OF THE MEMORY MANAGEMENT SYSTEM
; CALLS SUPPORTED BY THE SARA OPERATING SYSTEM.  IT IS
; ALSO CALLED BY THE BUFFER MANAGER.
;
;***************************************************************************************************
;
              .EXPORT    MMGR
;
              .EXPORT    ST_CNT: ZP
              .EXPORT    ST_ENTRY
              .EXPORT    ST_FREE
              .EXPORT    ST_FLINK
              .EXPORT    VRT_LIM: ABSOLUTE
;
              .IMPORT    SYSERR
              .IMPORTZP  BADSCNUM
              .IMPORTZP  BADBKPG
              .IMPORTZP  SEGRQDN
              .IMPORTZP  SEGTBLFULL
              .IMPORTZP  BADSEGNUM
              .IMPORTZP  SEGNOTFND
              .IMPORTZP  BADSRCHMODE
              .IMPORTZP  BADCHGMODE
              .IMPORTZP  BADPGCNT
;PAGE
;***************************************************************************************************
;
; SEGMENT TABLE
; (NOTE: ENTRY 0 IS NOT USED)
;
;***************************************************************************************************
;
ST_FREE:      .RES       1                                      ; PTR TO FIRST FREE SEG TABLE ENTRY
ST_ENTRY:     .RES       1                                      ; PTR TO HIGHEST ALLOC SEG TABLE ENTRY
ST_SIZ        =          7
ST_CNT        =          32
ST_TBL:       .RES       ST_SIZ*ST_CNT
ST_BLINK      =          ST_TBL                                 ; BACK LINK TO PREV ALLOC SEG ENTRY
ST_FLINK      =          ST_BLINK+ST_CNT                        ; FORWARD LINK     "
ST_BASEL      =          ST_FLINK+ST_CNT                        ; BASE BANK/PAGE
ST_BASEH      =          ST_BASEL+ST_CNT
ST_LIML       =          ST_BASEH+ST_CNT                        ; LIMIT BANK/PAGE
ST_LIMH       =          ST_LIML+ST_CNT
ST_ID         =          ST_LIMH+ST_CNT                         ; SEG ID
;PAGE
;***************************************************************************************************
;
; DATA DECLARATIONS
;
;***************************************************************************************************
;
ZPAGE         =          $40                                    ; BEGINNING OF ZPAGE TEMP SPACE FOR MEMORY MANAGER
VRT_BASE      =          $0                                     ; INTERNAL BK/PG PTR TO LOWEST VIRT PAGE
VRT_LIM       =          ZPAGE+$0                               ; &$1, INTERNAL BK/PG PTR TO HIGHEST VIRT PAGE
PHY1BASE      =          $0780                                  ; BANK "F",PAGE "0"
PHY1LIM       =          $079F                                  ; BANK "F",PAGE "1F"
PHY2BASE      =          $0820                                  ; BANK "10",PAGE "A0"
PHY2LIM       =          $087F                                  ; BANK "10",PAGE "FF"
;
; REQUEST.SEG DATA DECLARATIONS
;
M_TPARMX      =          $60                                    ; BEGINNING ADDRESS OF MMGR SOS CALL PARMS
M_RQCODE      =          M_TPARMX
RQ_BASE       =          M_TPARMX+1                             ; BASE_BANK/PAGE
RQ_LIM        =          M_TPARMX+3                             ; LIMIT_BANK/PAGE
RQ_ID         =          M_TPARMX+5
RQ_NUM        =          M_TPARMX+6
;
RQ_REGION     =          ZPAGE+$2                               ;VRT(0),PHY0(1),PHY1(2)
;
; FIND.SEG DATA DECLARATIONS
;
SRCHMODE      =          M_TPARMX+1                             ; SEARCH MODE (0,1,2)
F_ID          =          M_TPARMX+2                             ; SEG ID
F_PGCT        =          M_TPARMX+3                             ; PAGE COUNT (LO
FX_PGCT       =          ZPAGE+$3                               ; &$4, INTERNAL PAGE COUNT
F_BASE        =          M_TPARMX+5                             ; BASE_BANK/PAGE
F_LIM         =          M_TPARMX+7                             ; LIMIT_BANK/PAGE
F_NUM         =          M_TPARMX+9                             ; SEG NUM
F_ERR         =          ZPAGE+$5                               ; ERROR FLAG
TRUE          =          $80
FALSE         =          $0
CFS_PGCT      =          ZPAGE+$6                               ; &7, CURRENT FREE SEGMENT'S PAGE COUNT
CFS_BASE      =          ZPAGE+$8                               ; &9,        "             BASE_BANK/PAGE
CFS_LIM       =          ZPAGE+$A                               ; &$B,       "             LIMIT_BANK/PAGE
CFS_BLINK     =          ZPAGE+$C                               ;          "             BACK LINK
CFS_BASE0     =          ZPAGE+$D                               ; &$E,     "             BASE (SMODE=0)
CFS_BASE1     =          ZPAGE+$F                               ; &$10,     "             BASE (SMODE=1)
CFS_NEXT      =          ZPAGE+$11                              ;          "             NEXT ENTRY
CFS_PREV      =          ZPAGE+$12                              ;          "             PREV ENTRY
CFS_PTR       =          ZPAGE+$13                              ; &$14     "             POINTER TO NXT FREE PG
BFS_PGCT      =          ZPAGE+$15                              ; &$16, BIGGEST FREE SEGMENT'S PAGE COUNT
BFS_BASE      =          ZPAGE+$17                              ; &$18     "             BASE_BANK/PAGE
BFS_LIM       =          ZPAGE+$19                              ; &$1A     "             LIMIT_BANK/PAGE
BFS_BLINK     =          ZPAGE+$1B                              ;          "             BACK LINK
;
; CHANGE.SEG DATA DECLARATIONS
;
CHG_NUM       =          M_TPARMX+1                             ; SEGNUM PARM
CHG_MODE      =          M_TPARMX+2                             ; CHANGE MODE PARM
CHG_PGCT      =          M_TPARMX+3                             ; PAGE COUNT PARM
CHG_PGCTX     =          ZPAGE+$1C                              ; &$1D, INTERNAL STORE FOR PGCT
CHG_NEW       =          ZPAGE+$1E                              ; &$1F, BANK/PAGE OF SEG'S NEW LIMIT OR BASE
;
; GET.SEG.INFO DATA DECLARATIONS
;
GSI_NUM       =          M_TPARMX+1
GSI_BASE      =          M_TPARMX+2
GSI_LIM       =          M_TPARMX+4
GSI_PGCT      =          M_TPARMX+6
GSI_ID        =          M_TPARMX+8
;
; GET.SEG.NUM DATA DECLARATIONS
;
GSN_BKPG      =          M_TPARMX+1
GSN_NUM       =          M_TPARMX+3
;
; RELEASE.SEG DATA DECLARATIONS
;
RLS_NUM       =          M_TPARMX+1                             ; SEG NUM
;
; REGION - DATA DECLARATIONS
;
RGN_BKPG:     .RES       2                                      ; TEMP CONTAINER FOR BANK/PAGE
;PAGE
;***************************************************************************************************
;
; MMGR
;
; THIS ROUTINE IS THE MAIN ENTRANCE TO THE MEMORY MANAGER
; MODULE.  IT FUNCTIONS AS A SWITCH, BASED UPON THE RECEIVED
; REQUEST CODE, TO TRANSFER CONTROL TO THE ROUTINE THAT
; HANDLES THE SPECIFIC SYSTEM CALL.
;
;***************************************************************************************************
;
MMGR          =          *
              LDA        M_RQCODE
              BEQ        MMGR010                                ; "REQ_SEG"
              CMP        #1
              BEQ        MMGR020                                ; "FIND_SEG"
              CMP        #2
              BEQ        MMGR030                                ; "CHANGE_SEG"
              CMP        #3
              BEQ        MMGR040                                ; "GET_SEG_INFO"
              CMP        #4
              BEQ        MMGR050                                ; "GET_SEG_NUM"
              CMP        #5
              BEQ        MMGR060                                ; "RELEASE_SEG"
;
              LDA        #BADSCNUM
              JSR        SYSERR
;
MMGR010:      JMP        REQ_SEG
MMGR020:      JMP        FIND_SEG
MMGR030:      JMP        CHG_SEG
MMGR040:      JMP        GET_SEG_INFO
MMGR050:      JMP        GET_SEG_NUM
MMGR060:      JMP        RELEASE_SEG
;PAGE
;***************************************************************************************************
;
; REQUEST.SEG(IN.BASE.BANKPAGE,LIMIT.BANKPAGE,SEGID; OUT.SEGNUM)
;
;***************************************************************************************************
;
REQ_SEG       =          *
;
; CONVERT CALLER'S BASE.BANK/PAGE TO INTERNAL FMT
;
              LDX        RQ_BASE
              LDY        RQ_BASE+1
              JSR        CNVRT_IBP
              BCC        RQ005
;
RQ_ERR:       RTS                                               ; ERR EXIT - INVALID BANK/PAGE
;
RQ005:        STX        RQ_BASE
              STY        RQ_BASE+1
              STA        RQ_REGION
;
; CONVERT CALLER'S LIMIT.BANK/PAGE TO INTERNAL FMT
;
              LDX        RQ_LIM
              LDY        RQ_LIM+1
              JSR        CNVRT_IBP
              BCS        RQ_ERR                                 ; ERR - INVALID BANK/PAGE
              STX        RQ_LIM
              STY        RQ_LIM+1
;
; IF BASE AND LIMIT ARE IN DIFFERENT REGIONS THEN ERR
;
              CMP        RQ_REGION
              BNE        RQ_ERR1                                ; ERR - INVALID BANK/PAGE PAIR
; IF CALLER'S BASE > LIMIT THEN ERR
;
              LDA        RQ_LIM
              CMP        RQ_BASE
              LDA        RQ_LIM+1
              SBC        RQ_BASE+1
              BCC        RQ_ERR1                                ; ERR - INVALID BANK/PAGE PAIR
;
; PREV SEGNUM:=NULL; NEXT SEGNUM:=FIRST ENTRY
;
              LDX        #0
              LDY        ST_ENTRY                               ; NOTE: PREV/NEXT CARRIED IN X & Y REGISTERS
;
; IF NO SEGS IN SEG TABLE THEN ALLOCATE REQUESTED SEG
;
              BEQ        RQ030
;
; IF FIRST SEG IN SEG TABLE BELOW REQUESTED SEG
; THEN ALLOCATE SEG
;
              LDA        ST_LIML,Y
              CMP        RQ_BASE
              LDA        ST_LIMH,Y
              SBC        RQ_BASE+1
              BCC        RQ030
;
; ADVANCE TO NEXT SEG ENTRY
;
RQ010:        TYA
              TAX
              LDA        ST_FLINK,Y
              TAY
;
; IF THERE IS NO NEXT SEG ENTRY
;   IF REQUESTED SEG IS BELOW PREV SEG
;      THEN ALLOCATE REQ SEG
;      ELSE ERR
;
              BNE        RQ020
              LDA        RQ_LIM
              CMP        ST_BASEL,X
              LDA        RQ_LIM+1
              SBC        ST_BASEH,X
              BCC        RQ030
;
              BCS        RQ_ERR2                                ; ERR - SEGMENT REQUEST DENIED
;
; IF REQUESTED LIMIT >= PREV SEG'S BASE THEN ERR
;
RQ020:        LDA        RQ_LIM
              CMP        ST_BASEL,X
              LDA        RQ_LIM+1
              SBC        ST_BASEH,X
              BCS        RQ_ERR2                                ; ERR - SEGMENT REQUEST DENIED
;
; IF REQUESTED BASE > NEXT SEG'S LIMIT
;    THEN ALLOCATE REQUESTED SEGMENT
;
              LDA        ST_LIML,Y
              CMP        RQ_BASE
              LDA        ST_LIMH,Y
              SBC        RQ_BASE+1
              BCS        RQ010                                  ; NO, ADVANCE TO NEXT SEGMENT
;
RQ030:        TXA                                               ; ALLOCATE REQUESTED SEGMENT
              JSR        GET_FREE
              BCS        RQ_ERR3                                ; ERR - SEG TABLE FULL
;
; ENTER BASE,LIMIT AND ID IN NEW SEG ENTRY
;
              TAX
              LDA        RQ_BASE
              STA        ST_BASEL,X
              LDA        RQ_BASE+1
              STA        ST_BASEH,X
;
              LDA        RQ_LIM
              STA        ST_LIML,X
              LDA        RQ_LIM+1
              STA        ST_LIMH,X
;
              LDA        RQ_ID
              STA        ST_ID,X
;
; RETURN NEW SEG NUM TO CALLER AND RETURN
;
              LDY        #0
              TXA
              STA        (RQ_NUM),Y
;
              CLC
              RTS                                               ; NORMAL EXIT
;
RQ_ERR1:      LDA        #BADBKPG
              JSR        SYSERR                                 ; ERR EXIT
RQ_ERR2:      LDA        #SEGRQDN
              JSR        SYSERR                                 ; ERR EXIT
;
RQ_ERR3:      LDA        #SEGTBLFULL
              JSR        SYSERR                                 ; ERR EXIT
;PAGE
;***************************************************************************************************
;
; FIND.SEG(IN.SRCHMODE,SEGID; INOUT.PAGECT;
;          OUT.BASE.BKPG,LIMIT.BKPG,SEGNUM)
;
;***************************************************************************************************
;
FIND_SEG      =          *
;
; RETRIEVE PAGE COUNT PARAMETER AND CLEAR ERR FLAG
;
              LDY        #0
              LDA        (F_PGCT),Y
              STA        FX_PGCT
              INY
              LDA        (F_PGCT),Y
              STA        FX_PGCT+1
;
              BNE        FIND001
              LDA        FX_PGCT
              BNE        FIND001
              LDA        #BADPGCNT                              ; ERR, PAGECT=0, EXIT
              JSR        SYSERR
;
FIND001:      LDA        #FALSE
              STA        F_ERR
;
; IF SEARCH MODE>2 THEN ERR
;
              LDA        SRCHMODE
              CMP        #3
              BCC        FIND005
              LDA        #BADSRCHMODE
              JSR        SYSERR                                 ; ERR EXIT
;
; INITIALIZE NEXT FREE SEGMENT SUBROUTINE,
; AND BIGGEST FREE SEGMENT PAGE COUNT
;
FIND005:      JSR        NXTFRSEG_I
              LDA        #0
              STA        BFS_PGCT
              STA        BFS_PGCT+1
;
; GET NEXT FREE SEGMENT
;
FIND010:      JSR        NXTFRSEG
              BCC        FIND015                                ; PROCESS FREE SEGMENT
;
; NO MORE FREE SEGMENTS LEFT
; RETURN BIGGEST FREE SEGMENT FOUND
; ALONG WITH ERR
;
              LDA        #TRUE
              STA        F_ERR
              LDX        #0                                     ; SEG#:=0
              JMP        FIND070
;
; FREE SEGMENT FOUND.
;   IF FREE SEGMENT > BIGGEST FREE SEGMENT THEN BFS:=CFS
;
FIND015:      LDA        BFS_PGCT
              CMP        CFS_PGCT
              LDA        BFS_PGCT+1
              SBC        CFS_PGCT+1
              BCS        FIND030
;
              LDX        #6
FIND020:      LDA        CFS_PGCT,X
              STA        BFS_PGCT,X
              DEX
              BPL        FIND020
;
; IF BFS.PGCT<F.PGCT THEN GET NEXT FREE SEGMENT
;
FIND030:      LDA        BFS_PGCT
              CMP        FX_PGCT
              LDA        BFS_PGCT+1
              SBC        FX_PGCT+1
              BCC        FIND010
;
; BFS.BASE:=BFS.LIM-FX.PGCT+1
; BFS.PGCT:=FX.PGCT
;
              LDA        BFS_LIM
              SBC        FX_PGCT
              STA        BFS_BASE
              LDA        BFS_LIM+1
              SBC        FX_PGCT+1
              STA        BFS_BASE+1
              INC        BFS_BASE
              BNE        FIND050
              INC        BFS_BASE+1
;
FIND050:      LDA        FX_PGCT
              STA        BFS_PGCT
              LDA        FX_PGCT+1
              STA        BFS_PGCT+1
;
; DELINK ENTRY FROM FREE LIST, AND LINK
; IT INTO SEGMENT LIST
;
              LDA        BFS_BLINK
              JSR        GET_FREE
              BCC        FIND060
              RTS                                               ; ERR - SEG TABLE FULL
;
; ST.ID(NEW):=F.ID
; ST.BASE(NEW):=BFS.BASE
; ST.LIM(NEW):=BFS.LIM
;
FIND060:      TAX
              LDA        F_ID
              STA        ST_ID,X
;
              LDA        BFS_BASE
              STA        ST_BASEL,X
              LDA        BFS_BASE+1
              STA        ST_BASEH,X
;
              LDA        BFS_LIM
              STA        ST_LIML,X
              LDA        BFS_LIM+1
              STA        ST_LIMH,X
;
; RETURN SEGNUM, PAGE COUNT, BASE BANK/PAGE, AND LIMIT BANK/PAGE
; TO CALLER
FIND070:      LDY        #0
              TXA
              STA        (F_NUM),Y
;
              LDA        BFS_PGCT
              STA        (F_PGCT),Y
              INY
              LDA        BFS_PGCT+1
              STA        (F_PGCT),Y
;
              LDX        BFS_BASE
              LDY        BFS_BASE+1
              JSR        CNVRT_XBP
              TYA
              LDY        #1
              STA        (F_BASE),Y
              DEY
              TXA
              STA        (F_BASE),Y
;
              LDX        BFS_LIM
              LDY        BFS_LIM+1
              JSR        CNVRT_XBP
              TYA
              LDY        #1
              STA        (F_LIM),Y
              DEY
              TXA
              STA        (F_LIM),Y
;
              LDA        F_ERR                                  ; IF ERR FLAG TRUE THEN REPORT IT_
              BNE        FIND_ERR
;
              CLC
              RTS                                               ; NORMAL EXIT
;
FIND_ERR:     LDA        #SEGRQDN
              JSR        SYSERR                                 ; ERR EXIT
;PAGE
;***************************************************************************************************
;
; NEXT FREE SEGMENT - INITIALIZATION
;
; INPUT:  SEGMENT TABLE
; OUTPUT: CFS.PTR "1ST FREE BANK/PAGE IN VIRTUAL MEMORY
;         CFS.PREV "PREVIOUS SEGMENT EXAMINED"
;         CFS.NEXT "SEGMENT FOLLOWING CFS.PREV"
; ERROR:  NONE (IF NO FREE BK/PG FOUND, THEN CFS.PTR="FFFF")
;
;***************************************************************************************************
;
NXTFRSEG_I    =          *
;
; CFS.PTR := VRT.LIM
; CFS.PREV := 0
; CFS.NEXT := ST.ENTRY
;
              LDA        A:VRT_LIM
              STA        CFS_PTR
              LDA        A:VRT_LIM+1
              STA        CFS_PTR+1
;
              LDA        #0
              STA        CFS_PREV
;
              LDX        ST_ENTRY
              STX        CFS_NEXT
;
; L0:  IF CFS.NEXT=0 THEN DONE
;
FRSGI010:     BEQ        FRSGI_EXIT
;
; IF ST.LIM(CFS.NEXT)<=VRT.LIM THEN GOTO L1
;
              LDA        A:VRT_LIM
              CMP        ST_LIML,X
              LDA        A:VRT_LIM+1
              SBC        ST_LIMH,X
              BCS        FRSGI020
;
; CFS.PREV:=CFS.NEXT
; CFS.NEXT:=ST.FLINK(CFS.NEXT)
; GOTO L0
;
              STX        CFS_PREV
              LDA        ST_FLINK,X
              TAX
              STX        CFS_NEXT
              JMP        FRSGI010
;
; L1:  IF ST.LIM(CFS.NEXT)<VRT.LIM THEN DONE
;
FRSGI020:     LDA        ST_LIML,X
              CMP        A:VRT_LIM
              LDA        ST_LIMH,X
              SBC        A:VRT_LIM+1
              BCC        FRSGI_EXIT
;
;
              JSR        NXTFRPG
;
FRSGI_EXIT:   RTS                                               ; NORMAL EXIT
;PAGE
;***************************************************************************************************
;
; NEXT FREE SEGMENT
;
; INPUT:  SEG TABLE
; OUTPUT: CFS.BLINK
;         CFS.BASE
;         CFS.LIMIT
;         CFS.PGCT
; OWN:    CFS.PREV
;         CFS.NEXT
;         CFS.PTR
;
; BUILDS A CANDIDATE FREE SEGMENT, WHOSE LIMIT BANK/PAGE =
; THE CURRENT FREE PAGE (CFS.PTR).
;
;***************************************************************************************************
;
NXTFRSEG      =          *
;
; IF CFS.PTR="FFFF" THEN EXIT
;
              LDA        CFS_PTR+1
              BPL        FRSG010
;
              SEC
              RTS                                               ; EXIT - NO MORE FREE SEGMENTS LEFT
;
; CFS.BLINK:=CFS.PREV
; CFS.LIM:=CFS.PTR
;
FRSG010:      LDA        CFS_PREV
              STA        CFS_BLINK
;
              LDA        CFS_PTR
              STA        CFS_LIM
              LDA        CFS_PTR+1
              STA        CFS_LIM+1
;
; IF CFS.NEXT=0 THEN CFS.BASE:=0
;    ELSE CFS.BASE:=ST.LIM(CFS.NEXT)+1
;
              LDA        CFS_NEXT
              BNE        FRSG020
              LDA        #0
              STA        CFS_BASE
              STA        CFS_BASE+1
              BEQ        FRSG030
;
FRSG020:      LDX        CFS_NEXT
              CLC
              LDA        ST_LIML,X
              ADC        #1
              STA        CFS_BASE
              LDA        ST_LIMH,X
              ADC        #0
              STA        CFS_BASE+1
;
; CFS.BASE0:=CFS.LIM AND $FF80
;
FRSG030:      LDY        CFS_LIM+1
              STY        CFS_BASE0+1
              LDA        CFS_LIM
              AND        #$80
              STA        CFS_BASE0
;
; CFS.BASE1:=CFS.BASE0-32K
;
              SEC
              SBC        #$80
              STA        CFS_BASE1
              TYA
              SBC        #0
              STA        CFS_BASE1+1
              BCS        FRSG035
              LDA        #0
              STA        CFS_BASE1
              STA        CFS_BASE1+1
;
; IF CFS.BASE>=CFS.BASE0 THEN GOTO L1
;
FRSG035:      LDA        CFS_BASE
              CMP        CFS_BASE0
              LDA        CFS_BASE+1
              SBC        CFS_BASE0+1
              BCS        FRSG050
;
; IF SEARCH MODE=0 THEN CFS.BASE:=CFS.BASE0
; GOTO L1
;
              LDA        SRCHMODE
              BNE        FRSG040
              LDA        CFS_BASE0
              STA        CFS_BASE
              LDA        CFS_BASE0+1
              STA        CFS_BASE+1
              JMP        FRSG050
;
; IF CFS.BASE<CFS.BASE1 AND SEARCH MODE=1
;    THEN CFS.BASE:=CFS.BASE1
;
FRSG040:      LDA        CFS_BASE
              CMP        CFS_BASE1
              LDA        CFS_BASE+1
              SBC        CFS_BASE1+1
              BCS        FRSG050
;
              LDA        SRCHMODE
              CMP        #1
              BNE        FRSG050
;
              LDA        CFS_BASE1
              STA        CFS_BASE
              LDA        CFS_BASE1+1
              STA        CFS_BASE+1
;
; L1:  CFS.PGCT:=CFS.LIM-CFS.BASE+1
;
FRSG050:      SEC
              LDA        CFS_LIM
              SBC        CFS_BASE
              STA        CFS_PGCT
              LDA        CFS_LIM+1
              SBC        CFS_BASE+1
              STA        CFS_PGCT+1
              INC        CFS_PGCT
              BNE        FRSG052
              INC        CFS_PGCT+1
;
; ADVANCE FREE PAGE POINTER TO NEXT FREE PAGE
;
; IF SEARCH MODE<>1 THEN L2:
;
FRSG052:      LDA        SRCHMODE
              CMP        #1
              BNE        FRSG060
;
; IF CFS.BASE < CFS.BASE0 THEN CFS.PTR:=CFS.BASE0-1
;
              LDA        CFS_BASE
              CMP        CFS_BASE0
              LDA        CFS_BASE+1
              SBC        CFS_BASE0+1
              BCS        FRSG060
;
              LDY        CFS_BASE0+1
              LDX        CFS_BASE0
              BNE        FRSG055
              DEY
FRSG055:      DEX
              STX        CFS_PTR
              STY        CFS_PTR+1
;
              JMP        FRSG070                                ; AND EXIT
; L2: CFS.PTR:=CFS.BASE-1
;
FRSG060:      SEC
              LDA        CFS_BASE
              SBC        #1
              STA        CFS_PTR
              LDA        CFS_BASE+1
              SBC        #0
              STA        CFS_PTR+1
;
; IF CFS.PTR="FFFF" OR CFS.NEXT=0 THEN EXIT
;
              BCC        FRSG070
              LDA        CFS_NEXT
              BEQ        FRSG070
;
; IF CFS.PTR > ST.LIM(CFS.NEXT) THEN EXIT
;
              LDX        CFS_NEXT
              LDA        ST_LIML,X
              CMP        CFS_PTR
              LDA        ST_LIMH,X
              SBC        CFS_PTR+1
              BCC        FRSG070
;
; OTHERWISE, ADVANCE CFS PTR TO NEXT FREE PAGE BELOW NEXT
; SEGMENT IN SEGMENT LIST
;
              JSR        NXTFRPG
;
FRSG070:      CLC
              RTS                                               ; EXIT - FREE SEGMENT FOUND
;PAGE
;***************************************************************************************************
;
; NEXT FREE PAGE
;
; "WALKS" THE FREE PAGE PTR (CFS.PTR) TO THE NEXT FREE PAGE
; IMMEDIATELY BELOW THE CURRENT FREE SEGMENT.
;
;***************************************************************************************************
;
NXTFRPG       =          *
;
; L0: CFS.PTR:=ST.BASE(CFS.NEXT)-1
;     IF CFS.PTR="FFFF" THEN DONE
;
              LDX        CFS_NEXT
              SEC
              LDA        ST_BASEL,X
              SBC        #1
              STA        CFS_PTR
              LDA        ST_BASEH,X
              SBC        #0
              STA        CFS_PTR+1
              BCC        NFRPG_EXIT
;
; CFS.PREV:=CFS.NEXT
; CFS.NEXT:=ST.FLINK(CFS.NEXT)
;
              STX        CFS_PREV
              LDA        ST_FLINK,X
              TAX
              STX        CFS_NEXT
;
; IF CFS.NEXT=0 OR ST.LIM(CFS.NEXT)<CFS.PTR
;    THEN DONE
;    ELSE GOTO L0
;
              BEQ        NFRPG_EXIT
              LDA        ST_LIML,X
              CMP        CFS_PTR
              LDA        ST_LIMH,X
              SBC        CFS_PTR+1
              BCS        NXTFRPG
;
NFRPG_EXIT:   RTS                                               ; NORMAL EXIT
;PAGE
;***************************************************************************************************
;
; CHANGE.SEG(IN.SEGNUM,CHG.MODE; INOUT.PAGECT) SYSTEM CALL
;
;***************************************************************************************************
;
CHG_SEG       =          *
;
; MOVE CALLER'S PAGE COUNT TO INTERNAL BUFFER
;
              LDY        #0
              LDA        (CHG_PGCT),Y
              STA        CHG_PGCTX
              INY
              LDA        (CHG_PGCT),Y
              STA        CHG_PGCTX+1
;
; IF SEG# OUT OF RANGE OR ST.FLINK(SEG#)=FREE THEN ERR
;
              LDX        CHG_NUM
              BEQ        CHGS_ERR
              CPX        #ST_CNT
              BCS        CHGS_ERR
              LDA        ST_FLINK,X
              BPL        CHGS005
;
CHGS_ERR:     LDA        #BADSEGNUM
              JSR        SYSERR                                 ; ERR EXIT
;***************************************************************************************************
; CASE OF CHANGE MODE
;***************************************************************************************************
CHGS005:      LDY        CHG_MODE
              CPY        #1
              BCC        CHGS010
              BEQ        CHGS020
              CPY        #3
              BCC        CHGS030
              BEQ        CHGS040
;
              LDA        #BADCHGMODE
              JSR        SYSERR                                 ; ERR EXIT
;PAGE
;***************************************************************************************************
; CHANGE MODE = 0(BASE UP)
;***************************************************************************************************
; CHG.NEW:=ST.BASE(SEG#)+PGCT
;
CHGS010:      CLC
              LDA        ST_BASEL,X
              ADC        CHG_PGCTX
              STA        CHG_NEW
              LDA        ST_BASEH,X
              ADC        CHG_PGCTX+1
              STA        CHG_NEW+1
;
              BCS        CHGS014                                ; OVERFLOW, PEG IT
;
; IF CHG.NEW <= ST.LIM(SEG#) THEN EXIT
;
              LDA        ST_LIML,X
              CMP        CHG_NEW
              LDA        ST_LIMH,X
              SBC        CHG_NEW+1
              BCS        CHGS016
;
; OTHERWISE, CHG.NEW:=ST.LIM(SEG#)
;
CHGS014:      LDA        ST_LIML,X
              STA        CHG_NEW
              LDA        ST_LIMH,X
              STA        CHG_NEW+1
;
CHGS016:      JMP        CHGS_EXIT
;***************************************************************************************************
; CHANGE MODE = 1(BASE DOWN)
;***************************************************************************************************
; CHG.NEW:=ST.BASE(SEG#)-PGCT
;
CHGS020:      SEC
              LDA        ST_BASEL,X
              SBC        CHG_PGCTX
              STA        CHG_NEW
              LDA        ST_BASEH,X
              SBC        CHG_PGCTX+1
              STA        CHG_NEW+1
              BCS        CHGS050
              BCC        CHGS052                                ; OVERFLOW, PEG IT
;***************************************************************************************************
; CHANGE MODE = 2(LIMIT UP)
;***************************************************************************************************
; CHG.NEW:=ST.LIM(SEG#)+PGCT
;
CHGS030:      CLC
              LDA        ST_LIML,X
              ADC        CHG_PGCTX
              STA        CHG_NEW
              LDA        ST_LIMH,X
              ADC        CHG_PGCTX+1
              STA        CHG_NEW+1
              BCC        CHGS050
              BCS        CHGS052                                ; OVERFLOW, PEG IT
;***************************************************************************************************
; CHANGE MODE = 3(LIMIT DOWN)
;***************************************************************************************************
; CHG.NEW:=ST.LIM(SEG#)-PGCT
;
CHGS040:      SEC
              LDA        ST_LIML,X
              SBC        CHG_PGCTX
              STA        CHG_NEW
              LDA        ST_LIMH,X
              SBC        CHG_PGCTX+1
              STA        CHG_NEW+1
              BCC        CHGS044                                ; OVERFLOW, PEG IT
;
; IF CHG.NEW >= ST.BASE(SEG#) THEN EXIT
;
              LDA        CHG_NEW
              CMP        ST_BASEL,X
              LDA        CHG_NEW+1
              SBC        ST_BASEH,X
              BCS        CHGS046
;
; OTHERWISE CHG.NEW:=ST.BASE(SEG#)
;
CHGS044:      LDA        ST_BASEL,X
              STA        CHG_NEW
              LDA        ST_BASEH,X
              STA        CHG_NEW+1
;
CHGS046:      JMP        CHGS_EXIT
;
; DETERMINE NEW BANK/PAGE'S REGION,
; IF NEW BANK/PAGE IS INVALID THEN
; SET TO BASE OR LIMIT (CASE CHANGE MODE)
;
CHGS050:      LDX        CHG_NEW
              LDY        CHG_NEW+1
              JSR        REGION
              BCS        CHGS052
              BNE        CHGS052
              BEQ        CHGS100
CHGS052:      LDA        CHG_MODE
              CMP        #1
              BNE        CHGS054
              LDX        #<VRT_BASE
              LDY        #>VRT_BASE
              JMP        CHGS056
CHGS054:      LDX        A:VRT_LIM
              LDY        A:VRT_LIM+1
CHGS056:      STX        CHG_NEW
              STY        CHG_NEW+1
;PAGE
;
; COMPUTE BANK/PAGE OF ADJACENT SEGMENT, IF ANY
;   CASE CHANGE MODE
;
CHGS100:      LDX        CHG_NUM
              LDA        CHG_MODE
              CMP        #1
              BNE        CHGS200
;   "1" IF ST.FLINK(SEG#)=0 THEN EXIT
              LDA        ST_FLINK,X
              BEQ        CHGS_EXIT
;       X,Y:=ST.LIM(ST.FLINK(SEG#))+1
              TAY
              LDA        ST_LIML,Y
              TAX
              LDA        ST_LIMH,Y
              TAY
              INX
              BNE        CHGS110
              INY
;       IF CHG.NEW < X,Y THEN CHG.NEW:=X,Y
CHGS110:      CPY        CHG_NEW+1
              BCC        CHGS_EXIT
              BEQ        CHGS120
              BCS        CHGS300
CHGS120:      CPX        CHG_NEW
              BCC        CHGS_EXIT
              BCS        CHGS300
;   "2" IF ST.BLINK(SEG#)=0 THEN EXIT
CHGS200:      LDA        ST_BLINK,X
              BEQ        CHGS_EXIT
;       X,Y:= ST.BASE(ST.BLINK(SEG#))-1
              TAY
              LDA        ST_BASEL,Y
              TAX
              LDA        ST_BASEH,Y
              TAY
              TXA
              BNE        CHGS210
              DEY
CHGS210:      DEX
;       IF CHG.NEW > X,Y THEN CHG.NEW:=X,Y
              CPY        CHG_NEW+1
              BCC        CHGS300
              BEQ        CHGS220
              BCS        CHGS_EXIT
CHGS220:      CPX        CHG_NEW
              BCS        CHGS_EXIT
;
CHGS300:      STX        CHG_NEW
              STY        CHG_NEW+1
;PAGE
;***************************************************************************************************
;
; COMPUTE DELTA PAGE COUNT AND RETURN IT TO CALLER
; (CASE OF CHG.MODE)
;
;***************************************************************************************************
CHGS_EXIT:    LDX        CHG_NUM
              LDY        #0
              LDA        CHG_MODE
              CMP        #1
              BCC        CHGS500
              BEQ        CHGS510
              CMP        #3
              BCC        CHGS520
              BEQ        CHGS530
;
; "0" -- PAGECOUNT:=NEW-BASE
;
CHGS500:      SEC
              LDA        CHG_NEW
              SBC        ST_BASEL,X
              STA        (CHG_PGCT),Y
              LDA        CHG_NEW+1
              SBC        ST_BASEH,X
              JMP        CHGS600
;
; "1" -- PAGECOUNT:=BASE-NEW
;
CHGS510:      SEC
              LDA        ST_BASEL,X
              SBC        CHG_NEW
              STA        (CHG_PGCT),Y
              LDA        ST_BASEH,X
              SBC        CHG_NEW+1
              JMP        CHGS600
;
; "2" -- PAGECOUNT:=NEW-LIM
;
CHGS520:      SEC
              LDA        CHG_NEW
              SBC        ST_LIML,X
              STA        (CHG_PGCT),Y
              LDA        CHG_NEW+1
              SBC        ST_LIMH,X
              JMP        CHGS600
;
; "3" -- PAGECOUNT:=LIM-NEW
;
CHGS530:      SEC
              LDA        ST_LIML,X
              SBC        CHG_NEW
              STA        (CHG_PGCT),Y
              LDA        ST_LIMH,X
              SBC        CHG_NEW+1
;
CHGS600:      INY
              STA        (CHG_PGCT),Y
;
; IF NEW PAGE COUNT < REQUESTED PAGECOUNT THEN ERR
;
              TAX
              DEY
              LDA        (CHG_PGCT),Y
              CMP        CHG_PGCTX
              TXA
              SBC        CHG_PGCTX+1
              BCS        CHGS610
              LDA        #SEGRQDN
              JSR        SYSERR                                 ; ERR EXIT
;
; OTHERWISE, ENTER CHG.NEW IN SEGMENT TABLE AND EXIT
;
CHGS610:      LDX        CHG_NUM
              LDA        CHG_MODE
              CMP        #2
              LDA        CHG_NEW
              LDY        CHG_NEW+1
              BCS        CHGS620
;
              STA        ST_BASEL,X
              TYA
              STA        ST_BASEH,X
              CLC
              RTS                                               ; NORMAL EXIT
;
;
CHGS620:      STA        ST_LIML,X
              TYA
              STA        ST_LIMH,X
              CLC
              RTS                                               ; NORMAL EXIT
;PAGE
;***************************************************************************************************
;
; GET.SEG.INFO(IN.SEGNUM; OUT.BASE.BKPG,LIMIT.BKPG,PGCT,SEGID)
;
;***************************************************************************************************
;
GET_SEG_INFO  =          *
;
; IF SEG# OUT OF BOUNDS OR ST.FLINK(SEG#)=ST.FREE THEN ERR
;
              LDX        GSI_NUM
              BEQ        GSI_ERR                                ; ERR - INVALID SEGNUM
              CPX        #ST_CNT
              BCS        GSI_ERR                                ; ERR - INVALID SEGNUM
              LDA        ST_FLINK,X
              BMI        GSI_ERR                                ; ERR - INVALID SEGNUM
;
; RETURN BASE.BKPG TO CALLER
;
              LDY        ST_BASEH,X
              LDA        ST_BASEL,X
              TAX
              JSR        CNVRT_XBP
              TYA
              LDY        #1
              STA        (GSI_BASE),Y
              DEY
              TXA
              STA        (GSI_BASE),Y
;
; RETURN LIMIT.BKPG TO CALLER
;
              LDX        GSI_NUM
              LDY        ST_LIMH,X
              LDA        ST_LIML,X
              TAX
              JSR        CNVRT_XBP
              TYA
              LDY        #1
              STA        (GSI_LIM),Y
              DEY
              TXA
              STA        (GSI_LIM),Y
;
; RETURN SEGID TO CALLER
;
              LDX        GSI_NUM
              LDA        ST_ID,X
              STA        (GSI_ID),Y
;
; COMPUTE PAGE COUNT
;
              SEC
              LDA        ST_LIML,X
              SBC        ST_BASEL,X
              TAY
              LDA        ST_LIMH,X
              SBC        ST_BASEH,X
              TAX
              INY
              BNE        GSI010
              INX
;
; RETURN PAGE COUNT TO CALLER
;
GSI010:       TYA
              LDY        #0
              STA        (GSI_PGCT),Y
              INY
              TXA
              STA        (GSI_PGCT),Y
;
              CLC
              RTS                                               ; NORMAL EXIT
;
GSI_ERR:      LDA        #BADSEGNUM
              JSR        SYSERR                                 ; ERR EXIT
;PAGE
;***************************************************************************************************
;
; GET.SEG.NUM(IN.BANKPAGE; OUT.SEGNUM) SYSTEM CALL
;
;
;***************************************************************************************************
;
GET_SEG_NUM   =          *
;
; CONVERT BANKPAGE TO INTERNAL FORMAT
;
              LDX        GSN_BKPG
              LDY        GSN_BKPG+1
              JSR        CNVRT_IBP
              BCS        GSN_ERR                                ; ERR - INVALID BANK PAGE
              STX        GSN_BKPG
              STY        GSN_BKPG+1
;
; QUIT IF NO ENTRIES IN SEG TABLE
;
              LDA        ST_ENTRY
              BEQ        GSN_ERR1                               ; ERR - SEG NOT FOUND
;
; L1: IF BANKPAGE>ST.LIM(SEG#) THEN ERR
;
GSN010:       TAX
              LDA        ST_LIML,X
              CMP        GSN_BKPG
              LDA        ST_LIMH,X
              SBC        GSN_BKPG+1
              BCC        GSN_ERR1                               ; ERR - SEG NOT FOUND
;
; IF BANKPAGE>=ST.BASE(SEG#) THEN FOUND!
;
              LDA        GSN_BKPG
              CMP        ST_BASEL,X
              LDA        GSN_BKPG+1
              SBC        ST_BASEH,X
              BCS        GSN020
;
; SEG#:=ST.FLINK(SEG#); GOTO L1
;
              LDA        ST_FLINK,X
              BEQ        GSN_ERR1                               ; ERR - SEG NOT FOUND
              JMP        GSN010
;
; RETURN SEG# TO CALLER
;
GSN020:       LDY        #0
              TXA
              STA        (GSN_NUM),Y
              CLC
              RTS                                               ; NORMAL EXIT
;
GSN_ERR:      RTS                                               ; ERROR EXIT
;
GSN_ERR1:     LDA        #SEGNOTFND
              JSR        SYSERR                                 ; ERROR EXIT
;PAGE
;***************************************************************************************************
;
; RELEASE.SEG(IN.SEGNUM) SYSTEM CALL
;
;***************************************************************************************************
;
RELEASE_SEG   =          *
;
; IF ST.FLINK(SEG#)=ST.FREE THEN ERR
;
              LDX        RLS_NUM
              BEQ        RLS_ALL                                ; RELEASE_SEG(SEG#=0)
              CPX        #ST_CNT
              BCS        RLS_ERR                                ; ERR - SEG# TOO LARGE
              LDA        ST_FLINK,X
              BMI        RLS_ERR                                ; ERR - INVALID SEGNUM
              BPL        REL_SEG                                ; RELEASE_SEG(SEG#<0)
;***************************************************************************************************
;
; RELEASE ALL
;
;***************************************************************************************************
RLS_ALL:      LDX        ST_ENTRY
              BEQ        RLS0_EXIT
              STX        RLS_NUM
;
RLS0_LOOP:    LDA        ST_ID,X
              CMP        #$10                                   ; CARRY SET/CLEARED HERE
;
              LDA        ST_FLINK,X
              PHA
              BCC        RLS006                                 ; IF ID=SYS SEG THEN SKIP
              JSR        REL_SEG                                ; RELEASE ONE SEGMENT
RLS006:       PLA
              BEQ        RLS0_EXIT
              STA        RLS_NUM
              TAX
              BNE        RLS0_LOOP                              ; ALWAYS TAKEN
;
RLS0_EXIT:    CLC
              RTS                                               ; NORMAL EXIT ; ALL NON SYSTEM SEGMENTS RELEASED_
;***************************************************************************************************
;
; REL SEG
;
;***************************************************************************************************
; Y:=ST.FLINK(SEG#)
; X:=ST.BLINK(SEG#)
;
REL_SEG:      TAY
              LDA        ST_BLINK,X
              TAX
;
; IF X<>0 THEN ST.FLINK(X):=Y
;         ELSE ST.ENTRY:=Y
;
              BEQ        RLS010
              TYA
              STA        ST_FLINK,X
              JMP        RLS020
RLS010:       STY        ST_ENTRY
;
; IF Y<>0 THEN ST.BLINK(Y):=X
;
              TYA
RLS020:       BEQ        RLS030
              TXA
              STA        ST_BLINK,Y
;
; ST.FLINK(SEG#):=ST.FREE
; ST.FREE:=SEG# AND #$80
;
RLS030:       LDA        ST_FREE
              LDX        RLS_NUM
              STA        ST_FLINK,X
              TXA
              ORA        #$80
              STA        ST_FREE
;
              CLC
              RTS                                               ; NORMAL EXIT
;
RLS_ERR:      LDA        #BADSEGNUM
              JSR        SYSERR                                 ; ERR EXIT
;PAGE
;***************************************************************************************************
;
; CONVERT INTERNAL BANK PAGE
;
; INPUT:  EXTERNAL BANK (X)
;            "     PAGE (Y)
; OUTPUT: INTERNAL BKPG LOW  (X)
;            "     BKPG HIGH (Y)
;         REGION (A) 0=>VIRT BANK
;                    1=>PHY BANK (0-$2000)
;                    2=>   "     ($A000-$FFFF)
; ERROR:  CARRY SET ("INVALID BANK PAGE")
;
;***************************************************************************************************
;
CNVRT_IBP     =          *
;
; CONVERT FROM EXTERNAL TO INTERNAL FORMAT
;
; CASE OF BANK:  ADD PAGE BIAS
;
              TYA
              CPX        #$F
              BEQ        CNVI010
              BCS        CNVI020
;
              CMP        #$20                                   ; BANK < "F"
              BCC        CNVI_ERR1
              CMP        #$A0
              BCS        CNVI_ERR1
              SEC
              SBC        #$20
              JMP        CNVI030
;
CNVI010:      CMP        #$20                                   ; BANK = "F"
              BCS        CNVI_ERR1
              CLC
              ADC        #$80
              JMP        CNVI030
;
CNVI020:      CPX        #$10                                   ; BANK = "10"
              BNE        CNVI_ERR1
              CMP        #$A0
              BCC        CNVI_ERR1
              SEC
              SBC        #$80
;
CNVI030:      TAY                                               ; SHIFT BANK RIGHT ONE BIT
              TXA                                               ; INTO HIGH BIT OF PAGE BYTE_
              LSR        A
              TAX
              TYA
              BCC        CNVI040
              ORA        #$80
;
; EXCHANGE X & Y
;
CNVI040:      PHA
              TXA
              TAY
              PLA
              TAX
;
; COMPUTE REGION (VIRT=0,PHY1=1,PHY2=2)
;
              JSR        REGION                                 ; REGION RETURNED IN A REG_
              BCS        CNVI_ERR1                              ; ERR - INVALID BANK PAGE
;
              RTS                                               ; NORMAL EXIT
;
CNVI_ERR1:    LDA        #BADBKPG
              JSR        SYSERR
;PAGE
;***************************************************************************************************
;
; CONVERT EXTERNAL BANK PAGE
;
; INPUT:  INTERNAL BKPG LOW  (X)
;             "         HIGH (Y)
; OUTPUT: EXTERNAL BANK (X)
;             "    PAGE (Y)
; ERROR:  NO ERROR CHECKING DONE.  ASSUMES THAT INTERNAL #S
; ARE VALID.
;
;***************************************************************************************************
;
CNVRT_XBP     =          *
;
; CONVERT FROM INTERNAL TO EXTERNAL FORMAT
;
              TXA
              ASL        A
              TXA
              AND        #$7F
              TAX
              TYA
              ROL        A
              TAY
;
; CASE OF BANK: ADD PAGE BIAS
;
              TXA
              CPY        #$F
              BEQ        CNVX020                                ; BANK = "F"
              BCS        CNVX010
;
              CLC                                               ; BANK < "F"
              ADC        #$20
              JMP        CNVX020
;
CNVX010:      CLC                                               ; BANK = "10"
              ADC        #$80
;
; EXCHANGE X & Y
;
CNVX020:      PHA
              TYA
              TAX
              PLA
              TAY
              RTS                                               ; NORMAL EXIT
;PAGE
;***************************************************************************************************
;
; REGION
;
; INPUT:  INTERNAL BKPG LOW  (X)
;             "         HIGH (Y)
; OUTPUT: REGION (A)
;         INTERNAL BKPG LOW  (X) UNCHANGED
;             "         HIGH (Y)    "
; ERROR:  CARRY SET ("INVALID BANK/PAGE")
;
;***************************************************************************************************
;
REGION        =          *
              STX        RGN_BKPG
              STY        RGN_BKPG+1
;
; IF BANKPAGE>PHY2LIM THEN ERR
;
              LDA        #<PHY2LIM
              CMP        RGN_BKPG
              LDA        #>PHY2LIM
              SBC        RGN_BKPG+1
              BCC        RGN_ERR                                ; ERR - INVALID BANK PAGE
;
; IF BANKPAGE>=PHY2BASE THEN REGION:=2
;
              LDA        RGN_BKPG
              CMP        #<PHY2BASE
              LDA        RGN_BKPG+1
              SBC        #>PHY2BASE
              BCC        RGN010
              LDA        #2
              BNE        RGN040
;
; IF BANKPAGE>PHY1LIMIT THEN ERR
;
RGN010:       LDA        #<PHY1LIM
              CMP        RGN_BKPG
              LDA        #>PHY1LIM
              SBC        RGN_BKPG+1
              BCC        RGN_ERR                                ; ERR - INVALID BANK PAGE
;
; IF BANKPAGE>=PHY1BASE THEN REGION:=1
;
              LDA        RGN_BKPG
              CMP        #<PHY1BASE
              LDA        RGN_BKPG+1
              SBC        #>PHY1BASE
              BCC        RGN020
              LDA        #1
              BNE        RGN040
;
; IF BANKPAGE>VIRTUAL LIMIT THEN ERR
;
RGN020:       LDA        A:VRT_LIM
              CMP        RGN_BKPG
              LDA        A:VRT_LIM+1
              SBC        RGN_BKPG+1
              BCC        RGN_ERR
              LDA        #0
;
RGN040:       CLC                                               ; "N" FLAG ALWAYS REFLECTS REGION VAL IN A REG!
              RTS                                               ; NORMAL EXIT
;
RGN_ERR:      SEC                                               ; INVALID BANK PAGE
              RTS
;PAGE
;***************************************************************************************************
;
; GET FREE
;
; INPUT:  PREVIOUS SEG # (A)
; OUTPUT: NEW SEG #      (A)
; ERROR:  CARRY SET ("SEG TBL FULL")
;
;***************************************************************************************************
;
GET_FREE      =          *
;
; SAVE PREV SEG # IN X
; NOTE: PREV SEG # CARRIED IN X
;       NEW SEG # CARRIED IN Y
;
              TAX
;
; IF NO FREE ENTRIES THEN ERR
;
              LDA        ST_FREE
              CMP        #$80
              BEQ        GTFR_ERR
;
; TURN OFF FREE FLAG (BIT7) AND DELINK FROM FREE LIST
;
              AND        #$7F
              TAY
              LDA        ST_FLINK,Y
              STA        ST_FREE
;
; IF PREV SEG # IS NULL THEN LINK NEW ENTRY TO START
; OF SEGMENT LIST
;
              CPX        #0
              BNE        GTFR010
              LDA        ST_ENTRY
              STA        ST_FLINK,Y
              LDA        #0
              STA        ST_BLINK,Y
              STY        ST_ENTRY
              JMP        GTFR020
;
; OTHERWISE LINK NEW ENTRY TO PREV SEG #
;
GTFR010:      LDA        ST_FLINK,X
              STA        ST_FLINK,Y
              TXA
              STA        ST_BLINK,Y
              TYA
              STA        ST_FLINK,X
;
; IF ST.FLINK(NEW)<>NULL THEN
;    ST.BLINK(ST.FLINK(NEW)):=NEWSEG #
GTFR020:      LDA        ST_FLINK,Y
              BEQ        GTFR030
              LDA        ST_FLINK,Y
              TAX
              TYA
              STA        ST_BLINK,X
;
; RETURN WITH NEW SEG #
;
GTFR030:      TYA
              CLC
              RTS                                               ; NORMAL EXIT
;
GTFR_ERR:     LDA        #SEGTBLFULL
              JSR        SYSERR
;
;LST ON
ZZEND         =          *
ZZLEN         =          ZZEND-ZZORG
              .IF        ZZLEN-LENMEMMG
              .FATAL     "SOSORG FILE IS INCORRECT FOR MEMMGR"
              .ENDIF


