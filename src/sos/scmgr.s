; Update ADR_HIGH to $BC00 to allow call from print driver routine
; SOS does not allow to call itself, this increases the address
; so it passes from the print driver routine
;
; Updates by Robert Justice
;
;SBTL "SOS 1_1 SYSTEM CALL MANAGER"
;.RELOC
             .SEGMENT   "CODE"
             .INCLUDE   "SOSORG"
             .ORG       ORGSCMGR
ZZORG        =          *
;MSB OFF
;***************************************************************************************************
;          COPYRIGHT (C) APPLE COMPUTER INC. 1980
;                    ALL RIGHTS RESERVED
;***************************************************************************************************
;
; SYSTEM CALL MANAGER (VERSION = 1.1O  )
;                     (DATE    = 8/04/81)
;
; THE SYSTEM CALL MANAGER:
; (1) RETRIEVE THE SYSCALL #,
; (2) DETERMINE THE LOCATION OF THE SYSTEM CALL PARMS AND
;     MOVE THEM TO THE SOS ZPAGE,
; (3) TRANSFER CONTROL TO THE APPROPRIATE INTERFACE MANAGER,
;     (FILE,DEVICE,UTILITY,MEMORY)
;
;***************************************************************************************************
;
             .EXPORT    SCMGR
;
             .IMPORT    FMGR
             .IMPORT    DMGR
             .IMPORT    UMGR
             .IMPORT    MMGR
             .IMPORT    DBUGBRK
;
             .IMPORT    SYSERR
             .IMPORT    SERR
             .IMPORTZP  BADSCNUM
             .IMPORTZP  BADCZPAGE
             .IMPORTZP  BADXBYTE
             .IMPORTZP  BADSCPCNT
             .IMPORTZP  BADSCBNDS
;
             .IMPORT    SZPAGE
             .IMPORT    SXPAGE
             .IMPORT    CZPAGE
             .IMPORT    CXPAGE
             .IMPORT    CSPAGE
;PAGE
;***************************************************************************************************
;
; SYSTEM CALL PARAMETER DEFINITION TABLES
;
; EACH ENTRY IS FOUR BYTES LONG.  THE FIRST BYTE CONTAINS THE
; NUMBER OF PARMS IN THE CALL.  THE REMAINING SIX NIBBLES, EACH
; DEFINE A PARAMETER IN THE CALL.  THE FIRST BIT OF THE
; NIBBLE DEFINES WHETHER THE PARM IS INPUT (0) OR OUTPUT (1).
; THE NEXT BIT DEFINES WHETHER THE PARM IS BY VALUE (0)
; OR BY REFERENCE (1).  THE FINAL TWO BITS SPECIFY THE
; PARM LENGTH IN BYTES (E.G. 0=LENGTH OF 1, 3=LENGTH OF 4 BYTES)
;
;***************************************************************************************************
;
;   FILE SYSTEM CALL DEFINITIONS
;
FSC_CNT      =          $13
FSC_TBL      =          *
             .BYTE      $3,$5D,$00,$00                        ; SCNUM=$C0 - CREATE
             .BYTE      $1,$50,$00,$00                        ;   "  =$C1 - DESTROY
             .BYTE      $2,$55,$00,$00                        ;   "  =$C2 - RENAME
             .BYTE      $3,$5D,$00,$00                        ;   "  =$C3 - SET_FILE_INFO
             .BYTE      $3,$5D,$00,$00                        ;   "  =$C4 - GET_FILE_INFO
             .BYTE      $4,$55,$99,$00                        ;   "  =$C5 - VOLUME
             .BYTE      $1,$50,$00,$00                        ;   "  =$C6 - SET_PREFIX
             .BYTE      $2,$50,$00,$00                        ;   "  =$C7 - GET_PREFIX
             .BYTE      $4,$58,$D0,$00                        ;   "  =$C8 - OPEN
             .BYTE      $3,$00,$00,$00                        ;   "  =$C9 - NEW_LINE
             .BYTE      $4,$05,$19,$00                        ;   "  =$CA - READ
             .BYTE      $3,$05,$10,$00                        ;   "  =$CB - WRITE
             .BYTE      $1,$00,$00,$00                        ;   "  =$CC - CLOSE
             .BYTE      $1,$00,$00,$00                        ;   "  =$CD - FLUSH
             .BYTE      $3,$00,$30,$00                        ;   "  =$CE - SET_MARK
             .BYTE      $2,$0B,$00,$00                        ;   "  =$CF - GET_MARK
             .BYTE      $3,$00,$30,$00                        ;   "  =$D0 - SET_EOF
             .BYTE      $2,$0B,$00,$00                        ;   "  =$D1 - GET_EOF
             .BYTE      $1,$00,$00,$00                        ;   "  =$D2 - SET_LEVEL
             .BYTE      $1,$80,$00,$00                        ;   "  =$D3 - GET_LEVEL
;PAGE
;
;   DEVICE SYSTEM CALL DEFINITIONS
;
DSC_CNT      =          5
DSC_TBL      =          *
             .BYTE      $5,$05,$11,$90                        ; SCNUM=$80 - D_READ
             .BYTE      $4,$05,$11,$00                        ;   "  =$81 - D_WRITE
             .BYTE      $3,$00,$50,$00                        ;   "  =$82 - D_STATUS
             .BYTE      $3,$00,$50,$00                        ;   "  =$83 - D_CONTROL
             .BYTE      $2,$58,$00,$00                        ;   "  =$84 - GET_DEV_NUM
             .BYTE      $4,$05,$D0,$00                        ;   "  =$85 - D_INFO
;
;   UTILITY SYSTEM CALL DEFINITIONS
;
USC_CNT      =          5
USC_TBL      =          *
             .BYTE      $1,$00,$00,$00                        ; SCNUM=$60 - SET_FENCE
             .BYTE      $1,$80,$00,$00                        ;   "  =$61 - GET_FENCE
             .BYTE      $1,$50,$00,$00                        ;   "  =$62 - SET_TIME
             .BYTE      $1,$50,$00,$00                        ;   "  =$63 - GET_TIME
             .BYTE      $2,$0B,$00,$00                        ;   "  =$64 - JOYSTICK
             .BYTE      $0,$00,$00,$00                        ;   "  =$65 - COLD_START
;
;   MEMORY SYSTEM CALL DEFINITIONS
;
MSC_CNT      =          5
MSC_TBL      =          *
             .BYTE      $4,$11,$08,$00                        ; SCNUM=$40 - REQUEST_SEG
             .BYTE      $6,$00,$99,$98                        ;   "  =$41 - FIND_SEG
             .BYTE      $3,$00,$90,$00                        ;   "  =$42 - CHANGE_SEG
             .BYTE      $5,$09,$99,$80                        ;   "  =$43 - GET_SEG_INFO
             .BYTE      $2,$18,$00,$00                        ;   "  =$44 - GET_SEG_NUM
             .BYTE      $1,$00,$00,$00                        ;   "  =$45 - RELEASE_SEG
;
;   DEBUG SYSTEM CALL DEFINITION
;
DBUG         =          $FE
;PAGE
;***************************************************************************************************
;
; DATA DECLARATIONS
;
;***************************************************************************************************
Z_REG        =          $FFD0
SP_SAVE      =          $01FF
Z_SAVE       =          $01FD
B_SAVE       =          $01FC
;
ADR_LOW      =          $2000                                 ; LOW    ADDRESS   (BOUNDS CHECKING)
ADR_HIGH     =          $BC00 ;B800  ;hack this to allow driver print to work     ; HIGH   ADDRESS
ADR_MID      =          $A000                                 ; MIDDLE ADDRESS
;
; SCMGR'S VARIABLES
;
SCM_VARS     =          $E0
SCNUM        =          SCM_VARS+0                            ; SYSTEM CALL NUMBER
SCRNUM       =          SCM_VARS+0                            ; SYSTEM CALL REQUEST NUMBER
SCPTR        =          SCM_VARS+1                            ;&2  SYSTEM CALL POINTER
MOVE_VARS    =          SCPTR+2                               ; !! (LOOKOUT) !!
;
;
F_TPARMX     =          $A0                                   ; FILE SYS CALL PARM START LOC
D_TPARMX     =          $C0                                   ; DEVICE SYS CALL PARM START LOC
U_TPARMX     =          $C0                                   ; UTILITY SYS CALL PARM START LOC
M_TPARMX     =          $60                                   ; MEMORY SYS CALL PARM START LOC
;
; MOVE.PARM'S VARIABLES
;
TPARMX       =          MOVE_VARS+0                           ; TARGET ADR OF SYS CALL PARMS
DFN_PTR      =          MOVE_VARS+1                           ;&2
DFN_PTRX     =          MOVE_VARS+3
SCPTRX       =          MOVE_VARS+4
RGHT_NIB     =          MOVE_VARS+5
SCT_DFN      =          MOVE_VARS+6
SCT_DCNT     =          MOVE_VARS+7
PARM_CNT     =          MOVE_VARS+8
;PAGE
;***************************************************************************************************
;
; SYSTEM CALL MANAGER
;
;***************************************************************************************************
;
SCMGR        =          *
             LDA        #>SZPAGE                              ; SET Z REG TO SOS ZPAGE
             STA        Z_REG
;
; SET SYSTEM X BYTES TO ABSOLUTE ADDRESS MODE.
;
             LDA        #0
             STA        SXPAGE+SCPTR+1
             STA        SERR                                  ; AND INIT SYSTEM ERR CODE
;
; CALLER'S Z REG MUST BE $1A !!
; (B REG NOT CHECKED)
;
             LDA        Z_SAVE
             CMP        #>CZPAGE
             BEQ        SCM005
             LDA        #BADCZPAGE
             JSR        SYSERR                                ; EXIT TO DISPATCHER
;
; RETRIEVE CALLER'S PC ON HIS STACK
;
SCM005:      LDX        SP_SAVE
             LDA        CSPAGE+6,X
             STA        SCPTR+1
             LDA        CSPAGE+5,X
             STA        SCPTR
             BNE        SCM010                                ; AND POINT IT TO SYS CALL NUM
             DEC        SCPTR+1
SCM010:      DEC        SCPTR
;
; ADVANCE CALLER'S PC ON HIS STACK.
;
             CLC
             LDA        CSPAGE+5,X
             ADC        #2
             STA        CSPAGE+5,X
             BCC        SCM020
             INC        CSPAGE+6,X
;
; RETRIEVE SYSTEM CALL NUMBER
;
SCM020:      LDY        #0
             LDA        (SCPTR),Y
             CMP        #DBUG
             BNE        SCM025
             JSR        DBUGBRK                               ; DEBUG SYSTEM CALL
SCM025:      STA        SCNUM
;
; RETRIEVE SYSTEM CALL PARAMETER ADDRESS
;
             INY
             LDX        #<SCPTR
             JSR        POINTER
             BCC        SCM030
             RTS                                              ; ERROR EXIT
;
; CASE INTERFACE CODE OF SYSTEM CALL NUMBER
;  (INTERFACE CODE STRIPPED, LEAVING REQUEST CODE)
;
SCM030:      LDA        #$20
             BIT        SCNUM
             BPL        SCM050
             LDA        SCNUM
             AND        #$3F
             STA        SCRNUM
             BVC        SCM040
;
             LDA        #F_TPARMX                             ; "11XXXXXX" - JMP TO FILE MANAGER_
             STA        TPARMX
             LDX        #<FSC_TBL
             LDY        #>FSC_TBL
             LDA        #FSC_CNT
             JSR        MOVE_PARMS
             BCS        SCM_ERR1                              ; ERR EXIT
             JMP        FMGR
;
SCM040:      LDA        #D_TPARMX                             ; "10XXXXXX" - JMP TO DEVICE MANAGER_
             STA        TPARMX
             LDX        #<DSC_TBL
             LDY        #>DSC_TBL
             LDA        #DSC_CNT
             JSR        MOVE_PARMS
             BCS        SCM_ERR1                              ; ERR EXIT
             JMP        DMGR
;
SCM050:      BVC        SCM_ERR
             PHP
             LDA        SCNUM
             AND        #$1F
             STA        SCRNUM
             PLP
             BEQ        SCM060
;
             LDA        #U_TPARMX                             ; "011XXXXX" - JMP TO UTILITY MANAGER_
             STA        TPARMX
             LDX        #<USC_TBL
             LDY        #>USC_TBL
             LDA        #USC_CNT
             JSR        MOVE_PARMS
             BCS        SCM_ERR1                              ; ERR EXIT
             JMP        UMGR
;
SCM060:      LDA        #M_TPARMX                             ; "010XXXXX" - JMP TO MEMORY MANAGER_
             STA        TPARMX
             LDX        #<MSC_TBL
             LDY        #>MSC_TBL
             LDA        #MSC_CNT
             JSR        MOVE_PARMS
             BCS        SCM_ERR1                              ; ERR EXIT
             JMP        MMGR
;
SCM_ERR:     LDA        #BADSCNUM                             ; ERROR, INVALID SYSTEM CALL NUMBER_
SCM_ERR1:    JSR        SYSERR                                ;   EXIT TO DISPATCHER ON ERROR
;PAGE
;***************************************************************************************************
;
; MOVE.PARMS
;
; MOVES THE CALLER'S PARAMETERS TO THE OPERATING SYSTEM'S
; ZERO PAGE, ACCORDING TO THE SPECIFICATIONS CONTAINED
; IN THE SPECIFIED SYS CALL DFN TABLE.
;
; INPUT: (A) = MAX # ENTRIES IN PARM DFN TABLE
;        (X) = PARM DFN TBL ADR (LO)
;        (Y) =         "        (HI)
;      SCPTR = ADR OF CALLER'S SYS CALL PARMS
; ERROR: CARRY SET (SYSERR)
;
;***************************************************************************************************
;
MOVE_PARMS   =          *
             STX        DFN_PTR                               ; SAVE ADR OF DEFINITION TABLE
             STY        DFN_PTR+1
;
; IF REQ NUM > MAX REQ NUM (A REG)
;
             CMP        SCRNUM
             BCS        MOVE010
;
;   THEN ERR(BAD SYS CALL NUM)
;
             LDA        #BADSCNUM
             BCC        SYSERR1                               ;BRANCH ALWAYS TAKEN
;
; CALCULATE DEFINITION TABLE INDEX
;  AND INIT SYS CALL PARM INDEX
;
MOVE010:     LDA        SCRNUM
             ASL        A
             ASL        A
             STA        DFN_PTRX
             LDA        #0
             STA        SXPAGE+DFN_PTR+1                      ; AND X BYTE
             STA        SCPTRX
;
; IF SCPTR(SCPTRX)<>DFN.PTR(DFN.PTRX) THEN ERR
;
             TAY
             LDA        (SCPTR),Y
             LDY        DFN_PTRX
             CMP        (DFN_PTR),Y
             BEQ        INITLOOPCT
;
             LDA        #BADSCPCNT                            ; ERR, CALLER'S PARM COUNT INVALID
SYSERR1:     JSR        SYSERR                                ; EXIT
;
; INIT LOOP CTR(PARM.CNT) TO # OF PARMS IN SYS CALL
;
INITLOOPCT:  STA        PARM_CNT
;
; ADVANCE PTRS
;
;
             INC        SCPTRX
             INC        DFN_PTRX
;
; MOVE REQ CODE TO SYS ZPAGE PARM LIST
;  AND ADVANCE SYS ZPAGE PTR (X=TPARMX)
;
             LDA        SCRNUM
             LDX        TPARMX
             STA        0,X
             INX
;
; INIT NIBBLE FLAG TO "RIGHT" NIBBLE
;  ZERO STATE="LEFT" NIBBLE
;
             LDA        #$FF
             STA        RGHT_NIB
;***************************************************************************************************
;
;  BEGIN PARAMETER PROCESSING LOOP
;
PARMLOOP:    LDA        RGHT_NIB
             EOR        #$FF                                  ; COMPLEMENT NIBBLE FLAG
             STA        RGHT_NIB
;
; IF "LEFT" NIBBLE
;
             BNE        ELSE_RNIB
;
; THEN FETCH SYS CALL PARM DFN
;  AND # OF BYTES IN PARM WITHIN IT
;
             LDY        DFN_PTRX
             LDA        (DFN_PTR),Y
             STA        SCT_DFN
             AND        #$30
             LSR        A
             LSR        A
             LSR        A
             LSR        A
             STA        SCT_DCNT
             BPL        VALUE                                 ;BRANCH ALWAYS
;
; ELSE FETCH SYS CALL PARM DFN
;  AND # OF BYTES IN PARM WITHIN IT
;  FROM "RIGHT" NIBBLE OF DFN BYTE
;
ELSE_RNIB:   LDA        SCT_DFN
             TAY
             AND        #$03
             STA        SCT_DCNT
             TYA
             ASL        A
             ASL        A
             ASL        A
             ASL        A
             STA        SCT_DFN
             INC        DFN_PTRX                              ; ADVANCE SYS CALL DFN PTR
;***************************************************************************************************
;
;  PARAMETER PASSED BY VALUE
;
;***************************************************************************************************
VALUE:       BIT        SCT_DFN
             BVS        REFERENCE
             BMI        VAL_OUT
;
;  INPUT BY VALUE
;
             LDY        SCPTRX                                ; MOVE BYTES TO ZPAGE
VAL_IN:      LDA        (SCPTR),Y
             STA        0,X
             INY
             INX
             DEC        SCT_DCNT
             BPL        VAL_IN
             STY        SCPTRX
             JMP        ENDLOOP1
;
;  OUTPUT BY VALUE
;
VAL_OUT:     CLC                                              ; BUILD PTR TO PARM ON ZPAGE
             LDA        SCPTR
             ADC        SCPTRX
             STA        0,X
             INX
             LDA        SCPTR+1
             ADC        #0
             STA        0,X
;
             CLC                                              ; ADVANCE INDEX TO NEXT PARM
             LDA        SCPTRX
             ADC        SCT_DCNT
             STA        SCPTRX
;
             LDA        SXPAGE+SCPTR+1                        ; INCLUDE X BYTE
             STA        SXPAGE,X
             JMP        ENDLOOP2
;***************************************************************************************************
;
;  PARAMETER PASSED BY REFERENCE
;
;***************************************************************************************************
REFERENCE:   BPL        REF1
;
; "LIST" PTR FOUND, CHK IF "LENGTH" PARM = 0
;
             LDY        SCPTRX
             INY
             INY
             LDA        (SCPTR),Y
             BEQ        ENDLOOP0                              ; "LENGTH" PARM=0, SKIP "LIST" PARM
;
REF1:        LDY        SCPTRX                                ; MOVE PTR TO ZPAGE
             JSR        POINTER
             BCS        PARM_ERR                              ; ERROR EXIT
;
; ADVANCE SYSTEM ZPAGE POINTER (X), CALLER'S PARM PTR.
; DECREMENT PARM CTR AND CHECK IF LAST PARM PROCESSED.
;
ENDLOOP0:    INX
             INC        SCPTRX
ENDLOOP2:    INX
             INC        SCPTRX
ENDLOOP1:    DEC        PARM_CNT
             BEQ        PARM_EXIT
             BMI        PARM_EXIT                             ;SPECIAL FOR 'COLD START'
             JMP        PARMLOOP
;
;  END OF PARAMETER PROCESSING LOOP
;
;***************************************************************************************************
;
PARM_EXIT:   CLC                                              ; NO ERRORS
PARM_ERR:    RTS                                              ; RETURN TO SYS CALL MANAGER
;PAGE
;***************************************************************************************************
;
; POINTER
;
; INPUT:   SRC ADR   (SCPTR),Y & (SCPTR),Y+1
;          DEST ADR  (X)
;
; OUTPUT:  SCPTR     UNCHANGED
;          X REG         "
;          A,Y REGS  FLATTENED
;
; ERROR:   CARRY SET (SYSERR)
;
; POINTER.  RETRIEVES THE CALLER'S POINTER PARAMETER IN
; (SCPTR),Y, PERFORMS ADDRESS COMPENSATION, IF NECESSARY
; AND PLACES THE RESULTING POINTER AT X, X+1 AND SXPAGE+1,X.
;
;***************************************************************************************************
;
POINTER      =          *
             LDA        (SCPTR),Y
             PHA
             INY
             LDA        (SCPTR),Y
             BEQ        INDIRECT
;
             STA        1,X                                   ; DIRECT POINTER
             PLA
             STA        0,X
             LDY        #0
             BEQ        PTR010
;
INDIRECT:    PLA                                              ; INDIRECT POINTER
             TAY
             LDA        CZPAGE,Y
             STA        0,X
             LDA        CZPAGE+1,Y
             STA        1,X
             LDA        CXPAGE+1,Y
             TAY
;
PTR010:      LDA        1,X
;
; CHECK BOUNDS OF CALLER'S POINTER PARAMETER
;
             CPY        #$8F
             BCC        PTR_X808E
             BEQ        PTR_X8F
             BCS        PTR_ERR1                              ; ERROR, INVALID X BYTE
PTR_X8F:     CMP        #>ADR_LOW
             BCC        PTR_ERR
             CMP        #>ADR_HIGH
             BCS        PTR_ERR
             BCC        PTR_EXIT
;
; X BYTE = 80..8E
;
PTR_X808E:   CPY        #$80
             BCC        PTR_X0
             CMP        #0
             BEQ        PTR_ERR
             CMP        #$FF
             BNE        PATCH
             INY                                              ; $8N:FFXX --> $8N+1:7FXX
             LDA        #$7F
             BNE        PTR_EXIT
;
; X BYTE = 0
;
PTR_X0:      CPY        #0
             BNE        PTR_ERR1
             CMP        #>ADR_LOW
             BCC        PTR_ERR
             CMP        #>ADR_HIGH
             BCS        PTR_ERR
             CMP        #>ADR_MID
             BCS        PTR_EXIT
;
             PHA
             LDA        B_SAVE
             AND        #$0F
             BNE        PTR030
             PLA                                              ; $B=0:2000__9FFF --> $8F:2000_9FFF
             LDY        #$8F
             BNE        PTR_EXIT
;
PTR030:      ORA        #$80                                  ; $B<>0:2000__9FFF --> $8B:0000__7FFF
             TAY
             PLA
             SEC
             SBC        #$20
             BNE        PATCH
             DEY                                              ; $8B:00XX --> $8B-1:80XX
             LDA        #$80
;
PATCH:       CPY        #$80                                  ; KLUDGE FOR BFM:  $8N:01XX --> $8N-1:81XX
             BCC        PTR_EXIT
             CMP        #1
             BNE        PTR_EXIT
             CPY        #$80
             BEQ        PTR_ERR                               ; ERROR, $80:01XX NOT ALLOWED
             DEY
             LDA        #$81
;
PTR_EXIT:    STA        1,X
             TYA
             STA        SXPAGE+1,X
             CLC
             RTS
;
;
PTR_ERR:     LDA        #BADSCBNDS
             JSR        SYSERR
PTR_ERR1:    LDA        #BADXBYTE
             JSR        SYSERR
;
;LST ON
ZZEND        =          *
ZZLEN        =          ZZEND-ZZORG
             .IF        ZZLEN-LENSCMGR
             .FATAL     "SOSORG FILE IS INCORRECT FOR SCMGR"
             .ENDIF

