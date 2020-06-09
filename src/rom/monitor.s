;*********************************************************
;* APPLE /// ROM - MONITOR
;* COPYRIGHT 1979 BY APPLE COMPUTER, INC.
;*********************************************************

           .setcpu "6502"
		   .segment "CODE"
		   
;           .PROC   MONITOR
           .ORG    $F7FE
;
;
RET1:      RTS
           .BYTE   $3F
           SBC     #01
           BEQ     RET1
           SBC     #01
           BEQ     RET1
           SBC     #01
           BEQ     RET1
           SBC     #01
           BEQ     RET1
           SBC     #01
           BEQ     RET1
           SBC     #01
           BEQ     RET1
           SBC     #01
           BEQ     RET1
           SBC     #01
           BEQ     RET1
           SBC     #01
           BEQ     RET1
           SBC     #01
           BEQ     RET1
           SBC     #01
           BEQ     RET1
           SBC     #01
           BEQ     RET1
           SBC     #01
           BEQ     RET1
           SBC     #01
           BEQ     RET1
           SBC     #01
           BEQ     RET1
           SBC     #01
           BEQ     RET1
           SBC     #01
           BEQ     RET1
           SBC     #01
           BEQ     RET1
           SBC     #01
           BEQ     RET1
           SBC     #01
           BEQ     RET1
           SBC     #01
           BEQ     RET1
           SBC     #01
           BEQ     RET1
           SBC     #01
           BEQ     RET1
           SBC     #01
           BEQ     RET1
           SBC     #01
           BEQ     RET1
           SBC     #01
           BEQ     RET1
           SBC     #01
           BEQ     RET1
           SBC     #01
           BEQ     RET1
           SBC     #01
           BEQ     RET1
           SBC     #01
           BEQ     RET1
           SBC     #01
           BEQ     RET1
           SBC     #01
           BEQ     RET3
           SBC     #01
 RET3:     BEQ     RET2
           SBC     #01
           BEQ     RET2
           SBC     #01
           BEQ     RET2
           SBC     #01
           BEQ     RET2
           SBC     #01
           BEQ     RET2
           SBC     #01
           BEQ     RET2
           SBC     #01
           BEQ     RET2
           SBC     #01
           BEQ     RET2
           SBC     #01
           BEQ     RET2
           SBC     #01
           BEQ     RET2
           SBC     #01
           BEQ     RET2
           SBC     #01
           BEQ     RET2
           SBC     #01
           BEQ     RET2
           SBC     #01
           BEQ     RET2
           SBC     #01
           BEQ     RET2
           SBC     #01
           BEQ     RET2
           SBC     #01
           BEQ     RET2
           SBC     #01
           BEQ     RET2
           SBC     #01
           BEQ     RET2
           SBC     #01
           BEQ     RET2
           SBC     #01
           BEQ     RET2
           SBC     #01
           BEQ     RET2
           SBC     #01
           BEQ     RET2
           SBC     #01
           BEQ     RET2
           SBC     #01
           BEQ     RET2
           SBC     #01
           BEQ     RET2
           SBC     #01
           BEQ     RET2
           SBC     #01
           BEQ     RET2
           SBC     #01
           BEQ     RET2
           SBC     #01
           BEQ     RET2
           SBC     #01
           BEQ     RET2
           SBC     #01
           BEQ     RET2
RET2:      RTS


SCRNLOC    =    $58

LMARGIN    =    SCRNLOC
RMARGIN    =    SCRNLOC+1
WINTOP     =    SCRNLOC+2
WINBTM     =    SCRNLOC+3
CH         =    SCRNLOC+4
CV         =    SCRNLOC+5
BAS4L      =    SCRNLOC+6
BAS4H      =    SCRNLOC+7
BAS8L      =    SCRNLOC+8
BAS8H      =    SCRNLOC+9
TBAS4L     =    SCRNLOC+$0A
TBAS4H     =    SCRNLOC+$0B
TBAS8L     =    SCRNLOC+$0C
TBAS8H     =    SCRNLOC+$0D
FORGND     =    SCRNLOC+$0E
BKGND      =    SCRNLOC+$0F
MODES      =    SCRNLOC+$10
CURSOR     =    SCRNLOC+$11
STACK      =    SCRNLOC+$12
PROMPT     =    SCRNLOC+$13
TEMPX      =    SCRNLOC+$14
TEMPY      =    SCRNLOC+$15
CSWL       =    SCRNLOC+$16
CSWH       =    SCRNLOC+$17
KSWL       =    SCRNLOC+$18
KSWH       =    SCRNLOC+$19
PCL        =    SCRNLOC+$1A
PCH        =    SCRNLOC+$1B
A1L        =    SCRNLOC+$1C
A1H        =    A1L+1
A2L        =    A1L+2
A2H        =    A1L+3
A3L        =    A1L+4
A3H        =    A1L+5
A4L        =    A1L+6
A4H        =    A1L+7
STATE      =    A1L+8
YSAV       =    A1L+9
INBUF      =    A1L+$0A
TEMP       =    A1L+$0C
MASK       =    CURSOR

KBD        =    $C000
KBDSTRB    =    $C010

USERADR    =    $3F8
BLOCKIO    =    $F479
RECON      =    $F686      ;      AS OF 12/20/1979
DIAGN      =    $F4EE
INBUFLEN   =    $50        ;      ONLY 80 BYTES      ($3A0-$3EF)
IBSLOT     =    $81
IBDRVN     =    IBSLOT+1
IBBUFP     =    IBSLOT+4
IBCMD      =    IBSLOT+6

ENTRY      =    *
           TSX
           STX     STACK
MON:       CLD                  ; MUST BE HEX MODE
           JSR     BELL
MONZ:      LDX     STACK        ; RESTORE STACK TO ORIGINAL LOCATION
           TXS
           LDA     #$DF         ; PROMPT (APPLE) FOR SARA MONITOR
           STA     PROMPT
           JSR     GETLNZ       ; GET A LINE OF INPUT
SCAN:      JSR     ZSTATE       ; SET REGULAR SCAN
NXTINP:    JSR     GETNUM       ; ATTEMPT TO READ HEX BYTE
           STY     YSAV         ; STORE CURRENT INPUT POINTER
           LDY     #$12         ; 18 COMMANDS
CMDSRCH:   DEY
           BMI     MON          ; GIVE UP IF UNRECOGNIZABLE
           CMP     CMDTAB,Y     ; FOUND?
           BNE     CMDSRCH      ; NO KEEP LOOKING
           JSR     TOSUB        ; PERFORM FUNCTION
           LDY     YSAV         ; GET NEXT POINTER
           JMP     NXTINP       ; DO NEXT COMMAND

GETNUM:    LDX     #00          ; CLEAR A2
           STX     A2L
           STX     A2H
NXTCHR:    LDA     (INBUF),Y
           INY                  ; BUMP INDEX FOR NEXT TIME
           EOR     #$B0
           CMP     #$0A         ; TEST FOR DIGIT
           BCC     DIGIT        ; SAVE IT IF 1-9
           ADC     #$88         ; TEST FOR HEX A-F
           CMP     #$FA
           BCC     DIGRET
DIGIT:     LDX     #03
           ASL     A
           ASL     A
           ASL     A
           ASL     A
NXTBIT:    ASL     A            ; SHIFT HEX DIGITS INTO A2
           ROL     A2L
           ROL     A2H
           DEX
           BPL     NXTBIT       ; SHIFTED ALL YET?
NXTBAS:    LDA     STATE
           BNE     NXTBS2       ; IF ZERO THEN COPY TO A1,3
           LDA     A2H,X
           STA     A1H,X
           STA     A3H,X
NXTBS2:    INX
           BEQ     NXTBAS
           BNE     NXTCHR
;
; SWITCH ROUTINE FOR CHARACTER
;
TOSUB:     LDA     #$FA         ; PUSH ADDRESS OR FUNCTION
           PHA                  ; AND RETURN IT
           LDA     CMDVEC,Y
           PHA
           LDA     STATE        ; PASS MODE VIA ACC.
ZSTATE:    LDY     #00
           STY     STATE        ; RESET STATE OF SCAN
DIGRET:    RTS
CMDTAB     =    *
           .BYTE   $00          ; G    =GP (CALL) SUBROUTINE
           .BYTE   $03          ; J    =JUMP (CONT) PROGRAM
           .BYTE   $06          ; M    =MOVE MEMORY
           .BYTE   $EB          ; R    =READ DISK BLOCK
           .BYTE   $EC          ; S    =MEMORY SEARCH
           .BYTE   $EE          ; U    =USER FUNCTION
           .BYTE   $EF          ; V    =VERIFY MEMORY BLOCKS
           .BYTE   $F0          ; W    =WRITE DISK BLOCK
           .BYTE   $F1          ; X    =REPEAT COMMAND LINE
           .BYTE   $99          ; SP   =SPACE (BYTE SEPARATOR)
           .BYTE   $9B          ; *    =ASCII (HI BIT ON)
           .BYTE   $A0          ; '    =ASCII (HI BIT OFF)
           .BYTE   $93          ; :    =SET STORE MODE
           .BYTE   $A7          ; .    =RANGE SEPARATOR
           .BYTE   $A8          ; /    =COMMAND SEPARATOR
           .BYTE   $95          ; <    =DEST/SOURCE SEPARATOR
           .BYTE   $C6          ; CR   =CARRIAGE RETURN

CMDVEC     =    *
           .BYTE   $90          ; GO-1
           .BYTE   $8E          ; JUMP-1
           .BYTE   $3F          ; MOVE-1
           .BYTE   $D3          ; READ-1
           .BYTE   $08          ; SEARCH-1
           .BYTE   $8B          ; USER-1
           .BYTE   $4E          ; VRFY-1
           .BYTE   $D6          ; WRTE-1
           .BYTE   $2C          ; REPEAT-1
           .BYTE   $B7          ; SPCE-1
           .BYTE   $1A          ; ASCII-1
           .BYTE   $1C          ; ASCII0-1
           .BYTE   $CB          ; SETMODE-1
           .BYTE   $CB          ; SETMODE-1
           .BYTE   $AD          ; SEP-1
           .BYTE   $A4          ; DEST-1
           .BYTE   $39          ; CRMON-1
;
;
NXTA4:     INC     A4L          ; BUMP 16 BIT POINTERS
           BNE     NXTA1
           INC     A4H
NXTA1:     INC	   A1L          ; BUMP AL
           BNE     TSTA1      
           INC     A1H
           SEC                  ; IN CASE OF ROLL OVER
           BEQ     RETA1
TSTA1:     LDA     A1L
           SEC
           SBC     A2L
           STA     TEMP
           LDA     A1H
           SBC     A2H
           ORA     TEMP
           BNE     RETA1        ; IF AL LESS THAN OR EQUAL TO A2
           CLC                  ; THEN CARRY CLEAR ON RETURN
RETA1:     RTS
;
;
PRBYTE:    PHA                  ; SAVE LOW NIBBLE
           LSR     A
           LSR     A            ; SHIFT HI NIBBLE TO PRINT.
           LSR     A
           LSR     A
           JSR     PRHEXZ
           PLA
PRHEX:     AND     #$0F         ; STRIP HI NIBBLE
PRHEXZ:    ORA     #$B0         ; MAKE IT NUMERIC
           CMP     #$BA         ; IS IT >'9'
           BCC     PRHEX2
           ADC     #06          ; MAKE IT 'A'-'F'
PRHEX2:    JMP     COUT
;
PRBYCOL:   JSR     PRBYTE
;
PRCOLON:   LDA     #$BA         ; PRINT A COLON
           BNE     PRHEX2       ; BRANCH ALWAYS
;
TST80WID:  LDA     #07          ; ANTICIPATE
           BIT     MODES        ; TEST FOR 80
           BVC     SVMASK
           LDA     #$0F
SVMASK:    STA     MASK
           RTS
;
A1PC:      TXA                  ; TEST FOR NEW PC
           BEQ     OLDPC
A1PC1:     LDA     A1L,X
           STA     PCL,X
           DEX
           BPL     A1PC1
OLDPC:     RTS
;
ASCII1:    STA     MASK         ; SAVE HI BIT STATUS
ASCII2:    LDY     YSAV         ; MOVE ASCII TO MEMORY
           LDA     (INBUF),Y
           INC     YSAV         ; BUMP FOR NEXT THING.
           LDY     #00
           CMP     #$A2         ; ASCII " ?
           BNE     ASCII3       ; NOPE, CONTINUE.
           LDA     MASK
           BPL     BITON        ; HE'S CHANGED MODES.
           RTS
ASCII3:    CMP     #$A7         ; ASCII ' ?
           BNE     CRCHK        ; NO, TEST FOR EOL.
           LDA     MASK
           BMI     BITOFF       ; CHANGE MODES.
           RTS
;
CRCHK:     CMP     #$8D         ; END OF LINE?
           BEQ     ASCDONE      ; YES, FINISHED
           AND     MASK
           JSR     STOR1        ; GO STORE IT!
           BNE     ASCII2       ; DO NEXT.
ASCDONE:   RTS
;
;
SEARCH:    LDA     (A1L),Y      ; LOAD SEARCH BYTE
           CMP     A4L
           BNE     SRCH1
           JSR     PRINTA1      ; DUMP MEMORY
           JSR     CROUT
SRCH1:     JSR     NXTA1        ; INCREMENT POINTER
           BCC     SEARCH       ; CONTINUE SEARCH
           RTS                  ; RETURN
;
;
ASCII:     SEC                  ; INDICATE HI ON.
           .BYTE   $90          ; (BCC - NEVER TAKEN)
ASCII0:    CLC                  ; INDICATE HI OFF
CKMDE:     TAX                  ; SAVE STATE
           STX     STATE        ; RETAIN STATE
           EOR     #$BA         ; ARE WE IN STORE MODE?
           BNE     ERROR
BITON:     LDA     #$FF         ; SET HI BIT UNMASKED
           BCS     ASCII1
BITOFF:    LDA     #$7F         ; MASK HI BIT
           BPL     ASCII1       ; ALWAYS BRANCHES
REPEAT:    BIT     KBD          ; REPEAT UNTIL KEYPRESS
           BPL     REPEAT1
           JMP     KEYIN
REPEAT1:   PLA                  ; CLEAN UP STACK
LFA36:     PLA
           JMP     SCAN
;
;
CRMON:     JSR     BL1
           JMP     MONZ
;
;
MOVE:      JSR     TSTA1        ; TEST VALID RANGE
           BCS     ERROR
MOVNXT:    LDA     (A1L),Y      ; COMPARE BYTE FOR BYTE
           STA     (A4L),Y
           JSR     NXTA4        ; BUMP BOTH AL AND A4
           BCC     MOVNXT
           RTS                  ; ALL DONE WITH MOVE
;
VRFY:      JSR     TSTA1        ; TEST VALID RANGE
           BCS     ERROR
VRFY1:     LDA     (A1L),Y      ; COMPARE BYTE FOR BYTE
           CMP     (A4L),Y      ; MATCH?
           BEQ     VRFY2        ; YES, DO NEXT.
           JSR     MISMATCH     ; PRINT BOTH BYTES
           JSR     CROUT        ; GOTO NEWLINE
VRFY2:     JSR     NXTA4        ; BUMP BOTH AL AND A4
           BCC     VRFY1
           RTS                  ; VERIFY DONE.
;
MISMATCH:  LDA     A4H          ; PRINT ADDRESS OF A4
           JSR     PRBYTE
           LDA     A4L
           JSR     PRBYCOL      ; OUTPUT A COLON FOR SEPARATOR
           LDA     (A4L),Y      ; AND THE DATA
           JSR     PRBYTSP      ; PRINT THE BYTE AND A SPACE
PRINTA1:   JSR     PRSPC        ; LEAD WITH A SPACE
           LDA     A1H          ; OUTPUT ADDRESS AL
           JSR     PRBYTE
           LDA     A1L
           JSR     PRBYCOL      ; SEPARATE WITH A COLON
PRA1BYTE:  LDA     (A1L),Y      ; PRINT BYTE POINTED TO BY AL
PRBYTSP:   JSR     PRBYTE
PRSPC:     LDA     #$A0         ; PRINT A SPACE
           JMP     COUT         ; END VIA OUTPUT ROUTINE.
;
USER:      JMP     USERADR
;
JUMP:      PLA
           PLA                  ; LEAVE STACK WITH NOTHIN' ON IT.
GO:        JSR     A1PC         ; STUFF PROGRAM COUNTER
           JMP     (PCL)        ; JUMP TO USER PROD.
;
RWERROR    =       *            ; PRINT ERROR NUMBER
           JSR     PRBYTE       ; PRINT THE OFFENDER
           LDA     #$A1         ; FOLLOWED BY A "!"
           JSR     COUT
ERROR2:    JSR     NOSTOP       ; OUTPUT A CARRIAGE RETURN (NO STOPLST)
ERROR:     JMP     MON
;
DEST:      LDA     A2L          ; COPY A2 TO A4 FOR DESTINATION OP
           STA     A4L
           LDA     A2H
           STA     A4H
           RTS
;
SEP:       JSR     SPCE         ; SEPARATOR TEST STORE MODE OR DUMP.
           TYA                  ; ZERO MODE.
           BEQ     SETMDZ       ; BRANCH ALWAYS
;
BL1:       DEC     YSAV         ; TEST FOR NO LINE
           BEQ     DUMP8        ; IF NO LINE, GIVEM A ROW OF BYTES
SPCE:      DEX                  ; TEST IF AFTER ANOTHER SPACE
           BNE     SETMDZ
           CMP     #$BA         ; STORE MODE?
           BNE     TSTDUMP
STOR:      STA     STATE        ; KEEP IT IN STORE STATE
           LDA     A2L          ; GET BYTE TO BE STORED
STOR1:     STA     (A3L),Y      ; PUT IT IN MEMORY.
           INC     A3L          ; BUMP POINTER
           BNE     DUMMY
           INC     A3H
DUMMY:     RTS                  ; ALSO USED FOR '/' TO CLEAR MODE
;
SETMODE:   LDY     YSAV         ; USE INPUT CHARACTER
           DEY
           LDA     (INBUF),Y    ; TO SET MODE
SETMDZ:    STA     STATE
           RTS
;
READ:      LDA     #01          ; GET DISK COMMAND TO READ
           .BYTE   $2C          ; DUMMY BIT TO SKIP 2 BYTES
WRTE:      LDA     #02          ; SET DISK COMMAND TO WRITE
SAVCMD:    STA     IBCMD
RWLOOP:    LDA	   A1L
           STA     IBBUFP       ; COMMAND FORMAT IS
           LDA     A1H          ; BLOCKNUMBER <ADDRESS END ADDRESS
           STA     IBBUFP+1
           LDX     A4H          ; SEND BLOCK NUMBER VIA X & A
           LDA     A4L
           SEI                  ; NO INTERRUPTS WHILE IN MONITOR
           JSR     BLOCKIO      ; DO DISKO FEVER
           BCS     RWERROR      ; GIVE UP IF ERROR ENCOUNTERED
           INC     A4L          ; BUMP BLOCK NUMBER
           BNE     NOVER
           INC     A4H
NOVER:     INC     A1H          ; BUMP RAM ADDRESS BY 512 BYTES
           INC     A1H
           JSR     TSTA1        ; TEST FOR FINISHED
           BCC     RWLOOP       ; NOT DONE, DO NEXT BLOCK
           RTS
;
DUMP8:     LDA     A1H
           STA     A2H
           JSR     TST80WID     ; GET WIDTH MASK INTO ACC
           ORA	   A1L
           STA     A2L
           BNE     DUMP0        ; BRANCH ALWAYS
;
TSTDUMP:   LSR     A            ; DUMP?
ERROR1:    BCS     ERROR
DUMP:      JSR     TST80WID     ; SET FOR EITHER 80 OR 40 COLUMNS
DUMP0:     LDA	   A1L
           STA     A4L
           LDA     A1H
           STA     A4H
           JSR     TSTA1        ; TEST FOR VALID RANGE
           BCS     ERROR1
DUMP1:     JSR     PRINTA1      ; PRINT ADDRESS AND FIRST BYTE
DUMP2:     JSR     NXTA1
           BCS     DUMPASC      ; END WITH ASCII
           LDA	   A1L		; TEST END OF LINE
           AND     MASK         ; FOR 40/80 COLUMN
           BNE     DUMP3
           JSR     DUMPASC
           BNE     DUMP1        ; BRANCH ALWAYS
DUMP3:     JSR     PRA1BYTE     ; GO PRINT NEXT BYTE AND A SPACE
           BNE     DUMP2        ; ALWAYS (ACC JUST PULLED AS $A0)
;
DUMPASC:   LDA     A4L          ; RESET TO BEGINNING OF LINE
           STA	   A1L
           LDA     A4H
           STA     A1H
           JSR     PRSPC        ; PRINT AN EXTRA SPACE
ASC1:      LDY     #00          ; TO INDEX MEMORY INDIRECT
           LDA     (A1L),Y
           ORA     #$80         ; SET NORMAL VIDEO
           CMP     #$A0         ; TEST FOR CONTROL CHARACTERS
           BCS     ASC2         ; OK TO PRINT NON CONTROLS
           LDA     #$AE         ; OTHERWISE PRINT A SPACE
ASC2:      JSR     COUT         ; PUT IT OUT
           JSR     NXTA4        ; BUMP BOTH AL AND A4
           BCS     ASC3         ; FINISHED
           LDA     A1L          ; TEST END OF LINE
           AND     MASK
           BNE     ASC1         ; NOT DONE, PRINT NEXT
ASC3:      JMP     CROUT
;
;
COL80:     SEC                  ; INDICATE 80 COLUMNS
           LDA     $C053        ; GOTO 80 COLUMN MODE
           BCS     SET80        ; BRANCH ALWAYS
;
COL40:     CLC                  ; INDICATE 40 COLUMNS DESIRED
           LDA     $C052        ; GOTO 40 COLUMN MODE
SET80:     LDA     MODES
           ORA     #$40         ; ASSUME 80
           BCS     SET80A       ; AND BRANCH IF IT IS
           AND     #$BF         ; BUT FIX FOR 40 IF NOT
SET80A:    STA     MODES
           ORA     #$7F         ; ISOLATE BIT 7
           AND     #$A0         ; (BIT 7 SETS NORMAL/INVERSE)
           STA     FORGND
           BCS     SET80B       ; AGAIN ASSUMES 80 COLUMNS
           LDA     #$F0         ; IF NOT, SET FOR/BACKGROUND COLOR
SET80B:    STA     BKGND
;
CLSCRN:    LDA     LMARGIN      ; SET CURSOR TO TOP LEFT OF WINDOW
           STA     CH
           LDA     WINTOP
           STA     CV           ; NOW DROP INTO CLEAR END OF PAGE
;
CLEOP:     LDA     CH           ; SAVE CURRENT CURSOR POSITION
           PHA
           LDA     CV
           PHA
           JSR     SETCV
CLEOP1:    JSR     CLEOL        ; CLEAR TO END OF FIRST LINE
           LDA     LMARGIN
           STA     CH
           JSR     CURDOWN      ; GOTO NEXT LINE
           BCC     CLEOP1
           PLA
           TAY
           PLA                  ; RESTORE CURSOR POSITION
           STA     CH
           TYA                  ; GET OLD CV IN ACC AGAIN
           BCS     SETCV        ; BRANCH ALWAYS
;
CLEOL:     LDA     CH           ; CLEAR TO END OF LINE FIRST
           JMP     CLEOL1
;
CONTROL:   CMP     #$80
           BCC     DISPLAYX     ; IF INVERSE
TSTCR:     CMP     #$8D         ; IF CARRIAGE RETURN THEN NEW LINE
           BNE     TSTBACK
CARRAGE:   JSR     CLEOL        ; FIRST CLEAR TO THE END OF THIS LINE
           JSR     SETCHZ       ; RESET CURSOR AND GOTO NEXT LINE (CARRY IS SET)
           JMP     NXTLIN       ; THEN GOTO THE NEXT LINE.
;
;
CURUP:     LDA     CV           ; TEST FOR TOP OF SCREEN
           DEC     CV           ; ANTICIPATE 'NOT' TOP
           CMP     WINTOP
           BNE     CURUP1       ; IT'S NOT TOP, CONTINUE
           LDA     WINBTM       ; WRAP AROUND TO BOTTOM
CURUP1:    SEC                  ; DECREMENT BY ONE
           SBC     #01
SETCV:     STA     CV           ; SAVE NEW VERTICAL LINE
BASCALC    =	   *
CURDN1     =       *
           LDA     CV           ; GET VALUES FOR FIRST PAGE ($400)
           BPL     BASCALC1     ; ALWAYS
;
CURIGHT:   BIT     MODES        ; TEST FOR 80 OR 40
           BVS     RIGHT1
           INC     CH
RIGHT1:    INC     CH           ; BUMP CURSOR HORIZONTAL
           LDA     CH           ; TEST FOR NEW LINE
           CMP     RMARGIN
SETCHZ:    LDA     LMARGIN      ; JUST IN CASE WE HAVE.
           BCC     CTRLRET
SETCVH:    STA     CH           ; CURSOR AT START OF NEXT LINE
; DROP INTO CURDOWN FOR WRAP AROUND
;
CURDOWN:   INC     CV           ; MOVE CURSOR DOWN ONE LINE
           LDA     CV           ; ANTICIPATE NOT BOTTOM
           CMP     WINBTM       ; TEST FOR BOTTOM
           BCC     CURDN1
           LDA     WINTOP
           BCS     SETCV        ; BRANCH ALWAYS
;
TSTBACK:   CMP     #$88          ; BACKSPACE?
           BNE     TSTBELL
CURLEFT:   BIT     MODES        ; TEST FOR FOURTY OR EIGHTY MODE
           BVS     LEFT80
           DEC     CH
LEFT80:    DEC     CH
           BMI     LEFTUP
           LDA     CH           ; TEST FOR WRAP AROUND
           CMP     LMARGIN
           BPL     CTRLRET
LEFTUP:    JSR     CURUP
           LDA     RMARGIN
           STA     CH           ; SAVE NEW CURSOR POSITION
           BNE     CURLEFT      ; BRANCH ALWAYS
;
COUT2:     CMP     #$A0         ; IS IT CONTROL CHARACTER
           BCC     CONTROL
           BIT     MODES        ; TEST FOR INVERSE
           BMI     DISPLAYX     ; NO PUT IT OUT
           AND     #$7F          ; STRIP HI BIT
DISPLAYX:  JSR     DISPLAY
;
INCHORZ:   JSR     CURIGHT      ; MOVE CURSOR RIGHT
NXTLIN:    BCS     SCROLL       ; IT'S BOTTOM, RESET CH=0 AND SCROLL
           RTS                  ; RESET CH ONLY
;
BASCALC1:  PHP                  ; CALC BASE ADR IN BAS4L,H
           PHA
           LSR     A            ; FOR GIVEN LINE NO.
           AND     #03          ; 0<=LINE NO.<$17
           ORA     #04          ; ARG=000ABCDE, GENERATE
           STA     BAS4H        ; BAS4H=000001CD
           EOR     #$0C
           STA     BAS8H
           PLA                  ; AND
           AND     #$18         ; BAS4L=EABAB000
           BCC     BSCLC2
           ADC     #$7F
BSCLC2:    STA     BAS4L
           ASL     A
           ASL     A
           ORA     BAS4L
           STA     BAS4L
           STA     BAS8L        ; SAME FOR PAGE 2
           PLP
CTRLRET:   RTS
;
COUT:      PHA                  ; SAVE CHARACTER
           STY     TEMPY
           STX     TEMPX
           JSR     COUT1
           LDY     TEMPY
           LDX     TEMPX
           PLA
           RTS
COUT1:     JMP     (CSWL)        ; NORMALLY COUT1
;
TSTBELL:   CMP     #$87         ; BELL?
           BNE     LNFD         ; NO TEST FOR FORM FEED
BELL:      LDX     $C040        ; SOUND BELL
           RTS
LNFD:      CMP     #$8A         ; LINE FEED?
           BNE     CTRLRET
           JSR     CURDOWN      ; MOVE CURSOR DOWN A LINE
           BCC     CTRLRET      ; BRANCH IF NO SCROLL NECESSARY.
;
SCROLL:    LDA     WINTOP       ; START WITH TOP LINE
           PHA                  ; SAVE IT FOR NOW
           JSR     SETCV        ; GET BASCALC FOR THIS LINE
SCRL1:     LDX     #03          ; MOVE CURRENT BASCALC AS DESTINATION
SCRL2:     LDA     BAS4L,X
           STA     TBAS4L,X     ; (TEMPORARY BASE ADDR.)
           DEX
           BPL     SCRL2
           PLA                  ; GET DESTINATION LINE
           CLC
           ADC     #01          ; CALCULATE SOURCE LINE.
           CMP     WINBTM       ; IS IT THE LAST LINE?
           BCS     LASTLN       ; YES, CLEAR IT
           PHA                  ; SAVE AS NEXT DESTINATION LINE
           JSR     SETCV        ; GET BASE ADDR FOR SOURCE LINE
           LDA     RMARGIN      ; MOVE SOURCE TO DESTINATION
           LSR     A            ; DIVIDE BY 2
           TAY
SCRL3:     DEY                  ; DONE YET
           BMI     SCRL1        ; YES, DO NEXT LINE
           LDA     (BAS4L),Y
           STA     (TBAS4L),Y
           LDA     (BAS8L),Y
           STA     (TBAS8L),Y
           BCC     SCRL3        ; BRANCH ALWAYS
LASTLN:    LDA     LMARGIN      ; BLANK FILL THE LAST LINE
CLEOL1:    LSR     A            ; DIVIDE BY 2
           TAY
           BCS     CLEOL2
           LDA     FORGND       ; (NORMALLY A SPACE)
           STA     (BAS4L),Y
CLEOL2:    LDA     BKGND        ; (IF 80 COLUMNS, ALSO A SPACE)
           STA     (BAS8L),Y
           INY
           TYA                  ; TEST FOR END OF LINE
           ASL     A            ; MULT BY 2 AGAIN
           CMP     RMARGIN     
           BCC     CLEOL1       ; CONTINUE IF MORE TO DO.
           RTS                  ; ALL DONE.
;
DISPLAY:   BIT     MODES        ; TEST FOR 40 OR 80
           BVS     DSPL80       ; STORE THE SINGLE CHARACTERS AND RETURN
           LSR     CH           ; INSURE PROPER 40 COLUMN DISPLAY
           ASL     CH           ; BY DROPPING BIT 0
           JSR     DSPL80       ; DISPLAY IN $400 PAGE.
           LDA     BKGND        ; ALSO SET BACKGROUND COLOR
DSPBKGND:  STA     (BAS8L),Y
           RTS
;
DSPL80:    PHA                  ; PRESERVE CHARACTER
           LDA     CH           ; DETERMINE WHICH PAGE
           LSR     A
           TAY
           PLA
           BCS     DSPBKGND     ; BRANCH IF $900 PAGE
           STA     (BAS4L),Y
           RTS
;
NOTCR:     LDA     (INBUF),Y    ; ECHO CHARACTER
           JSR     COUT
           CMP     #$88         ; BACKSPACE
           BEQ     BKSPCE
           CMP     #$98         ; CANCEL?
           BEQ     CANCEL
           INC     TEMP
           LDA     TEMP
           CMP     #INBUFLEN
           BNE     NXTCHAR      ; NO WRAP AROUND ALLOWED.
CANCEL:    LDA     #$DC         ; OUTPUT BACKSLASH
           JSR     COUT
           JSR     CROUT
GETLNZ     =	   *
GETLN:     LDA     PROMPT
           JSR     COUT
           LDY     #01
           STY     TEMP         ; START AT BEGINNING OF INBUF
BKSPCE:    LDY     TEMP
           BEQ     GETLN
           DEC     TEMP         ; BACK UP INPUT BUFFER
NXTCHAR:   JSR     RDCHAR       ; GET INPUT
           LDY     TEMP
           STA     (INBUF),Y
           CMP     #$8D
           BNE     NOTCR
CROUT      =	   *
           BIT     KBD          ; TEST FOR START/STOP
           BPL     NOSTOP
           JSR     KEYIN3       ; READ KBD
           CMP     #$A0         ; IS IT A SPACE?
           BEQ     STOPLST      ; YES, PAUSE TIL NEXT KEYPRESS.
           CMP     #$89         ; QUIT THIS OPERATION
           BNE     NOSTOP       ; NO,      IGNORE THIS KEY.
           JMP     ERROR2       ; YES, RESTART
STOPLST:   LDA     KBD
           BPL     STOPLST
NOSTOP:    LDA     #$8D
           JMP     COUT
;
RDKEY:     JMP     (KSWL)
;
KEYIN:     LDA     #$7F         ; MAKE SURE FIRST IS CURSOR
           STA     TBAS4H
           JSR     PICK         ; GO READ SCREEN
KEYIN1:    PHA                  ; SAVE CHR AT CURSOR POSITION
           JSR     KEYWAIT      ; TEST FOR KEYPRESS
           BCS     KEYIN2       ; GO GET IT
           LDA     CURSOR       ; GIVE THEM AN UNDERSCORE FOR A TIME
           JSR     DISPLAY
           JSR     KEYWAIT      ; GO SEE IF KEYPRESSED
KEYIN2:    PLA
           PHP                  ; SAVE KEYPRESS STATUS
           PHA
           JSR     DISPLAY
           PLA
           PLP
           BCC     KEYIN1
KEYIN3:    LDA     KBD          ; READ KEYBOARD
KEYIN4:    BIT     KBDSTRB      ; CLEAR KEYBOARD STROBE
           RTS
KEYWAIT:   INC     TBAS4L       ; JUST KEEP COUNTING
           BNE     KWAIT2
           INC     TBAS4H
           LDA     #$7F         ; TEST FOR DONE
           CLC
           AND     TBAS4H
           BEQ     KEYRET       ; RETURN IF TIMED OUT
KWAIT2:    ASL     KBD
           BCC     KEYWAIT
KEYRET:    RTS
;
;
ESC3       =       *
           JSR     GOESC
ESCAPE:    LDA     MODES        ; SET TO + SIGN FOR CURSOR MOVES
           AND     #$80
           EOR     #$AB
           STA     CURSOR
ESC1:      JSR     RDKEY        ; READ NEXT CHARACTER
           LDY     #$08         ; TEST FOR ESCAPE COMMAND
ESC2:      CMP     ESCTABL,Y
           BEQ     ESC3
           DEY
           BPL     ESC2         ; LOOP TIL FOUND OR DONE
;
RDCHAR:    LDA     #$80         ; GO READ A CHARACTER
           AND     MODES
           STA     CURSOR       ; SAVE STANDARD CURSOR
           JSR     RDKEY
           CMP     #$9B         ; ESCAPE CHARACTER?
           BEQ     ESCAPE
           CMP     #$95         ; FORWARD COPY?
           BNE     KEYRET
           JSR     PICK         ; GET CHARACTER FROM SCREEN
           ORA     #$80         ; SET TO NORMAL ASCII
           RTS
;
GOESC:     LDA     #$FB
           PHA
           LDA     ESCVECT,Y
           PHA
           RTS
ESCVECT:   .BYTE   $A1          ; CLEOL-1
           .BYTE   $84          ; CLEOP-1
           .BYTE   $7C          ; CLSCRN-1
           .BYTE   $62          ; COL40-1
           .BYTE   $5C          ; COL80-1
           .BYTE   $EC          ; CURLEFT-1
           .BYTE   $CA          ; CURIGHT-1
           .BYTE   $DC          ; CURDOWN-1
           .BYTE   $B7          ; CURUP-1
;
PICK:      LDA     CH           ; GET A CHARACTER AT CURRENT CURSOR POSITION
           LSR     A            ; DETERMINE WHICH PAGE.
           TAY
           BIT     MODES        ; AND IF 80 COLUMN MODE
           BVC     PICK40       ; FORGET CARRY IF 40 COLUMNS
           BCC     PICK40       ; GET CHARACTER FROM $400
           LDA     (BAS8L),Y
           RTS
PICK40:    LDA     (BAS4L),Y
           RTS
;
CLDSTRT    =    *
           LDA     #03
           STA     $FFD0        ; ZERO PAGE IS ON 3!
SETUP      =    *
           CLD                  ; OF COURSE!
           LDX     #03
           STX     INBUF+1
SETUP1:    LDA     NMIRQ,X
           STA     $FFCA,X
           LDA     HOOKS,X
           STA     CSWL,X
           LDA     VBOUNDS,X
           STA     LMARGIN,X
           DEX
           BPL     SETUP1
           STA     IBDRVN
           LDA     #$A0         ; INPUT BUFFER AT $3A0
           STA     INBUF
           LDA     #$60
           STA     IBSLOT
           LDA     #$FF
           STA     MODES
           JSR     COL40        ; SET 40 COLUMNS, CLEAR SCREEN

ADR        =    $A0
CPORTL     =    ADR
CPORTH     =    ADR+1
CTEMP      =    ADR+2
CTEMP1     =    ADR+3
YTEMP      =    ADR+4
ROWTEMP    =    ADR+$14
CWRTON     =    $C0DB
CWRTOFF    =    $C0DA
CB2CTRL    =    $FFEC
CB2INT     =    $FFED
;
;
GENENTR:   LDA     #$78         ; INIT SCREEN INDX LOCATIONS
           STA     CPORTL
           LDA     #$08
           STA     CPORTH
           LDA     #$F0         ; SET UP INDEX TO CHRSET
           STA     YTEMP
           LDA     #00
           TAX
ZIPTEMPS:  STA     ROWTEMP,X
           INX
           CPX     #$20
           BNE     ZIPTEMPS
           LDA     #05          ; FAKE THE FIRST BIT PATTERN
           CLC                  ; (PHANTOM 9TH BIT SHIFTED AS BIT 0)
           PHP
           PHA
GENASC:    STX     CTEMP        ; GENERATE THE ASCII
GASCI1:    LDY     #07          ; CODES FOR THE FIRST PASS
GASCL2:    LDX     CTEMP
GASCI3:    TXA
           STA     (CPORTL),Y   ; $XXF=CHR 0 / 4
           INX                  ; $XXE=CHR 1 / 5
           DEY                  ; $XXD=CHR 2 / 6
           BMI     GASCI4       ; $XXC=CHR 3 / 7
           CPY     #03          ; $XXB=CHR 0 / 4
           BNE     GASCI3       ; $XXA=CHR 1 / 5
           BEQ     GASCL2       ; $XX9=CHR 2 / 6
GASCI4:    JSR     NXTPORT      ; $XX8=CHR 3 / 7
           BCS     CBYTES       ; GO DECODE CHARACTER TABLE
           CMP     #$0A         ; SECOND SET OF 4?
           BNE     GASCI1
           LDX     #$24
           BNE     GENASC       ; BRANCH ALWAYS
CBYTES:    PLA                  ; RESTORE BIT PATTERN
           PLP
           LDX     #$17         ; (4 CHARACTERS OF 6 ROWS)
CCOLMS:    LDY     #05          ; (FIVE COLUMNS)
CSHFT:     ROL     ROWTEMP+4,X  ; BREAK BYTE INTO
           ASL     A            ; 5 BIT GROUPS
           BNE     SHFTCNT      ; BRANCH IF MORE BITS IN THIS BYTE
           STY     CTEMP
           DEC     YTEMP        ; (NOTE. CARRY IS SET)
           BEQ     DONE         ; BRANCH IF ALL DONE
           LDY     YTEMP        ; GET CHARACTER TABLE INDEX
           LDA     CHRSET-1,Y
           ROL     A            ; (CARRY KEEPS BYTE NON-ZERO UNTIL ALL 8 ARE
                                ; ARE SHIFTED)
           LDY     CTEMP        ; RESTORE COLUMN COUNT
SHFTCNT:   DEY                  ; GOT ALL FIVE BITS?
           BNE     CSHFT        ; NO, DO NEXT
           DEX                  ; ALL ROWS DONE
           BPL     CCOLMS       ; NO, DO NEXT
           PHP                  ; SAVE REMAINING BIT PATTERN AND CARRY
           PHA
           JSR     STORCHRS     ; MOVE EM TO NON DISPLAYED VIDEO AREA
           JMP     CBYTES
;
DONE       =    *
;
STORCHRS:  LDX     #$1F         ; MOVE CHARACTER PATTERNS TO VIDEO AREA
STORSET:   LDY     #00
STOROW:    LDA     ROWTEMP,X
           ASL     A            ; SHIFT TO CENTER
           AND     #$3E         ; STRIP EXTRA GARBAGE
           STA     (CPORTL),Y
           DEX
           INY
           CPY     #$08         ; THIS GROUP DONE
           BNE     STOROW       ; NO, NEXT ROW
           JSR     NXTPORT
           CMP     #$08
           BEQ     GENDONE      ; ALL ROWS STORED?
           TXA
           BPL     STORSET
           RTS                  ; PARTIAL SET ($478-$5FF)
;
GENDONE:   LDA     #$01         ; SET NORMAL MODE
           STA     CTEMP
GEN1:      LDA     #$60         ; PREPARE TO SEND BYTES TO CHARACTER
           BIT     CWRTON       ; GENERATOR RAM
           JSR     VRETRCE      ; WAIT FOR NEXT VERTICAL RETRACE
           LDA     #$20         ; WAIT AGAIN
           JSR     VRETRCE
           BIT     CWRTOFF      ; CHARACTERS ARE NOW LOADED
           JSR     ALTCHR       ; REPEAT THIS SET FOR OTHER 64 CHARACTERS
           DEC     CTEMP        ; HAVE WE DONE ALTERNATES YET?
           BPL     GEN2         ; NO, DO IT!
           LDA     #08          ; BUMP ASCII VALUES FOR NEXT SET
           STA     CPORTH
NXTASCI:   LDY     #07          ; THE USUAL COUNTDOWN
NXTASC2:   LDA     (CPORTL),Y
           CLC
           ADC     #08
           STA     (CPORTL),Y
           DEY
           BPL     NXTASC2
           JSR     NXTPORT
           BCC     NXTASCI
           RTS
GEN2:      LDY     #03          ; SETUP ALTERNATE WITH UNDERLINES
           LDA     #$7F
UNDER:     STA     $05FC,Y
           STA     $07FC,Y
           DEY
           BPL     UNDER
           LDA     #08
           STA     CPORTH
           BNE     GEN1
;
ALTCHR:    LDY     #07          ; ADJUST ASCII FOR ALTERNATE SET
ALTC1:     LDA     (CPORTL),Y
           EOR     #$20         ; $20-->  $40-->$60
           STA     (CPORTL),Y
           DEY
           BPL     ALTC1        ; ADJUST THEM ALL
           JSR     NXTPORT
           BCC     ALTCHR
           RTS
;
NXTPORT:   LDA     CPORTL       ; CONVERT $78->$F8 OR $F8-$78
           EOR     #$80
           STA     CPORTL
           BMI     NOHIGH
           INC     CPORTH
NOHIGH:    LDA     CPORTH
           CMP     #$0C
           BNE     PORTDN
           LDA     #04
           STA     CPORTH
PORTDN:    RTS
;
VRETRCE:   STA     CTEMP1       ; SAVE BITS TO BE STORED
           LDA     CB2CTRL      ; CONTROL PORT FOR 'CB2'
           AND     #$3F         ; RESET HI BITS TO 0
           ORA     CTEMP1
           STA     CB2CTRL
           LDA     #08          ; TEST VERTICAL RETRACE
           STA     CB2INT
VWAIT:     BIT     CB2INT       ; WAIT FOR RETRACE
           BEQ     VWAIT
           RTS
;
CHRSET     =    *
;
           .BYTE   $F0,$01,$82,$18,$40,$84,$81,$2F,$58,$44,$81,$29,$02,$1E,$01,$91,$7C,$1F,$49,$30
           .BYTE   $8A,$08,$43,$14,$31,$2A,$22,$13,$E3,$F7,$C4,$91,$48,$A2,$DA,$24,$C6,$4A
           .BYTE   $62,$8C,$24,$C6,$F8,$63,$8C,$C1,$46,$17,$52,$8A,$AF,$16,$14,$E3,$33,$31
           .BYTE   $C6,$F8,$DC,$73,$3F,$46,$17,$62,$8C,$21,$E6,$18,$6A,$8D,$61,$CF,$18,$62
           .BYTE   $74,$D1,$B9,$18,$49,$4C,$91,$C0,$F3,$09,$2C,$91,$C0,$14,$1D,$8C,$EF,$07
           .BYTE   $17,$43,$88,$31,$84,$1E,$DF,$0B,$31,$84,$F8,$FE,$77,$3E,$3E,$17,$62,$8C,$FD
           .BYTE   $C7,$50,$E3,$0B,$51,$C5,$E8,$C8,$73,$18,$0C,$42,$3E,$01,$02,$20,$42,$3E
           .BYTE   $41,$18,$8C,$08,$00,$70,$EE,$00,$11,$11,$21,$11,$02,$E0,$3C,$21,$31,$02,$E0
           .BYTE   $1C,$00,$C8,$B9,$80,$62,$14,$1F,$46,$A2,$DE,$43,$2C,$04,$88,$BE,$FF,$CE
           .BYTE   $7D,$37,$49,$88,$95,$18,$98,$09,$62,$D1,$44,$E8,$88,$FB,$02,$90,$40,$00,$10
           .BYTE   $E0,$03,$02,$00,$40,$00,$00,$08,$00,$00,$28,$10,$42,$44,$25,$82,$B8,$2F,$48
           .BYTE   $25,$44,$10,$82,$02,$00,$2F,$5A,$40,$45,$02,$8E,$64,$50,$90,$01,$3E,$26,$42,$80
           .BYTE   $21,$80,$00,$05,$00,$F8,$80,$00,$05,$08,$F8,$80,$28,$05,$88
;           
HOOKS      =    *
           .WORD   COUT2
           .WORD   KEYIN
VBOUNDS    =    *
           .BYTE   $00,$50,$00,$18
;
NMIRQ:     JMP     RECON        ; IN DIAGNOSTICS
           RTI
;
;          ROM HAS HIGH BIT SET FOR ASCII CHARACTERS
;          341-0031-01
;          "(C) COPYRIGHT JULY, 1980 APPLE COMPUTER INC. JRH"          
;
		   .BYTE   $A8,$C3,$A9,$A0,$C3,$CF,$D0,$D9,$D2,$C9,$C7,$C8,$D4,$A0,$CA,$D5
           .BYTE   $CC,$D9,$AC,$A0,$B1,$B9,$B8,$B0,$A0,$C1,$D0,$D0,$CC,$C5,$A0,$C3
           .BYTE   $CF,$CD,$D0,$D5,$D4,$C5,$D2,$A0,$C9,$CE,$C3,$AE,$A0,$CA,$D2,$C8
;
ESCTABL:   .BYTE   $CC,$D0,$D3,$B4,$B8,$88,$95,$8A,$8B,$00
;           
      
NMI:       .WORD   $FFCA  
RESET:     .WORD   DIAGN        ; NOTHING
IRQ:       .WORD   $FFCD

;          .END