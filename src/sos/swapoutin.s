SWAPOUT     =       *
;
; SWAP OUT A VOLUME LOGGED ON A DEVICE
; INPUT ARGUMENT: DEVICE NUMBER "A"
; (STORED AS "DEVNUM")
; OUTPUT ARGUMENT: NONE
; CONDITION CODE: CARRY SET USER DID NOT COMPLY WITH REQUEST
;
; SAVE VCBPTR, FCBPTR, DEVNUM ON STACK
; 1) FIND UNSWAPPED VOLUME IN VCB
; 2) IF DIRTY BIT MAP FOR THIS VOLUME THEN DO
;   IF NOT ONLINE, REQUEST USER TO INSERT
;   IF REQUEST DENIED, UNCONDITIONALLY  CLOSE ALL FILES ON THIS VOLUME AND RTS
;   IF ONLINE, UPDATE AND RELEASE BIT MAP
; DOEND
; 3) SWAP IT (MARK VCBSWAP FIELD $80, MARK ALL FILES ON THIS VOLUME WITH SWAP MARK $8X WHERE X=VCB ENTRY)
; "VCB ENTRY" DEFINED AS: HIGH ORDER NIBBLE OF LOW ORDER BYTE OF ENTRIES VCB ADDRESS
; RESTORE VCBPTR, FCBPTR
; RTS
;
            TAX                                         ; SAVE DEVICE NUMBER
            JSR     SAVECBS
            STX     DEVNUM                              ; PERMANENTLY
SWAPOUTX:   JSR     DEVVCB                              ; FIND MATCHING UNSWAPPED ACTIVE VCB ENTRY (BY DEVNUM)
            BCS     SORTS                               ; NO FIND--RETURN WITHOUT ERROR
            LDY     #VCBSTAT
            LDA     (VCBPTR),Y                          ; GET STATUS OF FILES ON THIS VOLUME
            BPL     UNLOG                               ; IF NO OPEN FILES, JUST THROW VOLUME AWAY
            LDA     DEVNUM                              ; DIRTY BM EXIST ON THIS VOLUME?
            LDX     #0
            CMP     BMADEV,X                            ; IN BIT MAP "A"?
            BEQ     FDIRBM                              ; BRANCH IF YES
            LDX     #6                                  ; BIT MAP HEADER TABLE SIZE
            CMP     BMADEV,X                            ; IN BIT MAP "B"?
            BEQ     FDIRBM                              ; BRANCH IF YES
            JMP     MARKSWAP                            ; NO NEED TO WRITE BIT MAP
FDIRBM:     LDA     BMASTAT,X                           ; IS BIT MAP DIRTY?
            BPL     MARKSWAP                            ; BRANCH IF NOT
GETVOL:     JSR     VERFYVOL                            ; IS THE CORRECT VOLUME ON LINE NOW?
            BCC     VONLINE                             ; BRANCH IF YES
            JSR     USRREQ                              ; OTHERWISE, REQUEST USER INSERTION
            BCC     GETVOL                              ; AND VERIFY IT AGAIN
            JSR     CLOSEU                              ; USER SAID "NO": UNCONDITIONALLY CLOSE VOLUME
            JSR     RESTCBS
            SEC
            RTS                                         ; ERROR RETURN TO CALLER
VONLINE:    LDX     DEVNUM                              ; UPDATE THE
            JSR     UPBMAP                              ; DIRTY BIT MAP
MARKSWAP:   LDA     VCBPTR                              ; CALCULATE
            LSR     A                                   ; SWAP BYTE
            LSR     A                                   ; AND
            LSR     A                                   ; MARK ALL FILES
            LSR     A                                   ; BELONGING TO THIS VOLUME
            SEC                                         ; AS SWAPPED OUT
            ORA     #$80
            PHA                                         ; SAVE SWAP BYTE
            JSR     FCBSCAN
            PLA                                         ; MARK VCBSWAP
            LDY     #VCBSWAP                            ; BYTE
            STA     (VCBPTR),Y
SORTS:      JSR     RESTCBS                             ; RESTORE FCBPTR, VCBPTR, DEVNUM
            CLC
            RTS                                         ; SUCCESSFUL SWAP OUT
UNLOG:      LDA     #0
            STA     VCB,X                               ; UNLOG VOLUME
            BEQ     SORTS                               ; SWAP THE EASY WAY! (BRANCH ALWAYS)
;
;
;
SWAPIN      =       *
;
; UNSWAP A VOLUME AND ALL ITS FILES
;
; INPUT ARGUMENT: VOLUME NAME (VCBPTR)
; OUTPUT ARGUMENT: NONE
; CONDITION CODE: CARRY SET : USER DID NOT COMPLY WITH REQUEST
;
; SAVE VCBPTR, FCBPTR ON STACK
; 1) FIND SWAPPED VOLUME IN VCB, IF NOT FOUND, THEN RTS.
; 2) IF ANOTHER UNSWAPPED VOLUME ON DEVICE, THEN SWAP IT
; 3) VERIFY UNSWAPPED VOLUME, IF NOT OK THEN REQUEST INSERTION
; 4) UNMARK VCB'S AND FCB'S
; RTS
            JSR     SAVECBS                             ; SAVE FCB, VCB POINTERS, DEVNUM
            LDY     #VCBNML                             ; MAKE SURE VOLUME
            LDA     (VCBPTR),Y                          ; IS AT LEAST OPEN
            BEQ     USRTS                               ; BRANCH IF NOT RIGHT BACK TO CALLER
            LDY     #VCBSWAP                            ; SEE IF
            LDA     (VCBPTR),Y                          ; CURRENTLY SWAPPED
            BEQ     USRTS                               ; IF NOT, RETURN IMMEDIATELY TO CALLER
            LDY     #VCBDEV                             ; SAVE DEVICE NUMBER
            LDA     (VCBPTR),Y
            STA     DEVNUM
            PHA                                         ; SAVE DEVNUM AGAIN (SWAPOUTX TRASHES DEVNUM ON RETURN)
            JSR     SWAPOUTX                            ; AND MAKE SURE ANY CURRENT ACTIVE VOLUME IS SWAPPED OUT (NOTICE ENTRY POINT)
            PLA                                         ; RECALL CURRENT DEVICE NUMBER
            STA     DEVNUM                              ; AND SAVE IT TO ITS PROPER PLACE
SI1:        JSR     VERFYVOL                            ; VERIFY THE CURRENT VOLUME MOUNTED
            BCC     UNMARK                              ; IF THE RIGHT ONE, GO MARK IT AS UNSWAPPED
            JSR     USRREQ                              ; ELSE REQUEST USER TO INSERT
            BCC     SI1                                 ; USER SAID 'OK'
            JSR     CLOSEU                              ; OTHERWISE UNCONDITIONALLY CLOSE
            JSR     RESTCBS
            SEC
            RTS
UNMARK:     LDY     #VCBSWAP                            ; FETCH
            LDA     (VCBPTR),Y                          ; VOLUME
            PHA                                         ; SWAP BYTE
            LDA     #0                                  ; BUT CLEAR
            STA     (VCBPTR),Y                          ; VOLUME SWAP
            PLA
            CLC                                         ; "UNSWAPPED"
            JSR     FCBSCAN
            LDA     DEVNUM                              ; MAKE SURE BIT MAPS
            JSR     CLEARBMS                            ; ARE MARKED AS INVALID ON THIS DEVICE
USRTS:      JSR     RESTCBS                             ; RESTORE VCB, FCB PTRS
            CLC                                         ; NO ERRORS
            RTS
;
SAVEPTRS:   .RES    5                                   ; A RARE EMBEDDED TEMP SAVE AREA, USED ONLY BY ___
;
;
SAVECBS     =       *                                   ; SAVE FCBPTR, VCBPTR IN A TEMP SAVE AREA
            LDA     VCBPTR
            STA     SAVEPTRS
            LDA     VCBPTR+1
            STA     SAVEPTRS+1
            LDA     FCBPTR
            STA     SAVEPTRS+2
            LDA     FCBPTR+1
            STA     SAVEPTRS+3
            LDA     DEVNUM
            STA     SAVEPTRS+4
            RTS
;
RESTCBS     =       *                                   ; RESTORE FCBPTR, VCBPTR
; NOTICE THERE EXISTS A SEQUENCE OF CALLS (SWAPIN, WHICH MAY CALL SWAPOUT) THAT JSR'S TO SAVECBS ONCE BUT JSR'S RESTCBS TWICE.
            LDA     SAVEPTRS
            STA     VCBPTR
            LDA     SAVEPTRS+1
            STA     VCBPTR+1
            LDA     SAVEPTRS+2
            STA     FCBPTR
            LDA     SAVEPTRS+3
            STA     FCBPTR+1
            LDA     SAVEPTRS+4
            STA     DEVNUM
            RTS
;
;
; MARK ALL FILES BELONGING TO A VOLUME
; AS SWAPPED-IN OR SWAPPED-OUT.
;
; INPUT ARGS: DEVNUM -- DEVICE NUMBER OF MOUNTED VOLUME
;             A REGISTER - SWAP BYTE
;             CARRY -- CARRY FLAG SET MEANS SWAP OUT; ELSE SWAP IN
;
; OUTPUT ARGS: NONE
; GLOBALS AFFECTED: FCB, FCBPTR
; REGISTER STATUS: SCRAMBLED
;
FCBSCAN     =       *                                   ; MARK FILES BELONGING TO VOLUME AS SWAPPED OR UNSWAPPED
;
            TAX                                         ; SAVE SWAP BYTE
            LDY     A:FCBADDRH                          ; POINT TO
            STY     FCBPTR+1                            ; BEGINNING TO FCB
            LDY     #0
            STY     FCBPTR
            BCS     FCBOUT                              ; SWAP OUT A VOLUMES FILES
FCBIN       =       *                                   ; SWAPIN A VOLUMES FILES
            JSR     FCBFETCH                            ; GET NEXT ACTIVE FCB CANDIDATE
            BCS     FCBRTS                              ; NO MORE FILES TO PROCESS
            LDY     #FCBSWAP
            TXA
            CMP     (FCBPTR),Y                          ; SWAP BYTES MATCH?
            BNE     FCBIN1                              ; BRANCH IF NOT
            LDA     #0
            STA     (FCBPTR),Y                          ; MARK FILE AS SWAPPED IN
FCBIN1:     JSR     NEXTFCB                             ; ADVANCE FCB POINTER
            BCS     FCBRTS                              ; NO MORE TO LOOK AT
            JMP     FCBIN                               ; AND LOOK AT NEXT FILE
;
FCBOUT      =       *                                   ; SWAPPED OUT A VOLUMES FILES
            JSR     FCBFETCH                            ; GET NEXT ACTIVE FILE IN FCB
            BCS     FCBRTS                              ; NO MORE FILES -- RETURN TO USER
            LDY     #FCBSWAP                            ; COMPARE
            LDA     (FCBPTR),Y
            BNE     FCBOUT1                             ; ALREADY SWAPPED OUT
            TXA
            STA     (FCBPTR),Y                          ; MARK AS SWAPPED
FCBOUT1:    JSR     NEXTFCB                             ; ADVANCE FCB POINTER
            BCS     FCBRTS
            JMP     FCBOUT                              ; SWAP OUT NEXT FILE
;
FCBRTS:     RTS
FCBFETCH    =       *                                   ; GET NEXT ACTIVE FILE FROM FCB
; X REGISTER MUST NOT BE DISTURBED
; USES FCBPTR
            LDY     #FCBDEVN                            ; MAKE
            LDA     (FCBPTR),Y                          ; SURE DEVICE
            CMP     DEVNUM                              ; MATCHES
            BNE     NEXTFCB
            LDY     #FCBREFN                            ; MAKE SURE FILE IS ACTIVE
            LDA     (FCBPTR),Y
            BEQ     NEXTFCB                             ; BRANCH IF NOT
            CLC
            RTS                                         ; RETURN WITH CARRY CLEAR SHOWING AN ACTIVE FILE
NEXTFCB:    LDA     FCBPTR
            CLC
            ADC     #$20                                ; FCB ENTRY SIZE
            STA     FCBPTR
            BCC     FCBFETCH                            ; BRANCH IF NO PAGE CROSS
            LDA     FCBPTR+1
            INC     FCBPTR+1                            ; SECOND PAGE
            CMP     A:FCBADDRH
            BEQ     FCBFETCH                            ; LOOK AT PAGE TWO
NEXTEND:    SEC
            RTS                                         ; SHOW NO MORE FILES TO LOOK AT
USRREQ      =       *                                   ; OPERATOR CONSOLE MESSAGE INTERFACE
; PRODUCES A MESSAGE REQUESTING
; THE SYSTEM OPERATOR TO MOUNT THE VOLUME
; SPECIFIED BY "VCBPTR" ON DEVICE SPECIFIED
; BY DEVNUM.  THIS MODULE INSISTS
; UPON THE CORRECT OPERATOR ACTION
; UPON THREE FAILURES TO COMPLY,
; THE MODULE WILL SIGNIFY FAILURE WITH
; CARRY SET.  IF THE CORRECT ACTION IS TAKEN,
; CARRY WILL BE RETURNED CLEAR
;
; INPUT ARGS: VOLUME NAME (VCBPTR)
;             DEVICE NUMBER (DEVNUM)
;
; OUTPUT ARGS: CC = OPERATOR COMPLIED WITH REQUESTED ACTION
;              CS = OPERATOR COULDN'T/DIDN'T COMPLY
;
; GLOBALS AFFECTED: NONE
;
; STATUS OF REGISTERS: UNCERTAIN
;
VNML        =       ZPGTEMP                             ; VOLUME NAME LENGTH
            LDY     #VCBNML                             ; IF ILLEGAL VCB
            LDA     (VCBPTR),Y                          ; GET OUT QUICK
            BEQ     NEXTEND                             ; BRANCH TO SEC RTS
            LDX     #$E                                 ; LENGTH OF NAMED AREA-1
            LDA     #$0                                 ; NULLS
UR1:        STA     MDEV,X                              ; BOTH CLEAR
            STA     MVOL,X                              ; IN ONE LOOP
            DEX
            BPL     UR1
;
; DO A D-INFO TO FETCH THE DEVICE NAME
;
            LDA     #5                                  ; DO ALL
            STA     $C0                                 ; NECESSARY
            LDA     DEVNUM                              ; HOUSKEEPING
            STA     $C1                                 ; TO SET UP
            LDA     #<(MDEV-1)                          ; A DEVICE MANAGER CALL
            STA     $C2
            LDA     #>(MDEV-1)
            STA     $C3
            LDA     #$8F                                ; EXTEND BYTE
            STA     $14C3
            LDA     #0
            STA     $14C2
            STA     $C4
            STA     $C5
            STA     $C6                                 ; ZERO SUPERFLUOUS PARMS
            STA     URDERR                              ; RESET FAILURE COUNT
            JSR     RPEATIO0                            ; GET INFO FROM BOBS CODE
            LDA     #$20                                ; "SPACE" RESTORED
            STA     MDEV-1                              ; RESTORED
            LDY     #VCBNML
            LDA     (VCBPTR),Y                          ; LENGTH OF VOLUME NAME
            STA     VNML                                ; SAVED FOR WORK
            LDA     #0
            TAX
            LDY     #VCBNAM                             ; POINT TO BEGINNING OF VOLUME NAME
UR2:        LDA     (VCBPTR),Y
            STA     MVOL,X
            INX
            INY                                         ; VOLUME NAME MOVED
            DEC     VNML                                ; TO MESSAGE BUFFER
            BNE     UR2                                 ; CHARACTER BY CHARACTER
URDU:       LDX     #<UMB                               ; PASS THE AREA'S ADDR
            LDY     #>UMB                               ; IN X AND Y REGS,LOW, HIGH)
            JSR     OPMSGRPLY                           ; HAVE MESSAGE SYSTEM PRINT IT
            JSR     VERFYVOL                            ; DID THE USER COMPLY?
            BCS     URDU1                               ; BRANCH IF NOT
            RTS                                         ; EXIT--CARRY IS CLEAR
URDU1:      INC     URDERR                              ; COLLECT USER ERRORS
            LDA     URDERR
            CMP     #3                                  ; ONLY THREE TRIES ALLOWED
            BCC     URDU                                ; RETRY MESSAGE IF LESS THAN THREE TRIES
            RTS                                         ; OTHERWISE RETURN WITH CARRY SET
;
;
;
;
;
; CLOSE UNCONDITIONAL
;
; (USER HAS REPLIED 'N' TO A VOLUME MOUNT REQUEST
; CLOSE ALL FILES ON VOLUME/UNLOG VOLUME
;
; INPUT ARGUMENT: (VCBPTR)
; OUTPUT ARGUMENT: NONE
;
CLOSEU      =       *
VSWA        =       ZPGTEMP                             ; THE 'SWAP BYTE' STORED HERE
            LDY     #VCBDEV                             ; FETCH
            LDA     (VCBPTR),Y                          ; THE DEVICE NUMBER
            STA     DEVNUM                              ; OF THIS VOLUME & SAVE IT
            LDY     #VCBSWAP                            ; FETCH THE
            LDA     (VCBPTR),Y                          ; SWAP BYTE
            STA     VSWA                                ; SAVE FOR REFERENCE, TOO
            LDA     #0
            LDY     #VCBNML                             ; UNLOG THE VOLUME
            STA     (VCBPTR),Y                          ; BY SETTING LEN OF VOL NAME TO ZERO
            LDY     #VCBSWAP
            STA     (VCBPTR),Y                          ; TURN OFF SWAP FLAG
            LDY     A:FCBADDRH                          ; SET UP FCB SCAN FROM BEGINNING OF FCB
            STY     FCBPTR+1
            LDY     #0
            STY     FCBPTR
VFCBLOP:    LDY     #FCBDEVN                            ; FETCH
            LDA     (FCBPTR),Y                          ; THE DEVICE
            CMP     DEVNUM                              ; NUMBER AND SEE IF A MATCH
            BNE     VFCBNXT                             ; BRANCH IF NO MATCH
            LDY     #FCBREFN                            ; SEE EVEN IF FILE OPEN
            LDA     (FCBPTR),Y
            BEQ     VFCBNXT                             ; BRANCH IF NOT
            LDY     #FCBSWAP                            ; CHECK TO SEE IF ATTACHED
            LDA     (FCBPTR),Y                          ; TO SAME VOLUME
            CMP     VSWA
            BNE     VFCBNXT                             ; BRANCH IF NOT
            LDY     #FCBBUFN                            ; RELEASE
            LDA     (FCBPTR),Y                          ; ANY
            JSR     RELBUF                              ; BUFFERS ASSOCIATED
            LDY     #FCBSWAP                            ; AND CLEAR
            LDA     #0                                  ; THE SWAP BYTE
            STA     (FCBPTR),Y
            LDY     #FCBREFN                            ; AND FINALLY
            STA     (FCBPTR),Y                          ; SAY 'CLOSED'
VFCBNXT:    LDA     FCBPTR
            CLC
            ADC     #$20                                ; FCB ENTRY SIZE
            STA     FCBPTR
            BCC     VFCBLOP
            LDA     FCBPTR+1
            INC     FCBPTR+1                            ; LOOK AT SECOND PAGE
            CMP     A:FCBADDRH
            BEQ     VFCBLOP                             ; CHECK PAGE TWO OF FCB
            RTS                                         ; RETURN TO USER W/O ERROR
;
FCBUSED     =       *                                   ; MARK AS FCB AS DIRTY SO
; THE DIRECTORY WILL BE FLUSHED ON 'FLUSH'
            STY     ZPGTEMP
            PHA                                         ; SAVE REGS
            LDY     #FCBDIRTY
            LDA     (FCBPTR),Y                          ; FETCH CURRENT FCBDIRTY BYTE
            ORA     #FCBMOD                             ; MARK FCB AS DIRTY
            STA     (FCBPTR),Y                          ; SAVE IT BACK
            PLA
            LDY     ZPGTEMP                             ; AND RESTORE REGS
            RTS
;
URDERR:     .RES    1                                   ; ERROR COUNT FOR USRREQ
;
;
UMB         =       *
            .BYTE   $49,$6E,$73,$65,$72,$74,$20
            .BYTE   $76,$6F,$6C,$75,$6D,$65
            .BYTE   $3A,$20                             ; "INSERT VOLUME: "
MVOL:       .RES    15
            .BYTE   $0D                                 ; CR LINE TERMINATOR
            .BYTE   $20,$20,$20,$20,$69,$6E,$20
            .BYTE   $64,$65,$76,$69,$63,$65
            .BYTE   $3A,$20                             ; "    IN DEVICE: "
MDEV:       .RES    15
            .BYTE   $0D                                 ; CR LINE TERMINATOR
            .BYTE   $74,$68,$65,$6E,$20,$70,$72
            .BYTE   $65,$73,$73,$20,$74,$68,$65,$20
            .BYTE   $41,$4C,$50,$48,$41,$20,$4C
            .BYTE   $4F,$43,$4B,$20,$6B,$65,$79
            .BYTE   $20,$74,$77,$69,$63,$65
; "THEN PRESS THE ALPHA LOCK KEY TWICE"
; FOLLOWED WITH $FF MESSAGE TERMINATOR (HIGH BIT SIGNIFICANT)
            .BYTE   $FF                                 ; MESSAGE TERMINATOR (HIGH BIT)
;
ZZEND       =       *
ZZLEN       =       ZZEND-ZZORG
            .IF     ZZLEN-LENBFM
            .FATAL  "SOSORG FILE IS INCORRECT FORMBFM"
            .ENDIF

