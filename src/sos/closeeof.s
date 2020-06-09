;PAGE
;
;
CLOSE:      LDA   C_REFNUM               ; CLOSE ALL?
            BNE   CLOSE1                 ; NO, JUST ONE OF 'EM
            STA   CFERR                  ; CLEAR GLOBAL CLOSE ERROR
            JSR   GFCBADR                ; SET UP POINTER TO FCB
CLOSALL:    LDA   #0                     ; BEGIN AT THE BEGINNING_
CLSALL1:    STA   FCBPTR                 ; SAVE CURRENT LOW BYTE OF POINTER
            LDY   #FCBLEVL               ; FETCH THE LEVEL AT WHICH
            LDA   (FCBPTR),Y             ; FILE WAS OPENED
            CMP   LEVEL                  ; TEST AGAINST CURRENT GLOBAL LEVEL
            BCC   NXTCLOS                ; DONT CLOSE IF FILES LEVEL IS < GLOBAL LEVEL
            LDY   #FCBREFN               ; INDEX TO REFERENCE NUMBER
            LDA   (FCBPTR),Y             ; IS THIS REFERENCE FILE OPEN?
            BEQ   NXTCLOS                ; NO, TRY NEXT_
            JSR   FLUSH2                 ; CLEAN IT OUT___
            BCS   CLOSERR                ; RETURN FLUSH ERRORS
            JSR   CLOSE2                 ; UPDATE FCB & VCB
            LDY   C_REFNUM
            BEQ   NXTCLOS                ; NO ERR IF CLOSE ALL
            BCS   CLOSERR
NXTCLOS:    LDA   FCBPTR                 ; BUMP POINTER TO NEXT FILE CONTROL BLOCK_
            CLC
            ADC   #$20
            BCC   CLSALL1                ; BRANCH IF WITHIN SAME PAGE_
            LDA   FCBPTR+1
            INC   FCBPTR+1               ; BUMP TO NEXT PAGE_
            CMP   A:FCBADDRH             ; HAVE WE CHECKED BOTH PAGES?
            BEQ   CLOSALL                ; YES, RETURN NO ERROR_
            CLC
            LDA   CFERR                  ; ON FINAL CLOSE OF CLOSE ALL REPORT LOGGED ERRORS
            BEQ   C3                     ; BRANCH IF NO ERRORS
            SEC
C3:         RTS
;
;
CFERR:      .RES  1                      ; GLOBAL ERROR FLAG FOR FLUSH AND CLOSE ALL
;
;
CLOSE1:     JSR   FLUSH1                 ; FLUSH FILE FIRST (INCLUDING UPDATING BIT MAP)
            BCS   CLOSERR
CLOSE2:     LDY   #FCBBUFN
            LDA   (FCBPTR),Y
            JSR   RELBUF
            BCS   CLOSERR
            LDA   #0
            LDY   #FCBREFN
            STA   (FCBPTR),Y
            INY                          ; BUMP TO 'FCBDEVN'
            LDA   (FCBPTR),Y
            STA   DEVNUM                 ; GO LOOK FOR ASSOCIATED VCB_
            JSR   DEVVCB
            LDX   VCBPTR                 ; GET VCBPTR
            DEC   VCB+VCBOPNC,X          ; INDICATE ONE LESS FILE OPEN_
            BNE   CLOSEND                ; BRANCH IF THAT WASN'T THE LAST___
            LDA   VCB+VCBSTAT,X
            AND   #$7F                   ; STRIP 'FILES OPEN' BIT
            STA   VCB+VCBSTAT,X
CLOSEND:    CLC
            RTS
CLOSERR:    JMP   GLBERR                 ; DON'T REPORT CLOSALL ERR NOW
;
;PAGE
;
FLUSH:      LDA   C_REFNUM               ; FLUSH ALL?
            BNE   FLUSH1                 ; NO, JUST ONE OF 'EM
            STA   CFERR                  ; CLEAR GLOBAL FLUSH ERROR
            JSR   GFCBADR                ; SET UP POINTER TO FCB
FLSHALL:    LDA   #0                     ; BEGIN AT THE BEGINNING_
FLSHAL1:    STA   FCBPTR                 ; SAVE CURRENT LOW BYTE OF POINTER
            LDY   #FCBREFN               ; INDEX TO REFERENCE NUMBER
            LDA   (FCBPTR),Y             ; IS THIS REFERENCE FILE OPEN?
            BEQ   NXFLUSH                ; NO, TRY NEXT_
            JSR   FLUSH2                 ; CLEAN IT OUT___
            BCS   FLSHERR                ; RETURN ANY ERRORS
;
            BCS   CLOSERR
NXFLUSH:    LDA   FCBPTR                 ; BUMP POINTER TO NEXT FILE CONTROL BLOCK_
            CLC
            ADC   #$20
            BCC   FLSHAL1                ; BRANCH IF WITHIN SAME PAGE_
            LDA   FCBPTR+1
            INC   FCBPTR+1               ; BUMP TO NEXT PAGE_
            CMP   A:FCBADDRH             ; HAVE WE CHECKED BOTH PAGES?
            BEQ   FLSHALL                ; YES, RETURN NO ERROR_
FLUSHEND:   CLC
            LDA   CFERR                  ; ON LAST FLUSH OF A FLUSH(0)
            BEQ   F3                     ; BRANCH IF NO LOGGED ERRORS
            SEC                          ; REPORT ERROR NOW
F3:         RTS
FLSHERR:    JMP   GLBERR                 ; FLUSH ALL OR ONE?
;
FLUSH2:     JSR   FNDFCBUF               ; MUST SET UP ASSOCIATED VCB AN BUFFER LOCATIONS FIRST_
            BCC   FLUSH2A                ; BRANCH IF NO ERROR ENCOUNTERED_
            JMP   GLBERR                 ; CHECK FOR CLOSE OR FLUSH ALL
;
FLUSH1:     LDA   #0                     ; CLEAR
            STA   CFERR                  ; GLOBAL ERROR FOR NORMAL REFNUM FLUSH
            JSR   FINDFCB                ; SET UP POINTER TO FCB USER REFERENCES
            BCS   FLSHERR                ; RETURN ANY ERRORS
FLUSH2A:    LDY   #FCBATTR               ; TEST TO SEE IF FILE IS
            LDA   (FCBPTR),Y             ; MODIFIED_ FIRST TEST WRITE ENABLED_
            AND   #WRITEN
            BEQ   FLUSHEND               ; BRANCH IF 'READ ONLY'
            LDY   #FCBDIRTY              ; SEE IF EOF HAS BEEN MODIFIED
            LDA   (FCBPTR),Y
            BMI   FLUSH2B                ; BRANCH IF IT HAS
            LDY   #FCBSTAT               ; NOW TEST FOR DATA MODIFIED_
            LDA   (FCBPTR),Y             ; (IN OTHER WORDS: WAS FILE ACTUALLY
            AND   #USEMOD+EOFMOD+DATMOD  ; WRITTEN TO WHILE IT'S BEEN OPEN?)
            BEQ   FLUSHEND               ; BRANCH IF FILE NOT MODIFIED_
FLUSH2B:    JSR   TWRPROT1               ; DISK SWITCH CHECKING
            LDA   DSWGLOB
            BEQ   FLUSH2C                ; BRANCH IF NO SWITCH
            LDA   #XDISKSW
            SEC
            RTS                          ; FORCES A VERIFIED RETRY
FLUSH2C:    LDY   #FCBSTAT               ; NOW TEST FOR DATA MODIFIED_
            LDA   (FCBPTR),Y
            AND   #DATMOD                ; DOES CURRENT DATA BUFFER NEED TO BE
            BEQ   FLUSH3                 ; WRITTEN? BRANCH IF NOT_
            JSR   WFCBDAT                ; IF SO, GO WRITE IT STUPID!
            BCS   FLSHERR
FLUSH3:     LDY   #FCBSTAT               ; CHECK TO SEE IF THE INDEX BLOCK (TREE FILES ONLY)
            LDA   (FCBPTR),Y             ; NEEDS TO BE WRITTEN_
            AND   #IDXMOD
            BEQ   FLUSH4                 ; BRANCH IF NOT___
            JSR   WFCBIDX
            BCS   FLSHERR                ; RETURN ANY ERRORS_
;PAGE
;
FLUSH4:     LDY   #FCBENTN               ; NOW PREPARE TO UPDATE DIRECTORY
OWNRMOV:    LDA   (FCBPTR),Y             ; NOTE: THIS CODE DEPENDS ON THE
            STA   D_DEV-FCBDEVN,Y        ; DEFINED ORDER OF THE FILE CONTROL
            DEY                          ; BLOCK AND THE TEMPORARY DIRECTORY AREA IN 'WORKSPC'! *************
            CPY   #FCBDEVN-1
            BNE   OWNRMOV
            LDA   D_HEAD                 ; READ IN THE DIRECTORY HEADER FOR THIS FILE
            STA   BLOKNML
            LDA   D_HEAD+1
            STA   BLOKNMH
            LDA   D_DEV
            STA   DEVNUM
            JSR   RDGBUF                 ; READ IT INTO THE GENERAL PURPOSE BUFFER
            BCS   FLSHERR                ; BRANCH IF ERROR_
            JSR   MOVHED0                ; MOVE HEADER INFO_
            LDA   D_ENTBLK               ; GET ADDRESS OF DIRECTORY BLOCK THAT
            LDY   D_ENTBLK+1             ; CONTAINS THE FILE ENTRY_
            CMP   D_HEAD                 ; TEST TO SEE IF IT'S THE SAME BLOCK THAT
            BNE   FLSHEBLK               ; THE HEADER IS IN_ BRANCH IF NOT_
            CPY   D_HEAD+1
            BEQ   FLUSH5                 ; BRANCH IF HEADER BLOCK = ENTRY BLOCK_
FLSHEBLK:   STA   BLOKNML
            STY   BLOKNMH
            JSR   RDGBUF                 ; GET BLOCK WITH FILE ENTRY IN GENERAL BUFFER_
FLUSH5:     JSR   ENTCALC                ; SET UP POINTER TO ENTRY
            JSR   MOVENTRY               ; MOVE ENTRY TO TEMP ENTRY BUFFER IN 'WORKSPC'
            LDY   #FCBUSE                ; UPDATE 'BLOCKS USED' COUNT_
            LDA   (FCBPTR),Y
            STA   A:DFIL+D_USAGE
            INY
            LDA   (FCBPTR),Y
            STA   A:DFIL+D_USAGE+1         ; HI BYTE TOO___
            LDY   #FCBEOF                ; AND MOVE IN END OF FILE MARK WHETHER
EOFUPDTE:   LDA   (FCBPTR),Y             ; WE NEED TO OR NOT_
            STA   A:DFIL+D_EOF-FCBEOF,Y
            INY                          ; MOVE ALL THREE BYTES_
            CPY   #FCBEOF+3
            BNE   EOFUPDTE
            LDY   #FCBFRST               ; ALSO MOVE IN THE ADDRESS OF
            LDA   (FCBPTR),Y             ; THE FILE'S FIRST BLOCK SINCE
            INY                          ; IT MIGHT HAVE CHANGED SINCE THE FILE
            STA   A:DFIL+D_FRST          ; FIRST OPENED_
            LDA   (FCBPTR),Y
            STA   A:DFIL+D_FRST+1
;PAGE
            LDY   #FCBSTYP               ; AND THE LAST THING TO UPDATE IS
            LDA   (FCBPTR),Y             ; THE STORAGE TYPE_
            ASL   A                      ; (SHIFT IT INTO THE HI NIBBLE)
            ASL   A
            ASL   A
            ASL   A
            STA   SCRTCH
            LDA   A:DFIL+D_STOR          ; GET OLD TYPE BYTE (IT MIGHT BE THE SAME)
            AND   #$F                    ; STRIP OFF OLD TYPE
            ORA   SCRTCH                 ; ADD IN THE NEW TYPE,
            STA   A:DFIL+D_STOR          ; AND PUT IT AWAY_
            JSR   DREVISE                ; GO UPDATE DIRECTORY!
            BCS   FLUSHERR
            LDY   #FCBDIRTY              ; MARK
            LDA   (FCBPTR),Y             ; FCB/DIRECTORY
            AND   #$FF-FCBMOD            ; AS
            STA   (FCBPTR),Y             ; UNDIRTY
            LDX   #0                     ; NOW CHECK TO SEE IF A BIT MAP
            LDA   D_DEV                  ; IS LYING AROUND THAT SHOULD BE WRITTEN_
            CMP   BMADEV                 ; IS IT IN MAP BUFFER A?
            BEQ   BMAPUP                 ; YES, PUT IT ON THE DISK IF NECESSARY_
            LDX   #BMTABSZ               ; SET INDEX TO BIT MAP TABLE 'B'
            CMP   BMBDEV                 ; NO, WHAT ABOUT BIT MAP BUFFER B?
            BNE   FLSHEND1               ; NOPE, ALL DONE_
BMAPUP:     LDA   BMASTAT,X              ; TEST TO SEE IF IT'S BEEN MODIFIED_
            BPL   FLSHEND1               ; NOPE, ALL DONE AS I SAID_
            STX   BMTAB
            JSR   WRTBMAP                ; GO PUT IT AWAY_
            BCS   FLUSHERR
            LDX   BMTAB                  ; MARK MAP AS UPDATED
            LDA   #0
            STA   BMASTAT,X
FLSHEND1:   CLC
            RTS
FLUSHERR    =     *                      ; DROP INTO GLBERR
;
GLBERR      =     *                      ; REPORT ERROR IMMEDIATELY
; ONLY IF NOT A CLOSE ALL OR FLUSH ALL
            LDX   C_REFNUM
            BNE   GLBERR1                ; NOT AN 'ALL' SO REPORT NOW
            CLC
            STA   CFERR                  ; SAVE FOR LATER
GLBERR1:    RTS
;
;
GFCBADR:    LDA   FCBANKNM               ; GET BANK THAT FCB IS IN
            STA   SISFCBP
            LDA   A:FCBADDRH             ; AND HIGH BYTE ADDRESS OF FILE CONTORL BLOCK_
            STA   FCBPTR+1
            RTS                          ; SILLY THAT IT'S SO SHORT___
;
SETERR:     LDA   #ACCSERR
            SEC
EOFRETN:    RTS
;PAGE
;
SETEOF:     LDY   #FCBSTYP               ; ONLY KNOW HOW TO MOVE EOF OF TREE TYPE
            LDA   (FCBPTR),Y
            CMP   #TRETYP+1
            BCS   SETERR                 ; BRANCH IF OTHER THAN TREE
            LDY   #FCBATTR               ; NOW CHECK TO INSURE WRITE IS ENABLED_
            LDA   (FCBPTR),Y
            AND   #WRITEN                ; CAN WE SET NEW EOF?
            BEQ   SETERR                 ; NOPE, ACCESS ERROR_
            JSR   TSTWPROT               ; FIND OUT IF MOD IS POSIBLE (HARDWARE WRITE PROTECT)
            BCS   SETERR
            LDY   #FCBEOF+2              ; SAVE OLD EOF
            LDX   #2                     ; SO IT CAN BE SEEN
SETSAVE:    LDA   (FCBPTR),Y             ; WHETHER BLOCKS NEED
            STA   OLDEOF,X               ; TO BE RELEASED
            DEY                          ; UPON
            DEX                          ; CONTRACTION
            BPL   SETSAVE                ; ALL THREE BYTES OF THE EOF
            JSR   ADJMARK                ; GET ADJUSTED END OF FILE ACCORDING TO 'C_BASE' INTO TPOS_
            BCS   EOFRETN                ; RETURN ANY ERROR IMMEDIATELY
            LDX   #2
NEOFPOS:    LDA   TPOSLL,X               ; POSITION MARK TO NEW EOF
            STA   C_NEWEOF,X
            DEX
            BPL   NEOFPOS
            LDY   #FCBMARK+2             ; FIND OUT IF EOF < MARK_
            LDX   #2
NEOFTST:    LDA   (FCBPTR),Y
            CMP   C_NEWEOF,X             ; COMPARE UNTIL NOT EQUAL OR CARRY CLEAR
            BCC   SETEOF1                ; BRANCH IF EOF>MARK
            BNE   SETEOF0                ; BRANCH IF EOF<MARK
            DEY
            DEX
            BPL   NEOFTST                ; LOOP ON ALL THREE BYTES
SETEOF0:    JSR   RDPOSN                 ; READ IN NEW POSITION_
            BCS   EOFRETN                ; RETURN ANY ERRORS_
SETEOF1:    LDX   #2
            LDY   #FCBEOF+2              ; MOVE NEW EOF TO FCB_
SETEOF2:    LDA   C_NEWEOF,X
            STA   (FCBPTR),Y
            DEY
            DEX
            BPL   SETEOF2                ; MOVE ALL THREE BYTES_
            JSR   FCBUSED                ; MARK FCB AS DIRTY (FOR FLUSH)
;
            LDX   #2                     ; POINT TO THIRD BYTE
PURTEST:    LDA   OLDEOF,X               ; SEE IF EOF MOVED BACKWARDS
            CMP   C_NEWEOF,X             ; SO BLOCKS CAN
            BCC   PURTEST1               ; BE RELEASED (BRANCH IF NOT)
            BNE   PURGE                  ; BRANCH IF BLOCKS TO BE RELEASED
            DEX
            BPL   PURTEST                ; ALL THREE BYTES
PURTEST1:   JMP   FLSHEND1               ; NEW EOF NOT SMALLER
TRELEAS1:   JMP   TRELEASE               ; OVERFLOW PREVENTER
;
PURGE:      LDY   #FCBSTYP               ; FIND OUT WHAT TYPE OF TREE
            LDA   (FCBPTR),Y             ; TO PERFORM THE PROPER
            CMP   #SEEDTYP               ; STYLE OF BLOCK RELEASE
            BEQ   EOFOUT                 ; SEED DON'T DEALLOCATE
            CMP   #TRETYP                ; FULL TREE?
            BEQ   TRELEAS1               ; BRANCH IF YES
;
; IF WE GET HERE, WE ARE RELEASING
; BLOCKS AT THE END OF A SAPLING FILE: CALCULATE CORRECT POSITION
; WITHIN THE INDEX BLOCK AND ALLOW SUBROUTINE
; PURGE LATTER BLOCKS TO DEALLOCATE
; ALL THE DATA BLOCKS THAT FOLLOW
;
            JSR   FNDBMAP                ; REFRESH THE RIGHT MAP FOR THIS VOLUME
            LDX   TPOSHI                 ; PRELOAD
            LDY   TPOSLH                 ;  THE THREE EOF
            LDA   TPOSLL                 ;    BYTES
            BNE   PUR1                   ; BRANCH IF NO BOUNDARY ADJUSTMENT NEEDED
            CPY   #0
            BNE   PUR2                   ; MIDDLE BYTE ZERO MEANS NO CARRY
            CPX   #0                     ; ALL BYTES ZERO??
            BEQ   PUR1                   ; BRANCH IF YES
            DEX
;
; THESE LINES IF CODE, SOMEWHAT CRYPTIC,
; CALCULATE THE POINT AT WHICH THE
; LAST BLOCK CONTAINING THE LAST BIT
; OF DATUM
;
; THE FOLLOWING IS ROUGHLY A /512
; ALGORITHM
;
PUR2:       DEY
PUR1:       TXA
            LSR   A
            TYA
            ROR   A
;
            JSR   PURLBLKS               ; MAKES A GOOD PTR TO DO THE RELEASING
            LDY   #FCBSTAT               ; MARK INDEX BLOCK
            LDA   (FCBPTR),Y             ; AS DIRTY
            ORA   #IDXMOD
            STA   (FCBPTR),Y
            LDA   PURUSE                 ; INDICATE NEW NUMBER OF BLOCKS USED
            CLC
            ADC   #2                     ; ACCOUNT FOR CARDINAL AND INDEX
            LDY   #FCBUSE
            STA   (FCBPTR),Y             ; FILE LOW BYTE
            INY
            LDA   #0                     ; ANTICIPATE <257 BLOCKS
            BCC   PURHI
            LDA   #1                     ; >256 BLOCKS IN FILE
PURHI:      STA   (FCBPTR),Y             ; HIGH BYTE BLOCKS USED
EOFOUT:     CLC
            RTS                          ; NO ERRORS POSSIBLE
;
PURLBLKS    =     *                      ; PURGE LATTER BLOCKS
; INPUT ARG: A REGISTER CONTAINING
; POINTER TO CURRENT DATA BLOCK WITHIN THE
; CURRENT INDEX BLOCK (TINDX)
; DEALLOCATE ALL LEGAL BLOCKS AFTER
; THE A REGISTER PTR. NO ERRORS POSSIBLE
;
            TAY                          ; MAKE PROPER INDEX
            STY   PURUSE                 ; INDICATES NUMBER OF BLOCKS IN USE IN FILE
PURLOOP:    INY                          ; POINT TO A PTR TO DATA BLK TO DEALLOCATE
            BEQ   PURLRTS                ; NO MORE BLOCKS IN INDEX
            INC   TINDX+1                ; GET HIGH PART OF BLOCK ADDR
            LDA   (TINDX),Y
            TAX                          ; X IS A PASSING PARM
            LDA   #0                     ; TELL INDEX BLOCK THAT THE DATA
            STA   (TINDX),Y              ; BLOCK IS NOW FREE
            TXA
            DEC   TINDX+1                ; AND LOW PART
            ORA   (TINDX),Y
            BEQ   PURLOOP                ; INDICATED ADDR WAS ZERO-ZERO
            LDA   (TINDX),Y              ; A REG IS ANOTHER PASSING PARM
            PHA
            LDA   #0
            STA   (TINDX),Y              ; AND SET LOW DATA ADDR AS FREED
            PLA
            STY   PURPLACE               ; TEMP STORAGE
            JSR   DEALLOC                ; DEALLOCATE BLOCK (ADDR: A (LOW), X ( HIGH)
            LDY   #VCBTFRE
            CLC
            LDA   (VCBPTR),Y             ; ADJUST NUMBER OF FREE BLOCKS ON VOLUME
            ADC   #1
            STA   (VCBPTR),Y
            INY
            LDA   (VCBPTR),Y             ; HIGH BYTE OF TOTAL FREE
            ADC   #0
            STA   (VCBPTR),Y
            LDY   PURPLACE
            JMP   PURLOOP
PURLRTS:    RTS
PURUSE:     .RES  1                      ; CURRENT NUMBER OF BLOCKS USED
PURPLACE:   .RES  1                      ; CURRENT PLACE IN RELEASE-BLOCK CYCLE
TRELEASE    =     *
            JMP   EOFOUT                 ; RELEASE TWO LEVEL TREE CODE GOES HERE
;
GETEOF:     LDY   #FCBEOF                ; INDEX TO END OF FILE MARK
            LDX   #0                     ; WE'VE GOT INDIRECT BOTH WAYS (IN & OUT)
OUTEOF:     LDA   (FCBPTR),Y
            STA   (C_OUTEOF,X)
            INY
            CPY   #FCBEOF+3
            BEQ   OFFRTS                 ; BRANCH IF ALL THREE BYTES TRANSFERED_
            INC   C_OUTEOF               ; BUMP USER'S POINTER_
            BNE   OUTEOF
            INC   C_OUTEOF+1
            BNE   OUTEOF                 ; BRANCH ALWAYS
;

