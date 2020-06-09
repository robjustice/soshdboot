;PAGE
READ:       CLC                                 ; FIRST DETERMINE IF REQESTED
            LDY   #FCBATTR                      ; READ IS LEGAL
            LDA   (FCBPTR),Y
            AND   #READEN                       ; IS READ ENABLED?
            BNE   READ1                         ; YES, CONTINUE___
            LDA   #ACCSERR                      ; REPORT ILLEGAL ACCESS_
            SEC
            RTS
;
READ1:      LDY   #FCBMARK                      ; GET CURRENT MARK INTO 'TPOS' AND
            LDA   (FCBPTR),Y                    ; DETERMINE IF RESULTING POSITION
            STA   TPOSLL                        ; EXCEEDS CURRENT END OF FILE_
            ADC   C_BYTES
            STA   SCRTCH
            INY
            LDA   (FCBPTR),Y
            STA   TPOSLH
            ADC   C_BYTES+1                     ; (THIS WAS DONE STRAIT-LINE SINCE
            STA   SCRTCH+1                      ; WE'RE ADDING A TWO BYTE TO A THREE
            INY                                 ; BYTE QUANTITY)
            LDA   (FCBPTR),Y
            STA   TPOSHI
            ADC   #0                            ; ADD IN REMAINING CARRY_
            STA   SCRTCH+2
            LDY   #FCBEOF+2                     ; NOW TEST EOF AGAINST POSITION GENERATED
EOFTEST:    LDA   SCRTCH-FCBEOF,Y
            CMP   (FCBPTR),Y                    ; IS NEW POSITION > EOF?
            BCC   READ2                         ; NO, PROCEED_
            BNE   ADJSTCNT                      ; YES, ADJUST 'C_BYTES' REQUEST
            DEY
            CPY   #FCBEOF-1                     ; HAVE WE COMPARED ALL TREE BYTES?
            BNE   EOFTEST                       ; NO, TEST NEXT LOWEST_
ADJSTCNT    =     *                             ; ADJUST REQUEST TO READ UP TO (BUT
            LDY   #FCBEOF                       ; NOT INCLUDING) END OF FILE_
            LDA   (FCBPTR),Y                    ; RESULT= (EOF-1)-POSITION
            SBC   TPOSLL
            STA   C_BYTES
            INY
            LDA   (FCBPTR),Y
            SBC   TPOSLH
            STA   C_BYTES+1
            ORA   C_BYTES                       ; IF BOTH BYTES ARE ZERO, REPORT EOF ERROR_
            BNE   READ2
            LDA   #EOFERR
            JSR   SYSERR
READ2:      LDA   C_BYTES
            STA   RWREQL
            BNE   READ3                         ; BRANCH IF READ REQUEST DEFINITELY NON-ZERO_
            CMP   C_BYTES+1
            BNE   READ3                         ; BRANCH IF READ REQUEST<>ZERO
            STA   RWREQH
GORDDNE:    JMP   READONE                       ; DO NOTHING_
;PAGE
;
READ3:      LDA   C_BYTES+1
            STA   RWREQH
            LDA   C_OUTBUF                      ; MOVE POINTER TO USERS BUFFER TO BFM
            STA   USRBUF                        ; Z-PAGE AREA_
            LDX   #C_OUTBUF                     ; <SRS 82_162>
            JSR   WRAPADJ                       ; ADJUST FOR BANK CROSSING_ <SRS 82_162>
            STA   USRBUF+1
            STY   SISUSRBF                      ; SAVE VALID USER BUFFER ADDRESS (THAT WILL NOT CROSS BANKS)
            LDY   #FCBSTYP                      ; NOW FIND OUT IF IT'S A TREE READ OR OTHER_
            LDA   (FCBPTR),Y
            CMP   #TRETYP+1
            BCC   TREAD                         ; BRANCH IF A TREE FILE_
            JMP   DREAD                         ; OTHEWISE ASSUME IT'S A DIRECTORY_
;
TREAD:      JSR   RDPOSN                        ; GET DATA POINTER SET UP_
            BCC   TREAD0                        ; REPORT ANY ERRORS
            JMP   ERRFIX1
TREAD0:     JSR   PREPRW                        ; TEST FOR NEWLINE, SETS UP FOR PARTIAL READ_
            JSR   READPART                      ; MOVE CURRENT DATA BUFFER CONTENTS TO USER AREA
            BVS   GORDDNE                       ; BRANCH IF REQUEST IS SATISFIED_
            BCS   TREAD                         ; CARRY SET INDICATES NEWLINE IS SET_
            LDA   RWREQH                        ; FIND OUT HOW MANY BLOCKS ARE TO BE READ
            LSR   A                             ; IF LESS THAN TWO, THEN DO IT THE SLOW WAY_
            BEQ   TREAD
            STA   BULKCNT                       ; SAVE BULK BLOCK COUNT_
            LDY   #FCBSTAT                      ; MAKE SURE CURRENT DATA AREA
            LDA   (FCBPTR),Y                    ; DOESN'T NEED TO BE WRITTEN BEFORE
            AND   #DATMOD                       ; RESETTING POINTER TO READ DIRECTLY INTO
            BNE   TREAD                         ; USER'S AREA_ BRANCH IF DATA NEED TO BE WRITTEN
            STA   IOACCESS                      ; TO FORCE FIRST CALL THRU ALL DEVICE HANDLER CHECKING_
            LDA   USRBUF                        ; MAKE THE DATA BUFFER THE USER'S SPACE_
            STA   DATPTR
            LDA   USRBUF+1
            STA   DATPTR+1
            LDA   SISUSRBF
            STA   SISDATP
;
;PAGE
RDFAST:     JSR   RDPOSN                        ; GET NEXT BLOCK DIRECTLY INTO USER SPACE_
            BCS   ERRFIX                        ; BRANCH ON ANY ERROR_
RDFAST0:    INC   DATPTR+1                      ; BUMP ALL POINTERS BY 512 (ONE BLOCK)
            INC   DATPTR+1
            DEC   RWREQH
            DEC   RWREQH
            INC   TPOSLH
            INC   TPOSLH
            BNE   RDFAST1                       ; BRANCH IF POSITION DOES NOT GET TO A 64K BOUNDARY_
            INC   TPOSHI                        ; OTHERWISE, MUST CHECK FOR A 128K BOUNDARY
            LDA   TPOSHI                        ; SET CARRY IF MOD 128K HAS BEEN REACHED
            EOR   #1
            LSR   A
RDFAST1:    DEC   BULKCNT                       ; HAVE WE READ ALL WE CAN FAST?
            BNE   RDFAST2                       ; BRANCH IF MORE TO READ_
            JSR   FXDATPTR                      ; GO FIX UP DATA POINTER TO SOS BUFFER_
            LDA   RWREQL                        ; TEST FOR END OF READ_
            ORA   RWREQH                        ; ARE BOTH ZERO?
            BEQ   READONE
            BNE   TREAD                         ; NO, READ LAST PARTIAL BLOCK_
;
RDFAST2:    BCS   RDFAST
            LDA   TPOSHI                        ; GET INDEX TO NEXT BLOCK ADDRESS
            LSR   A
            LDA   TPOSLH
            ROR   A
            TAY                                 ; INDEX TO ADDRESS IS INT(POS/512)
            LDA   (TINDX),Y                     ; GET LOW ADDRESS
            STA   BLOKNML
            INC   TINDX+1
            CMP   (TINDX),Y                     ; ARE BOTH HI AND LOW ADDRESS THE SAME?
            BNE   REALRD                        ; NO, IT'S A REAL BLOCK ADDRESS_
            CMP   #0                            ; ARE BOTH BYTES ZERO?
            BNE   REALRD                        ; NOPE -- MUST BE REAL DATA
            STA   IOACCESS                      ; DON'T DO REPEATIO JUST AFTER SPARSE
            BEQ   NOSTUF                        ; BRANCH ALWAYS (CARRY SET)
REALRD:     LDA   (TINDX),Y                     ; GET HIGH ADDRESS BYTE
            CLC
NOSTUF:     DEC   TINDX+1
            BCS   RDFAST                        ; BRANCH IF NO BLOCK TO READ
            STA   BLOKNMH
            LDA   IOACCESS                      ; HAS FIRST CALL GONE TO DEVICE YET?
            BEQ   RDFAST                        ; NOPE, GO THRU NORMAL ROUTE___
            LDA   DATPTR+1                      ; RESET HI BUFFER ADDRESS FOR DEVICE HANDLER
            STA   DBUFPH
            JSR   REPEATIO
            BCC   RDFAST0                       ; BRANCH IF NO ERRORS_
;PAGE
ERRFIX:     PHA                                 ; SAVE ERROR CODE
            JSR   FXDATPTR                      ; GO RESTORE DATA POINTERS, ETC___
            PLA
ERRFIX1:    PHA                                 ; SAVE ERROR CODE
            JSR   READONE                       ; PASS BACK NUMBER OF BYTES ACTUALLY READ_
            PLA
            SEC                                 ; REPORT ERROR
            RTS
;
READONE:    LDY   #0                            ; RETURN TOTAL NUMBER OF BYTES ACTUALLY READ
            SEC                                 ; THIS IS DERIVED FROM C_BYTES-RWREQ
            LDA   C_BYTES
            SBC   RWREQL
            STA   (C_OUTCNT),Y
            INY
            LDA   C_BYTES+1
            SBC   RWREQH
            STA   (C_OUTCNT),Y
            JMP   RDPOSN                        ; LEAVE WITH VALID POSITION IN FCB_
;
PREPRW:     SEC                                 ; ADJUST POINTER TO USER'S BUFFER TO
            LDA   USRBUF                        ; MAKE THE TRANSFER
            SBC   TPOSLL
            STA   USRBUF
            BCS   PREPRW1                       ; BRANCH IF NO ADJUSTMENT TO HI ADDR_ NEEDED_
            DEC   USRBUF+1                      ; NOTE: SARA ALLOWS INDIRECT FROM $101 UP
PREPRW1:    LDY   #FCBATTR                      ; AS LONG AS ACTUAL RESULTING ADDRESS IS >=$200
            LDA   (FCBPTR),Y                    ; TEST FOR NEW LINE ENABLED
            AND   #NLINEN                       ; SET CARRY IF IT IS_
            CLC
            BEQ   NONEWLIN                      ; BRANCH IF NEWLINE IS NOT ENABLED
            SEC
            LDY   #FCBNEWL
            LDA   (FCBPTR),Y                    ; MOVE NEWLINE CHARACTER TO MORE
            STA   NLCHAR                        ; ACCESSABLE SPOT_
NONEWLIN:   LDY   TPOSLL                        ; GET INDEX TO FIRST DATA
            LDA   DATPTR                        ; RESET LOW ORDER OF POSPTR TO BEGINNING OF PAGE_
            STA   POSPTR
            LDX   RWREQL                        ; AND LASTLY GET LOW ORDER COUNT OF REQUESTED BYTES_
            RTS                                 ; RETURN STATUSES___
;
READPART:   TXA
            BNE   RDPART0                       ; BRANCH IF REQUEST IS NOT A EVEN PAGES
            LDA   RWREQH                        ; A CALL OF ZERO BYTES SHOULD NEVER GET HERE!
            BEQ   SETRDNE                       ; BRANCH IF NOTHIN' TO DO_
            DEC   RWREQH
RDPART0:    DEX
RDPART:     LDA   (POSPTR),Y                    ; MOVE DATA TO USER'S BUFFER
            STA   (USRBUF),Y                    ; ONE BYTE AT A TIME_
            TXA                                 ; NOTE: THIS ROUTINE IS CODED TO BE
            BEQ   ENDRQCHK                      ; FASTEST WHEN NEWLINE IS DISABLED_
RDPART1:    BCS   TSTNEWL                       ; BRANCH IF NEW LINE NEEDS TO BE TESTED_
RDPART2:    DEX
            INY                                 ; PAGE CROSSED?
            BNE   RDPART                        ; NO_ MOVE NEXT BYTE_
            LDA   POSPTR+1                      ; TEST FOR END OF BUFFER
            INC   USRBUF+1                      ; BUT FIRST ADJUST USER BUFFER POINTER
            INC   TPOSLH                        ; AND POSITION_
            BNE   RDPART3
            INC   TPOSHI
RDPART3:    INC   POSPTR+1                      ; AND SOS BUFFER HIGH ADDRESS_
            EOR   DATPTR+1                      ; (CARRY HAS BEEN CLEVERLY UNDISTURBED_)
            BEQ   RDPART                        ; BRANCH IF MORE TO READ IN BUFFER_
            CLV                                 ; INDICATE NOT FINISHED_
            BVC   RDPRTDNE                      ; BRANCH ALWAYS_
;
ENDRQCHK:   LDA   RWREQH
            BEQ   RDRQDNE                       ; BRANCH IF REQEST SATISFIED_
            INY                                 ; DONE WITH THIS BLOCK OF DATA?
            BNE   ENDRCHK1                      ; NO, ADJUST HIGH BYTE OF REQUEST_
            LDA   POSPTR+1                      ; MAYBE- CHECK FOR END OF BLOCK BUFFER_
            EOR   DATPTR+1                      ; (DON'T DISTURB CARRY)
            BNE   ENDRCHK2                      ; BRANCH IF HI COUNT CAN BE DEALT WITH NEXT TIME_
ENDRCHK1:   DEC   RWREQH
ENDRCHK2:   DEY                                 ; RESTORE PROPER VALUE TO 'Y'
            JMP   RDPART1
;
TSTNEWL:    LDA   (POSPTR),Y                    ; GET LAST BYTE TRANSFERED AGAIN_
            EOR   NLCHAR                        ; HAVE WE MATCHED NEWLINE CHARACTER?
            BNE   RDPART2                       ; NO, READ NEXT_
RDRQDNE:    INY                                 ; ADJUST POSITION_
            BNE   SETRDNE
            INC   USRBUF+1                      ; BUMP POINTERS_
            INC   TPOSLH
            BNE   SETRDNE
            INC   TPOSHI
SETRDNE:    BIT   SETVFLG                       ; (SET V FLAG)
RDPRTDNE:   STY   TPOSLL                        ; SAVE LOW POSITION
            BVS   RDONE1
            INX                                 ; LEAVE REQUEST AS +1 FOR NEXT CALL
RDONE1:     STX   RWREQL                        ; AND REMAINDER OF REQUEST COUNT_
            PHP                                 ; SAVE STATUSES
            CLC                                 ; ADJUST USER'S LOW BUFFER ADDRESS
            TYA
            ADC   USRBUF
            STA   USRBUF
            BCC   RDPART4
            INC   USRBUF+1                      ; ADJUST HI ADDRESS AS NEEDED_
RDPART4:    PLP                                 ; RESTORE RETURN STATUSES
SETVFLG:    RTS                                 ; (THIS BYTE <$60> IS USED TO SET V FLAG)
;
FXDATPTR:   LDA   DATPTR                        ; PUT CURRENT USER BUFFER
            STA   USRBUF                        ; ADDRESS BACK TO NORMAL
            LDA   DATPTR+1
            STA   USRBUF+1                      ; BANK PAIR BYTE SHOULD BE MOVED ALSO_
            LDA   SISDATP
            STA   SISUSRBF
            LDY   #FCBBUFN                      ; RESTORE BUFFER ADDRESS
            LDA   (FCBPTR),Y
            LDX   #DATPTR
            JMP   GETBUFADR                     ; END VIA CALL TO BOB'S CODE_
;
;PAGE
;
; READ DIRECTORY FILE...
;
DREAD:      JSR   RDPOSN
            BCS   ERRDRD                        ; PASS BACK ANY ERRORS
            JSR   PREPRW                        ; PREPARE FOR TRANSFER_
            JSR   READPART                      ; MOVE DATA TO USER'S BUFFER
            BVC   DREAD                         ; REPEAT UNTIL REQUEST IS SATISFIED_
            JSR   READONE                       ; UPDATE FCB AS TO NEW POSITION_
            BCC   DREDONE                       ; BRANCH IF ALL IS WELL_
            CMP   #EOFERR                       ; WAS LAST READ TO END OF FILE?
            SEC                                 ; ANTICIPATE SOME OTHER PROBLEM
            BNE   DREDERR                       ; BRANCH IF NOT EOF ERROR_
            JSR   SVMARK
            JSR   ZIPDATA                       ; CLEAR OUT DATA BLOCK_
            LDY   #FCBDATB+1                    ; PROVIDE DUMMY BACK POINTER FOR FUTURE RE-POSITION
            LDA   (FCBPTR),Y                    ; GET HI BYTE OF LAST BLOCK_
            PHA
            DEY
            LDA   (FCBPTR),Y                    ; AND LOW BYTE_
            PHA
            LDA   #0                            ; NOW MARK CURRENT BLOCK AS IMPOSIBLE_
            STA   (FCBPTR),Y
            INY
            STA   (FCBPTR),Y
            TAY                                 ; NOW MOVE LAST BLOCK ADDRESS TO DATA BUFFER AS BACK POINTER_
            PLA
            STA   (DATPTR),Y
            PLA
            INY
            STA   (DATPTR),Y
DREDONE:    CLC                                 ; INDICATE NO ERROR
DREDERR:    RTS
;
ERRDRD:     JMP   ERRFIX1                       ; REPORT HOW MUCH WE COULD TRANSFER BEFORE ERROR_
;
;PAGE
WRITE:      CLC                                 ; FIRST DETERMINE IF REQESTED
            LDY   #FCBATTR                      ; WRITE IS LEGAL
            LDA   (FCBPTR),Y
            AND   #WRITEN                       ; IS WRITE ENABLED?
            BNE   WRITE1                        ; YES, CONTINUE___
ERRACCS:    LDA   #ACCSERR                      ; REPORT ILLEGAL ACCESS_
            SEC
WPERROR:    RTS
;
WRITE1:     JSR   TSTWPROT                      ; OTHERWISE, MAKE SURE DEVICE IS NOT WRITE PROTECTED_
            BCS   WPERROR                       ; REPORT WRITE PROTECTED AND ABORT OPERATION_
;
            LDY   #FCBMARK                      ; GET CURRENT MARK INTO 'TPOS' AND
            LDA   (FCBPTR),Y                    ; DETERMINE IF RESULTING POSITION
            STA   TPOSLL                        ; EXCEEDS CURRENT END OF FILE_
            ADC   C_BYTES
            STA   SCRTCH
            INY
            LDA   (FCBPTR),Y
            STA   TPOSLH
            ADC   C_BYTES+1                     ; (THIS WAS DONE STRAIGHT-LINE SINCE
            STA   SCRTCH+1                      ; WE'RE ADDING A TWO BYTE TO A THREE
            INY                                 ; BYTE QUANTITY)
            LDA   (FCBPTR),Y
            STA   TPOSHI
            ADC   #0                            ; ADD IN REMAINING CARRY_
            STA   SCRTCH+2
            LDY   #FCBEOF+2                     ; NOW TEST EOF AGAINST POSITION GENERATED
WEOFTST:    LDA   SCRTCH-FCBEOF,Y
            CMP   (FCBPTR),Y                    ; IS NEW POSITION > EOF?
            BCC   WRITE2                        ; NO, PROCEED_
            BNE   WADJEOF                       ; YES, ADJUST END OF FILE
            DEY
            CPY   #FCBEOF-1                     ; HAVE WE COMPARED ALL TREE BYTES?
            BNE   WEOFTST                       ; NO, TEST NEXT LOWEST_
WADJEOF:    CLC                                 ; ADJUST REQUEST TO WRITE UP TO (BUT
            LDY   #FCBEOF                       ; NOT INCLUDING) END OF FILE_
WRTADJEOF:  LDA   (FCBPTR),Y                    ; SAVE OLD EOF IN CASE OF LATER ERROR
            STA   OLDEOF-FCBEOF,Y
            LDA   SCRTCH-FCBEOF,Y               ; RESULT=EOF
;
            STA   (FCBPTR),Y
            INY
            CPY   #FCBEOF+3
            BNE   WRTADJEOF
WRITE2:     LDA   C_BYTES
            STA   RWREQL
            BNE   WRITE3                        ; BRANCH IF WRITE REQUEST DEFINITELY NON-ZERO_
            CMP   C_BYTES+1
            BNE   WRITE3                        ; BRANCH IF WRITE REQUEST<>ZERO
            STA   RWREQH
            JMP   WRITDONE                      ; DO NOTHING_
;
;PAGE
WRITE3:     LDA   C_BYTES+1
            STA   RWREQH
            LDA   C_OUTBUF                      ; MOVE POINTER TO USERS BUFFER TO BFM
            STA   USRBUF                        ; Z-PAGE AREA_
            LDA   C_OUTBUF+1
            STA   USRBUF+1                      ; (SO IT MAY BE ADJUSTED WITHOUT LOOSING
            LDA   SISOUTBF                      ; ORIGINAL ADDRESS_)
            STA   SISUSRBF
            LDY   #FCBSTYP                      ; NOW FIND OUT IF IT'S A TREE WRITE OR OTHER_
            LDA   (FCBPTR),Y
            CMP   #TRETYP+1
            BCC   TWRITE                        ; BRANCH IF A TREE FILE_
            JMP   ERRACCS                       ; OTHEWISE RETURN AN ACCESS ERROR!
TWRITE:     JSR   RDPOSN                        ; READ BLOCK WE'RE
            BCS   WRITERROR
            LDY   #FCBSTAT
            LDA   (FCBPTR),Y
            AND   #DATALC+IDXALC+TOPALC
            BEQ   TREWRT1
            LDY   #0                            ; FIND OUT IF ENOUGH DISK SPACE IS AVAILABLE FOR
TWRTALC:    INY                                 ; INDEXES AND DATA BLOCK
            LSR   A
            BNE   TWRTALC
            STY   REQL
            STA   REQH
            JSR   TSFRBLK
            BCS   WRITERROR                     ; PASS BACK ANY ERRORS_
            LDY   #FCBSTAT
            LDA   (FCBPTR),Y                    ; NOW GET MORE SPECIFIC_
            AND   #TOPALC                       ; ARE WE LACKING A TREE TOP?
            BEQ   TSTSAPWR                      ; NO, TEST FOR LACK OF SAPLING LEVEL INDEX_
            JSR   TOPDOWN                       ; GO ALLOCATE TREE TOP AND ADJUST FILE TYPE_
            BCC   DBLOKALC                      ; CONTINUE WITH ALLOCATION OF DATA BLOCK_
WRITERROR:  PHA                                 ; SAVE ERROR
            LDY   #FCBEOF
WRITERR01:  LDA   OLDEOF-FCBEOF,Y
            STA   (FCBPTR),Y                    ; RESTORE OLD EOF UPON ERR
            INY
            CPY   #FCBEOF+3
            BNE   WRITERR01
            LDY   #FCBMARK
WRITERR02:  LDA   OLDMARK-FCBMARK,Y
            STA   (FCBPTR),Y                    ; AND RESTORE OLD MARK!
            INY
            CPY   #FCBMARK+3
            BNE   WRITERR02
            PLA
            SEC
            RTS                                 ; ERROR RETURN
;
TWRITEGO:   BVC   TWRITE                        ; A PIGGY-BACK BACKWARD BRANCH
;
;PAGE
TSTSAPWR:   LDA   (FCBPTR),Y                    ; GET STATUS BYTE AGAIN_
            AND   #IDXALC                       ; DO WE NEED A SAPLING LEVEL INDEX BLOCK?
            BEQ   DBLOKALC                      ; NO, ASSUME IT'S JUST A DATA BLOCK NEEDED_
            JSR   SAPDOWN                       ; GO ALLOCATE AN INDEX BLOCK AND UPDATE TREE TOP_
            BCS   WRITERROR                     ; RETURN ANY ERRORS_
DBLOKALC:   JSR   ALCWBLK                       ; GO ALLOCATE FOR DATA BLOCK_
            BCS   WRITERROR
            LDA   TPOSHI                        ; CALCULATE POSITION WITHIN INDEX BLOCK_
            LSR   A
            LDA   TPOSLH
            ROR   A
            TAY                                 ; NOW PUT BLOCK ADDRESS INTO INDEX BLOCK
            INC   TINDX+1                       ; HIGH BYTE FIRST_
            LDA   SCRTCH+1
            TAX
            STA   (TINDX),Y
            DEC   TINDX+1                       ; (RESTORE POINTER TO LOWER PAGE OF INDEX BLOCK)
            LDA   SCRTCH                        ; GET LOW BLOCK ADDRESS
            STA   (TINDX),Y                     ; NOW STORE LOW ADDRESS_
            LDY   #FCBDATB                      ; ALSO UPDATE FILE CONTROL BLOCK TO INDICATE
            STA   (FCBPTR),Y                    ; THAT THIS BLOCK IS ALLOCATED_
            INY
            TXA                                 ; GET HIGH ADDRESS AGAIN_
            STA   (FCBPTR),Y
            LDY   #FCBSTAT
            LDA   (FCBPTR),Y
            ORA   #IDXMOD
            AND   #$FF-DATALC-IDXALC-TOPALC     ; CLEAR ALLOCATION REQUIREMENT BITS_
            STA   (FCBPTR),Y
TREWRT1:    LDX   #USRBUF                       ; LOCATE POINTER TO ADJUST <SRS 82_162>
            JSR   WRAPADJ                       ; ADJUST FOR BANK CROSSING <SRS 82_162>
            JSR   PREPRW                        ; WRITE ON
            JSR   WRTPART
            BVC   TWRITEGO
WRITDONE:   JMP   RDPOSN                        ; UPDATE FCB WITH NEW POSITION_
;
;PAGE
WRTPART:    TXA
            BNE   WRPART                        ; BRANCH IF REQUEST IS NOT A EVEN PAGES
            LDA   RWREQH                        ; A CALL OF ZERO BYTES SHOULD NEVER GET HERE!
            BEQ   SETWRDNE                      ; DO NOTHING!
;
            DEC   RWREQH
WRPART:     DEX
            LDA   (USRBUF),Y                    ; MOVE DATA FROM USER'S BUFFER
            STA   (POSPTR),Y                    ; ONE BYTE AT A TIME_
            TXA
            BEQ   ENDWQCHK
WRPART2:    INY                                 ; PAGE CROSSED?
            BNE   WRPART                        ; NO_ MOVE NEXT BYTE_
            LDA   POSPTR+1                      ; TEST FOR END OF BUFFER
            INC   USRBUF+1                      ; BUT FIRST ADJUST USER BUFFER POINTER
            INC   TPOSLH                        ; AND POSITION_
            BNE   WRPART3
            INC   TPOSHI
WRPART3:    INC   POSPTR+1                      ; AND SOS BUFFER HIGH ADDRESS_
            EOR   DATPTR+1                      ; (CARRY HAS BEEN CLEVERLY UNDISTURBED_)
            BEQ   WRPART                        ; BRANCH IF MORE TO WRITE TO BUFFER_
            CLV                                 ; INDICATE NOT FINISHED_
            BVC   WRPRTDNE                      ; BRANCH ALWAYS_
;
ENDWQCHK:   LDA   RWREQH
            BEQ   WRTRQDNE                      ; BRANCH IF REQEST SATISFIED_
            INY                                 ; ARE WE DONE WITH THIS BLOCK OF DATA?
            BNE   ENDWCHK1                      ; BRANCH IF NOT_
            LDA   POSPTR+1
            EOR   DATPTR+1                      ; WHILE THIS IS REDUNDANT, IT'S NECESSARY FOR
            BNE   ENDWCHK2                      ; PROPER ADJUSTMENT OF REQUEST COUNT_
ENDWCHK1:   DEC   RWREQH                        ; (NOT FINISHED- OK TO ADJUST HI BYTE_)
ENDWCHK2:   DEY                                 ; RESET MODIFIED Y
            JMP   WRPART2
;
WRTRQDNE:   INY                                 ; AND POSITION_
            BNE   SETWRDNE
            INC   USRBUF+1                      ; BUMP POINTERS_
            INC   TPOSLH
            BNE   SETWRDNE
            INC   TPOSHI
SETWRDNE:   BIT   SETVFLG                       ; (SET V FLAG)
WRPRTDNE:   STY   TPOSLL                        ; SAVE LOW POSITION
            STX   RWREQL                        ; AND REMAINDER OF REQUEST COUNT_
            PHP                                 ; SAVE STATUSES
            LDY   #FCBSTAT
            LDA   (FCBPTR),Y
            ORA   #DATMOD+USEMOD
            STA   (FCBPTR),Y
            CLC                                 ; ADJUST USER'S LOW BUFFER ADDRESS
            LDA   TPOSLL
            ADC   USRBUF
            STA   USRBUF
            BCC   WRPART4
            INC   USRBUF+1                      ; ADJUST HI ADDRESS AS NEEDED_
WRPART4:    JSR   FCBUSED                       ; SET DIRECTORY FLUSH BIT
            PLP                                 ; RESTORE RETURN STATUSES
            RTS
;PAGE
TOPDOWN:    JSR   SWAPDOWN                      ; FIRST MAKE CURRENT 1ST BLOCK AN ENTRY IN NEW TOP_
            BCS   TPDWNERR                      ; RETURN ANY ERRORS
            LDY   #FCBSTYP                      ; FIND OUT IF STORAGE TYPE HAS BEEN CHANGED TO 'TREE'_
            LDA   (FCBPTR),Y                    ; (IF NOT, ASSUME IT WAS ORIGINALLY A SEED AND
            CMP   #TRETYP                       ; BOTH LEVELS NEED TO BE BUILT_
            BEQ   TOPDWN1                       ; OTHERWISE, ONLY AN INDEX NEED BE ALLOCATED)
            JSR   SWAPDOWN                      ; MAKE PREVIOUS SWAP A SAP LEVEL INDEX BLOCK_
            BCS   TPDWNERR
TOPDWN1:    JSR   ALCWBLK                       ; GET ANOTHER BLOCK ADDRESS FOR THE SAP LEVEL INDEX_
            BCS   TPDWNERR
            LDA   TPOSHI                        ; CALCULATE POSITION OF NEW INDEX BLOCK
            LSR   A                             ; IN THE TOP OF THE TREE_
            TAY
            LDA   SCRTCH                        ; GET ADDRESS OF NEWLY ALOCATED INDEX BLOCK AGAIN
            TAX
            STA   (TINDX),Y
            INC   TINDX+1
            LDA   SCRTCH+1
            STA   (TINDX),Y                     ; SAVE HI ADDRESS
            DEC   TINDX+1
            LDY   #FCBIDXB+1                    ; MAKE NEWLY ALLOCATED BLOCK THE CURRENT INDEX BLOCK_
            STA   (FCBPTR),Y
            TXA
            DEY
            STA   (FCBPTR),Y
            JSR   WFCBFST                       ; SAVE NEW TOP OF TREE_
            BCS   TPDWNERR
            JMP   ZTMPIDX                       ; END BY RE-CLEARING CURRENT (NEW) INDEX BLOCK_
;
SAPDOWN:    LDY   #FCBSTYP                      ; FIND OUT IF WE'RE DEALING WITH A TREE
            LDA   (FCBPTR),Y                    ; OR A SIMPLE SEED_
            CMP   #SEEDTYP                      ; IF SEED THEN AN ADJUSTMENT TO FILE TYPE IS NECESSARY_
            BEQ   SAPDWN1                       ; BRANCH IF SEED_
            JSR   RFCBFST                       ; OTHERWISE READ IN TOP OF TREE_
            BCC   TOPDWN1                       ; BRANCH IF NO ERROR_
TPDWNERR:   RTS                                 ; RETURN ERRORS
;
;PAGE
SAPDWN1     =     *                             ; MAKE CURRENT SEED INTO A SAPLING
;
SWAPDOWN:   JSR   ALCWBLK                       ; ALLOCATE A BLOCK BEFORE SWAP
            BCS   SWAPERR                       ; RETURN ERRORS IMMEDIATELY_
            LDY   #FCBFRST                      ; GET PREVIOUS FIRST BLOCK
            LDA   (FCBPTR),Y                    ; ADDRESS INTO INDEX BLOCK_
            PHA                                 ; SAVE TEMPORARLY WHILE SWAPPING IN NEW TOP INDEX
            LDA   SCRTCH                        ; GET NEW BLOCK ADDRESS (LOW)
            TAX
            STA   (FCBPTR),Y
            INY
            LDA   (FCBPTR),Y
            PHA
            LDA   SCRTCH+1                      ; AND HIGH ADDRESS TOO_
            STA   (FCBPTR),Y
            LDY   #FCBIDXB+1                    ; MAKE NEW TOP ALSO THE CURRENT INDEX IN MEMORY_
            STA   (FCBPTR),Y
            TXA                                 ; GET LOW ADDRESS AGAIN
            DEY
            STA   (FCBPTR),Y
            LDY   #0                            ; MAKE PREVIOUS THE FIRST ENTRY IN SUB INDEX
            INC   TINDX+1
            PLA
            STA   (TINDX),Y
            DEC   TINDX+1
            PLA
            STA   (TINDX),Y
            JSR   WFCBFST                       ; SAVE NEW FILE TOP_
            BCS   SWAPERR
            LDY   #FCBSTYP                      ; NOW ADJUST STORAGE TYPE
            LDA   #1                            ; BY ADDING 1 (THUS SEED BECOMES SAPLING BECOMES TREE)
            ADC   (FCBPTR),Y
            STA   (FCBPTR),Y
            LDY   #FCBSTAT
            LDA   (FCBPTR),Y                    ; MARK STORAGE TYPE MODIFIED_
            ORA   #STPMOD
            STA   (FCBPTR),Y
            CLC                                 ; RETURN 'NO ERROR' STATUS_
SWAPERR:    RTS
;
;PAGE
ALCWBLK:    JSR   ALC1BLK
            BCS   ALUSERR
            LDY   #FCBUSE
            LDA   (FCBPTR),Y                    ; BUMP CURRENT USAGE COUNT BY 1_
            CLC
            ADC   #1
            STA   (FCBPTR),Y
            BCC   INCUSG1
            INY
            LDA   (FCBPTR),Y
            ADC   #0
            STA   (FCBPTR),Y
INCUSG1:    LDY   #FCBSTAT                      ; MARK USAGE AS MODIFIED_
            LDA   (FCBPTR),Y
            ORA   #USEMOD
            STA   (FCBPTR),Y
            CLC                                 ; INDICATE NO ERROR
ALUSERR:    RTS                                 ; ALL DONE
;
TSTWPROT:   LDY   #FCBSTAT                      ; CHECK FOR A 'NEVER BEEN MODIFIED' CONDITION
            LDA   (FCBPTR),Y                    ; GET STATUS BYTE
            AND   #USEMOD+DATMOD+IDXMOD+EOFMOD
            CLC                                 ; ANTICIPATE WRITE OK
            BNE   ALUSERR                       ; ORDINARY RTS
            LDY   #FCBDEVN                      ; GET FILE'S DEVICE NUMBER
            LDA   (FCBPTR),Y
            STA   DEVNUM                        ; GET CURRENT STATUS OF BLOCK DEVICE
TWRPROT1:   LDA   #STATCMD
            STA   DHPCMD
            LDA   #STATSUB                      ; STORE SUB COMMAND OF STATUS CALL
            STA   DSTATREQ
            LDA   #<TWRCODE
            STA   DSTATBFL                      ; FETCH RETURN CODE IN SCRATCH AREA
            LDA   #>TWRCODE
            STA   DSTATBFH
            LDA   #0                            ; MAKE SURE REGULAR RAM IS SELECTED (NO BANKS)
            STA   SISDSTAT
            STA   SERR                          ; CLEAR GLOBAL ERROR FLAG
            LDA   DEVNUM                        ; SET UP LAST PARM
            STA   UNITNUM                       ; FOR DEVICE CALL
            JSR   DMGR                          ; MAKE THE EXTERNAL CALL
            BCS   WPROTRET                      ; RETURN ANY SPECIFIC ERRORS
            LDA   TWRCODE                       ; GET STATUS BYTE
            LSR   A                             ; SHIFT WRITE PROTECT STATE INTO CARRY
            LSR   A
            LDA   #XNOWRITE                     ; ANTICIPATE WRITE PROTECTED_
            RTS                                 ; CARRY IS INDETERMINATE
WPROTRET    =     *
            CMP   #XDISKSW                      ; IF EXPLICITLY DISK SWITCH
            BNE   WPROT1                        ; BRANCH IF XNODRIVE OR XNOWRITE
            STA   DSWGLOB                       ; IF DISKSW, FLAG UNTIL ENTIRE OPERATION IS COMPLETE
            CLC
            RTS                                 ; DISKSWITCH DOESNT SET CARRY
WPROT1:     SEC
            RTS
DSWGLOB:    .RES  1                             ; DISK SWITCH GLOBAL
TWRCODE:    .RES  1                             ; A RARE EMBEDDED TEMP STORE
;
;PAGE
;
; MEMORY 'WRAP-AROUND' ADJUST ROUTINE.  THIS ROUTINE ADJUSTS
; ADDRESSES THAT CROSS BANK PAIR BOUNDARIES.  ON ENTRY, X CONTAINS
; THE OFFSET OF THE ZERO PAGE EXTENDED POINTER TO BE ADJUSTED.
; ON EXIT, THE POINTER WILL HAVE BEEN ADJUSTED, IF NECESSARY,
; AND THE ASSOCIATED X-BYTE WILL ALSO HAVE BEEN ADJUSTED.
; ONLY ADDRESSES IN THE RANGE $8200-$8E00 WILL BE ADJUSTED.
;
; UPON EXIT, A CONTAINS HIGH BYTE OF ADDRESS & Y CONTAINS UPDATED X-BYTE.
; THIS ROUTINE LEAVES X UNCHANGED.
;
WRAPADJ:    LDA   1,X                           ; GET HIGH ADDRESS BYTE <SRS 82_162>
            LDY   SISTER+1,X                    ; CHECK X-BYTE <SRS 82_162>
            BPL   WRAPDNE                       ; NOT AN EXTENDED ADDRESS_ <SRS 82_162>
            CMP   #$82                          ; DOES IT NEED UPDATING? <SRS 82_162>
            BCC   WRAPDNE                       ; NO <SRS 82_162>
            CPY   #$8F                          ; SPECIAL BANK? <SRS 82_162>
            BCS   WRAPDNE                       ; NO <SRS 82_162>
            AND   #$7F                          ; ADJUST THE ADDRESS <SRS 82_162>
            STA   1,X                           ; UPDATE <SRS 82_162>
            INC   SISTER+1,X                    ; INCREMENT X-BYTE <SRS 82_162>
            INY                                 ; UPDATE Y ALSO <SRS 82_162>
;
WRAPDNE:    RTS                                 ; RETURN VALID HIGH ADDRESS AND BANK BYTE_

