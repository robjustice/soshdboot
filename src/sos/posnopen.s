;PAGE
GETMARK:    LDY   #FCBMARK                   ; MOVE CURRENT POSITION MARKER TO
GMARK1:     LDA   (FCBPTR),Y                 ; USER'S 4 BYTE BUFFER POINTED TO BY
            PHA                              ; C_MRKPTR IN SOS ZPAGE
            INY
            CPY   #FCBMARK+3                 ; USE STACK AS TEMPORARY STORAGE FOR THREE BYTE
            BNE   GMARK1                     ; POSITION VALUE_
            LDA   #0                         ; THE FOURTH (HIGHEST ORDER) BYTE IS ALWAYS ZERO_
            LDY   #3
            PHA
MOVMRK:     PLA
            STA   (C_MRKPTR),Y               ; MOVE TO USER'S SPACE
            DEY                              ; IS THERE ANOTHER TO PULL FROM STACK?
            BPL   MOVMRK                     ; YES, GET NEXT LOWER BYTE FROM STACK_
            CLC                              ; INDICATE NO ERROR_
            RTS
;
SETMARK:    JSR   ADJMARK                    ; MAKE ADJUSTMENTS TO REQUESTED MARK ACCORDING TO BASE_
            BCC   SMARK1                     ; BRANCH IF ADJUSTMENT WAS VALID_
            RTS
SMARK1:     LDX   #2                         ; NOW COMPARE END OF FILE WITH NEW
            LDY   #FCBEOF+2                  ; POSITION TO BE SURE IT'S WITHIN
CMPEOF:     LDA   TPOSLL,X                   ; THE BOUNDS OF CURRENTLY DEFINED
            CMP   (FCBPTR),Y                 ; LIMITS_
            BCC   CKSAMBLK                   ; BRANCH IF MARK<EOF
            BNE   ERRMEOF                    ; RETURN ERROR IF MARK>= EOF
            DEY
            DEX
            BPL   CMPEOF
            BMI   CKSAMBLK                   ; BRANCH ALWAYS
ERRMEOF:    LDA   #POSNERR                   ; TELL USER MARK IS OUT OF RANGE_
            RTS                              ; (CARRY IS SET TO INDICATE ERROR)
;
ADJMARK:    LDA   C_MARK+3                   ; MAKE SURE FOURTH BYTE OF DISPLACE IS ZIP
            BNE   ERRPOSN                    ; BRANCH TO ERR IF NOT
            LDX   #$FD                       ; ANTICIPATE OTHER THAN BASE OF ZERO
            LDY   #FCBMARK                   ; FURTHER ASSUME IT'S A BASE OFFSET FROM CURRENT POSITION
            LDA   C_BASE                     ; NOW FIND OUT WHAT IT REALLY IS_
            LSR   A                          ; (CARRY SET=SUBTRACT, NON ZERO REMAINDER= OFFSET FROM EOF)
            BCS   SUBMARK
            BEQ   ADJMRK                     ; BRANCH IF MARK IS FROM BEGINNING OF FILE
ADDPOSN:    LDA   (FCBPTR),Y                 ; ADD USER QUANTITY TO CURRENT
            ADC   C_MARK+3,X                 ; POSITION TO FORM NEW POSITION_
            STA   <(TPOSLL-$FD),X            ; (NOTE: ZERO PAGE REFERENCE WRAPS AROUND IN Z-PAGE)
            INY
            INX
            BNE   ADDPOSN                    ; ADD ALL THREE BYTES
            BCS   ERRPOSN                    ; BRANCH IF OVERFLOW
            BEQ   ADJMRK1                    ; BRANCH ALWAYS
;
;PAGE
SUBMARK:    BNE   SUBPOSN                    ; BRANCH IF IT'S AN OFFSET FROM CURRENT POSITION
            LDY   #FCBEOF                    ; OTHERWISE ASSUME OFFSET FROM END OF FILE_
SUBPOSN:    LDA   (FCBPTR),Y                 ; SUBTRACT USER QUANTITY TO FORM
            SBC   C_MARK+3,X                 ; NEW POSITION_ IF FINAL
            STA   <(TPOSLL-$FD),X            ; RESULT IS L_T_ ZERO, THEN REPORT
            INY                              ; POSITION ERROR___
            INX
            BNE   SUBPOSN
            BCS   ADJMRK1                    ; BRANCH IF LEGAL POSITION CALCULATED_
ERRPOSN:    LDA   #POSNERR
            SEC                              ; INDICATE ERROR
            RTS
;
ADJMRK:     LDX   #2                         ; FIRST SET UP POSITION TEMPS USED
ADJMRK0:    LDA   C_MARK,X                   ; BY BOTH POSITION ROUTINES
            STA   TPOSLL,X
            DEX
            BPL   ADJMRK0
ADJMRK1:    CLC                              ; NO ERRORS
            RTS
;
;
RDPOSN      =     *
CKSAMBLK    =     *
            LDY   #FCBMARK+1                 ; FIRST TEST TO SEE IF NEW POSITION IS
            LDA   (FCBPTR),Y                 ; WITHIN THE SAME (CURRENT) DATA BLOCK_
            AND   #$FE
            STA   SCRTCH
            INY                              ; BUMP TO ACCESS HIGHEST ORDER ADDRESS BYTE
            LDA   TPOSLH                     ; GET MIDDLE BYTE OF NEW POSITION
            SEC
            SBC   SCRTCH
            STA   SCRTCH
            BCC   TYPMARK                    ; BRANCH IF POSSIBLY L_T_ CURRENT POSITION
            CMP   #2                         ; MUST BE WITHIN 512 BYTES OF BEGINNING OF CURRENT
            BCS   TYPMARK
            LDA   TPOSHI                     ; NOW MAKE SURE WERE TALKIN ABOUT
            CMP   (FCBPTR),Y                 ; THE SAME 64K CHUNK!
            BNE   TYPMARK                    ; BRANCH IF WE AREN'T_
            JMP   SVMARK                     ; IF WE IS, ADJUST FCB AND POSPTR AND RETURN_
;
TYPMARK:    LDY   #FCBSTYP                   ; NOW FIND OUT WHICH TYPE
            LDA   (FCBPTR),Y                 ; OF FILE WE'RE POSITIONING ON_
            BEQ   FERRTYP                    ; THERE IS NO SUCH TYPE AS ZERO, BRANCH NEVER!
            CMP   #4                         ; IS IT A TREE CLASS FILE?
            BCC   CHKDSKSW                   ; YES, GO POSITION
            JMP   DIRMARK                    ; NO, TEST FOR DIRECTORY TYPE_
;
CHKDSKSW    =     *                          ; MAKE SURE S/HE HASN'T MOVED THE VOLUME
            LDY   #FCBDEVN
            LDA   (FCBPTR),Y
            STA   DEVNUM                     ; MAKE SURE DEVICE NUMBER PARM IS CURRENT
            JSR   TWRPROT1                   ; PASSES DEVNUM (CHECK DISK SWITCH)
            LDA   DSWGLOB                    ; DISK SWITCH GLOBAL
            BEQ   TREPOS                     ; BRANCH IF NONE DETECTED
CHKDSKS1:   JSR   VERFYVOL                   ; MATCHES VCBPTR VS_ DEVNUM
            BCC   TREPOS                     ; BRANCH IF DISK HASN'T SWITCHED
            JSR   USRREQ                     ; POLITELY ASK USER TO MOUNT
            BCC   CHKDSKS1                   ; SAID HE DID, CHECK AGAIN
            LDA   #VNFERR                    ; REFUSES TO MOUNT
            RTS
;
FERRTYP:    LDY   #FCBREFN                   ; CLEAR ILLEGALLY TYPED FCB ENTRY
            STA   (FCBPTR),Y
            LDA   #BADREFNUM                 ; TELL EM THERE IS NO SUCH FILE
            SEC
            RTS
;
;PAGE
TREPOS:     LDY   #FCBSTYP                   ; USE STORAGE TYPE AS NUMBER
            LDA   (FCBPTR),Y                 ; OF LEVELS (SINCE 1=SEED, 2=SAPLING, AND 3=TREE)
            STA   LEVELS
            LDY   #FCBSTAT                   ; SINCE IT'S A DIFFERENT DATA
            LDA   (FCBPTR),Y                 ; BLOCK, MUST NOT FORGET PREVIOUS DATA_
            AND   #DATMOD                    ; THEREFORE, SEE IF PREVIOUS DATA WAS MODIFIED
            BEQ   POSNEW1                    ; THEN DISK MUST BE UPDATED_
            JSR   WFCBDAT                    ; GO WRITE CURRENT DATA BLOCK_
            BCS   POSERR                     ; RETURN ANY ERROR ENCOUNTERED_
;
POSNEW1:    LDY   #FCBMARK+2                 ; TEST TO SEE IF CURRENT
            LDA   (FCBPTR),Y                 ; INDEX BLOCK IS GOING TO BE USABLE___
            AND   #$FE                       ; OR IN OTHER WORDS-
            STA   SCRTCH                     ; IS NEW POSITION WITHIN 128K OF THE BEGINNING
            LDA   TPOSHI                     ; OF CURRENT SAPLING LEVEL CHUNK_
            SEC
            SBC   SCRTCH
            BCC   POSNEW2                    ; BRANCH IF A NEW INDEX BLOCK IS ALSO NEEDED
            CMP   #2                         ; NEW POSITION IS > THAN BEGINING OF OLD_ IS IT WITHIN 128K?
            BCS   POSNEW2                    ; BRANCH IF NOT_
            LDX   LEVELS                     ; IS THE FILE WE'RE DEALING WITH A SEED?
            DEX
            BNE   DATLEVEL                   ; NO, USE CURRENT INDEXES_
TSTINY:     LDA   TPOSLH                     ; IS NEW POSITION UNDER 512?
            LSR   A
            ORA   TPOSHI
            BNE   NOIDXDAT                   ; NO, MARK BOTH DATA AND INDEX BLOCK AS UN-ALLOCATED_
            LDY   #FCBFRST
            LDA   (FCBPTR),Y                 ; FIRST BLOCK IS ONLY BLOCK AND IT'S DATA!
            STA   BLOKNML
            INY
            LDA   (FCBPTR),Y                 ; (HIGH BLOCK ADDRESS)
            JMP   RNEWPOS                    ; GO READ IN BLOCK AND SET APPROPRIATE STATUSES_
;
;PAGE
POSNEW2:    LDY   #FCBSTAT                   ; GOTA CHECK TO SEE IF PREVIOUS
            LDA   (FCBPTR),Y                 ; INDEX BLOCK WAS MODIFIED_
            AND   #IDXMOD
            BEQ   POSNIDX                    ; READ IN OVER IT IF CURRENT IS UP TO DATE_
            JSR   WFCBIDX                    ; GO UPDATE INDEX ON DISK (BLOCK ADDR IN FCB)
            BCS   POSERR
POSNIDX:    LDX   LEVELS                     ; BEFORE READING IN TOP INDEX, CHECK TO BE SURE
            CPX   #3                         ; THAT THERE IS A TOP INDEX___
            BEQ   POSINDEX                   ; BRANCH IF FILE IS FULL BLOWN TREE_
            LDA   TPOSHI                     ; IS NEW POSITION WITHIN RANGE OF A
            LSR   A                          ; SAPLING FILE (L_T_ 128K)?
            PHP                              ; ANTICIPATE NO GOOD_
            LDA   #TOPALC+IDXALC+DATALC      ; (TO INDICATE NO LEVEL IS ALLOCATED FOR NEW POSITION_)
            PLP                              ; Z FLAG TELLS ALL___
            BNE   NODATA                     ; GO MARK 'EM ALL DUMMY_
            JSR   CLRSTATS                   ; GO CLEAR STATUS BITS 0,1,2 (INDEX/DATA ALLOC STATUS)_
            DEX                              ; (UNAFFECTED SINCE LOADED ABOVE) CHECK FOR SEED
            BEQ   TSTINY                     ; IF SEED, CHECK FOR POSITION L_T_ 512___
            JSR   RFCBFST                    ; GO GET ONLY INDEX BLOCK
            BCS   POSERR                     ; BRANCH IF ERROR
            LDY   #FCBIDXB                   ; SAVE NEWLY LOADED INDEX BLOCK'S ADDRESS
            LDA   BLOKNML
            STA   (FCBPTR),Y
            INY
            LDA   BLOKNMH
            STA   (FCBPTR),Y
            BCC   DATLEVEL                   ; BRANCH ALWAYS___
POSERR:     SEC
            RTS
;
POSINDEX:   JSR   CLRSTATS                   ; CLEAR ALL ALLOCATION REQUIREMENTS FOR PREVIOUS POSITION
            JSR   RFCBFST                    ; GET HIGHEST LEVEL INDEX BLOCK_
            BCS   POSERR
            LDA   TPOSHI                     ; THEN TEST FOR A SAP LEVEL INDEX BLOCK
            LSR   A
            TAY
            LDA   (TINDX),Y
            INC   TINDX+1
            CMP   (TINDX),Y                  ; (BOTH HI AND LO WILL BE ZERO IF NO INDEX EXISTS)
            BNE   SAPLEVEL
            CMP   #0                         ; ARE BOTH BYTES ZERO?
            BNE   SAPLEVEL
            DEC   TINDX+1                    ; DON'T LEAVE WRONG POINTERS LAYING AROUND!
NOIDXDAT:   LDA   #IDXALC+DATALC             ; SHOW NEITHER INDEX OR DATA BLOCK ALLOCATED_
            JMP   NODATA
;
;PAGE
SAPLEVEL:   STA   BLOKNML                    ; READ IN NEXT LOWER INDEX BLOCK
            LDA   (TINDX),Y                  ; (HI ADDRESS)
            STA   BLOKNMH
            DEC   TINDX+1
            JSR   RFCBIDX                    ; READ IN SAPLING LEVEL
            BCS   POSERR
DATLEVEL:   LDA   TPOSHI                     ; NOW GET BLOCK ADDRESS OF DATA BLOCK
            LSR   A
            LDA   TPOSLH                     ; ( IF THERE IS ONE )
            ROR   A
            TAY
            LDA   (TINDX),Y                  ; DATA BLOCK ADDRESS LOW
            INC   TINDX+1
            CMP   (TINDX),Y
            BNE   POSNEW3
            CMP   #0
            BNE   POSNEW3
            LDA   #DATALC                    ; SHOW DATA BLOCK AS NEVER BEEN ALLOCATED
            DEC   TINDX+1
;
NODATA:     LDY   #FCBSTAT
            ORA   (FCBPTR),Y                 ; SET STATUS TO SHOW WHATS MISSIN'
            STA   (FCBPTR),Y
            LSR   A                          ; THROW AWAY BIT THAT SAYS DATA BLOCK UN-ALLOCATED
            LSR   A                          ; CUZ WE KNOW THAT_ CARRY NOW INDICATES IF INDEX BLOCK
            JSR   ZIPDATA                    ; ALSO IS INVALID AND NEEDS TO BE ZEROED (CARRY UNDISTURBED)
            BCC   SVMARK                     ; BRANCH IF INDEX BLOCK DOESN'T NEED ZIPPIN_
ZIPIDX:     STA   (TINDX),Y
            INY
            BNE   ZIPIDX
            INC   TINDX+1
ZPIDX1:     STA   (TINDX),Y
            INY
            BNE   ZPIDX1
            DEC   TINDX+1                    ; RESTORE PROPER ADDRESS
            JMP   SVMARK
;
ZIPDATA:    LDA   #0                         ; ALSO IS INVALID AND NEEDS TO BE ZEROED_
            TAY
ZIPDAT0:    STA   (DATPTR),Y                 ; ZERO OUT DATA AREA
            INY
            BNE   ZIPDAT0
            INC   DATPTR+1
ZPDAT1:     STA   (DATPTR),Y
            INY
            BNE   ZPDAT1
            DEC   DATPTR+1
            RTS
;
;PAGE
;
POSNEW3:    STA   BLOKNML                    ; GET DATA BLOCK OF NEW POSITION
            LDA   (TINDX),Y                  ; (HI ADDRESS)
            DEC   TINDX+1
RNEWPOS:    STA   BLOKNMH
            JSR   RFCBDAT
            BCS   PRITZ                      ; RETURN ANY ERROR
            JSR   CLRSTATS                   ; SHOW WHOLE CHAIN IS ALLOCATED
SVMARK:     LDY   #FCBMARK+2                 ; UPDATE POSITION IN FILE CONTROL BLOCK
            LDX   #2
SVMRK1:     LDA   (FCBPTR),Y                 ; REMEMBER OLDMARK IN CASE
            STA   OLDMARK-FCBMARK,Y          ; CALLING ROUTINE FAILS LATER
            LDA   TPOSLL,X
            STA   (FCBPTR),Y
            DEY
            DEX                              ; MOVE 3 BYTE POSITION MARKER
            BPL   SVMRK1
;
            CLC                              ; LAST, BUT NOT LEAST, SET UP
            LDA   DATPTR                     ; INDIRECT ADDRESS TO BUFFER PAGE POINTED
            STA   POSPTR                     ; TO BY THE CURRENT POSITION MARKER_
            LDA   TPOSLH
            AND   #1
            ADC   DATPTR+1
            STA   POSPTR+1
            LDA   SISDATP
            STA   SISPOSP                    ; SISTER PAGE BYTE ALSO_
            RTS                              ; CARRY SHOULD ALWAYS BE CLEAR
PRITZ:      SEC                              ; RANDOM ERROR
            RTS                              ; RETURN
;
;
CLRSTATS:   LDY   #FCBSTAT                   ; CLEAR ALLOCATION STATES FOR DATA BLOCK
            LDA   (FCBPTR),Y                 ; AND BOTH LEVELS OF INDEXES_
            AND   #$FF-TOPALC-IDXALC-DATALC
            STA   (FCBPTR),Y                 ; THIS SAYS THAT EITHER THEY EXIST CURRENTLY
            RTS                              ; OR THAT THEY'RE UNNECESSARY FOR CURRENT POSITION_
;
;PAGE
;
DIRMARK:    CMP   #DIRTYP                    ; IS IT A DIRECTORY?
            BEQ   DIRPOS                     ; YES___
            LDA   #CPTERR                    ; NO, THERE IS A COMPATABLITY PROBLEM-
            JSR   SYSERR                     ; THE DAMN THING SHOULD OF NEVER BEEN OPENED!
;
DIRPOS:     LDA   SCRTCH                     ; RECOVER RESULTS OF PREVIOUS SUBTRACTION_
            LSR   A                          ; USE DIFFERENCE AS COUNTER AS TO HOW MANY
            STA   CNTENT                     ; BLOCKS MUST BE READ TO GET TO NEW POSITION_
            LDY   #FCBMARK+1                 ; TEST FOR POSITION DIRECTION_
            LDA   (FCBPTR),Y
            CMP   TPOSLH                     ; CARRY INDICATES DIRECTION___
            BCC   DIRFWRD                    ; IF SET, POSITION FORWARD_
DIRVRSE:    LDY   #0                         ; OTHERWISE, READ DIRECTORY FILE IN REVERSE ORDER_
            JSR   DIRPOS1                    ; READ PREVIOUS BLOCK_
            BCS   DRPOSERR                   ; BRANCH IF ANYTHING GOES WRONG_
            INC   CNTENT                     ; COUNT UP TO 128
            BPL   DIRVRSE                    ; LOOP IF THERE IS MORE BLOCKS TO PASS OVER_
            BMI   SVMARK                     ; BRANCH ALWAYS_
;
DIRFWRD:    LDY   #2                         ; POSITION IS FORWARD FROM CURRENT POSITION_
            JSR   DIRPOS1                    ; READ NEXT DIRECTORY BLOCK_
            BCS   DRPOSERR
            DEC   CNTENT
            BNE   DIRFWRD                    ; LOOP IF POSITION NOT FOUND IN THIS BLOCK_
            BEQ   SVMARK                     ; BRANCH ALWAYS_
;
DIRPOS1:    LDA   (DATPTR),Y                 ; GET LINK ADDRESS OF PREVIOUS OR
            STA   BLOKNML                    ; NEXT DIRECTORY BLOCK_
            INY                              ; BUT FIRST BE SURE THERE IS A LINK_
            CMP   (DATPTR),Y
            BNE   DIRPOS2                    ; BRANCH IF CERTAIN LINK EXISTS
            CMP   #0                         ; ARE BOTHE LINK BYTES 0?
            BNE   DIRPOS2                    ; NOPE, JUST HAPPEN TO BE THE SAME VALUE_
            LDA   #EOFERR                    ; SOMETHING IS WRONG WITH THIS DIRECTORY FILE!
DRPOSERR:   SEC                              ; INDICATE ERROR
            RTS
;
DIRPOS2:    LDA   (DATPTR),Y                 ; (HIGH ORDER BLOCK ADDRESS)
            STA   BLOKNMH
; DROP INTO 'RFCBDAT' (READ FILE'S DATA BLOCK)
;
; NOTE: FOR DIRECTORY POSITIONING NO OPTIMIZATION HAS BEEN
; DONE SINCE DIRECTORY FILES WILL ALMOST ALWAYS BE LESS
; THAN 6 BLOCKS. IF MORE SPEED IS REQUIRED OR DIRECTORY
; TYPE FILES ARE TO BE USED FOR OTHER PURPOSES REQUIRING
; MORE BLOCKS, THEN THE RECOMMENDED METHOD IS TO CALL
; 'RFCBDAT' FOR THE FIRST BLOCK AND GO DIRECTLY TO
; DEVICE (VIA JMP (IOUNITL)) HANDLER FOR SUBSEQUENT
; ACCESSES.
; ALSO NOTE THAT NO CHECKING IS DONE FOR READ/WRITE
; ENABLE SINCE A DIRECTORY FILE CAN ONLY BE OPENED
; FOR READ ACCESS.
;
;PAGE
;
RFCBDAT:    LDA   #RDCMD                     ; SET READ COMMAND_
            STA   DHPCMD
            LDX   #DATPTR                    ; USE X TO POINT AT ADDRESS OF DATA BUFFER
            JSR   FILEIO1                    ; GO DO FILE INPUT_
            LDY   #FCBDATB                   ; SAVE BLOCK NUMBER JUST READ IN FCB_
            BCC   FCBLOKNM                   ; BRANCH IF NO ERRORS HAPPENED_
            RTS                              ; RETURN ERROR
;
RFCBIDX:    LDA   #RDCMD                     ; PREPARE TO READ IN INDEX BLOCK_
            STA   DHPCMD
            LDX   #TINDX                     ; POINT AT ADDRESS OF CURRENT INDEX BUFFER
            JSR   FILEIO1                    ; GO READ INDEX BLOCK_
            BCS   RDFCBERR                   ; REPORT ERROR
            LDY   #FCBIDXB                   ; SAVE BLOCK ADDRESS OF THIS INDEX IN FCB_
FCBLOKNM:   LDA   BLOKNML
            STA   (FCBPTR),Y
            INY
            LDA   BLOKNMH
            STA   (FCBPTR),Y
            CLC
RDFCBERR:   RTS
;
RFCBFST:    LDX   #TINDX                     ; POINT AT ADDRESS OF INDEX BUFFER
            LDY   #FCBFRST                   ; AND BLOCK ADDRESS OF FIRST FILE BLOCK IN FCB
            LDA   #RDCMD                     ; AND LASTLY, MAKE IT A READ!
; DROP INTO DOFILEIO
;
DOFILEIO:   STA   DHPCMD                     ; SAVE COMMAND_
            LDA   (FCBPTR),Y                 ; GET DISK BLOCK ADDRESS FROM FCB_
            STA   BLOKNML
            INY                              ; BLOCK ZERO NOT LEGAL_
            CMP   (FCBPTR),Y
            BNE   FILEIO
            CMP   #0                         ; ARE BOTH BYTES ZERO?
            BNE   FILEIO                     ; NO, CONTINUE WITH REQUEST_
            LDA   #ALCERR                    ; OTHERWISE REPORT ALLOCATION ERROR_
            JSR   SYSDEATH                   ; NEVER RETURNS___
;
;PAGE
FILEIO:     LDA   (FCBPTR),Y                 ; GET HIGH ADDRESS OF DISK BLOCK
            STA   BLOKNMH
FILEIO1:    LDA   0,X                        ; GET MEMORY ADDRESS OF BUFFER FROM
            STA   DBUFPL                     ; S_O_S_ ZERO PAGE POINTED TO BY
            JSR   WRAPADJ                    ;GO ADJUST FOR BANK CROSSING <SRS 82_162>
            LDA   1,X
            STA   DBUFPH                     ; SET HI BYTE
            LDA   SISTER+1,X                 ; AND BANK PAIR BYTE_ <SRS 82_162>
            STA   SISBPH
            LDY   #FCBDEVN
            LDA   (FCBPTR),Y                 ; OF COURSE HAVING THE DEVICE NUMBER
            STA   DEVNUM                     ; WOULD MAKE THE WHOLE OPERATION MORE MEANINGFUL___
FILEIO2:    LDA   #2                         ; ALSO, SET UP BYTE COUNT TO 512 AND
            STA   RQCNTH                     ; SET 'BYTES READ' POINTER TO
            STA   IOACCESS                   ; (INTERUPT! SET TO INDICATE REG CALL MADE TO DEV HANDLER_ RETURN INTERUPT!)
            LDA   #<TRASH                    ; A PLACE TO THROW BYTES READ AWAY
            STA   BRDPTR
            LDA   #>TRASH                    ; LOCALLY DEFINED
            STA   BRDPTR+1
            LDA   #0                         ; SO THAT IT DOESN'T MESS UP ANY OTHER DATA_
            STA   RQCNTL
            STA   SSBRDPH                    ; ('BYTES READ' IS THROWN AWAY)
RPEATIO1:   LDA   DEVNUM                     ; TRANSFER THE DEVICE NUMBER FOR DISPATCHER TO CONVERT TO UNIT NUMBER_
            STA   UNITNUM
RPEATIO0:   LDY   #$9                        ; PREPARE TO SAVE DEVICE PARMS
SAVPRMS:    LDA   DEVICE,Y                   ; MOVE FROM Z PAGE
            STA   RPTBLOK,Y                  ; TO MY OWN SPACE
            DEY                              ; FROM $C9 THROUGH $C0
            BPL   SAVPRMS
DMGRGO      =     *                          ; CALL EXTERNAL DEVICE MANAGER
            LDA   #0
            STA   SERR                       ; CLEAR GLOBAL ERROR VALUE
            JSR   DMGR                       ; CALL THE DRIVER
            BCC   RRITZ                      ; RTS IF NO ERRORS
            CMP   #XDISKSW                   ; DISKSWITCH ITERATES
            BEQ   RPEATIO2                   ; BRANCH IF DISK SWITCH AND REPEAT I/O REQUEST
            SEC                              ; REPORT ERROR
RRITZ:      RTS
RPEATIO2:   LDY   #$9                        ; LENGTH OF PARM BLOCK
GETPRMS:    LDA   RPTBLOK,Y
            STA   DEVICE,Y                   ; RESTORE POSSIBLY DISTURBED PARM BLOCK
            DEY
            BPL   GETPRMS
            JMP   DMGRGO                     ; AND TRY THE I/O AGAIN
;
;
TRASH:      .RES  2                          ; ONLY USED TO PUT BYTES READ TO SLEEP
RPTBLOK:    .RES  10                         ; DMGR PARM SAVE BLOCK
;
;
WFCBFST:    LDY   #FCBDEVN                   ; FETCH THE
            LDA   (FCBPTR),Y                 ;  DEVICE NUMBER
            TAX                              ;    AND UPDATE
            JSR   UPBMAP                     ;      ITS BITMAP
            LDX   #TINDX                     ; POINT AT ADDRESS OF INDEX BLOCK
            LDY   #FCBFRST                   ; AND THE DISK ADDRESS OF FILE'S FIRST BLOCK IN FCB
            LDA   #WRTCMD                    ; LASTLY, MAKE IT A WRITE REQUEST_
            JMP   DOFILEIO                   ; AND GO DO IT!
;
WFCBDAT:    LDX   #DATPTR
            LDY   #FCBDATB                   ; POINT AT MEMORY ADDRESS WITH X AND DISK ADDRESS WITH Y_
            LDA   #WRTCMD                    ; WRITE DATA BLOCK_
            JSR   DOFILEIO
            BCS   FILIOERR                   ; REPORT ANY ERRORS
            LDA   #$FF-DATMOD                ; MARK DATA STATUS AS CURRENT_
            JMP   FCBUPDAT
;
WFCBIDX:    LDY   #FCBDEVN                   ; MAKE SURE
            LDA   (FCBPTR),Y                 ;  THE BITMAP
            TAX                              ;    FOR THIS DEVICE ("X")
            JSR   UPBMAP                     ;      IS UPDATED
            LDX   #TINDX                     ; POINT AT ADDRESS OF INDEX BUFFER
            LDY   #FCBIDXB                   ; AND BLOCK ADDRESS OF THAT INDEX BLOCK_
            LDA   #WRTCMD
            JSR   DOFILEIO                   ; GO WRITE OUT INDEX BLOCK_
            BCS   FILIOERR                   ; REPORT ANY ERRORS
            LDA   #$FF-IDXMOD                ; MARK INDEX STATUS AS CURRENT_
FCBUPDAT:   LDY   #FCBSTAT                   ; CHANGE STATUS BYTE TO
            AND   (FCBPTR),Y                 ; REFLECT SUCCESSFUL DISK FILE UPDATE_
            STA   (FCBPTR),Y                 ; (CARRY IS UNAFFECTED)
FILIOERR:   RTS
;
;
;PAGE
OPEN:       JSR   FINDFILE                   ; FIRST OF ALL LOOK UP THE FILE___
            BCC   OPEN0
            CMP   #BADPATH                   ; IS AN ATTEMPT TO OPEN A ROOT DIRECTORY?
            BNE   ERROPN                     ; NO, PASS BACK ERROR
;
OPEN0:      JSR   TSTOPEN                    ; FIND OUT IF ANY OTHER FILES ARE WRITING
            BCC   OPEN1                      ; TO THIS SAME FILE_ (BRANCH IF NOT)
ERRBUSY:    LDA   #FILBUSY                   ; REPORT SHARED ACCESS NOT ALLOWED_
ERROPN:     SEC
            RTS                              ; RETURN ERROR_
;
OPEN1:      LDA   DATPTR                     ; GET ADDRESS OF FIRST FREE FCB FOUND
            STA   FCBPTR                     ; DURING TEST OPEN SEQUENCE AND USE
            LDA   DATPTR+1                   ; IT AS FILE CONTROL AREA_ IF HIGH BYTE OF
            STA   FCBPTR+1                   ; POINTER IS ZERO, THEN NO FCB
            BNE   ASGNFCB                    ; IS AVAILABLE FOR USE_
            LDA   #FCBFULL                   ; REPORT FCB FULL ERROR_
            SEC
            RTS
;
ASGNFCB:    LDY   #$1F                       ; ASSIGN FCB, BUT FIRST
            LDA   #0                         ; CLEAN OUT ANY OLD RUBBISH LEFT AROUND___
CLRFCB:     STA   (FCBPTR),Y
            DEY
            BPL   CLRFCB
            LDY   #FCBENTN                   ; NOW BEGIN CLAIM BY MOVING IN FILE
FCBOWNR:    LDA   D_DEV-1,Y                  ; OWNERSHIP INFORMATION_
            STA   (FCBPTR),Y                 ; NOTE: THIS CODE DEPENDS UPON THE DEFINED
            DEY                              ; ORDER OF BOTH THE FCB AND DIRECTORY ENTRY
            BNE   FCBOWNR                    ; BUFFER (D_)_ BEWARE OF CHANGES!!! *************
            LDA   A:DFIL+D_STOR                ; GET STORAGE TYPE_
            LSR   A                          ; STRIP OFF FILE NAME LENGTH_
            LSR   A
            LSR   A                          ; (BY DIVIDING BY 16)
            LSR   A
            TAX                              ; SAVE IN X FOR LATER TYPE COMPARISON
            LDY   #FCBSTYP
            STA   (FCBPTR),Y                 ; SAVE STORAGE TYPE_
            LDA   C_OPLSTLN                  ; IS THERE AN OPEN LIST?
            BEQ   DEFOPEN                    ; NO, USE DEFAULT REQUST ACCESS___
            LDY   #0                         ; YES, FIND OUT WHAT ACCESS IS REQUESTED_
            LDA   (C_OPLIST),Y               ; IF REQ-ACCESS IS ZERO, THEN
            BEQ   DEFOPEN                    ; USE DEFAULTS___
            AND   A:DFIL+D_ATTR                ; CHECK REQUEST AGAINST ATTRIBUTES_
            CMP   (C_OPLIST),Y               ; WERE ALL ACCESS REQUESTS SATISFIED?
            BEQ   SVATTRB                    ; YES, SAVE ATTRIBUTES_
            LDA   #ACCSERR                   ; REPORT ACCESS REQUEST CAN'T BE MET_
            SEC
            RTS
;PAGE
DEFOPEN:    LDA   A:DFIL+D_ATTR                ; GET FILES ATTRIBUTES AND
            AND   #READEN+WRITEN             ; USE IT AS A DEFAULT ACCESS REQUEST_
SVATTRB:    LDY   #FCBATTR
            CPX   #DIRTYP                    ; IF DIRECTORY, DON'T ALLOW WRITE ENABLE
            BNE   SVATTR1
            AND   #READEN
SVATTR1:    STA   (FCBPTR),Y
            AND   #WRITEN                    ; CHECK FOR WRITE ENABLED REQUESTED_
            BEQ   OPEN2                      ; BRANCH IF READ ONLY OPEN_
            LDA   TOTENT                     ; OTHERWISE, BE SURE NO ONE ELSE IS READING SAME
            BNE   ERRBUSY                    ; FILE (SET UP BY TSTOPEN)_
OPEN2:      LDA   A:DFIL+D_COMP                ; OH, BY THE WAY___ IS THIS FILE
            BEQ   OPEN3                      ; COMPATABLE WITH VERSION 0000? ***************
ERRCMPAT:   LDA   #CPTERR                    ; REPORT FILE IS INCOMPATABLE!
            SEC
            RTS
;
OPEN3:      CPX   #TRETYP+1                  ; IS IT A TREE TYPE FILE?
            BCC   OPEN4                      ; TEST FOR FURTHER COMPATABLITY_ IT MUST
            CPX   #DIRTYP                    ; BE EITHER A TREE OR A DIRECTORY_
            BNE   ERRCMPAT                   ; REPORT INCOMPATABLE_
OPEN4:      LDY   #FCBFRST                   ; MOVE ADDRESS OF FIRST BLOCK OF FILE
            LDA   A:DFIL+D_FRST                ; INTO FCB_ NO CHECKING IS DONE FOR VALIDITY_
            STA   (FCBPTR),Y
            STA   BLOKNML
            INY
            LDA   A:DFIL+D_FRST+1
            STA   (FCBPTR),Y                 ; NOTE: THE FCB HAS NOT BEEN OFFICIALLY
            STA   BLOKNMH                    ; CLAIMED YET_ TO DO THIS, THE FIRST BYTE
            LDY   #FCBEOF                    ; MUST CONTAIN A VALID REFERENCE NUMBER_
EOFCBMV:    LDA   A:DFIL+D_EOF-FCBEOF,Y        ; MOVE CURRENT END OF FILE
            STA   (FCBPTR),Y                 ; TO FCB_
            INY
            CPY   #FCBEOF+3
            BNE   EOFCBMV
            LDA   A:DFIL+D_USAGE
            STA   (FCBPTR),Y                 ; AND CURRENT BLOCK COUNT OF FILE_
            INY
            LDA   A:DFIL+D_USAGE+1
            STA   (FCBPTR),Y
            LDA   C_OPLSTLN                  ; NOW THAT WE'VE COME THIS FAR, FIND
            BEQ   DEFBUFR                    ; OUT WHICH TYPE OF BUFFER AND ALLOCATE IT!
            CMP   #1                         ; WAS IT ONLY TO SET ATTRIBUTES?
            BEQ   DEFBUFR
            CMP   #4                         ; IS A FULL ADDRESS INCLUDED?
            BEQ   UBUFSPEC
            LDA   #BADLSTCNT
            SEC
            RTS
;
;PAGE
UBUFSPEC:   LDY   #1                         ; (INDEX TO 'PAGECNT' OF OPEN LIST)
            LDA   (C_OPLIST),Y               ; IS USER SPECIFING THE BUFFER?
            BEQ   DEFBUFR                    ; NO, USE DEFAULT BUFFER (DYNAMIC)
            CPX   #TRETYP+1                  ; IF TREE TYPE FILE, THEN AT LEAS 4 PAGES ARE NEEDED_
            BCC   ONEKTST                    ; BRANCH IF TREE TYPE_
            CMP   #2                         ; DID USER GIVE AT LEAST 2 PAGES FOR DIRECTORY TYPE?
            BCS   FIXDBUF                    ; YES, LOG IT WITH BUFFER MANAGER
ERRBTS:     LDA   #BTSERR                    ; REPORT NOT ENOUGH BUFFER SPACE_
            SEC
            RTS
;
ONEKTST:    CMP   #4                         ; IS THERE AT LEAST ONE KILOBYTE BUFFER FOR TREES?
            BCC   ERRBTS                     ; NO, THEN TO HELL WITH IT!_
FIXDBUF:    JSR   REQFXBUF                   ; CALL BOB AND ASK FOR HIM TO FIX IT___
            BCC   FCBUFFER                   ; GO SAVE BUFFER NUMBER_
ERROPN1:    RTS                              ; RETURN ANY ERROR ENCOUNTERED_
;
DEFBUFR:    LDA   #4                         ; ASSUME TREE FILE (4 PAGES REQUIRED)
            CPX   #TRETYP+1
            BCC   BUFREQST                   ; BRANCH IF IT IS A TREE_
            LDA   #2                         ; OTHERWIZE, WE JUST NEED TWO PAGES_
BUFREQST:   JSR   REQBUF                     ; CALL BOB TO ALLOCATE A DYNAMIC BUFFER_
            BCS   ERROPN1                    ; REPORT ANY ERRORS_
FCBUFFER:   LDY   #FCBBUFN                   ; SAVE BUFFER NUMBER AND THEN
            STA   (FCBPTR),Y                 ; FIND OUT WHERE IT IS_
            JSR   GTBUFFRS                   ; HAVE BOB RETURN ADDRESS IN DATA & INDEX POINTERS_
            BCS   ERROPEN2                   ; IF ERROR, FREE BUFFER BEFOR RETURNING_
            LDY   #FCBREFN                   ; NOW CLAIM FCB FOR THIS FILE_
            LDA   CNTENT                     ; THIS WAS SET UP BY 'TSTOPEN'_____________
            STA   (FCBPTR),Y
            LDY   #FCBLEVL                   ; MARK LEVEL
            LDA   LEVEL                      ; AT WHICH
            STA   (FCBPTR),Y                 ; FILE WAS OPENED
            LDY   #FCBSTYP                   ; GET STORAGE TYPE AGAIN_
            LDA   (FCBPTR),Y                 ; FILE MUST BE POSITIONED TO BEGINNING_
            CMP   #TRETYP+1                  ; IS IT A TREE FILE?
            BCS   OPNDIR                     ; NO, ASSUME IT'S A DIRECTORY_
            LDA   #$FF                       ; FOOL THE POSITION ROUTINE INTO GIVING
            LDY   #FCBMARK                   ; A VALID POSITION WITH PRELOADED DATA, ETC_
OPNPOS:     STA   (FCBPTR),Y
            INY
            CPY   #FCBMARK+3
            BNE   OPNPOS
            LDY   #2                         ; SET DESIRED POSITION TO ZERO_
            LDA   #0
OPNPOS1:    STA   TPOSLL,Y
            DEY
            BPL   OPNPOS1
            JSR   RDPOSN                     ; LET TREE POSITION ROUTINE DO THE REST_
            BCC   OPENDONE                   ; BRANCH IF SUCCESSFUL_
;
;PAGE
ERROPEN2:   PHA                              ; SAVE ERROR CODE_
            LDY   #FCBBUFN                   ; SINCE ERROR WAS ENCOUNTERED BEFORE FILE
            LDA   (FCBPTR),Y                 ; WAS SUCCESSFULLY OPENED, THEN
            JSR   RELBUF                     ; IT'S NECESSARY TO FREE THE BUFFER AND
            LDY   #FCBREFN                   ; FILE CONTROL BLOCK_
            LDA   #0
            STA   (FCBPTR),Y
            PLA
            SEC
            RTS
;
OPNDIR:     JSR   RFCBDAT                    ; READ IN FIRST BLOCK OF DIRECTORY FILE_
            BCS   ERROPEN2                   ; RETURN ANY ERROR AFTER FREEING BUFFER & FCB
OPENDONE:   LDY   #VCBOPNC                   ; INCREMENT OPEN COUNT FOR THIS
            LDA   (VCBPTR),Y                 ; VOLUME_ ALSO MARK STATUS_
            CLC
            ADC   #1
            STA   (VCBPTR),Y
            LDY   #VCBSTAT                   ; HI BIT INDICATES VOLUME BUSY
            LDA   (VCBPTR),Y
            ORA   #$80
            STA   (VCBPTR),Y                 ; DOESN'T MATTER HOW MANY, JUST BE SURE IT'S SET_
            LDY   #FCBREFN                   ; PASS USER HIS REFERENCE NUMBER
            LDA   (FCBPTR),Y
            LDY   #0
            STA   (C_OUTREF),Y
            CLC
            RTS
;
;PAGE
;
TSTOPEN:    LDA   A:FCBADDRH                 ; TEST FOR SHARED ACCESS FILES WITH WRITE ENABLED_
            STA   FCBPTR+1
            LDA   FCBANKNM
            STA   SISFCBP
            LDA   #0
            STA   DATPTR+1                   ; MARK AS NO FREE FOUND_
            STA   CNTENT
            STA   TOTENT                     ; ALSO, INIT COUNT OF MATCHING FILES
TSTOPN1:    STA   FCBPTR                     ; SAVE NEW LOW ORDER ADDRESS
            LDX   DATPTR+1                   ; FIND OUT IF A FREE SPOT HAS BEEN FOUND YET_
            BNE   TSTOPN2                    ; YES, DON'T INCREMENT REFNUM (CNTENT)_
            INC   CNTENT                     ; BUMP REFNUM
TSTOPN2:    LDY   #FCBREFN                   ; TEST FOR IN USE FCB
            LDA   (FCBPTR),Y                 ; (NON ZERO)
            BNE   CHKACTV                    ; THIS FCB IS IN USE, COPARE OWNERSHIP_
            TXA                              ; TEST AGAIN FOR FREE FCB
            BNE   TSNXFCB                    ; BRANCH IF A FREE SPOT HAS ALREADY BEEN FOUND_
            LDA   FCBPTR                     ; TRANSFER CURRENT POINTER SO IT MAY BE
            STA   DATPTR                     ; USED AS A FREE FCB BY OPEN_
            LDA   FCBPTR+1                   ; HIGH BYTE ALWAYS NON ZERO_
            STA   DATPTR+1
            JMP   TSNXFCB
;
CHKACTV     =     *                          ; IF MATCHING FILE IS SWAPPED, IT DOESNT COUNT
            LDY   #FCBSWAP
            LDA   (FCBPTR),Y
            BNE   TSNXFCB                    ; BRANCH IF SWAPPED
            LDY   #FCBENTN                   ; NOTE: THIS CODE DEPENDS ON THE
WHOWNS:     LDA   (FCBPTR),Y                 ; DEFINED ORDER OF FCB AND DIRECTORY
            CMP   D_DEV-1,Y                  ; *****************************
            BNE   TSNXFCB                    ; BRANCH IF THIS ONE HAS A DIFFERENT OWNER_
            DEY
            BNE   WHOWNS
            INC   TOTENT                     ; REPORT THIS ONE AS A CO-OWNER_
            LDY   #FCBATTR                   ; NOW FIND OUT IF THIS ONE WANTS TO WRITE_
            LDA   (FCBPTR),Y
            AND   #WRITEN                    ; IF WRITE IS NOT ENABLED THEN CONTINUE_
            BEQ   TSNXFCB
            SEC                              ; OTHERWISE, JUST SET THE CARRY TO SHOW
            RTS                              ; THAT THE FILE CAN'T BE SHARED_
;
TSNXFCB:    LDA   FCBPTR                     ; CALCULATE NEXT FCB AREA (+$20)
            CLC
            ADC   #$20
            BCC   TSTOPN1                    ; LOOP IF NO PAGE CROSS_
            LDX   FCBPTR+1
            INC   FCBPTR+1
            CPX   A:FCBADDRH                 ; HAVE WE LOOKED AT BOTH PAGES?
            BEQ   TSTOPN1                    ; NOPE, LOOK AT PAGE TWO_
            CLC                              ; INDICATE NO FILES THAT SHARE HAVE WRITE ENABLED,
            RTS
;

