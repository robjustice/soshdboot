;PAGE
;
NEWLINE:    LDY    #FCBATTR                            ; ADJUST NEWLINE STATUS FOR OPEN FILE_
            LDA    C_ISNEWL                            ; ON OR OFF?
            BPL    OFFNEWL                             ; BRANCH IF NEW LINE IS TO BE CLEARED_
            LDA    #NLINEN
            ORA    (FCBPTR),Y                          ; SET NEW LINE BIT IN ATTRIBUTES
            STA    (FCBPTR),Y
            LDY    #FCBNEWL                            ; AND MOVE IN NEW 'NEW-LINE' BYTE_
            LDA    C_NEWL
            STA    (FCBPTR),Y
            CLC
            RTS                                        ; NO ERROR POSSIBLE_
;
OFFNEWL:    LDA    #$FF-NLINEN
            AND    (FCBPTR),Y
            STA    (FCBPTR),Y                          ; CLEAR NEW-LINE BIT_
OFFRTS:     CLC                                        ; THE NEW LINE CHARACTER DOES'T MATTER___
            RTS
;PAGE
;
GETINFO:    JSR    FINDFILE                            ; LOOK FOR FILE THEY WANT OT KNOW ABOUT_
            BCC    GTINFO1                             ; BRANCH IF NO ERRORS_
            CMP    #BADPATH                            ; WAS IT A ROOT DIRECTORY FILE?
            SEC                                        ; (IN CASE OF NO MATCH)
            BNE    GINFOERR
            LDA    #$F0
            STA    A:DFIL+D_STOR                       ; FOR GET INFO, REPORT PROPER STORAGE TYPE
            LDA    #0                                  ; FORCE A COUNT OF FREE BLOCKS_
            STA    REQL
            STA    REQH
            JSR    TSFRBLK                             ; (RETURNS IF IMMEDIATELY IF COUNT HAS PREVIOUSLY BEEN TAKEN)
            LDY    #VCBTFRE+1
            LDA    (VCBPTR),Y                          ; RETURN TOTAL BLOCKS AND TOTAL IN USE_
            STA    REQH                                ; FIRST TRANSFER 'FREE' BLOCKS TO ZPAGE FOR LATER SUBTRACT
            DEY
            LDA    (VCBPTR),Y                          ; TO DETERMINE THE 'USED' COUNT
            STA    REQL
            DEY
            LDA    (VCBPTR),Y                          ; TRANSFER TO 'D_' TABLE AS AUX I_D_
            STA    A:DFIL+D_AUXID+1                    ; (TOTAL BLOCK COUNT IS CONSIDERED AUX I_D_ FOR THE VOLUME)
            TAX
            DEY
            LDA    (VCBPTR),Y
            STA    A:DFIL+D_AUXID
            SEC                                        ; NOW SUBTRACT AND REPORT THE NUMBER OF BLOCKS 'IN USE'
            SBC    REQL
            STA    A:DFIL+D_USAGE
            TXA
            SBC    REQH
            STA    A:DFIL+D_USAGE+1
GTINFO1:    LDY    #0                                  ; TRANSFER BYTES FROM THERE INTERNAL ORDER TO CALL SPEC VIA 'INFTABL' TRANSLATION
GTINFO2:    LDA    INFTABL,Y
            BPL    GTINFO3                             ; BRANCH IF THIS IS DATA IS VALID AS IS_
            AND    #$7F                                ; IS THIS THE 4TH BYTE OF THE EOF PARAMETER?
            BEQ    GTINFO4                             ; YES, AND IT'S ALWAYS A ZERO_
            CMP    #D_STOR+1                           ; IS THIS THE STORAGE TYPE BYTE?
            BNE    GINFOEND                            ; NO, IT'S THE END OF INFO THAT CAN BE RETURNED_
            LDA    A:DFIL+D_STOR                       ; GET STORAGE TYPE
            LSR    A
            LSR    A
            LSR    A
            LSR    A                                   ; MAKE IT A VALUE 1-$F BY SHIFTING OUT FILE NAME LENGTH_
            BPL    GTINFO4                             ; BRANCH ALWAYS
;
GTINFO3:    TAX                                        ; USE AS OFFSET INTO 'D_' TABLE_
            LDA    DFIL,X
GTINFO4:    STA    (C_FILIST),Y                        ; PASS TO USER'S BUFFER
            INY
            CPY    C_FILSTLN                           ; HAS REQUEST BEEN FILLED?
            BNE    GTINFO2                             ; NO, PASS NEXT
GINFOEND:   CLC                                        ; INDICATE NO ERRORS
GINFOERR:   RTS
;
;
;PAGE
;
SETINFO:    JSR    FINDFILE                            ; FIND WHAT USER WANTS___
            BCS    SINFOERR                            ; RETURN ANY FAILURE_
            LDA    C_FILSTLN                           ; TEST FOR NUL CHANGE
            BEQ    SINFEND                             ; BRANCH IF NOTHING TO CHANGE_
            LDY    #0                                  ; INIT POINTER TO USER SUPPLIED LIST_
            LDA    (C_FILIST),Y                        ; FETCH FILE ATTRIBUTES
            AND    #$1C                                ; FORBIDDEN BITS? <SRS 82_162>
            BEQ    SETINF1                             ; NO
            LDA    #ACCSERR                            ; YES
            SEC
            RTS                                        ; RETURN AN ERROR
SETINF1:    LDA    BACKMASK                            ; GET CURRENT BACKMASK <SRS 82_162>
; BACKUP KNOWS HOW TO RESET THIS BIT. <SRS 82.162>
            STA    BKBITFLG                            ; BIT (USED BY DREVISE)
SETINF1X:   LDX    INFTABL,Y                           ; GET INDEX INTO CORESPONDING 'D_' TABLE
            BMI    SETINF2                             ; BRANCH IF WE'VE REACHED STORAGE TYPE PARAMETER
            LDA    (C_FILIST),Y
            STA    DFIL,X
            INY                                        ; HAS USER'S REQUEST BEEN SATISFIED?
            CPY    C_FILSTLN
            BNE    SETINF1X                            ; NO, MOVE NEXT BYTE_
SINFEND:    JMP    DREVISE                             ; GO UPDATE DIRECTORY WITH CURRENT TIME_
;
SETINF2:    LDY    C_FILSTLN                           ; TEST TO SEE IF USER WANTS HIS TIME STAMP ADDED
            CPY    #$F                                 ; (LIST MUST BE AT LEAST $F BYTES LONG)
            BCC    SINFEND                             ; NO PUT CURRENT TIME INSTEAD_
            LDY    #$B                                 ; MOVE IN THE NEXT GROUP OF BYTES
SETINF3:    LDX    INFTABL,Y
            BMI    SINFEND1
            LDA    (C_FILIST),Y
            STA    DFIL,X
            INY
            CPY    C_FILSTLN                           ; SATISFACTION YET?
            BNE    SETINF3                             ; NOPE, KEEP EM PUMPIN'
SINFEND1:   JMP    DREVISE1
;
BKBITFLG:   .RES   1                                   ; FOR TURNING OFF BACKUP BIT
;
;
INFTABL:    .BYTE  D_ATTR,D_FILID,D_AUXID,D_AUXID+1
            .BYTE  D_STOR+1+$80,D_EOF,D_EOF+1,D_EOF+2  ; (D_STOR=0 THUS D_STOR+1 WAS NECESSARY)
            .BYTE  $80,D_USAGE,D_USAGE+1,D_MODDT       ; (THE $80 IS FOR THE FOURTH BYTE OF EOF)
            .BYTE  D_MODDT+1,D_MODTM,D_MODTM+1,$FF     ; TABLE ALWAYS ENDS IN $FF
;PAGE
;
RENAME:     JSR    LOOKFILE                            ; LOOK FOR SOURCE (ORIGINAL) FILE_
            BCC    RNAME0                              ; BRANCH IF FOUND_
            CMP    #BADPATH                            ; TRYING TO RENAME A VOLUME?
            BNE    RNAMERR                             ; NO, RETURN OTHER ERROR_
            JSR    RENPATH                             ; SYNTAX NEW NAME_
            BCS    RNAMERR
            LDA    WRKPATH                             ; FIND OUT IF ONLY ROOTNAME FOR NEW NAME
            CMP    PATHNML
            BNE    RNBADPTH                            ; NOT SINGLE NAME, RETURN ERROR!
            LDY    #VCBSTAT                            ; TEST FOR OPEN FILES BEFORE CHANGING
            LDA    (VCBPTR),Y
            BPL    RNAMEVOL                            ; BRANCH IF VOLUME NOT BUSY
            LDA    #FILBUSY
SINFOERR    =      *
            RTS                                        ; (CARRY IS SET)
RNAMEVOL:   LDY    #0                                  ; GET NEWNAME'S LENGTH_
            LDA    (WRKPATH),Y
            TAY
            ORA    #$F0                                ; (ROOT FILE STORAGE TYPE)
            JSR    MVROTNAM                            ; UPDATE ROOT DIRECTORY_
            BCS    RNAMERR
            LDY    #0
            LDA    (WRKPATH),Y                         ; UPDATE VCB ALSO_
            TAY
RNMEVOL:    LDA    (WRKPATH),Y
            STA    (VCBPTR),Y
            DEY
            BPL    RNMEVOL
            CLC
            RTS
;
RNAME0:     JSR    RENPATH                             ; SET UP AND SYNTAX NEW NAME_
            BCS    RNAMERR
            LDY    #0                                  ; VERIFY THAT BOTH NAMES HAVE SAME ROOT_
            LDA    (PATHNML),Y
            TAY
TSTSMROT:   LDA    (PATHNML),Y                         ; COMPARE NEWNAME'S ROOT NAME WITH
            CMP    (VCBPTR),Y                          ; OLD NAME'S VOLUME NAME_
            BNE    RNBADPTH                            ; RETURN 'BADPATH' IF NOT SAME VOLUME_
            DEY
            BPL    TSTSMROT                            ; (TEST SAME 'ROT')
            JSR    LOOKFILE                            ; TEST FOR DUPLICATE FILE NAME_
            BCS    TSTFNF1                             ; BRANCH IF ERROR TO TEST FOR FILE NOT FOUND_
            LDA    #DUPERR                             ; TELL USER THAT NEW NAME ALREADY EXISTS_
RNAMERR:    SEC
            RTS
;PAGE
TSTFNF1:    CMP    #FNFERR                             ; WAS IT A VALID FILE NOT FOUND?
            BNE    RNAMERR                             ; NO, RETURN OTHER ERROR CODE_
            LDX    #2                                  ; NOW MOVE NEW NAME'S OWNERSHIP (DIRECTORY HEADER) I_D_
SVENEWID:   LDA    D_DEV,X                             ; THIS CONSISTS OF THE UNIT NUMBER,
            STA    NPATHDEV,X                          ; AND THE ADDRESS OF THE DIRECTORY THE FILE
            DEX                                        ; WASN'T FOUND IN_ LOGIC BY NEGATION___
            BPL    SVENEWID
            JSR    SETPATH                             ; NOW SYNTAX THE PATHNAME OF THE FILE TO BE CHANGED_
            BCS    RNAMERR
            JSR    FINDFILE                            ; GET ALL THE INFO ON THIS ONE_
            BCS    RNAMERR
            JSR    TSTOPEN                             ; DON'T ALLOW RENAME TO OCCUR IF FILE IS IN USE_
            LDA    #FILBUSY                            ; ANTICIPATE ERROR
            BCS    RNAMERR
            LDA    A:DFIL+D_ATTR                       ; TEST BIT THAT SAYS IT'S OK TO RENAME
            AND    #RENAMEN
            BNE    RNAME1                              ; BRANCH IF IT'S ALRIGHT TO RENAME_
            LDA    #ACCSERR                            ; OTHERWISE REPORT ILLEGAL ACCESS_
            SEC
            RTS
;
RNAME1:     LDX    #2                                  ; NOW TEST TO SEE IF NEW PATHNAME FITS IN THE
SAMOWNR:    LDA    D_DEV,X                             ; SAME DIRECTORY FILE_
            CMP    NPATHDEV,X
            BEQ    RNAME2
RNBADPTH:   LDA    #BADPATH                            ; TELL USER THAT PATHNAMES INCOMPATABLE_
            SEC
            RTS
;
RNAME2:     DEX                                        ; TEST ALL THREE BYTES_
            BPL    SAMOWNR
            JSR    RENPATH                             ; WELL___ SINCE BOTH NAMES WOULD GO INTO THE
            BCS    RNAMERR                             ; DIRECTORY, RE-SYNTAX THE NEW NAME TO GET LOCAL NAME ADDRESS_
            TYA                                        ; (Y CONTAINS THE LOCAL NAME LENGTH+1)
            BEQ    RNBADPTH                            ; REPORT ERROR IF LENGTH INFO NOT IMMEDIATELY AVAILABLE_
            DEY                                        ; (REMOVE THE +1)
RNAME3:     LDA    (WRKPATH),Y                         ; MOVE LOCAL NAME TO DIR ENTRY WORKSPACE_
            STA    A:DFIL+D_STOR,Y
            DEY
            BNE    RNAME3
            LDA    A:DFIL+D_STOR                       ; PRESERVE FILE STORAGE TYPE_
            AND    #$F0                                ; STRIP OFF OLD NAME LENGTH_
            TAX
            ORA    (WRKPATH),Y                         ; ADD IN NEW NAME'S LENGTH
            STA    A:DFIL+D_STOR
            CPX    #DIRTYP*16                          ; THAT FILE MUST BE CHANGED ALSO_
            BNE    RNAMDONE                            ; BRANCH IF NOT DIRECTORY TYPE_
;PAGE
            LDA    A:DFIL+D_FRST                       ; READ IN FIRST (HEADER) BLOCK OF SUB DIRECTORY
            STA    BLOKNML
            LDA    A:DFIL+D_FRST+1
            STA    BLOKNMH
            JSR    RDGBUF
            BCS    RNAMERR                             ; REPORT ERRORS
            LDY    #0                                  ; CHANGE THE HEADER'S NAME TO MATCH THE OWNER'S NEW NAME_
            LDA    (WRKPATH),Y                         ; GET LOCAL NAME LENGTH AGAIN
            TAY
            ORA    #HEDTYP*16                          ; ASSUME IT'S A HEADER_
            JSR    MVROTNAM
            BCS    RNAMERR
RNAMDONE:   JMP    DREVISE1                            ; END BY UPDATING ALL PATH DIRECTORIES
;
;
MVROTNAM:   STA    GBUF+4
MVHEDNAM:   LDA    (WRKPATH),Y
            STA    GBUF+4,Y
            DEY
            BNE    MVHEDNAM
            JMP    WRTGBUF                             ; WRITE CHANGED HEADER BLOCK_
;
;
RENPATH:    LDA    C_NWPATH                            ; GET ADDRESS TO NEW PATHNAME_
            STA    TPATH
            LDA    C_NWPATH+1                          ; SET UP FOR SYNTAXING ROUTINE (SYNPATH)_
            STA    TPATH+1
            LDA    SSNWPATH                            ; (MOVE BYTE FOR SISTER PAGE, TOO_)
            STA    SISTPATH
            JMP    SYNPATH                             ; GO SYNTAX IT_ (RETURNS LAST LOCAL NAME LENGTH IN Y)_
;
;
DEALBLK:    LDY    #0                                  ; BEGIN AT THE BEGINNING_
DALBLK1:    STY    SAPTR                               ; SAVE CURRENT INDEX_
            LDA    GBUF,Y                              ; GET ADDRESS (LOW) OF BLOCK TO BE DEALLOCATED_
            CMP    GBUF+$100,Y                         ; TEST FOR NUL BLOCK_
            BNE    DALBLK2                             ; BRANCH IF NOT NUL_
            CMP    #0
            BEQ    DALBLK3                             ; SKIP IT IF NUL_
DALBLK2:    LDX    GBUF+$100,Y                         ; GET THE REST OF THE BLOCK ADDRESS_
            JSR    DEALLOC                             ; FREE IT UP ON VOLUME BIT MAP_
            BCS    DALBLKERR                           ; RETURN ANY ERROR_
            LDY    SAPTR                               ; GET INDEX TO SAPLING LEVEL INDEX BLOCK AGAIN_
DALBLK3:    INY                                        ; POINT AT NEXT BLOCK ADDRESS_
            BNE    DALBLK1                             ; BRANCH IF MORE TO DEALLOCATE (OR TEST)_
            CLC                                        ; INDICATE NO ERROR_
DALBLKERR:  RTS
;
;
;PAGE
;
DESTROY:    JSR    FINDFILE                            ; LOOK FOR FILE TO BE WIPED OUT_
            BCS    DESTERR                             ; PASS BACK ANY ERROR_
            JSR    TSTOPEN                             ; IS THIS FILE OPEN?
            LDA    TOTENT
            BEQ    DSTROY1                             ; BRANCH IF FILE NOT OPEN_
            LDA    #FILBUSY
            SEC                                        ; INFORM USER THAT FILE CAN'T BE DESTORYED AT THIS TIME_
            RTS
;
DSTROY1:    LDA    #0                                  ; FORCE PROPER FREE COUNT IN VOLUME_
            STA    REQL                                ; (NO DISK ACCESS OCCURS IF ALREADY PROPER)
            STA    REQH
            JSR    TSFRBLK
            BCC    DSTROY2
            CMP    #OVRERR                             ; WAS IT JUST A FULL DISK?
            SEC
            BNE    DESTERR                             ; NOPE, REPORT ERROR_
;
DSTROY2:    LDA    A:DFIL+D_ATTR                       ; MAKE SURE IT'S OK TO DESTROY THIS FILE_
            AND    #DSTROYEN
            BNE    DSTROY3                             ; BRANCH IF OK_
            LDA    #ACCSERR                            ; TELL USER IT'S NOT KOSHER_
            JSR    SYSERR                              ; (RETURNS TO CALLER OF DESTORY)
;
DSTROY3:    JSR    TWRPROT1                            ; BEFORE GOING THRU DEALLOCATION,
            BCS    DESTERR                             ; TEST FOR WRITE PROTECTED HARDWARE_
            LDA    A:DFIL+D_STOR                       ; FIND OUT WHICH STORAGE TYPE_
            AND    #$F0                                ; STRIP OFF NAME LENGTH_
            CMP    #(TRETYP+1)*$10                     ; IS IT A SEED, SAPLING, OR TREE?
            BCC    DSTREE                              ; BRANCH IF IT IS_
            JMP    DSTDIR                              ; OTHERWISE TEST FOR DIRECTORY DESTROY_
;
DSTREE:     JSR    GTTINDX                             ; GET A BIT MAP BUFFER AND TEMPORARY INDEX BUFFER_
            BCS    DESTERR
            LDA    A:DFIL+D_STOR                       ; GET STORAGE TYPE AGAIN
            AND    #$F0
            CMP    #TRETYP*$10                         ; IS THIS A TREE (FULL 2-LEVEL)?
            BNE    DSTSAP                              ; NO, TEST FOR SAPLING_
            JSR    RDFRST                              ; READ IN ROOT INDEX FOR THIS FILE_
            BCC    DSTRE2                              ; BRANCH IF ALL IS WELL_
DESTERR:    RTS                                        ; OTHERWISE RETURN ERROR_
;
DSTSAP:     CMP    #SAPTYP*$10                         ; IS IT A SAPLING
            BNE    DSTLAST                             ; NO, JUST DEALLOCATE FIRST (AND ONLY) BLOCK_
            JSR    ZTMPIDX                             ; CLEAR OUT TEMPORARY INDEX BUFFER_
            LDA    A:DFIL+D_FRST                       ; MAKE THIS SAP LOOK LIKE A TREE___
            LDY    #0                                  ; THIS IS DONE BY PLACING THE FIRST BLOCK ADDRESS
            STA    (TINDX),Y                           ; IN THE TEMP (TOP) INDEX BUFFER AS
            INC    TINDX+1
            LDA    A:DFIL+D_FRST+1                     ; A SUB INDEX WOULD APPEAR_
            STA    (TINDX),Y
            DEC    TINDX+1
DSTRE2:     LDY    #0                                  ; BEGIN SCAN OF TOP LEVEL INDEX AT ZERO_
DSTNXT:     STY    TREPTR                              ; SAVE POINTER TO TREE LEVEL_
            LDA    (TINDX),Y                           ; GET BLOCK ADDRESS OF A SUB INDEX BLOCK
            INC    TINDX+1                             ; (TEST FOR NUL BLOCK)
            CMP    (TINDX),Y
            BNE    DSTRE3                              ; BRANCH IF WE'VE GOT AN BLOCK TO DEALLOCATE_
            CMP    #0                                  ; IS ENTIRE ADDRESS ZERO?
            BEQ    DSTRE4                              ; YES, DO NEXT_ (CARRY SET)
DSTRE3:     CLC                                        ; INDICATE THERE IS A BLOCK OF INDEXES TO FREE UP_
            STA    BLOKNML
            LDA    (TINDX),Y                           ; GET HI ADDRESS TOO_
            STA    BLOKNMH
DSTRE4:     DEC    TINDX+1                             ; (RESTORE PROPER ADDRESS FOR BUFFER)
            BCS    DSTNXT1                             ; BRANCH IF NO SUB INDEX_
            JSR    RDGBUF                              ; USE GENERAL BUFFER FOR SUB INDEX BUFFER_
            BCS    DESTERR
            JSR    DEALBLK                             ; GO FREE UP BLOCKS IN SUB INDEX
            BCS    DESTERR
            LDY    TREPTR                              ; AND FREE UP SUB INDEX BLOCK TOO_
            INC    TINDX+1
            LDA    (TINDX),Y
            TAX
            DEC    TINDX+1
            LDA    (TINDX),Y
            JSR    DEALLOC
            BCS    DESTERR
            LDY    TREPTR
DSTNXT1:    INY                                        ; HAVE ALL SUB INDEXES BEEN LOCATED?
            BNE    DSTNXT                              ; NO, DO NEXT___
DSTLAST:    LDA    A:DFIL+D_FRST                       ; DEALLOCATE FIRST BLCOK OF FILE_
            LDX    A:DFIL+D_FRST+1
            JSR    DEALLOC
            BCS    DESTERR
            LDA    #0                                  ; UPDATE DIRECTORY TO FREE ENTRY SPACE_
            STA    A:DFIL+D_STOR
            CMP    H_FCNT                              ; FILE ENTRY WRAP?
            BNE    DST1                                ; BRANCH IF NO CARRY ADJUSTMENT
            DEC    H_FCNT+1                            ; TAKE CARRY FROM HIGH BYTE OF FILE ENTRIES
DST1:       DEC    H_FCNT                              ; MARK HEADER WITH ONE LESS FILE
            LDX    BMTAB                               ; UPDATE (LAST) BITMAP_
            JSR    BMAPUP
            BCS    DESTERR
            LDY    #VCBTFRE
            LDA    A:DFIL+D_USAGE
            ADC    (VCBPTR),Y
            STA    (VCBPTR),Y                          ; UPDATE CURRENT FREE BLOCK COUNT_
            INY
            LDA    A:DFIL+D_USAGE+1
            ADC    (VCBPTR),Y
            STA    (VCBPTR),Y
            LDA    #0                                  ; FORCE RESCAN FROM FIRST BITMAP
            LDY    #VCBCMAP
            STA    (VCBPTR),Y
            JMP    DREVISE                             ; UPDATE DIRECTORY LAST___
;
;PAGE
;
DSTDIR:     CMP    #DIRTYP*16                          ; IS THIS A DIRECTORY FILE?
            BEQ    DSDIR1                              ; YES, PROCEED_
            LDA    #CPTERR                             ; FILE IS NOT COMPATABLE_
            JSR    SYSERR                              ; GIVE UP_
;
DSDIR1:     JSR    FNDBMAP                             ; MAKE SURE A BUFFER IS AVAILABLE FOR THE BITMAP_
            BCS    DSDIRERR
            LDA    A:DFIL+D_FRST                       ; READ IN FIRST BLOCK OF DIRECTORY INTO GBUF_
            STA    BLOKNML
            LDA    A:DFIL+D_FRST+1
            STA    BLOKNMH
            JSR    RDGBUF
            BCS    DSDIRERR
            LDA    GBUF+HCENT+4                        ; FIND OUT IF ANY FILES EXIST ON THIS DIRECTORY_
            BNE    DSDIRACC                            ; BRANCH IF ANY EXIST_
            LDA    GBUF+HCENT+5
            BEQ    DSDIR2
DSDIRACC:   LDA    #ACCSERR
            JSR    SYSERR
;
DSDIR2:     LDA    GBUF+2                              ; GET FORWARD LINK_
            CMP    GBUF+3                              ; TEST FOR NO LINK_
            BNE    DSDIR3
            CMP    #0
            BEQ    DSTLAST                             ; IF NO LINK, THEN FINISHED_
DSDIR3:     LDX    GBUF+3
            JSR    DEALLOC                             ; FREE THIS BLOCK_
            BCS    DSDIRERR
            LDA    GBUF+2
            STA    BLOKNML
            LDA    GBUF+3
            STA    BLOKNMH                             ; READ IN LINKED BLOCK_
            JSR    RDGBUF
            BCC    DSDIR2                              ; LOOP UNTIL ALL ARE FREED_
DSDIRERR:   RTS
;
;
;PAGE
WORKSPC     =      *
V_STATUS:   .RES   1                                   ; VOLUME STATUS, INCLUDES 'ACTIVE' IN BIT 7
H_CREDT:    .RES   2                                   ; DIRECTORY CREATION DATE
            .RES   2                                   ; DIRECTORY CREATION TIME
            .RES   1                                   ; VERSION UNDER WHICH THIS DIRECTORY WAS CREATED
            .RES   1                                   ; EARLIEST VERSION THAT IT'S COMPATABLE WITH
H_ATTR:     .RES   1                                   ; ATTRIBUTES (PROTECT BIT, ETC_)
H_ENTLN:    .RES   1                                   ; LENGTH OF EACH ENTRY IN THIS DIRECTORY_
H_MAXENT:   .RES   1                                   ; MAXIMUM NUMBER OF ENTRIES PER BLOCK
H_FCNT:     .RES   2                                   ; CURRENT NUMBER OF FILES IN THIS DIRECTORY
            .RES   2                                   ; ADDRESS OF FIRST ALLOCATION BIT MAP
            .RES   2                                   ; TOTAL NUMBER OF BLOCKS ON THIS UNIT
            .RES   5                                   ; (FOR FUTURE EXPANSION)
;
D_DEV:      .RES   1                                   ; DEVICE NUMBER OF THIS DIRECTORY ENTRY
D_HEAD:     .RES   2                                   ; ADDRESS OF <SUB> DIRECTORY HEADER
D_ENTBLK:   .RES   2                                   ; ADDRESS OF BLOCK WHICH CONTAINS THIS ENTRY
D_ENTNUM:   .RES   1                                   ; ENTRY NUMBER WITHIN BLOCK_
DFIL        =      *
D_STOR      =      *-DFIL                              ; STORAGE TYPE * 16 + FILE NAME LENGTH
            .RES   1
;: *-DFIL ; FILE NAME
            .RES   15
D_FILID     =      *-DFIL                              ; USER'S IDENTIFICATION BYTE
            .RES   1
D_FRST      =      *-DFIL                              ; FIRST BLOCK OF FILE
            .RES   2
D_USAGE     =      *-DFIL                              ; NUMBER OF BLOCKS CURRENTLY ALLOCATED TO THIS FILE
            .RES   2
D_EOF       =      *-DFIL                              ; CURRENT END OF FILE MARKER
            .RES   3
D_CREDT     =      *-DFIL                              ; DATE OF FILE'S CREATION
            .RES   2
;: *-DFIL ; TIME OF FILE'S CREATION
            .RES   2
;:  EQU *-DFIL ; SOS VERSION THAT CREATED THIS FILE
            .RES   1
D_COMP      =      *-DFIL                              ; BACKWARD VERSION COMPATABILTY
            .RES   1
D_ATTR      =      *-DFIL                              ; 'PROTECT', READ/WRITE 'ENABLE' ETC_
            .RES   1
D_AUXID     =      *-DFIL                              ; USER AUXILLARY IDENTIFACATION
            .RES   2
D_MODDT     =      *-DFIL                              ; FILE'S LAST MODIFICATION DATE
            .RES   2
D_MODTM     =      *-DFIL                              ; FILE'S LAST MODIFICATION TIME
            .RES   2
D_DHDR      =      *-DFIL                              ; HEADER BLOCK ADDRESS OF FILE'S DIRECTORY
            .RES   2
;
CMDADR:     .RES   2
SCRTCH:     .RES   13                                  ; SCRATCH AREA FOR ALLOCATION ADDRESS CONVERSION
OLDEOF:     .RES   3                                   ; TEMP USED IN W/R
OLDMARK:    .RES   3                                   ; USED BY 'RDPOSN' AND 'WRITE'
SCRHIGH     =      >SCRTCH                             ; AND DEVICE NUMBERS FROM BOB'S CODE_
;

