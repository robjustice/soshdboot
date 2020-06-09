;SBTL 'SOS 1_1 BLOCK FILE MANAGER' L
; 01-FEB-82
;.RELOC
            .SEGMENT   "CODE"
;IBUFSIZ 1
;SBUFSIZ 40
            .INCLUDE   "SOSORG"
            .ORG       ORGBFM                           ; BITMAPS $B800-$BBFF
ZZORG       =          *
;***************************************************************************************************
;          (C) COPYRIGHT 1981 BY APPLE COMPUTER INC.
;                     ALL RIGHTS RESERVED
;***************************************************************************************************
;MSB OFF
;LST VSYM
;
            .EXPORT    BFMGR
;
; BFM INITIALIZATION ENTRIES
; (INIT CODE FOUND IN INIT.SRC)
;
            .EXPORT    BFMFCB1                          ; FCB PAGE 1 ADDR
            .EXPORT    BFMFCB2                          ; AND PAGE 2
            .EXPORT    FCBZPP: ABSOLUTE
            .EXPORT    SISTER
            .EXPORT    PATHBUF
            .EXPORT    VCB
            .EXPORT    WORKSPC
            .EXPORT    PFIXPTR: ABSOLUTE
            .EXPORT    BMAPAGE
            .EXPORT    BMBPAGE
            .EXPORT    FCBADDRH: ABSOLUTE
            .EXPORT    BMAMADR: ABSOLUTE
            .EXPORT    BMBMADR: ABSOLUTE
;
;
            .IMPORT    LEVEL                            ; FILE LEVEL (LOW BYTE)
            .IMPORT    OPMSGRPLY                        ; OPERATOR MESSAGE
            .IMPORT    DATETIME                         ; THANKS TOM___
            .IMPORT    DMGR                             ; THANKS BOB___
            .IMPORT    REQBUF                           ;   "
            .IMPORT    REQFXBUF                         ;   "
            .IMPORT    GETBUFADR                        ;  "
            .IMPORT    RELBUF                           ;   "
            .IMPORT    BLKDLST                          ;   "
            .IMPORT    SERR
            .IMPORT    BACKMASK
;
; ERRORS
;
            .IMPORT    SYSERR
;
            .IMPORTZP  BADPATH                          ; INVALID PATHNAME SYNTAX
            .IMPORTZP  FCBFULL                          ; FILE CONTROL BLOCK FULL
            .IMPORTZP  BADREFNUM                        ; INVALID REFNUM
            .IMPORTZP  PATHNOTFND                       ; PATHNAME NOT FOUND
            .IMPORTZP  VNFERR                           ; VOLUME NOT FOUND
            .IMPORTZP  FNFERR                           ; FILE NOT FOUND
            .IMPORTZP  DUPERR                           ; DUPLICATE FILE NAME ERROR
            .IMPORTZP  DUPVOL                           ; DUPLICATE VOLUME CAN'T BE LOGGED IN_
            .IMPORTZP  OVRERR                           ; NOT ENOUGH DISK SPACE FOR PREALLOCATION
            .IMPORTZP  DIRFULL                          ; DIRECTORY FULL ERROR
            .IMPORTZP  CPTERR                           ; FILE INCOMPATIBLE SOS VERSION
            .IMPORTZP  TYPERR                           ; NOT CURRENTLY SUPPORTED FILE TYPE
            .IMPORTZP  EOFERR                           ; POSITION ATTEMPTED BEYOND END OF FILE
            .IMPORTZP  POSNERR                          ; ILLEGAL POSITION (L_T_ 0 OR G_T_ $FFFFFF)
            .IMPORTZP  ACCSERR                          ; FILE ACCESS R/W REQUEST CONFLICTS WITH ATTRIBUTES_
            .IMPORTZP  BTSERR                           ; USER SUPPLIED BUFFER TOO SMALL
            .IMPORTZP  FILBUSY                          ; EITHER WRITE WAS REQUESTED OR WRITE ACCESS ALREADY ALLOCATED_
            .IMPORTZP  NOTSOS                           ; NOT A SOS DISKETTE
            .IMPORTZP  BADLSTCNT                        ; INVALID VALUE IN LIST PARAMETER
            .IMPORTZP  XDISKSW                          ; DISK SWITCHED
            .IMPORTZP  NOTBLKDEV                        ; NOT A BLOCK DEVICE
            .IMPORTZP  XNOWRITE                         ; DISK/MEDIA IS HARDWARE WRITE PROTECTED
            .IMPORTZP  XIOERROR                         ; INFORMATION ON BLOCK DEVICE NOT ACCESSABLE
            .IMPORTZP  DIRERR                           ; DIRECTORY ENTRY COUNT INCONSISTENT WITH ACTUAL ENTRIES
            .IMPORTZP  BITMAPADR                        ; BIT MAP DISK ADDRESS IMPOSSIBLE
;
; FATAL ERRORS
;
            .IMPORT    SYSDEATH
;
            .IMPORTZP  VCBERR                           ; VOLUME CONTROL BLOCK NOT USABLE
            .IMPORTZP  ALCERR                           ; ALLOCATION BLOCKS INVALID
            .IMPORTZP  TOOLONG                          ; PATHNAME BUFFER OVERFLOW
;PAGE
;
; CONSTANTS
;
DLIMIT      =          $2F                              ; DELIMITER IS CURRENTLY AN ASCII '/'
SEEDTYP     =          1
SAPTYP      =          2
TRETYP      =          3
DIRTYP      =          $D
HEDTYP      =          $E
RDCMD       =          $0
WRTCMD      =          $1
RPTCMD      =          $9
STATCMD     =          $02                              ; REQUEST STATUS OF BLOCK DEVICE_ (BIT 0 = WRITE PROTECTED)
STATSUB     =          $0
PRETIME     =          $20                              ; COMMAND NEEDS CURRENT DATE/TIME STAMP
PREREF      =          $40                              ; COMMAND REQUIRES FCB ADDRESS AND VERIFICATION
PREPATH     =          $80                              ; COMMAND HAS PATHNAME TO PREPROCESS
SISTER      =          $1400
;
; VOLUME STATUS CONSTANTS (BITS)
;
DSWITCH     =          $40                              ; FOR DISK SWITCHED ERROR RECOVERY_
;
; FILE STATUS CONSTANTS
;
DATALC      =          $1                               ; DATA BLOCK NOT ALLOCATED_
IDXALC      =          $2                               ; INDEX NOT ALLOCATED
TOPALC      =          $4                               ; TOP INDEX NOT ALLOCATED
STPMOD      =          $8                               ; STORAGE TYPE MODIFIED
USEMOD      =          $10                              ; FILE USAGE MODIFIED
EOFMOD      =          $20                              ; END OF FILE MODIFIED
DATMOD      =          $40                              ; DATA BLOCK MODIFIED
IDXMOD      =          $80                              ; INDEX BLOCK MODIFIED
FCBMOD      =          $80                              ; HAS FCB/DIRECTORY BEEN MODIFIED? (FLUSH)
;
; FILE ATTRIBUTES CONSTANTS
;
READEN      =          $1                               ; READ ENABLED
WRITEN      =          $2                               ; WRITE ENABLED
NLINEN      =          $10                              ; NEW LINE ENABLED
BKBITVAL    =          $20                              ; FILE NEEDS BACKUP IF SET (BKBITFLG)
RENAMEN     =          $40                              ; RENAME OK WHEN ON_
DSTROYEN    =          $80                              ; DESTROY OK WHEN ON_
;PAGE
; HEADER INDEX CONSTANTS
;
HNLEN       =          $0                               ; HEADER NAME LENGTH (OFFSET INTO HEADER)
;HNAME EQU $1 ; HEADER NAME
HPENAB      =          $10                              ; PASSWORD ENABLE BYTE
HPASS       =          $11                              ; ENCODED PASSWORD
HCRDT       =          $18                              ; HEADER CREATION DATE
; HCRTM EQU $1A ; HEADER CREATION TIME
HVER        =          $1C                              ; SOS VERSION THAT CREATED DIRECTORY
HCMP        =          $1D                              ; BACKWARD COMPATIBLE WITH SOS VERSION
HATTR       =          $1E                              ; HEADER ATTRIBUTES- PROTECT ETC_
; HENTLN EQU $1F ; LENGTH OF EACH ENTRY
; HMENT EQU $20 ; MAXIMUM NUMBER OF ENTRIES/BLOCK
HCENT       =          $21                              ; CURRENT NUMBER OF FILES IN DIRECTORY
HRBLK       =          $23                              ; OWNER'S DIRECTORY ADDRESS
HRENT       =          $25                              ; OWNER'S DIRECTORY ENTRY NUMBER
HRELN       =          $26                              ; OWNER'S DIRECTORY ENTRY LENGTH
VBMAP       =          HRBLK
VTBLK       =          HRENT                            ; (USED FOR ROOT DIRECTORY ONLY)
;
; VOLUME CONTROL BLOCK INDEX CONSTANTS
;
VCBSIZE     =          $20                              ; CURRENT VCB IS 32 BYTES PER ENTRY (VER 0)
VCBNML      =          0                                ; VOLUME NAME LENGTH BYTE
VCBNAM      =          1                                ; VOLUME NAME
VCBDEV      =          $10                              ; VOLUME'S DEVICE
VCBSTAT     =          $11                              ; VOLUME STATUS_ (80=FILES OPEN_ 40=DISK SWITCHED_)
VCBTBLK     =          $12                              ; TOTAL BLOCKS ON THIS VOLUME
VCBTFRE     =          $14                              ; NUMBER OF UNUSED BLOCKS
VCBROOT     =          $16                              ; ROOT DIRECTORY (DISK) ADDRESS
;VCBMORG EQU $18 ; MAP ORGANIZATION (NOT SUPPORTED BY V 0)
;VCBMBUF EQU $19 ; BIT MAP BUF NUM
VCBDMAP     =          $1A                              ; FIRST (DISK) ADDRESS OF BITMAP(S)
VCBCMAP     =          $1C                              ; RELATIVE ADDRESS OF BIT MAP WITH SPACE (ADD TO VCBDMAP)
;VCBMNUM EQU $1D ; RELATIVE BIT MAP CURRENTLY IN MEMORY
VCBOPNC     =          $1E                              ; CURRENT NUMBER OF OPEN FILES_
VCBSWAP     =          $1F                              ; $8X IF VOLUME SWAPPED; $00 IF UNSWAPPED WHERE X=LOW ORDER BYTE OF VCB ADR/16
;
; FILE CONTROL BLOCK INDEX CONSTANTS
;
FCBREFN     =          0                                ; FILE REFERENCE NUMBER (POSITION SENSITIVE)
FCBDEVN     =          1                                ; DEVICE (NUMBER) ON WHICH FILE RESIDES
;FCBHEAD EQU 2 ; BLOCK ADDRESS OF FILE'S DIRECTORY HEADER
;FCBDIRB EQU 4 ; BLOCK ADDRESS OF FILE'S DIRECTORY
FCBENTN     =          6                                ; ENTRY NUMBER WITHIN DIRECTORY BLOCK
FCBSTYP     =          7                                ; STORAGE TYPE - SEED, SAPLING, TREE, ETC_
FCBSTAT     =          8                                ; STATUS - INDEX/DATA/EOF/USAGE/TYPE MODIFIED_
FCBATTR     =          9                                ; ATTRIBUTES - READ/WRITE ENABLE, NEWLINE ENABLE_
FCBNEWL     =          $A                               ; NEW LINE TERMINATOR (ALL 8 BITS SIGNIFICANT)_
FCBBUFN     =          $B                               ; BUFFER NUMBER
FCBFRST     =          $C                               ; FIRST BLOCK OF FILE
FCBIDXB     =          $E                               ; BLOCK ADDRESS OF INDEX (0 IF NO INDEX)
FCBDATB     =          $10                              ; BLOCK ADDRESS OF DATA
FCBMARK     =          $12                              ; CURRENT FILE MARKER_
FCBEOF      =          $15                              ; LOGICAL END OF FILE_
FCBUSE      =          $18                              ; ACTUAL NUMBER OF BLOCKS ALLOCATED TO THIS FILE_
FCBSWAP     =          $1A                              ; $8N = SWAPPED, $00 = UNSWAPPED VOLUME ("N" = VCB ENTRY NUMBER)
FCBLEVL     =          $1B                              ; LEVEL AT WHICH THIS FILE WAS OPENED
FCBDIRTY    =          $1C                              ; FCB MARKED AS MODIFIED
;PAGE
;
; ZERO PAGE STUFF
;
PAR         =          $A0
COMMAND     =          PAR
C_DNAMP     =          PAR+1
C_PATH      =          PAR+1
C_REFNUM    =          PAR+1
C_ISNEWL    =          PAR+2
C_OUTEOF    =          PAR+2
C_BASE      =          PAR+2
C_MRKPTR    =          PAR+2
C_OUTBUF    =          PAR+2
C_NWPATH    =          PAR+3
C_FILIST    =          PAR+3
C_NEWL      =          PAR+3
C_OUTVOL    =          PAR+3
C_OUTREF    =          PAR+3
C_XLIST     =          PAR+3
C_MAXPTH    =          PAR+3
C_MARK      =          PAR+3
C_NEWEOF    =          PAR+3
C_BYTES     =          PAR+4
C_FILSTLN   =          PAR+5
C_OUTBLK    =          PAR+5
C_OPLIST    =          PAR+5
C_XLEN      =          PAR+5
C_FILID     =          PAR+6
C_OUTCNT    =          PAR+6
C_OPLSTLN   =          PAR+7
C_AUXID     =          PAR+7
C_STOR      =          PAR+9
C_EOFLL     =          PAR+$A
C_EOFLH     =          PAR+$B
C_EOFHL     =          PAR+$C
DEBUPTR     =          PAR+$D                           ; NOTE SAME AS BELOW
C_EOFHH     =          PAR+$D
; C.SPARE EQU PAR+$E
;
DEVICE      =          $C0
DHPCMD      =          DEVICE
UNITNUM     =          DEVICE+1
DSTATREQ    =          DEVICE+2
DBUFPL      =          DEVICE+2
DBUFPH      =          DBUFPL+1
DSTATBFL    =          DEVICE+3                         ; TO PASS BACK BUSY, WRITE PROTECT, READ PROTECT_
DSTATBFH    =          DSTATBFL+1
RQCNTL      =          DEVICE+4
RQCNTH      =          RQCNTL+1
BLOKNML     =          DEVICE+6
BLOKNMH     =          BLOKNML+1
BRDPTR      =          DEVICE+8                         ; (AND 9)
;
DVNAMP      =          DEVICE+1                         ; USED FOR 'VOLUME' TO CALL
DVDNUM      =          DEVICE+3                         ; 'GET_DNUM' IN DEVICE MANAGER_
;
SISBPH      =          SISTER+DBUFPH
SISDSTAT    =          SISTER+DSTATBFH
SSBRDPH     =          SISTER+BRDPTR+1
;
;PAGE
;
; ZERO PAGE TEMPORARIES
;
ZTEMPS      =          $B0
PATHNML     =          ZTEMPS
PATHNMH     =          PATHNML+1
USRBUF      =          ZTEMPS
TPATH       =          ZTEMPS+2
WRKPATH     =          ZTEMPS+4
TINDX       =          ZTEMPS+2
DRBUFPL     =          ZTEMPS+4
DRBUFPH     =          DRBUFPL+1
VCBPTR      =          ZTEMPS+6
BMADR       =          ZTEMPS+8
FCBPTR      =          ZTEMPS+$A
DATPTR      =          ZTEMPS+$C
POSPTR      =          ZTEMPS+$E
;
MAXTEMPS    =          $F
SISTEMPS    =          SISTER+ZTEMPS
SSTIDXH     =          SISTER+TINDX+1
SISPATH     =          SISTER+C_PATH+1
SSNWPATH    =          SISTER+C_NWPATH+1
SISUSRBF    =          SISTER+USRBUF+1
SISOUTBF    =          SISTER+C_OUTBUF+1
SISTPATH    =          SISTER+TPATH+1
SISBMADR    =          SISTER+BMADR+1
SISFCBP     =          SISTER+FCBPTR+1
SISDATP     =          SISTER+DATPTR+1
SISPOSP     =          SISTER+POSPTR+1
;
;
; ADDRESSES
;
PATHBUF     =          $1000                            ; NOTE: THIS IS $100 BYTES LONG_
VCB         =          $1100
GBUF        =          $1200                            ; THRU $13FF
;
; INITIALIZATION EQUATES
;
BFMFCB1     =          $1C                              ; FCB PAGE 1 ADDR
BFMFCB2     =          $1D                              ; FCB PAGE 2 ADDR
BMAPAGE     =          >$B800                           ; BIT MAP A ADDR
BMBPAGE     =          >$BA00                           ; BIT MAP B ADDR
FCBZPP      =          FCBPTR
;
;
;
;PAGE
;DSECT 
;.ORG $0 ; (THE FOLLOWING DO NOT NEED TO BE ON ZERO PAGE_ 7/16/80 JRH_)
ZPAGE       =          $00                              ; modified for ca65 
DATBLKL     =          ZPAGE                            ;.RES 1
DATBLKH     =          ZPAGE+1                          ;.RES 1
IDXADRL     =          ZPAGE+2                          ;.RES 1 ; DISK ADDRESS OF INDEX BLOCK
IDXADRH     =          ZPAGE+3                          ;.RES 1
REQL        =          ZPAGE+4                          ;.RES 1
REQH        =          ZPAGE+5                          ;.RES 1
INDXBLK     =          ZPAGE+6                          ;.RES 1
LEVELS      =          ZPAGE+7                          ;.RES 1
TOTENT      =          ZPAGE+8                          ;.RES 1
ENTCNTL     =          ZPAGE+9                          ;.RES 1
ENTCNTH     =          ZPAGE+10                         ;.RES 1
CNTENT      =          ZPAGE+11                         ;.RES 1
NOFREE      =          ZPAGE+12                         ;.RES 1
BMCNT       =          ZPAGE+13                         ;.RES 1
SAPTR       =          ZPAGE+14                         ;.RES 1
TREPTR      =          ZPAGE+15                         ;.RES 1
TLINK       =          ZPAGE+16                         ;.RES 2
FLINK       =          ZPAGE+18                         ;.RES 2
PATHCNT     =          ZPAGE+20                         ;.RES 1
PFIXPTR     =          ZPAGE+21                         ;.RES 2
BMPTR       =          ZPAGE+23                         ;.RES 1
BASVAL      =          ZPAGE+24                         ;.RES 1
HALF        =          ZPAGE+25                         ;.RES 1
;
;
;PAGE
;
; BIT MAP INFO TABLES (A & B)
;
BMTABSZ     =          $6
BMTAB       =          ZPAGE+26                         ;.RES 1
BMBUFBNK    =          ZPAGE+27                         ;.RES 1
BMASTAT     =          ZPAGE+28                         ;.RES 1
BMADEV      =          ZPAGE+29                         ;.RES 1
BMAMADR     =          ZPAGE+30                         ;.RES 1
BMADADR     =          ZPAGE+31                         ;.RES 2
BMACMAP     =          ZPAGE+33                         ;.RES 1 ; SIMILAR TO VCBCMAP
BMBSTAT     =          ZPAGE+34                         ;.RES 1
BMBDEV      =          ZPAGE+35                         ;.RES 1
BMBMADR     =          ZPAGE+36                         ;.RES 1
                                                        ;.RES 2 ; BMBDADR
                                                        ;.RES 1 ; BMBCMAP
;
FCBADDRH    =          ZPAGE+40                         ;.RES 1 ; FILE CONTROL BLOCK'S BUFFER ADDRESS_
FCBANKNM    =          ZPAGE+41                         ;.RES 1 ; AND BANK (SISTER PAGE) BYTE_
TPOSLL      =          ZPAGE+42                         ;.RES 1
TPOSLH      =          ZPAGE+43                         ;.RES 1
TPOSHI      =          ZPAGE+44                         ;.RES 1
RWREQL      =          ZPAGE+45                         ;.RES 1
RWREQH      =          ZPAGE+46                         ;.RES 1
BULKCNT     =          ZPAGE+47                         ;.RES 1
NLCHAR      =          ZPAGE+48                         ;.RES 1
NPATHDEV    =          ZPAGE+49                         ;.RES 3 ; FOR NEW PATHNAME DEVICE AND DIRECTORY HEADER ADDRESS
IOACCESS    =          ZPAGE+52                         ;.RES 1 ; USED TO DETERMINE IF A CALL HAS BEEN MADE TO THE DISK DEVICE HANDLER
DEVNUM      =          ZPAGE+53                         ;.RES 1 ; CURRENT DEVICE TO BE ACCESSED_
TOTDEVS     =          ZPAGE+54                         ;.RES 1 ; USED FOR ACCESSING DRIVES IN NUMERIC ORDER
CMDTEMP     =          ZPAGE+55                         ;.RES 1 ; USED FOR TESTING REFNUM, TIME, AND DSKSWTCH (PRE)PROCESSING_
DATELO      =          ZPAGE+56                         ;.RES 1 ; DATE AND TIME MUST RESIDE ON ZERO PAGE_
DATEHI      =          ZPAGE+57                         ;.RES 1
TIMELO      =          ZPAGE+58                         ;.RES 1
TIMEHI      =          ZPAGE+59                         ;.RES 1
;
DUPLFLAG    =          ZPAGE+60                         ;.RES 1 ; USED FOR DIFFERENCE BETWEEN VNFERR AND DUPVOL BY SYNPATH
ZPGTEMP     =          ZPAGE+61                         ;.RES 1 ; A ONE-BYTE UNSTABLE TEMPORARY
VCBENTRY    =          ZPAGE+62                         ;.RES 1 ; POINTER TO CURRENT VCB ENTRY
;
;
;PAGE
;
;
;
BFMGR:      LDX        COMMAND                          ; WHAT CALL?
;
;
;
            LDA        DISPTCH,X                        ; TRANSLATE INTO COMMAND ADDRESS
            ASL        A                                ; (BIT 7 INDICATES IT'S GOT A PATHNAME TO PREPROCESS)
            STA        CMDTEMP
            AND        #$3F                             ; (BIT 6 IS REFNUM PREPROCESS, 5 IS FOR TIME, SO STRIP EM_)
            TAX
            LDA        CMDTABLE,X                       ; MOVE ADDRESS FOR INDIRECT JUMP_
            STA        CMDADR
            LDA        CMDTABLE+1,X                     ; (HIGH BYTE)
            STA        CMDADR+1
            LDA        #>VCB
            STA        VCBPTR+1                         ; INSURE DEFAULT HI ADDRESS TO VCB BEFORE CALLS
            LDA        #BKBITVAL                        ; INIT "BACKUP BIT FLAG"
            STA        BKBITFLG                         ; TO SAY "FILE MODIFIED"
            LDY        #MAXTEMPS                        ; ZERO OUT SISTER PAGE FOR TEMPS
            LDA        #0
            STA        SERR                             ; MAKE GLOBAL ERROR SAY "NONE"
            STA        DSWGLOB                          ; "DISK SWITCH GLOBAL"
            STA        DUPLFLAG                         ; "DUPLICATE VOLUME ON LINE"
            STA        CFLAG                            ; SET "CREATE" TO NO
            STA        BLOKSAVE
            STA        BLOKSAVE+1                       ; SET PARENT DIRECTORY TO NULL
CLRSIS:     STA        SISTEMPS,Y
            DEY
            BPL        CLRSIS                           ; CARRY IS UNDISTURBED BY THIS LOOP
            BCC        NOPATH
            JSR        SETPATH                          ; GO PROCESS PATHNAME BEFORE CALLING COMMAND
            BCS        ERRORSYS                         ; BRANCH IF BAD NAME_
NOPATH:     ASL        CMDTEMP                          ; TEST FOR REFNUM PREPROCESSING
            BCC        NOPREREF
            JSR        FINDFCB                          ; GO SET UP POINTERS TO FCB AND VCB OF THIS FILE_
            BCS        ERRORSYS                         ; BRANCH IF ANY ERRORS ARE ENCOUNTERED_
NOPREREF:   ASL        CMDTEMP                          ; LASTLY CHECK FOR NECESSITY OF TIME STAMP_
            BCC        TSWVRFY
            LDX        #DATELO                          ; PASS Z PAGE ADDRESS OF WHERE TO RETURN DATE/TIME
            JSR        DATETIME                         ; (NO ERROR POSIBLE)
TSWVRFY:    LDX        COMMAND                          ; TEST FOR NECESSITY OF VOLUME VERIFICATION
            LDA        #PREPATH+PREREF+PRETIME          ; TO ENSURE VCB IS SET
            AND        DISPTCH,X
            BEQ        EXECUTE
            LDY        #VCBSTAT
            LDA        (VCBPTR),Y
            AND        #DSWITCH                         ; WAS THE VOLUME PREVIOUSLY SWITCHED?
            BEQ        EXECUTE
            DEY                                         ; GET DEVICE NUMBER
            LDA        (VCBPTR),Y
            STA        DEVNUM
DVERIFY:    JSR        VERFYVOL                         ; SEE IF PROPER VOLUME NOW ON LINE
            BCC        CLRDSWT                          ; BRANCH IF YES
            JSR        USRREQ                           ; OTHERWISE REQUEST IT BE PUT ON LINE
            BCC        DVERIFY                          ; USER SEZ S/HE DID: CHECK IT OUT
            LDA        #VNFERR                          ; VOLUME NOT FOUND IF USER REFUSES
            BNE        ERRORSYS                         ; REPORT ERROR (BRANCH ALWAYS)
CLRDSWT:    LDY        #VCBSTAT                         ; GET VOLUME
            LDA        (VCBPTR),Y                       ; STATUS
            AND        #$FF-DSWITCH                     ; TURN OFF DISK SWITCH
            STA        (VCBPTR),Y                       ; SO WE WON'T VERIFY NEXT TIME
EXECUTE:    JSR        GOCMD                            ; EXECUTE COMMAND
            BCC        GOODOP                           ; BRANCH IF SUCCESSFUL
            CMP        #XDISKSW                         ; DISK SWITCH?
            BNE        ERRORSYS                         ; NO, REPORT SOME OTHER
            LDY        #VCBSTAT                         ; MARK VCB WITH SWITCH
            LDA        (VCBPTR),Y
            AND        #$FF-DSWITCH                     ; TO ENSURE VOLUME VERIFIED
            BPL        ERRCMD                           ; NO FILES OPEN SO DSWITCH CANT APPLY
            ORA        #DSWITCH
ERRCMD:     STA        (VCBPTR),Y
            JMP        BFMGR                            ; TRY THE COMMAND AGAIN
;
ERRORSYS:   JSR        SYSERR
GOODOP:     RTS                                         ; GOOD RETURN
;
GOCMD:      JMP        (CMDADR)
;
;PAGE
;
CMDTABLE    =          *
            .WORD      CREATE
            .WORD      DESTROY
            .WORD      RENAME
            .WORD      SETINFO
            .WORD      GETINFO
            .WORD      VOLUME
            .WORD      SETPREFX
            .WORD      GETPREFX
            .WORD      OPEN
            .WORD      NEWLINE
            .WORD      READ
            .WORD      WRITE
            .WORD      CLOSE
            .WORD      FLUSH
            .WORD      SETMARK
            .WORD      GETMARK
            .WORD      SETEOF
            .WORD      GETEOF
;
DISPTCH     =          *
            .BYTE      PREPATH+PRETIME+0                ; CREATE
            .BYTE      PREPATH+PRETIME+1                ; DESTROY
            .BYTE      PREPATH+PRETIME+2                ; RENAME
            .BYTE      PREPATH+PRETIME+3                ; SETINFO
            .BYTE      PREPATH+4                        ; GETINFO
            .BYTE      5                                ; VOLUME
            .BYTE      6                                ; SETPREFIX, PATHNAME MOVED TO PREFIX BUFFER
            .BYTE      7                                ; GETPREFIX
            .BYTE      PREPATH+8                        ; OPEN
            .BYTE      PREREF+$9                        ; NEWLINE
            .BYTE      PREREF+$A                        ; READ
            .BYTE      PREREF+$B                        ; WRITE
            .BYTE      PRETIME+$C                       ; CLOSE
            .BYTE      PRETIME+$D                       ; FLUSH, REFNUM MAY BE ZERO TO FLUSH ALL_
            .BYTE      PREREF+$E                        ; SETMARK
            .BYTE      PREREF+$F                        ; GETMARK
            .BYTE      PREREF+$10                       ; SET EOF
            .BYTE      PREREF+$11                       ; GET EOF
;
;PAGE
;
SETPATH:    LDA        C_PATH                           ; FOR A REGULAR PATHNAME,
            STA        TPATH                            ; SET UP TEMP POINTER TO PROCESS
            LDA        C_PATH+1                         ; PATHNAME AND CHECK FOR SYNTAX ERRORS
            STA        TPATH+1
            LDA        SISPATH
            STA        SISTPATH                         ; (LEAVE CALL PARAMETERS ALONE!)
; DROP INTO 'SYNPATH'
;
SYNPATH:    LDA        #<PATHBUF                        ; SET UP DEFAULT ADDRESS FOR
            STA        PATHNML                          ; SYNTAXED PATHNAME -
            STA        WRKPATH                          ; LENGTH, NAME, LENGTH, NAME, ETC___
            LDA        #>PATHBUF
            STA        PATHNMH
            STA        WRKPATH+1                        ; (ASSUMES FULL PATHNAME, NO PREFIX)_
            LDX        #0                               ; USE INDEXED INDIRECT FOR SOURCE PATHNAME
            TXA                                         ; SET BEGINNING OF PATH
            STA        (PATHNML,X)                      ; TO ZERO TO INDICATE NOTHING PROCESSED_
            TAY
            LDA        (TPATH,X)                        ; GET TOTAL LENGTH OF SOURCE PATHNAME
            BMI        ERRSYN
            BEQ        ERRSYN
            STA        PATHCNT                          ; (THIS IS USED AS A 'COUNT-DOWN')
            JSR        INCTPTH                          ; INCREMENT SOURCE POINTER
            LDA        (TPATH,X)                        ; GET FIRST CHARACTER OF PATHNAME
            CMP        #DLIMIT                          ; IS IT A FULL PATHNAME (NO PREFIX)?
            BEQ        BUMPATH                          ; YES, WE'RE READY TO DO IT_
            CMP        #$2E                             ; IS IT A DRIVE NAME '_'?
            BNE        ADPREFIX                         ; NO, ADD PREFIX TO BEGINNING
DRIVENAM:   LDA        (TPATH,X)                        ; MOVE DRIVE NAME FOR VOLUME CALL
            CMP        #DLIMIT                          ; HAVE WE MOVED ENTIRE NAME?
            BEQ        PREVOLM                          ; YES, PROCESS IT_
            INY                                         ; (IF THIS IS THE FIRST, MAKE ROOM FOR LENGTH OF NAME)
            STA        (WRKPATH),Y
            JSR        INCTPTH                          ; BUMP POINTER TO GIVEN NAME_
            DEC        PATHCNT
            BNE        DRIVENAM
            BEQ        PREVOLM1
;
;PAGE
PREVOLM:    JSR        INCTPTH                          ; MAKE IT SO POINTING PAST DELIMITER_
            DEC        PATHCNT
PREVOLM1:   TYA                                         ; SAVE LENGTH OF DRIVE NAME_
            STA        (WRKPATH,X)
            LDA        #<PATHBUF                        ; POINT AT PATHNAME BUFFER FOR DEVICE ID CALL_
            STA        DVNAMP
            LDA        #>PATHBUF
            STA        DVNAMP+1
            LDA        #0                               ; MAKE VIRTUAL POINT AT SWITCHED IN BANK_
            STA        SISTER+DVNAMP+1
            JSR        SRCHDEV                          ; GO IDENTIFY WHICH VOLUME
            BCC        PREVOLM2                         ; BRANCH IF NO ERROR
            CMP        #VNFERR                          ; WAS IT REPORTED AS 'VOLUME NOT FOUND'?
            BNE        SPTHERR                          ; NO SOME OTHER ERROR WAS ENCOUNTERED_
            LDX        DUPLFLAG                         ; YES, WAS IT NOT FOUND BECAUSE SOME OTHER 'OPEN' VOLUME HAS SAME NAME?
            BEQ        SPTHERR                          ; NO, IT SIMPLY WASN'T FOUND_
            LDA        #DUPVOL                          ; (CARRY IS SET)
            RTS
;
PREVOLM2:   LDY        #0                               ; (X CONTAINS AN INDEX TO VCB)
            LDA        VCB,X                            ; GET VOLUME NAME LENGTH_
            STA        PATHBUF,Y
SPATH2:     INX                                         ; MOVE VOLUME NAME INTO PATH NAME BUFFER IN
            INY                                         ; PLACE OF DISK DEVICE NAME ('_D1' OR SIMULAR)
            LDA        VCB,X
            STA        PATHBUF,Y
            CPY        PATHBUF                          ; HAVE ALL CHARACTERS BEEN MOVED?
            BNE        SPATH2
            LDX        #0                               ; RESET X FOR INDEXING
            STX        PATHNML
            LDA        #>PATHBUF
            STA        PATHNMH
            LDA        PATHCNT                          ; IS THAT ALL THERE IS?
            BNE        SPATH3                           ; NO, MORE TO COME___
            CLC
            JMP        ENDPATH
;
SPATH3:     INY                                         ; BUMP TO END OF NAME+1
            STY        WRKPATH                          ; RESET WORKPATH POINTER TO CURRENT_
            LDA        #0                               ; RESET PATHNAME BUFFER POINTER_
            LDY        #>PATHBUF
            BNE        NOPREFX                          ; BRANCH ALWAYS___
;
ERRSYN:     LDA        #BADPATH                         ; RETURN SYNTAX ERROR
SPTHERR:    SEC
            RTS
;
ADPREFIX:   LDA        A:PFIXPTR                        ; GET POINTER TO BEGINNING OF THE
            LDY        A:PFIXPTR+1                      ; PREFIX_
NOPREFX:    STA        PATHNML
            STY        PATHNMH                          ; IF NO PRESET PREFIX, THIS IS THE SAME AS
            BNE        FRSTCHAR                         ; PATHBUF ADDRESS_ (BRANCH ALWAYS TAKEN)
;
;PAGE
;
BUMPATH:    DEC        PATHCNT                          ; FIRST ADJUST COUNT
            CLC                                         ; (JUST IN CASE OF LAST CHARACTER)
            BEQ        ENDPATH                          ; (MUST OF HAD TRAILING SPACES)
            JSR        INCTPTH
FRSTCHAR:   LDY        #0                               ; INIT COUNT FOR THIS PORTION OF THE
            TYA                                         ; PATHNAME_ ALSO PRESET LENGTH TO ZERO IN
            STA        (WRKPATH,X)                      ; CASE OF TRAILING SPACES_
            LDA        (TPATH,X)                        ; GET CHARACTER_
            AND        #$7F                             ; IGNORE HIGH BIT_
            CMP        #$20                             ; IS IT A LEADING SPACE?
            BEQ        BUMPATH                          ; IF SO, IGNORE IT_
            CMP        #$5B                             ; IS IT GREATER THAN (UPPER CASE) A 'Z'?
            BCC        ALFA1                            ; NO, MAKE SURE IT'S AN ALPHA CHARACTER
            AND        #$5F                             ; YES, ASSUME IT'S LOWER CASE, AND UPSHIFT
            CMP        #$5B                             ; WAS IT TRULY LOWER CASE?
            BCS        ERRSYN                           ; NO, GIVE ERROR_
;
ALFA1:      CMP        #$41                             ; IS IT LESS THAN 'A'?
            BCC        ERRSYN                           ; YES! IT'S CRAP___
            BCS        SAVPATH                          ; NO, IT'S GOOD_ SAVE IT_
;
NXTCHAR:    LDA        (TPATH,X)                        ; GET THE NEXT CHARACTER_
            AND        #$7F                             ; THESE CHARACTERS MAY BE ALPHA, NUMERIC,
            CMP        #$5B                             ; OR A PERIOD - ONLY THE FIRST HAD TO BE ALPHA
            BCC        ALFA2                            ; BRANCH IF LESS THAN 'Z'
            AND        #$5F                             ; UPSHIFT LOWER CASE_
            CMP        #$5B                             ; NOW IS IT VALID?
            BCS        ERRSYN                           ; NOPE_
;
ALFA2:      CMP        #$41                             ; IS IT GREATER THAN 'A'?
            BCS        SAVPATH                          ; YUP, IT IS WORTH SAVIN_
            CMP        #$3A                             ; >9?
            BCS        TSTDLIM                          ; YES
            CMP        #$30                             ; NO, <0?
            BCS        SAVPATH                          ; NO, IT'S VALID NUMERIC_
TSTDLIM:    CMP        #DLIMIT                          ; IS IT THE DELIMITER?
            BEQ        ENDPATH                          ; YES_ CARRY SET INDICATES MORE TO COME_
            CMP        #$2E                             ; IS IT A '_' (PERIOD)?
            BNE        ERRSYN                           ; NO, IT'S AN ERROR (#@&##@!)
SAVPATH:    CLC
            INY                                         ; BUMP NAME LENGTH
            STA        (WRKPATH),Y
            DEC        PATHCNT                          ; IF ZERO, THAT WAS THE LAST CHARACTER
            BEQ        ENDPATH                          ; (CARRY CLEAR INDICATES END OF PATH)
            INC        TPATH                            ; BUMP POINTER TO SOURCE PATHNAME_
            BNE        NXTCHAR
            INC        TPATH+1                          ; HIGH ORDER, WHEN NECESSARY_
            BNE        NXTCHAR                          ; BRANCH ALWAYS_
;PAGE
;
ENDPATH:    TYA                                         ; GET CURRENT NAME LENGTH
            STA        (WRKPATH,X)                      ; AND PUT IT IN FRONT OF NAME
            BCC        LSTNAME                          ; BRANCH IF THAT WAS THE LAST OF PATH
            CMP        #$10                             ; WAS THE NAME ILLEGALLY LONG?
            BCS        ERRSYN1                          ; YES, REPORT IT_
            LDY        #0
            SEC                                         ; ADJUST WORK POINTER TO END OF PREVIOUS NAME_
            ADC        WRKPATH
            STA        WRKPATH                          ; REPLACE OLD POINTER_
            BCC        BUMPATH                          ; DO NEXT NAME_
            LDA        #TOOLONG                         ; THIS IS A NEVER ERROR!
            JSR        SYSDEATH                         ; (NEVER RETURNS)_
;
LSTNAME:    BEQ        TSTVALD
            CMP        #$10                             ; MAKE SURE LAST ISN'T TOO LONG
            BCS        ERRSYN1
            INY                                         ; PUT A ZERO AT END OF PROCESSED PATHNAME
            LDA        #0
TSTVALD:    STA        (WRKPATH),Y
            LDA        (PATHNML,X)                      ; SURE THERE IS A PATHNAME
            BEQ        ERRSYN1                          ; IF NOT, REPORT ERROR_
            CLC                                         ; INDICATE NO ERROR_
            RTS
;
ERRSYN1:    JMP        ERRSYN
;
INCTPTH:    INC        TPATH                            ; POINT AT NEXT CHARACTER
            BNE        INCPTH1
            INC        TPATH+1
INCPTH1:    RTS
;
;PAGE
SETPREFX:   JSR        SETPATH                          ; CALL IS MADE HERE SO A 'NUL' PATH MAY BE DETECTED_
            BCC        SETPRFX1                         ; BRANCH IF PATHNAME OK
            TAX                                         ; SAVE ERROR CODE
            LDY        #0
            LDA        (C_PATH),Y                       ; TEST FOR A NUL PATHNAME
            BEQ        RESETPFX                         ; BRANCH IF PREFIX TO BE RESET_
            TXA                                         ; RESTORE ERROR CODE
            RTS
RESETPFX:   STA        A:PFIXPTR
            CLC
            RTS
SETPRFX1:   LDA        PATHNML                          ; MAKE SURE NAME STARTED WITH A '/' DELIMITER_
            BNE        ERRSYN1                          ; BRANCH IF IT DID_
            LDY        WRKPATH                          ; FIND THE END OF THE INPUT PREFIX
            CLC                                         ; ADD LAST LOCAL NAME LENGTH TO FIND TRUE END_
            LDA        (PATHNML),Y
            BNE        SETPRFX3
            DEY
            TYA
            BNE        SETPRFX4
SETPRFX3:   ADC        WRKPATH
            TAY
SETPRFX4:   EOR        #$FF                             ; GET COMPLIMENT TO FIND BEGINNING ADDRESS_
            STA        A:PFIXPTR                        ; OF NEW PREFIX IN THE PREFIX BUFFER
            STA        WRKPATH                          ; (PREFIX ALWAYS ENDS AT THE LAST BYTE OF BUFFER)
MOVPRFX:    LDA        (PATHNML),Y
            STA        (WRKPATH),Y                      ; MOVE IN NEW PREFIX
            DEY
            BPL        MOVPRFX
            CLC                                         ; AND WE'RE FINISHED!
            RTS                                         ; NO ERRORS POSIBLE FROM THIS ROUTINE_
;
;PAGE
;
GETPREFX:   CLC                                         ; CALCULATE HOW BIG A BUFFER IS NEEDED TO
            LDA        A:PFIXPTR                        ; PASS THE PREFIX BACK TO THE USER_
            EOR        #$FF                             ; (EVEN IF NO PREFIX, 1 BYTE IS NEEDED TO SHOW 0 LENGTH)
            ADC        #2                               ; ADD 2 FOR LEADING AND ENDING "/"_
            CMP        C_MAXPTH                         ; IS THERE ENOUGH SPACE IN USER'S BUFFER?
            BCC        SENDPRFX                         ; BRANCH IF YES
            LDA        #BTSERR                          ; TELL USER BUFFER IS TOO SMALL_
            RTS                                         ; (CARRY IS SET TO INDICATE ERROR_)
;
SENDPRFX:   LDY        #0                               ; SAVE TOTAL LENGTH OF STRING TO BE RETURNED
            STA        (C_PATH),Y
            TAY
            DEY                                         ; DISCOUNT TRAILING DELIMITER_
            BEQ        NULPREFX                         ; BRANCH IF PREFIX IS SET TO NUL_
            INY
            LDX        A:PFIXPTR                        ; GET BEGINNING ADDRESS OF PREFIX AGAIN
            DEX
            STX        WRKPATH
            LDA        #>PATHBUF
            STA        WRKPATH+1
SNDLMIT:    LDA        #DLIMIT                          ; PLACE DELIMITER BEFORE, BETWEEN, AND AFTER LOCAL NAMES_
            STA        (C_PATH),Y
SNDPRFX1:   DEY
            BEQ        GOTPRFX                          ; BRANCH IF ALL OF PREFIX IS TRANSFERED_
            LDA        (WRKPATH),Y
            STA        (C_PATH),Y                       ; ASSUME IT'S A CHARACTER_
            AND        #$F0                             ; NOW TEST TO SEE IF IT WAS A LOCAL LENGTH_
            BEQ        SNDLMIT                          ; BRANCH IF IT WAS_
            BNE        SNDPRFX1                         ; GO MOVE NEXT CHAR IF IT WASN'T (ALWAYS TAKEN)_
NULPREFX:   TYA                                         ; RETURN NUL STRING_
            STA        (C_PATH),Y
GOTPRFX:    CLC                                         ; INDICATE NO ERROR_
            RTS
;PAGE
;
FINDFCB:    LDA        A:FCBADDRH                       ; INITIALIZE INDIRECT POINTER TO
            STA        FCBPTR+1                         ; FILE CONTROL BLOCK (ALLOCATED WHEN SYSTEM
            LDA        #0                               ; WAS FIRST BOOTED)_
            STA        FCBPTR                           ; NOTE: ALWAYS STARTS ON PAGE BOUNDARY_
            LDA        FCBANKNM                         ; SET SISTE PAGE BYTE TOO___
            STA        SISFCBP
            LDY        C_REFNUM                         ; GET REQUESTED REFERENCE
            BMI        ERRNOTBLK                        ; BRANCH IF IT'S NOT A BLOCK DEVICE REFERENCE
            DEY                                         ; (SHOULD BE IN THE RANGE OF 1-16 BEFORE DECREMENT)
            CPY        #$10                             ; IS IT A VALID REFNUM?
            BCS        REEFER                           ; NO, THE USER'S SMOKIN DOPE!
            TYA                                         ; TO FIND ASSOCIATED FILE CONTROL STUFF,
            ASL        A                                ; MULTIPLY (REFNUM-1) BY 32_
            ASL        A
            ASL        A
            ASL        A
            ASL        A
            BCC        SVFCBLO                          ; BRANCH IF IT'S WITHIN FIRST HALF OF FCB
            INC        FCBPTR+1                         ; BUMP TO SECOND HAVE (REFNUM>8)
SVFCBLO:    STA        FCBPTR                           ; SAVE LOW ADDRESS OF REFERENCED FCB
            LDA        C_REFNUM                         ; NOW VERIFY THAT FILE IS OPEN_
            LDY        #FCBREFN
            CMP        (FCBPTR),Y                       ; SHOULD BE EQUAL!
            BNE        ERRNOREF                         ; BRANCH IF THEY'RE NOT
FNDFCBUF:   LDY        #FCBBUFN                         ; IT'S A LEGAL FILE, NOW SET UP
            LDA        (FCBPTR),Y                       ; INDIRECT POINTERS TO DATA
GTBUFFRS:   LDX        #DATPTR                          ; (AND INDEX) BUFFER(S) IN ZERO PAGE
            JSR        GETBUFADR                        ; GET BUFFER ADDRESS UNLESS
            BCS        REEFER1                          ; BOB HAS BEEN SMOKIN DOPE___
            LDA        #2                               ; (ASSUME AN INDEX BLOCK BUFFER IS ALSO PRESENT)
            ADC        DATPTR+1
            STA        TINDX+1
            LDA        DATPTR
            STA        TINDX
            LDA        SISDATP
            STA        SSTIDXH
            LDY        #FCBDEVN
            LDA        (FCBPTR),Y                       ; MAKE SURE DEVICE
            STA        D_DEV                            ; NUMBER TEMPS MATCH
            STA        DEVNUM                           ; CURRENT FILE'S DEVICE
            LDA        #0                               ; LOOK AT ALL VOLUMES LOGGED IN
FNDFVOL:    TAX
            LDA        VCB+VCBDEV,X                     ; GET VOLUMES DEVICE NUMBER
            CMP        (FCBPTR),Y                       ; HVE WE FOUND A MATCH_
            BNE        FNDFV1
            LDY        #FCBSWAP                         ; SWAP BYTES
            LDA        VCB+VCBSWAP,X                    ; MISMATCH
            CMP        (FCBPTR),Y                       ; MEANS FILE BELONGS
            BNE        FNDFV_1                          ; TO ANOTHER VOLUME
            LDA        VCB,X                            ; IS THIS AN OPEN DEVICE?
            BEQ        FNDFV_1                          ; NO, TRY ANOTHER VOLUME
            JSR        FVOLFOUND                        ; YES, SAVE VCB ADDRESS
            LDA        VCB+VCBSWAP,X                    ; SWAPPED?
            BEQ        REEFER1                          ; NO, RETURN CALMLY TO USER
            JSR        SWAPIN                           ; YES, SWAP ME IN
            BCC        REEFER1                          ; RETURN WITHOUT ERROR
            LDA        #XIOERROR                        ; USER REFUSED TO MOUNT PROPER VOLUME
            RTS
;
FNDFV_1:    LDY        #FCBDEVN                         ; RELOAD Y WITH DEVICE INDEX
FNDFV1:     TXA
            CLC
            ADC        #VCBSIZE
            BCC        FNDFVOL                          ; LOOP UNTIL FOUND
            LDA        #VCBERR                          ; OTHERWISE DIE A SYSTEM DEATH!
            JSR        SYSDEATH
;PAGE
;
ERRNOREF:   LDA        #0                               ; DROP A ZERO INTO THIS FCB TO
            STA        (FCBPTR),Y                       ; SHOW FREE FCB
;
REEFER:     LDA        #BADREFNUM                       ; TELL USER THAT REQUESTED REFNUM
            SEC                                         ; IS ILLEGAL (OUT OF RANGE) FOR THIS CALL_
REEFER1:    RTS
;
ERRNOTBLK:  LDA        #NOTBLKDEV                       ; TELL USER THAT SPECIFIED DEVICE IS NOT A BLOCK DEVICE
            SEC
            RTS
;
SVCBADR     =          *
FVOLFOUND:  STX        VCBPTR
            LDA        #VCB/256
            STA        VCBPTR+1
            CLC                                         ; INDICATE LEGAL REFNUM
            RTS
;PAGE
; NAME    : GETDNUM
; FUNCTION: GET DEVICE NUMBER
; INPUT   : DVNAMP SETUP
; OUTPUT  : DEVNUM IN 'SCRTCH'
;         : 'BPL' IF NOT BLOCK DEV
;         : 'BCS' IF NO DEVICE
; VOLATILE: ALL REGS
;
GETDNUM     =          *
            LDA        #<SCRTCH+1                       ; SET UP POINTER TO SCRATCH AREA
            STA        DVDNUM                           ; TO RECIEVE DEVICE NUMBER_
            LDA        #SCRHIGH
            STA        DVDNUM+1
            LDA        #0                               ; PLACE A ZERO IN BANK BYTE SINCE
            STA        SISTER+DVDNUM+1                  ; IT'S NOT IN A BANK_
            STA        VCBPTR+1
            LDA        #4                               ; THE 'GET_DNUM' COMMAND_
            STA        DHPCMD
            JSR        RPEATIO0                         ; CALL BOB FOR THE INFO_
            RTS                                         ; RETURN WITH DEVMGR CC'S
;PAGE
;
; NAME    : SRCHDEV
; FUNCTION: SEARCH FOR A VOLUME
;
SRCHDEV     =          *
            JSR        GETDNUM                          ; GET DEVNUM
            BCS        VOLERR1                          ; BRANCH IF ANY ERROR OTHER THAN NOTBLOCKDEV
            BPL        ERRNOTBLK                        ; BRANCH IF NOT A BLOCK DEVICE
            LDA        #0                               ; NOW SEARCH FOR A VOL WITH THE
            STA        NFOPEN                           ; INIT TEMP VCB POINTER
VOLOOK:     TAX                                         ; SAME DEVNUM AS SCRTCH
            LDA        VCB+VCBSTAT,X                    ; ANY FILES OPEN?
            BNE        VLOOK00                          ; BRANCH IF SOME FILE OPEN
            STX        NFOPEN                           ; ELSE SAVE THE VCB ENTRY PTR
VLOOK00     =          *
            LDA        VCB+VCBSWAP,X                    ; VOLUME SWAPPED OUT?
            BNE        VNOTEQ                           ; YES, CANT BE THE ACTIVE VOL
            LDA        VCB+VCBDEV,X
            EOR        SCRTCH+1
            BEQ        VLOOK0                           ; BRANCH IF MATCH_
VNOTEQ:     LDA        VCB,X                            ; IS THIS A FREE VCB?
            BNE        VLOOK2                           ; BRANCH IF NOT FREE, OTHEWISE TAKE NEXT BRANCH_
VLOOK0:     EOR        VCB,X                            ; TEST FOR A VOLUME NAME LENGTH
            BEQ        VLOOK1                           ; BRANCH IF VCB FREE
            JSR        SVCBADR                          ; SAVE CURRENT ADDRESS OF VCB_
            LDA        VCB+VCBSTAT,X                    ; TEST FOR ANY OPEN FILES_
            BPL        VLOOK3                           ; LOG THE VOLUME IN JUST TO BE SURE
            LDA        SCRTCH+1                         ; SET UP
            STA        DEVNUM                           ; DEVICE NUMBER ARGUMENT
            TXA                                         ; SAVE PTR TO VCB
            PHA                                         ; ON STACK
            JSR        VERFYVOL                         ; COMPARES VCBPTR TO DEVNUM CONTENTS
            BCC        VNOSWIT
            CMP        #VNFERR                          ; SEE IF NOTHING IN DRIVE
            BEQ        VLOOK7                           ; BRANCH IF NOTHING IN DRIVE
            JSR        TSTSOS                           ; IS THE VOLUME AN UNRECOGNIZED SOS OR (UCSD OR DOS)?
            BCS        KNOTSOS                          ; DEFINITELY NOT SOS FORMAT
            LDX        #0                               ; START VCB SCAN AT BEGINNING
            JSR        SNSWIT1                          ; FIND A FREE VCB AND LOG IN THE NEW GUY
            BCS        VNOSWIT1                         ; CAN'T LOG IN NEW GUY--KEEP OLD
            PLA
            LDX        VCBPTR                           ; PASS BACK X AS NEW VCB
            RTS
;
NFOPEN:     .RES       1                                ; TEMP VCB PTR FOR VCB W/ NO FILES OPEN
;
VNOSWIT:    CLC                                         ; RETURN IT TO USER
            PLA                                         ; REMEMBER OLD VCB PTR
            TAX                                         ; AND PASS BACK TO USER
            RTS
;: RETURN TO CALLER X=POINTER TO VCB_
;
VOLERR1:    SEC                                         ; RETURN SOME VOLUME ERROR
            RTS
VNOSWIT1:   CMP        #DUPVOL
            BNE        VLOOK7                           ; REPORT OTHER ERROR FROM LOGGING IN NEW VOL AS VNF
            TAX
            PLA                                         ; MAKE STACK CORRECT
            TXA                                         ; RESTORE ERROR CODE
            SEC
            RTS                                         ; IF DUPLICATE VOLUME ERROR, RETURN FACT TO USER
KNOTSOS:    PLA                                         ; MAKE STACK CORRECT
            LDA        #NOTSOS                          ; FOR THE PASCAL FOLK
            RTS                                         ; NOTSOS MEANS UCSD OR DOS OR BAD SOS VOLUME
;
VLOOK7:     PLA                                         ; THROW AWAY OLD VCB PTR
            JMP        NOVOLM                           ; AND REPORT VOLUME NOT FOUND
;
VLOOK1:     JSR        SVCBADR                          ; SAVE ADDRESS OF FREE VCB_
VLOOK2:     TXA                                         ; BUMP TO NEXT VOLUME ENTRY_
            CLC
            ADC        #VCBSIZE
            BCC        VOLOOK                           ; BRANCH IF MORE TO CHECK_
            LDX        VCBPTR+1                         ; FREE VCB YET FOUND?
            BNE        VLOOK3                           ; BRANCH IF YES
            LDX        NFOPEN                           ; SAVE POSSIBLE FREE VCB
            JSR        SVCBADR                          ; AND SAVE PTR PERMANENTLY
VLOOK3:     LDA        VCBPTR+1                         ; WAS A FREE VCB FOUND?
            BEQ        NOVOLM                           ; BRANCH IF VOLUME CAN'T BE LOGGED IN_
            LDA        SCRTCH+1                         ; GET DEVICE NUMBER
            STA        DEVNUM                           ; SAVE DEVICE NUMBER_
            LDA        #1                               ; FAKE OUT 'LOKVOL'
            STA        SCRTCH                           ; TO THINK TO LOOK ONLY ONCE_
            STA        TOTDEVS
            LDA        #>VCB
            STA        VCBPTR+1
            STA        PATHNMH                          ; (TO MAKE HARMLESS)
            LDA        #0
            STA        SISTER+PATHNMH
            LDX        VCBPTR
            STX        PATHNML
            STA        VCB,X                            ; FORCE CURRENT VOLUME OFF LINE, THEN LOG WHATS THERE_
            JSR        FREEVCB                          ; GO READ ROOT DIRECTORY_
            BCS        RTVOLNAM                         ; RETURN ANY ERRORS
            LDX        VCBPTR                           ; MAKE SURE VOLUME WAS LOGGED IN
            LDA        VCB,X
            BEQ        NOVOLM                           ; RETURN ERROR
            RTS                                         ; ELSE RETURN NORMALLY
NOVOLM:     LDA        #VNFERR                          ; TELL USER 'NO VOLUME'
            SEC
RTVOLNAM:   TAX                                         ; SAVE REAL ERROR WHILE DUPLICATE IS CHECKED
            LDA        DUPLFLAG
            BEQ        RTV1                             ; BRANCH IF NOT DUPLICATE
            LDX        #DUPVOL
RTV1:       TXA                                         ; RECALL ERROR
            RTS
;PAGE
;***************************************************************************************************
; NAME    : VOLUME
; FUNCTION: RETURN VOLUME INFO
; INPUT   : DEVICE NAME
; OUTPUT  : THE INFO
; VOLATILE: ALL REGS
;***************************************************************************************************
;
VOLUME      =          *
            LDA        C_DNAMP                          ; TRANSFER DEVICE NAME
            STA        DVNAMP                           ; NAME FOR DMGR
            LDA        C_DNAMP+1
            STA        DVNAMP+1
            LDA        SISTER+C_DNAMP+1                 ; AND XTND
            STA        SISTER+DVNAMP+1
            JSR        GETDNUM                          ; GET DEVNUM
            BCC        VOL7                             ; =>SOME KINDA ERROR
            RTS                                         ; RETURN ERROR
VOL7:       BMI        VOL2                             ; =>IT'S GOOD___
            LDA        #NOTBLKDEV                       ; NOT BLOCKED
            JMP        VOLERR                           ; =>RETURN THE ERROR
;
; UNCONDITIONALLY READ ROOT DIRECTORY:
;
VOL2        =          *
            LDA        SCRTCH+1
            STA        DEVNUM                           ; SETUP DEV NUMBER
            LDA        #2                               ; BLKNUM=2
            LDX        #0
            JSR        GETROT0                          ; GET IT PLEASE
            LDA        #VNFERR                          ; ERROR CODE
            BCC        VOL8                             ; BRANCH IF NO ERROR ON READ
            RTS                                         ; =>ERROR, PASS IT ON_
;
VOL8:       LDA        #<VCB                            ; SET VCBPTR TO THE
            STA        VCBPTR                           ; FIRST OF THEM
            LDA        #>VCB
            STA        VCBPTR+1
;
; IS THIS VOLUME SOS OR OTHER?
;
            JSR        TSTSOS                           ; WHICH KIND?
            BCC        VLOGGED                          ; =>IT'S SOS
            JMP        VNOTSOS                          ; =>NOT SOS
;
; IS THIS SOS VOLUME LOGGED IN?
;
VLOGGED     =          *
            JSR        CMPVCB                           ; DOES VOLNAME MATCH?
            BCC        VFOUND                           ; =>YES, WE KNOW ABOUT IT_
            JSR        VNXTVCB                          ; BUMP TO NEXT
            BCC        VLOGGED                          ; =>TRY 'EM ALL___
            BCS        VNEW                             ; =>NOT FOUND, IT'S NEW (BRANCH ALWAYS)
;
;
; IT'S BEEN LOGGED IN BEFORE:
;  IS IT SWAPPED IN OR OUT?
;
VFOUND      =          *
            LDY        #VCBSWAP                         ; INDEX TO IT
            LDA        (VCBPTR),Y                       ; SWAPPED?
            BPL        VFOUND1                          ; =>IN_ RETURN THE INFO
;
; SWAPPED OUT. BEFORE WE SWAP IT
;  IN, MAKE SURE IT BELONGS ON
;  THIS DEVICE!
;
            LDY        #VCBDEV                          ; INDEX TO IT
            LDA        (VCBPTR),Y                       ; GET ITS DEVICE
            CMP        DEVNUM                           ; CORRECT DEVICE?
            BEQ        VSWAPIN                          ; =>YES
            LDA        #DUPVOL                          ; IF FOR ANOTHER DEV,
            JMP        VOLERR                           ; THEN IT'S AN ERROR!
;
; NOW SWAP-IN THIS VOLUME:
;
VSWAPIN     =          *
            JSR        SWAPIN                           ; SWAP IT IN
            JMP        VINFO                            ; AND RETURN THE INFO
;
VFOUND1:    LDY        #VCBDEV
            LDA        (VCBPTR),Y                       ; SAME DEVICES?
            CMP        DEVNUM
            BEQ        VINFO                            ; YES; RETURN THE INFORMATION
            LDY        #VCBSTAT
            LDA        (VCBPTR),Y                       ; OPEN FILES?
            BPL        VFOUND2                          ; BRANCH IF NOT
            LDA        #DUPVOL
            BNE        VOLERR                           ; ELSE REPORT DUPLICATE VOLUME ERROR (BRANCH ALWAYS)
VFOUND2:    LDY        #VCBNML                          ; MOVE THE LOGIN TO THIS NEW DEVICE
            LDA        #0                               ; BY UNLOGGING THE OLD
            STA        (VCBPTR),Y                       ; AND LOGGING IN THE NEW (DROP INTO VNEW)
;***************************************************************************************************
;
; IT'S A BRAND NEW VOLUME.
;  GUESS WE'LL HAVE TO LOG IT IN:
;
VNEW        =          *
            LDA        DEVNUM                           ; PASS A REG TO SWAPOUT
            JSR        SWAPOUT                          ; SWAP ANY ACTIVE VOL ON THIS DEVICE
            BCC        VNEW1                            ; BRANCH ON NO ERROR
            LDA        #XIOERROR
            RTS
VNEW1:      LDA        #<VCB                            ; FIND AN EMPTY VCB
            STA        VCBPTR
            LDA        #>VCB
            STA        VCBPTR+1
VFREE:      LDY        #VCBNML
            LDA        (VCBPTR),Y                       ; EMPTY VCB?
            BEQ        VLOGIN                           ; ITS FREE, USE IT
            LDY        #VCBDEV
            LDA        (VCBPTR),Y                       ; OR ONE WITH SAME DEVICE
            CMP        DEVNUM
            BNE        VFREEX                           ; BRANCH IF NO DEVICE MATCH
            LDY        #VCBSTAT
            LDA        (VCBPTR),Y                       ; AND NO OPEN FILES
            BPL        VLOGIN                           ; BRANCH IF OK TO REUSE THIS VCB
            LDA        DEVNUM                           ; THEN WE MUST SWAP OUT THIS VOLUME
            JSR        SWAPOUT
            BCC        VFREEX                           ; SWAPOUT PROCEEDED OK
            LDA        #XIOERROR                        ; ELSE REPORT ERROR
            RTS
VFREEX:     JSR        VNXTVCB                          ; TRY NEXT
            BCC        VFREE                            ; MORE TO COME
; RAN OUT OF MT'S ... FIND W/O FILES
VNFIL:      LDY        #VCBSTAT
            LDA        (VCBPTR),Y
            BPL        VLOGIN
            JSR        VNXTVCB
            BCC        VNFIL
; ALL OPEN ... REPORT VCBFULL
            LDA        #FCBFULL
            BNE        VOLERR
VLOGIN      =          *
            JSR        LOGVCB                           ; AND LOGIN THIS ONE
;***************************************************************************************************
;
; RETURN ALL THE NICE INFO:
;
VINFO       =          *
            LDA        #0
            LDY        #VCBTFRE                         ; FETCH VOLUME FREE BLOCK COUNT
            STA        (VCBPTR),Y                       ; FORCE RESCAN OF ALL
            INY                                         ; BITMAPS
            STA        (VCBPTR),Y                       ; TO MAKE SURE VCB INFO CURRENT
            STA        REQL                             ; FREE BLOCKS
            STA        REQH
            JSR        TSFRBLK
;
            LDX        VCBPTR                           ; GET VCB INDEX
            LDY        #0
VINFO1      =          *
            LDA        VCB+VCBTBLK,X                    ; MOVE TOTAL
            STA        (C_OUTBLK),Y                     ; BLOCKS AVAIL
            INX
            INY
            CPY        #4                               ; AND FREE ONES TOO
            BNE        VINFO1
;
            LDY        #0                               ; NOW DO VOLNAME
            LDA        (VCBPTR),Y
            TAY
VINFO2      =          *
            LDA        (VCBPTR),Y
            STA        (C_OUTVOL),Y
            DEY
            BPL        VINFO2
            CLC
            BCC        VOLRET                           ; =>DONE
;
VOLERR      =          *
            SEC
VOLRET      =          *
            RTS
;PAGE
;***************************************************************************************************
; THIS ISN'T A SOS VOLUME. MARK
;  THE ACTIVE VOL THIS DEVICE
;  SO THAT IT GETS CHECKED LATER:
;
VNOTSOS     =          *
            LDY        #VCBDEV                          ; IS VCB FOR THIS
            LDA        (VCBPTR),Y                       ; DEVICE?
            CMP        DEVNUM
            BNE        VNS2
            LDY        #VCBSTAT                         ; INDEX TO IT
            LDA        (VCBPTR),Y                       ; GET STATUS
            BPL        VNS2                             ; =>NOT ACTIVE_
            ORA        #DSWITCH                         ; SET 'SWITCHEROO'
            STA        (VCBPTR),Y                       ; PUT IT BACK
;
VNS2        =          *
            JSR        VNXTVCB                          ; GET NEXT VCB
            BCC        VNOTSOS                          ; =>TRY 'EM ALL_
;
            LDA        #NOTSOS                          ; GIVE THE ERROR
            BNE        VOLERR                           ; (BRANCH ALWAYS)
;SKP 5
; NAME    : VNXTVCB
; FUNCTION: BUMP VCBPTR TO NEXT VCB
; INPUT   : NOTHING
; OUTPUT  : VCBPTR UPDATED
;         : 'BCC' IF MORE TO GO
;         : 'BCS' IF DONE
; VOLATILE: AC
;
VNXTVCB     =          *
            LDA        VCBPTR
            CLC
            ADC        #VCBSIZE                         ; BUMP IT
            STA        VCBPTR
            RTS                                         ; CARRY SET IF END OF PAGE
;PAGE
CREATE      =          *
            INC        CFLAG                            ; SAY WE ARE IN CREATE (DIR EXTEND)
            JSR        LOOKFILE                         ; CHECK FOR DUPLICATE / GET FREE ENTRY
            BCS        TSTFNF                           ; ERROR CODE IN ACC MAY BE 'FILE NOT FOUND'
            LDA        #DUPERR                          ; TELL EM A FILE OF THAT NAME ALREADY EXISTS
CRERR1:     SEC                                         ; INDICATE ERROR ENCOUNTERED
            RTS                                         ; RETURN ERROR IN ACC_
;
TSTFNF:     CMP        #FNFERR                          ; 'FILE NOT FOUND' IS WHAT WE WANT
            BNE        CRERR1                           ; PASS BACK OTHER ERROR_
            LDA        NOFREE                           ; TEST FOR DIRECTORY SPACE
            BNE        CREAT1                           ; BRANCH IF VALID FREE ENTRY WAS FOUND_
            LDA        #DIRFULL                         ; RETURN DIRECTORY FULL ERROR
            SEC
            RTS
;
CREAT1:     LDY        #$9                              ; SET UP DEFAULT PARAMETERS FOR CREATE
            LDA        #0                               ; IN THE SPACE DIRECTLY FOLLOWING THE
ZERCALL:    STA        C_FILID,Y                        ; CALL SPECIFCATION AND THEN
            DEY                                         ; CHECK FOR ADDITIONAL PARAMETERS FROM
            BPL        ZERCALL                          ; USER'S CALL SPEC VIA 'C_CLIST'
            LDA        #SEEDTYP                         ; DEFAULT TYPE IS 'SEED' TREE INDEX
            STA        C_STOR
            LDY        C_XLEN                           ; GET THE LENGTH OF THE CALL XTENSION LIST
            BEQ        CRENAM                           ; IF ZERO THEN USE DEFAULTS
            DEY                                         ; (SINCE THE POINTER IS AT BYTE 0)
            CPY        #$9                              ; MAKE SURE WE DON'T HAVE TOO MANY PARAMETERS
            BCC        MOVPARM                          ; MOVE 'EM IF REASONABLE COUNT_
            LDA        #BADLSTCNT                       ; INVALID LIST COUNT
            RTS                                         ; RETURN ERROR_
;
MOVPARM:    LDA        (C_XLIST),Y                      ; MOVE IN THE USER SPECIFIED
            STA        C_FILID,Y                        ; PARAMETERS_ VALIDITY IS CHECKED
            DEY                                         ; AT VARIOUS POINTS FURTHER ALONG IN
            BPL        MOVPARM                          ; THIS PROCESS_
CRENAM:     LDY        #0                               ; MOVE LOCAL FILE NAME TO ENTRY BUFFER_
            LDA        (PATHNML),Y                      ; GET LENGTH OF LOCAL NAME
            TAY
CRENAM1:    LDA        (PATHNML),Y
            STA        A:DFIL+D_STOR,Y
            DEY                                         ; (MOVE ALL, INCLUDING LENGTH BYTE_)
            BPL        CRENAM1
            LDA        C_FILID                          ; MOVE FILE AND AUX ID_
            STA        A:DFIL+D_FILID
            LDA        C_AUXID
            STA        A:DFIL+D_AUXID
            LDA        C_AUXID+1
            STA        A:DFIL+D_AUXID+1
            LDA        #READEN+WRITEN+RENAMEN+DSTROYEN
            STA        A:DFIL+D_ATTR
            LDA        D_HEAD                           ; SAVE FILE'S HEADER ADDRESS TOO_
            STA        A:DFIL+D_DHDR
            LDA        D_HEAD+1
            STA        A:DFIL+D_DHDR+1
            JSR        TWRPROT1                         ; CAN WE WRITE TO THIS DISKETTE?
            BCS        CRERR1
            LDA        C_STOR                           ; NOW TEST STORAGE TYPE FOR TREE TYPE FILES
            CMP        #4                               ; NOTE: THIS IS HARD CODED SINCE ALL TREES ARE LESS THAN 4 ***********
            BCC        SEED                             ; BRANCH IF SOME TYPE OF TREE (SEED, SAPLING___)
            JMP        NOTREE                           ; GO TEST FOR SOME OTHER TYPE (SUCH AS DIRECTORY)_
;PAGE
;
SEED:       LDX        #SEEDTYP                         ; START OUT ASSUMING A SEED FILE
            LDA        C_EOFHH                          ; TEST FOR OUT OF RANGE PREALLOCATION
            BEQ        SEED1                            ; (HOPEFULLY BRANCH ALWAYS)
OVFLOW:     LDA        #OVRERR                          ; REPORT UNABLE TO SATISFY REQUEST_
            SEC                                         ; INDICATE ERROR
            RTS
;
SEED1:      LDA        C_EOFHL                          ; CALCULATE THE NUMBER OF
            STA        A:DFIL+D_EOF+2                   ; BLOCKS NEEDED FOR PRE-ALLOCATION
            LSR        A
            TAY                                         ; Y HOLDS THE NUMBER OF INDEX BLOCKS NEEDED
            STA        DATBLKH
            LDA        C_EOFLH                          ; (CARRY UNDISTURBED FROM LAST SHIFT)
            STA        A:DFIL+D_EOF+1
            ROR        A                                ; WE NOW HAVE THE LOW ORDER COUNT OF NEEDED DATA BLOCKS
            STA        DATBLKL
            LDA        C_EOFLL
            STA        A:DFIL+D_EOF                     ; (CARRY IN TACT FROM LOW COUNT)
            BNE        INCDATA                          ; BUMP THE COUNT ON DATA BLOCKS IF REQUEST
            BCC        TSTSAP                           ; IS NOT A MULTIPLE OF 512_
INCDATA:    INC        DATBLKL
            BNE        TSTSAP
            INY                                         ; MUST INCREASE NUMBER OF INDEXES ALSO_
            INC        DATBLKH
TSTSAP:     TYA                                         ; IF NON ZERO, THEN IT'S AT LEAST A SAPLING_
            BNE        SAPLING
            LDA        DATBLKL                          ; TO QUALIFY AS AN HONEST SEED,
            BNE        TSTSEED                          ; THEN ONE OR LESS DATA BLOCKS REQUESTED
            INC        DATBLKL                          ; (MUST BE AT LEAST ONE BLOCK ALLOCATED
            BNE        CREALC                           ; TYPE IS SEED_ BRANCH ALWAYS
TSTSEED:    CMP        #1                               ; IF GREATER THAN ONE, IT'S NOT A SEED_
            BEQ        CREALC                           ; IT IS A SEED_ CONTINUE CREATION
            INX                                         ; THE TYPE IS SAPLING_
            INY                                         ; ONE INDEX BLOCK IS NEEDED_
            BNE        CREALC                           ; BRANCH ALWAYS
;PAGE
;
SAPLING:    INX                                         ; TYPE IS AT LEAST SAPLING_
            CMP        #1                               ; NO MORE THAN ONE INDEX BLOCK FOR A SAPLING
            BNE        TREE
            LDA        DATBLKL                          ; MUST BE SURE THIS IS REAL MAX SAPLING (128K FILE)
            BEQ        CREALC                           ; BRANCH IF IT IS_
TREE:       INY                                         ; ACCOUNT FOR ADDITIONAL 2ND LEVEL INDEX
;
            INX                                         ; TYPE IS TREE (2 LEVEL INDEX)
            INY                                         ; ADD AN EXTRA INDEX BLOCK FOR TOP INDEX
CREALC:     STY        INDXBLK                          ; STORE INDEX BLOCK COUNT
            TXA                                         ; PUT STORAGE TYPE IN DIRECTORY ENTRY
            ASL        A
            ASL        A
            ASL        A
            ASL        A
            ORA        A:DFIL+D_STOR
            STA        A:DFIL+D_STOR
            STX        LEVELS                           ; SAVE NUMBER OF INDEX LEVELS FOR PREALLOCATION_
            TYA                                         ; NOW FIGURE THE TOTAL NUMBER OF
            CLC                                         ; BLOCKS NEEDED (DATA + INDEX BLOCKS)
            ADC        DATBLKL
            STA        A:DFIL+D_USAGE                   ; (MIGHT AS WELL RECORD IT IN DIR
            STA        REQL                             ; WHILE WE'RE AT IT_)
            LDA        DATBLKH
            ADC        #0                               ; UPDATE HI BYTE TOO
            STA        A:DFIL+D_USAGE+1
            STA        REQH
            LDX        D_DEV                            ; PASS ALONG THE DEVICE WE'RE TALKIN ABOUT_
            JSR        TSFRBLK                          ; 'TEST FREE BLOCKS' FINDS OUT IF ENOUGH FREE SPACE EXISTS
            BCS        OVFLOW                           ; BRANCH IF NOT ENOUGH SPACE_
            JSR        ALC1BLK                          ; GO ALLOCATE FIRST BLOCK
            BCS        CRERR
            STA        A:DFIL+D_FRST                    ; (RETURNS ACC=LOW Y=HIGH)
            STA        IDXADRL                          ; SAVE AS ADDRESS FOR INCORE INDEX ALSO_
            STY        A:DFIL+D_FRST+1
            STY        IDXADRH
            JSR        ZERGBUF                          ; GO CLEAN OUT GBUF
            JSR        GTTINDX                          ; GET TEMPORARY SPACE FOR AN INDEX BLOCK
            JSR        ZTMPIDX                          ; AND ZERO IT OUT_
            LDX        LEVELS
            DEX                                         ; TEST FOR NUMBER OF LEVELS NEEDED_
            BEQ        ENDCRE                           ; BRANCH IF SEED FILE_
            DEX                                         ; IS IT A SAPLING PRE-ALLOCATION_
            BEQ        SAPFILE
            LDY        INDXBLK                          ; LOAD NUMBER OF INDEX BLOCKS NEEDED
            DEY                                         ; REMOVE THE ONE JUST ALLOCATED_
            STY        REQL
            STY        INDXBLK
            JSR        ALCIDXS                          ; GO ALLOCATE INDEXES FOR LOWER INDEX BLOCKS_
            BCS        CRERR
            JSR        WRTDFRST                         ; GO WRITE TREE TOP INDEX BLOCK_
            BCS        CRERR                            ; BRANCH IF UNABLE TO DO THIS_
            LDA        #0                               ; INIT INDEX POINTER
            STA        TREPTR
;PAGE
FILLTREE:   LDY        TREPTR
            LDA        (TINDX),Y                        ; GET ADDRESS OF LOWER BLOCK
            STA        IDXADRL
            INC        TINDX+1                          ; BUMP TO PAGE 2 TO GET HI ADDRESS_
            LDA        (TINDX),Y                        ; GET HIGH ADDRESS_
            STA        IDXADRH
            DEC        TINDX+1                          ; CLEAN UP AFTER SELF___
            DEC        INDXBLK                          ; IS THIS THE LAST BLOCK ALLOCATED?
            BEQ        LSTSAP                           ; YES, ALLOCATE PARTIAL FILLED INDEX BLOCK
            LDA        #0                               ; ALLOCATE ALL 256 INDEXES
            STA        REQL
            JSR        SAPINDX                          ; AND WRITE ZEROED DATA BLOCKS_
            BCS        CRERR                            ; STOP IF ERROR ENCOUNTERED_
            JSR        WRTINDX                          ; WRITE INDEX BLOCK
            BCS        CRERR                            ; HOPEFULLY NEVER TAKEN_
            INC        TREPTR
            JSR        RDFRST                           ; READ IN TOP INDEX AGAIN_
            BCC        FILLTREE                         ; BRANCH IF NO ERROR_
CRERR:      SEC                                         ; JUST IN CASE IT WAS CLEAR_
            RTS                                         ; RETURN ERROR_
;
;
SAPFILE     =          *
LSTSAP:     LDA        DATBLKL                          ; GET NUMBER OF DATA BLOCKS (LOW BYTE) REQUESTED_
            STA        REQL
            JSR        SAPINDX                          ; GO ALLOCATE DATA BLOCKS AND WRITE EM_
            BCS        CRERR
ENDCRE:     JSR        WRTINDX                          ; GO WRITE INDEX BLOCK_ (FOR SEED THIS IS DATA_)
            BCS        CRERR
            LDX        #3                               ; MOVE CREATION TIME FOR THIS ENTRY
TRETIME:    LDA        DATELO,X
            STA        A:DFIL+D_CREDT,X
            DEX
            BPL        TRETIME
ENDCRE0:    INC        H_FCNT                           ; ADD ONE TO TOTAL NUMBER OF FILES IN SPECIFIED DIRECTORY_
            BNE        ENDCRE1
            INC        H_FCNT+1
            LDX        #3                               ; ENSURE MOD
ENDCRX:     LDA        DATELO,X                         ; DATE/TIME
            STA        A:DFIL+D_MODDT,X                   ; IS
            DEX                                         ; INITIALIZED
            BPL        ENDCRX
ENDCRE1:    LDX        D_DEV                            ; UPDATE APPROPRIATE BIT MAP
            JSR        UPBMAP
            BCS        CRERR2                           ; BRANCH ON BITMAP UPDATE ERR
            JSR        DREVISE                          ; UPDATE DIRECTORY LAST
            RTS                                         ; RETURN ERRORS OR OK RESULT
;
;PAGE
SAPINDX:    JSR        ZTMPIDX                          ; ZERO OUT ANY STUFF LEFT OVER_
            LDA        REQL                             ; PRESERVE REQUEST COUNT
            STA        TLINK
            JSR        ALCIDXS                          ; GO ALLOCATE REQUESTED NUMBER OF BLOCKS_
            BCS        CRERR
            LDY        #0                               ; THEN WRITE ZEROS TO DATA BLOCKS_
            STY        SAPTR                            ; USE AS POINTER TO INDEX BLOCK
            LDA        (TINDX),Y                        ; GET DATA BLOCK ADDRESS (LOW BYTE)_
            STA        BLOKNML
            INC        TINDX+1
            LDA        (TINDX),Y                        ; GET HIGH ADRRESS OF PRE-ALLOCATED DATA BLOCK_
            STA        BLOKNMH
            DEC        TINDX+1                          ; (RESET BUFFER ADDRESS)
            JSR        WRTGBUF                          ; WRITE DATA BLOCK
            BCS        CRERR
            LDA        TLINK                            ; GET NUMBER REQUESTED AGAIN
            STA        REQL
DATINIT:    LDY        SAPTR                            ; GET POINTER TO INDEX BLOCK AGAIN_
            INY                                         ; ANTICIPATE DOIN' THE NEXT DATA BLOCK
            DEC        REQL                             ; DO WE INDEED HAVE ANOTHER BLOCK TO WRITE_
            BEQ        DATDONE                          ; NO, ALL DONE (CARRY CLEAR)_
            STY        SAPTR                            ; USE AS POINTER TO INDEX BLOCK
            LDA        (TINDX),Y                        ; GET DATA BLOCK ADDRESS (LOW BYTE)_
            STA        BLOKNML
            INC        TINDX+1                          ; BUMP HI ADDR OF INDEX BUFFER TO ACCESS HIGH ADDR_
            TAX                                         ; WAS LOW ADDRESS A ZERO?
            BNE        DATIT1                           ; IF NOT, NO NEED TO CHECK VALIDITH OF HI BYTE
            CMP        (TINDX),Y
            BNE        DATIT1                           ; BOTH BYTES CAN'T BE ZERO_
            LDA        #ALCERR
            JSR        SYSDEATH
DATIT1:     LDA        (TINDX),Y                        ; GET HIGH ADRRESS OF PRE-ALLOCATED DATA BLOCK_
            STA        BLOKNMH
            DEC        TINDX+1                          ; (RESET BUFFER ADDRESS)
            LDA        #GBUF/256
            STA        DBUFPH                           ; RESET TO ADDR TO GBUF JUST TO BE SURE_
            JSR        REPEATIO                         ; WRITE DATA BLOCK
            BCC        DATINIT
DATDONE:    RTS                                         ; RETURN STATUS (CARRY SET IF ERROR)
;
REPEATIO    =          *
            LDA        #RPTCMD
            STA        DHPCMD
            JMP        RPEATIO1
;
ZERGBUF:    LDY        #0                               ; ZERO OUT THE GENERAL PURPOSE BUFFER
            TYA
ZGBUF:      STA        GBUF,Y                           ; WIPE OUT BOTH PAGES
            STA        GBUF+$100,Y                      ; WITH SAME LOOP_
            INY
            BNE        ZGBUF
            RTS
;
;
ZTMPIDX:    LDY        #0                               ; ZERO OUT TEMPORARY INDEX BLOCK
            TYA
ZINDX1:     STA        (TINDX),Y                        ; THIS HAS TO BE DONE A
            INY                                         ; TIME SINCE IT'S INDIRECT_
            BNE        ZINDX1
            INC        TINDX+1
ZINDX2:     STA        (TINDX),Y
            INY
            BNE        ZINDX2
            DEC        TINDX+1                          ; RESTORE PROPER ADDRESS
CRERR2:     RTS
;PAGE
NOTREE:     CMP        #DIRTYP                          ; IS A DIRECTORY TO BE CREATED?
            BEQ        ISDIR                            ; YES, DO SO___
            JMP        NOTDIR                           ; NO, TRY NEXT TYPE_
;
ISDIR:      LDA        C_EOFHH                          ; CAN'T CREATE A DIRECTORY LARGER THAN
            ORA        C_EOFHL                          ; 127 BLOCKS (THAT'S HUGE!)
            BEQ        ISDIR1                           ; BRANCH IF WITHIN LIMITS, OTHEWISE
DIROVR:     LDA        #OVRERR                          ; REQUESTED DIRECTORY SIZE CAN'T BE
            SEC                                         ; CREATED_ SET CARRY TO INDICATE ERROR_
            RTS
;
ISDIR1:     LDA        C_EOFLH                          ; CALCULATE HOW MANY BLOCKS WILL
            LSR        A                                ; BE NEEDED FOR THIS NEW DIRECTORY_
            TAY                                         ; (SAVE INITIAL COUNT IN Y)
            LDA        C_EOFLL                          ; IF REQUESTED EOF IS NOT AN EVEN BLOCK
            BNE        DADD1                            ; SIZE, THEN ROUND UP_
            BCC        TSDIRSZ                          ; BRANCH IF ROUNING UNNECESSARY_
DADD1:      INY                                         ; ADD ONE TO BLOCK COUNT_
TSDIRSZ:    TYA                                         ; TEST TO BE SURE SIZE IS GREATER THAN ZERO
            BEQ        DADD1                            ; IF ZERO THEN SIZE=1
            STA        A:DFIL+D_USAGE                   ; SAVE NUMBER OF BLOCKS TO BE USED_
            STA        REQL
            ASL        A                                ; NOW SAVE ADJUSTED END OF FILE
            STA        A:DFIL+D_EOF+1
            LDA        #0
            STA        A:DFIL+D_EOF
            STA        A:DFIL+D_EOF+2
            STA        REQH                             ; REQUESTED NUMBER OF BLOCKS NEVER EXCEEDS 128_
            JSR        TSFRBLK                          ; TEST TO BE SURE ENOUGH DISK SPACE IS FREE_
            BCS        DIROVR                           ; BRANCH IF REQUEST TOO LARGE_
            JSR        ZERGBUF                          ; CLEAR CRAP FROM GBUF_
            JSR        ALC1BLK                          ; GET ADDRESS OF FIRST (HEADER) BLOCK_
            BCS        CRERR2
            STA        A:DFIL+D_FRST
            STA        TLINK
            STY        A:DFIL+D_FRST+1
            STY        TLINK+1                          ; (TLINK IS FOR REVERSE LINKAGE_)
            LDA        SOSTMPL                          ; STORE SOS STAMP IN NEW DIRECTORY
            STA        GBUF
            LDA        SOSTMPH
            STA        GBUF+1
            LDY        #4                               ; MOVE OTHER VARIOUS THINGS
            BNE        DRSTUF1                          ; BRANCH ALWAYS
DRSTUF:     LDA        D_ENTBLK,Y                       ; MOVE OWNING ENTRY'S
            STA        GBUF+HRBLK+4,Y                   ; BLOCK ADDRESSES AND NUMBER TO NEW HEADER_
DRSTUF1:    LDA        SOSVER,Y                         ; MOVE VERSION, COMPATABLITY,
            STA        GBUF+HVER+4,Y                    ; ATTRIBUTES, AND ENTRY SIZE
            DEY
            BPL        DRSTUF
            LDA        H_ENTLN                          ; OVER WRITE LAST BYTE MOVED IN ABOVE LOOP WITH
            STA        GBUF+HRELN+4                     ; THE PARENT DIRECTORY ENTRY LENGTH_
            LDA        A:DFIL+D_STOR                    ; SET HEADER TYPE AND NAME
            TAY
            ORA        #HEDTYP*16
            STA        GBUF+HNLEN+4
            TYA                                         ; (AND WHILE WE'RE AT IT SET DIRECTORY TYPE)
            ORA        #DIRTYP*16
            STA        A:DFIL+D_STOR
;
MVHNAME:    LDA        A:DFIL+D_STOR,Y
            STA        GBUF+HNLEN+4,Y                   ; MOVE HEADER NAME
            DEY
            BNE        MVHNAME
            LDX        #3                               ; GET CURRENT DATE_
CRETIME:    LDA        DATELO,X
            STA        GBUF+HCRDT+4,X                   ; SAVE AS HEADER CREATION TIME
            STA        A:DFIL+D_CREDT,X                 ; AND DATE OF FILE CREATE_
            DEX
            BPL        CRETIME
            LDA        #$76
            STA        GBUF+HPENAB+4                    ; DUMMY PASSWORD
            DEC        REQL                             ; TEST FOR ONE BLOCK DIRECTORY
            BEQ        DIRCREND                         ; IT IS, FINISH UP_
            JSR        DIRWRT                           ; GO WRITE FIRST DIRECTORY BLOCK AND ALLOCATE NEXT
            BCS        DERROR                           ; PASS BACK ERROR_
            JSR        ZERGBUF                          ; CLEAN OUT GENERAL BUFFER AGAIN_
CRNXTDIR:   LDA        TLINK                            ; MOVE LAST BLOCK ADDRESS
            STA        GBUF                             ; AS BACKWARD LINK_
            LDA        TLINK+1
            STA        GBUF+1
            LDA        FLINK                            ; MAKE FORWARD LINK INTO CURRENT ADDRESS
            STA        TLINK
            LDA        FLINK+1
            STA        TLINK+1
            DEC        REQL                             ; IS THIS THE LAST BLOCK?
            BEQ        DIRCREND
            JSR        DIRWRT                           ; WRITE THIS BLOCK AND ALLOCATE NEXT_
            BCS        DERROR
            LDA        #0                               ; ZERO OUT FORWARD LINK
            STA        GBUF+2
            STA        GBUF+3
            BEQ        CRNXTDIR                         ; BRANCH ALWAYS
;
DIRCREND:   JSR        DIRWRT1                          ; WRITE LAST BLOCK OF THIS DIRECTORY
            BCS        DERROR
            JMP        ENDCRE0                          ; FINISH UP WRITING OWNER DIRECTORY STUFF_
;
DIRWRT:     JSR        ALC1BLK                          ; GET ADDRESS OF NEXT BLOCK_
            BCS        DERROR
            STA        GBUF+2
            STY        GBUF+3                           ; SAVE LINK ADDRESS
            STA        FLINK
            STY        FLINK+1
DIRWRT1:    LDA        TLINK                            ; GET ADDRESS OF CURRENT BLOCK
            STA        BLOKNML
            LDA        TLINK+1
            STA        BLOKNMH
            JMP        WRTGBUF                          ; GO WRITE IT OUT
;PAGE
;
ERRGBUF     =          *
DERROR:     RTS
;
;
SOSTMPL:    .BYTE      $0                               ; THE FOLLOWING TWO BYTES ARE THE 'SOS STAMP'
SOSTMPH:    .BYTE      $0
;
SOSVER:     .BYTE      0,0,0,$27,13
;
;
RNDTAB      =          *
ENTCALC:    LDA        #GBUF/256                        ; SET HIGH ADDRESS OF DIRECTORY ENTRY INDEX POINTER
            STA        DRBUFPH
            LDA        #4                               ; CALCULATE ADDRESS OF ENTRY BASED
            LDX        D_ENTNUM                         ; ON THE ENTRY NUMBER
ECALC0:     CLC
ECALC1:     DEX                                         ; ADDR=GBUF+((ENTNUM-1)*ENTLEN)
            BEQ        ECALC2
            ADC        H_ENTLN
            BCC        ECALC1
            INC        DRBUFPH                          ; BUMP HI ADDRESS
            BCS        ECALC0                           ; BRANCH ALWAYS_
;
ECALC2:     STA        DRBUFPL                          ; SAVE NEWLY CALCULATED LOW ADDRESS
            RTS
;PAGE
DERROR2:    RTS
;
DREVISE:    LDA        DATELO                           ; IF NO CLOCK,
            BEQ        DREVISE1                         ; THEN DON'T TOUCH MOD T/D
            LDX        #3                               ; MOVE LAST MODIFICATION DATE/TIME TO ENTRY BEING UPDATED_
MODTIME:    LDA        DATELO,X
            STA        A:DFIL+D_MODDT,X
            DEX
            BPL        MODTIME
;
DREVISE1:   LDA        A:DFIL+D_ATTR                    ; MARK ENTRY AS BACKUPABLE
            ORA        BKBITFLG                         ; BIT 5 = BACKUP NEEDED BIT
            STA        A:DFIL+D_ATTR
            LDA        D_DEV                            ; GET DEVICE NUMBER OF DIRECTORY
            STA        DEVNUM                           ; TO BE REVISED_
            LDA        D_ENTBLK                         ; AND ADDRESS OF DIRECTORY BLOCK
            STA        BLOKNML                          ; THAT CONTAINS THE ENTRY_
            LDA        D_ENTBLK+1
            STA        BLOKNMH
            JSR        RDGBUF                           ; READ BLOCK INTO GENERAL PURPOSE BUFFER_
            BCS        ERRGBUF
            JSR        ENTCALC                          ; FIX UP POINTER TO ENTRY LOCATION WITHIN GBUF_
            LDY        H_ENTLN                          ; NOW MOVE 'D_' STUFF TO DIRECTORY_
            DEY
MVDENT:     LDA        A:DFIL+D_STOR,Y
            STA        (DRBUFPL),Y
            DEY
            BPL        MVDENT
            LDA        D_HEAD                           ; IS THE ENTRY BLOCK THE SAME AS THE
            CMP        BLOKNML                          ; ENTRY'S HEADER BLOCK?
            BNE        SVENTDIR                         ; NO, SAVE ENTRY BLOCK
            LDA        D_HEAD+1                         ; MAYBE, TEST HIGH ADDRESSES
            CMP        BLOKNMH
            BEQ        UPHEAD                           ; BRANCH IF THEY ARE THE SAME BLOCK_
SVENTDIR:   JSR        WRTGBUF                          ; WRITE UPDATED DIRECTORY BLOCK
            BCS        DERROR2                          ; RETURN ANY ERROR_
            LDA        D_HEAD                           ; GET ADDRESS OF HEADER BLOCK
            STA        BLOKNML
            LDA        D_HEAD+1
            STA        BLOKNMH
            JSR        RDGBUF                           ; READ IN HEADER BLOCK FOR MODIFICATION
            BCS        DERROR2
UPHEAD:     LDY        #1                               ; UPDATE CURRENT NUMBER OF FILES IN THIS DIRECTORY
UPHED1:     LDA        H_FCNT,Y
            STA        GBUF+HCENT+4,Y                   ; (CURRENT ENTRY COUNT)
            DEY
            BPL        UPHED1
            LDA        H_ATTR                           ; ALSO UPDATE HEADER'S ATTRIBUTES_
            STA        GBUF+HATTR+4
            JSR        WRTGBUF                          ; GO WRITE UPDATED HEADER
DERROR1:    RTS                                         ; IMPLICITLY RETURN ANY ERRORS
;
;PAGE
;
NOTDIR:     LDA        #TYPERR                          ; NOT TREE OR DIRECTORY- NOT A RECOGNIZED TYPE!
TSTERR:     SEC
            RTS                                         ; DO NOTHING_
;
;
TSTSOS:     LDA        GBUF                             ; TEST SOS STAMP
            CMP        SOSTMPL
            BNE        TSTERR
            LDA        GBUF+1
            CMP        SOSTMPH
            BNE        TSTERR
            LDA        GBUF+4                           ; TEST FOR HEADER
            AND        #$E0
            CMP        #HEDTYP*16
            BNE        TSTERR                           ; BRANCH IF NOT SOS HEADER (NO ERROR NUMBER)
            CLC                                         ; INDICATE NO ERROR
            RTS
;
;PAGE
;
;
FINDFILE:   JSR        LOOKFILE                         ; SEE IF FILE EXISTS
            BCS        NOFIND                           ; BRANCH IF AN ERROR WAS ENCOUNTERED
MOVENTRY:   LDY        H_ENTLN                          ; MOVE ENTIRE ENTRY INFO TO A SAFE AREA
MOVENT1:    LDA        (DRBUFPL),Y
            STA        A:DFIL+D_STOR,Y
            DEY
            BPL        MOVENT1
            LDA        #0                               ; TO INDICATE ALL IS WELL
NOFIND:     RTS                                         ; RETURN CONDITION CODES_
;PAGE
;
;
LOOKFILE:   JSR        PREPROOT                         ; FIND VOLUME AND SET UP OTHER BORING STUFF
            BCS        FNDERR                           ; PASS BACK ANY ERROR ENCOUNTERED
            LDY        #0                               ; TEST TO SEE IF ONLY ROOT WAS SPECIFIED_
            LDA        (PATHNML),Y
            BNE        LOOKFIL0                         ; BRANCH IF MORE THAN ROOT_
            LDA        #GBUF/256                        ;  OTHERWISE, REPORT A BADPATH ERROR
            STA        DRBUFPH                          ; (BUT FIRST CREATE A PHANTOM ENTRY FOR OPEN)
            LDA        #4
            STA        DRBUFPL
            LDY        #D_AUXID                         ; FIRST MOVE IN ID, AND DATE STUFF_
PHANTM1:    LDA        (DRBUFPL),Y
            STA        A:DFIL,Y
            DEY
            CPY        #D_CREDT-1
            BNE        PHANTM1
PHANTM2:    LDA        ROOTSTUF-D_FILID,Y
            STA        A:DFIL,Y
            DEY
            CPY        #D_FILID-1
            BNE        PHANTM2
            LDA        #DIRTYP*$10                      ; FAKE DIRECTORY FILE
            STA        A:DFIL+D_STOR
            LDA        #BADPATH                         ; (CARRY IS SET)
            RTS
;
ROOTSTUF:   .BYTE      0,2,0,4
            .BYTE      0,0,8,0
;
LOOKFIL0:   LDA        #0                               ; RESET FREE ENTRY INDICATOR
            STA        NOFREE
            SEC                                         ; INDICATE THAT THE DIRECTORY TO BE SEARCHED HAS HEADER IN THIS BLOCK
LOOKFIL1:   LDA        #0                               ; RESET ENTRY COUNTER
            STA        TOTENT
            JSR        LOOKNAM                          ; LOOK FOR NAME POINTED TO BY 'PATHNML'
            BCC        NAMFOJMP                         ; BRANCH IF NAME WAS FOUND_
            LDA        ENTCNTL                          ; HAVE WE LOOKED AT ALL OF THE
            SBC        TOTENT                           ;  ENTRIES IN THIS DIRECTORY?
            BCC        DCRENTH                          ; MAYBE, CHECK HI COUNT_
            BNE        LOOKFIL2                         ; NO, READ NEXT DIRECTORY BLOCK
            CMP        ENTCNTH                          ; HAS THE LAST ENTRY BEEN LOOKED AT (ACC=0)
            BEQ        ERRFNF                           ; YES, GIVE 'FILE NOT FOUND' ERROR_
            BNE        LOOKFIL2                         ; BRANCH ALWAYS_
DCRENTH:    DEC        ENTCNTH                          ; SHOULD BE AT LEAST 1
            BPL        LOOKFIL2                         ; (THIS SHOULD BE BRANCH ALWAYS___)
ERRDIR:     LDA        #DIRERR                          ; REPORT DIRECTORY MESSED UP_
FNDERR:     SEC                                         ; INDICATE ERROR HAS BEEN ENCOUNTERED_
            RTS
NAMFOJMP:   JMP        NAMFOUND                         ; AVOID BRANCH OUT OF RANGE
;
;PAGE
LOOKFIL2:   STA        ENTCNTL                          ; KEEP RUNNING COUNT
            LDA        #GBUF/256                        ; RESET INDIRECT POINTER
            STA        DRBUFPH
            LDA        GBUF+2                           ; GET LINK TO NEXT DIRECTORY BLOCK
            BNE        NXTDIR0                          ; (IF THERE IS ONE)
            CMP        GBUF+3                           ; ARE BOTH ZERO, I_E_ NO LINK?
            BEQ        ERRDIR                           ; IF SO, THEN NOT ALL ENTRIES WERE ACCOUNTED FOR_
NXTDIR0:    STA        BLOKNML
            LDA        GBUF+3
            STA        BLOKNMH
            JSR        RDGBUF                           ; GO READ THE NEXT LINKED DIRECTORY IN_
            BCC        LOOKFIL1                         ; BRANCH IF NO ERROR_
            RTS                                         ; RETURN ERROR (IN ACCUMULATOR)_
TELFREEX:   JMP        TELFREE
;
FNF0X:      JMP        FNF0                             ; AVOID BRANCH OUT OF RANGE
;
CFLAG:      .RES       1                                ; AM I CREATING?
TTSAVE:     .RES       2                                ; CURRENT BLOCK ADDR
BLOKSAVE:   .RES       2                                ; PARENT DIR ADDR
;
ERRFNF:     LDA        NOFREE                           ; WAS ANY FREE ENTRY FOUND?
            BNE        FNF0X
            LDA        GBUF+2                           ; TEST LINK
            BNE        TELFREEX
            CMP        GBUF+3                           ; IF BOTH ARE ZERO, THEN GIVE UP
            BNE        TELFREEX                         ; BRANCH IF NOT LAST DIR BLOCK
            LDA        CFLAG                            ; DOING A CREATE?
            BEQ        FNF0X                            ; NO, SIMPLY REPORT NOT FOUND
;
; EXTEND THE DIRECTORY BY A BLOCK
;
            LDA        BLOKSAVE                         ; BUT NOT
            ORA        BLOKSAVE+1                       ;    IF A ROOT DIRECTORY!
            BEQ        FNF0X                            ;        FORU BLOCKS HARD CODED
            LDA        TTLINK                           ; FETCH CURRENT DIRECTORY
            STA        TLINK                            ; ADDR (GBUF)
            LDA        TTLINK+1                         ; AND ALLLOCATE A NEW
            STA        TLINK+1                          ; BY LINKING TO CURRENT
            JSR        DIRWRT
            BCS        FNF0                             ; RATS! NO SPACE SAY "DIRFULL"
;
; SAVE CURRENT BLOCK ADDR
;
            LDA        TTLINK
            STA        TTSAVE
            LDA        TTLINK+1
            STA        TTSAVE+1
;
; FETCH DESCENDENT
;
            LDA        GBUF+2
            STA        BLOKNML
            LDA        GBUF+3
            STA        BLOKNMH
            JSR        ZERGBUF                          ; INIT THE NEW DIR BLOCK
;
; AND INSERT BACK POINTER
; TO "CURRENT BLOCK"
;
            LDA        TTSAVE
            STA        GBUF
            LDA        TTSAVE+1
            STA        GBUF+1
            JSR        WRTGBUF
            BCS        ERTS
;
; UPDATE DIR'S HEADER IN PARENT
;
            LDA        BLOKSAVE
            STA        BLOKNML                          ; PREPARE TO READ PARENT
            LDX        BLOKSAVE+1
            STX        BLOKNMH
            JSR        RDGBUF                           ; FETCH PARENT
            LDY        #D_USAGE                         ; BUMP BLOCKS USED BY HEADER
            LDA        (DEBUPTR),Y
            SEC
            ADC        #0                               ; BY JUST ONE BLOCK
            STA        (DEBUPTR),Y
            INY
            LDA        (DEBUPTR),Y                      ; TWO BYTE BLOCKS USED
            ADC        #0
            STA        (DEBUPTR),Y
            LDY        #D_EOF+1                         ; INCREASE EOF BY $200
            LDA        (DEBUPTR),Y
            CLC
            ADC        #2
            STA        (DEBUPTR),Y
            INY
            LDA        (DEBUPTR),Y
            ADC        #0
            STA        (DEBUPTR),Y
            JSR        WRTGBUF                          ; REWRITE PARENT DIR BLOCK
            LDA        TTSAVE+1                         ; REFETCH CURRENT DIR BLOCK
            STA        BLOKNMH
            LDA        TTSAVE
            STA        BLOKNML
            JSR        RDGBUF                           ; BACK FROM THE SHADOWS AGAIN
            JMP        ERRFNF                           ; VOILA! WE HAVE EXTENDED THE DIRECTORY!
;
TELFREE:    STA        D_ENTBLK
            LDA        GBUF+3
            STA        D_ENTBLK+1                       ; ASSUME FIRST ENTRY OF NEXT BLOCK
            LDA        #1                               ;  IS FREE FOR USE_
            STA        D_ENTNUM
            STA        NOFREE                           ;  MARK D_ENTNUM AS VALID (FOR CREATE)
FNF0:       LDY        #0                               ; TEST FOR 'FILE NOT FOUND' VERSUS 'PATH NOT FOUND'
            LDA        (PATHNML),Y
            TAY
            INY
            LDA        (PATHNML),Y                      ; IF NON-ZERO THEN 'PATH NOT FOUND'
ERRPATH1:   SEC                                         ; IN EITHER CASE, INDICATE ERROR_
            BEQ        FNF1
            LDA        #PATHNOTFND                      ; REPORT NO SUCH PATH_
ERTS:       RTS
FNF1:       LDA        #FNFERR                          ; REPORT FILE NOT FOUND_
            RTS
;PAGE
;
NAMFOUND:   LDA        (PATHNML),Y                      ; (Y=0)
            SEC
            ADC        PATHNML                          ; TEST FOR LAST NAME IN PATH
            TAY                                         ; IF ZERO, THEN THAT WAS LAST NAME
            CLC                                         ; TO INDICATE SUCCESS
            LDA        PATHBUF,Y
            BEQ        FILFOUND
;NOW CHANGE THE PATHNAME POINTER TO POINT AT THE NEXT NAME IN THE PATH
            STY        PATHNML
            LDA        DRBUFPL                          ; SAVE PARENTS
            STA        DEBUPTR                          ; ENTRY POINTER
            LDA        DRBUFPH
            STA        DEBUPTR+1                        ; IN CASE ENTRY ON PAGE 2
            LDA        BLOKNML                          ; ADDRESS (DIR EXTEND)
            STA        BLOKSAVE
            LDA        BLOKNMH
            STA        BLOKSAVE+1
            LDY        #D_STOR                          ; BE SURE THIS IS A DIRECTORY ENTRY
            LDA        (DRBUFPL),Y                      ; HIGH NIBBLE WILL TELL
            AND        #$F0
            CMP        #DIRTYP*16                       ; IS IT A SUB-DIRECTORY?
            BNE        ERRPATH1                         ; REPORT THE USER'S MISTAKE
            LDY        #D_FRST                          ; GET ADDRESS OF FIRST SUB-DIRECTORY BLOCK
            LDA        (DRBUFPL),Y
            STA        BLOKNML                          ; (NO CHECKING IS DONE HERE FOR A VALID
            INY                                         ;  BLOCK NUMBER___ )
            STA        D_HEAD                           ; SAVE AS FILE'S HEADER BLOCK TOO_
            LDA        (DRBUFPL),Y
            STA        BLOKNMH
            STA        D_HEAD+1
            JSR        RDGBUF                           ; READ SUB-DIRECTORY INTO GBUF
            BCS        FNDERR1                          ; RETURN IMMEDIATELY ANY ERROR ENCOUNTERED_
            LDA        GBUF+HCENT+4                     ; GET THE NUMBER OF FILES
            STA        ENTCNTL                          ;  CONTAINED IN THIS DIRECTORY
            LDA        GBUF+HCENT+5
            STA        ENTCNTH
            LDA        GBUF+HCMP+4                      ; TEST BACKWARD COMPATIBILITY
            BEQ        MOVHEAD
ERRCOMP:    LDA        #CPTERR                          ; TELL THEM THIS DIRECTORY IS NOT COMPATABLE
NONAME      =          *
FNDERR1:    SEC
            RTS
MOVHEAD:    JSR        MOVHED0                          ; MOVE INFO ABOUT THIS DIRECTORY
            JMP        LOOKFIL0                         ; DO NEXT LOCAL PATHNAME
;
MOVHED0:    LDX        #$A                              ; MOVE INFO ABOUT THIS DIRECTORY
MOVHED1:    LDA        GBUF+HCRDT+4,X
            STA        H_CREDT,X
            DEX
            BPL        MOVHED1
            RTS
;
;PAGE
;
;
FILFOUND    =          *
ENTADR:     LDA        H_MAXENT                         ; FIGURE OUT WHICH IS ENTRY NUMBER THIS IS_
            SEC
            SBC        CNTENT                           ; MAX ENTRIES - COUNT ENTRIES + 1 = ENTRY NUMBER
            ADC        #0                               ; (CARRY IS/WAS SET)
            STA        D_ENTNUM
            LDA        BLOKNML
            STA        D_ENTBLK
            LDA        BLOKNMH                          ; AND INDICATE BLOCK NUMBER OF THIS DIRECTORY_
            STA        D_ENTBLK+1
            CLC
            RTS
;
LOOKNAM:    LDA        H_MAXENT                         ; RESET COUNT OF FILES PER BLOCK
            STA        CNTENT
            LDA        #GBUF/256
            STA        DRBUFPH
            LDA        #4
LOKNAM1:    STA        DRBUFPL                          ; RESET INDIRECT POINTER TO GBUF
            BCS        LOKNAM2                          ; BRANCH IF THIS BLOCK CONTAINS A HEADER
            LDY        #D_STOR
            LDA        (DRBUFPL),Y                      ; GET LENGTH OF NAME IN DIRECTORY
            BNE        ISNAME                           ; BRANCH IF THERE IS A NAME_
            LDA        NOFREE                           ; TEST TO SEE IF A FREE ENTRY HAS BEEN DECLARED_
            BNE        LOKNAM2                          ; YES BUMP TO NEXT ENTRY
            JSR        ENTADR                           ; SET ADDRESS FOR CURRENT ENTRY
            INC        NOFREE                           ; INDICATE A FREE SPOT HAS BEEN FOUND
            BNE        LOKNAM2                          ; BRANCH ALWAYS_
;
ISNAME:     AND        #$F                              ; STRIP TYPE (THIS IS CHECKED BY 'FILFOUND')
            INC        TOTENT                           ; (BUMP COUNT OF VALID FILES FOUND)
            CMP        (PATHNML),Y                      ; ARE BOTH NAMES OF THE SAME LENGTH?
            BNE        LOKNAM2                          ; NO, BUMP TO NEXT ENTRY
            TAY
CMPNAME:    LDA        (DRBUFPL),Y                      ; COMPARE NAMES LETTER BY LETTER
            CMP        (PATHNML),Y
            BNE        LOKNAM2
            DEY                                         ; HAVE ALL LETTERS BEEN COMPARED?
            BNE        CMPNAME                          ; NO, CONTINUE__
            CLC                                         ; BY GOLLY, WE GOT US A MATCH!
            RTS
;
LOKNAM2:    DEC        CNTENT                           ; HAVE WE CHECKED ALL POSSIBLE ENTRIES IN THIS BLOCK?
            BEQ        NONAME                           ; YES, GIVE UP_
            LDA        H_ENTLN                          ; ADD ENTRY LENGTH TO CURRENT POINTER
            CLC
            ADC        DRBUFPL
            BCC        LOKNAM1                          ; BRANCH IF WE'RE STILL IN THE FIRST PAGE_
            INC        DRBUFPH                          ; LOOK ON SECOND PAGE
            CLC                                         ; CARRY SHOULD ALWAYS BE CLEAR BEFORE LOOKING AT NEXT_
            BCC        LOKNAM1                          ; BRANCH ALWAYS___
;PAGE
;
;
PREPROOT:   JSR        FINDVOL                          ; FIND CORRECT VOLUME AND DEVICE NUMBER
            BCC        ROOT1                            ; BRANCH IF IT WAS FOUND_
ROOT0:      JSR        LOOKVOL                          ; OTHERWISE LOOK ON ALL DEVICES_
            BCS        SRITZ                            ; CAN'T FIND IT_
ROOT1:      LDA        #0                               ; ZERO OUT DIRECTORY TEMPS
            LDY        #42                              ; (DECIMAL)
CLRDSP:     STA        D_DEV,Y
            DEY
            BPL        CLRDSP
            LDY        #VCBDEV                          ; SET UP DEVICE NUMBER
            LDA        (VCBPTR),Y
            STA        DEVNUM
            STA        D_DEV                            ; FOR FUTURE REFERENCE
            INY
            LDA        (VCBPTR),Y                       ; GET CURRENT STATUS OF THIS VOLUME
            STA        V_STATUS
            LDY        #VCBROOT                         ; GET BLOCK ADDRESS OF ROOT DIRECTORY TOO_
            LDA        (VCBPTR),Y
            STA        BLOKNML
            STA        D_HEAD                           ; PRESERVE AS HEADER
            INY
            LDA        (VCBPTR),Y
            STA        BLOKNMH
            STA        D_HEAD+1
            JSR        RDGBUF                           ; GO READ IN ROOT
            BCC        ROOT2                            ; BRANCH IF NO ERROR
            PHA                                         ; SAVE ERROR CODE
            LDY        #VCBSTAT                         ; CHECK THIS BUGGER FOR AN OPEN FILE_
            LDA        (VCBPTR),Y
            ASL        A                                ; (SHIFT OPEN STATUS INTO CARRY)
            PLA                                         ; GET ERROR CODE AGAIN
            BCS        ROOTERR                          ;  BRANCH IF ERROR NEEDS TO BE REPORTED
            BNE        ROOT0                            ; OTHERWISE, LOOK ELSEWHERE (BRANCH ALWAYS)_
;
ROOT2:      JSR        CHKROOT                          ; VERIFY ROOT NAME
            BEQ        ROOT3                            ; BRANCH IF MATCHED_
            LDY        #VCBSTAT                         ; TEST FOR OPEN FILES ON THIS VOLUME BEFORE
            LDA        (VCBPTR),Y                       ;  LOOKING FOR IT ELSEWHERE_
            BPL        ROOT0
            JSR        USRREQ                           ;  REQUEST USER MOUNT VOLUME
            BCC        ROOT1                            ;  USER SAID S/HE DID-- CHECK IT
            LDA        #VNFERR                          ; REPORT VOLUME NOT FOUND ERR IF REFUSE TO INSERT
SRITZ:      RTS
;
;PAGE
ROOT3:      LDY        #$F                              ; (NOTE: X CONTAINS THE LENGTH OF THE ROOT NAME)
ROOTINFO:   LDA        GBUF+HCRDT+3,Y                   ; SAVE HEADER INFO_
            STA        V_STATUS,Y
            DEY
            BNE        ROOTINFO                         ; LOOP TIL ALL 15 BYTES MOVED
            LDA        H_FCNT
            STA        ENTCNTL
            LDA        H_FCNT+1
            STA        ENTCNTH
            TXA                                         ; NOW THAT ROOT IS IDENTIFIED, ADJUST
            SEC                                         ;  PATH NAME POINTER TO NEXT NAME IN THE PATH
            ADC        PATHNML
            STA        PATHNML
            CLC                                         ; INDICATE NO ERROR
ROOTERR:    RTS
;
;
CHKROOT:    LDY        #0                               ; GET LENGTH OF NAME
            LDA        (PATHNML),Y
            TAY
            TAX                                         ; SAVE IN X FOR LATTER ADJUSTMENT TO PATH POINTER
            EOR        GBUF+4
            AND        #$F                              ; DOES PATHNAME HAVE SAME LENGTH AS DIRECTORY NAME?
            BNE        NOTROOT                          ; BRANCH IF NOT
CKROOT1:    LDA        (PATHNML),Y                      ; COMPARE CHARACTER BY CHARACTER
            CMP        GBUF+4,Y
            BNE        NOTROOT
            DEY
            BNE        CKROOT1                          ; LOOP UNTIL ALL CHARACTERS MATCH
NOTROOT:    RTS
;
;PAGE
FINDVOL:    LDA        #VCB/256                         ; SEARCH VCB FOR VOLUME NAME
            STA        VCBPTR+1
            LDA        #0
            STA        D_DEV
            STA        VCBPTR
FNDVOL1:    PHA                                         ; SAVE LAST SEARCH POSITION
            TAX
            LDY        #0                               ; (INDEX TO PATHNAME POINTER)
            LDA        VCB,X                            ; GET LENGTH OF VOLUME NAME TO COMPARE
            BEQ        NXTVCB                           ; BRANCH IF VCB ENTRY IS EMPTY
            CMP        (PATHNML),Y                      ; ARE NAMES OF SAME LENGTH?
            BNE        NXTVCB                           ; NO, INDEX NEXT VCB
            CLC                                         ; SCAN NAME BACKWARDS
            TAY
            TXA
            ADC        VCB,X
            TAX                                         ; NOW BOTH INDEXES POINT TO LAST CHARACTER OF THE NAMES TO COMPARE
VOLNAM:     LDA        (PATHNML),Y
            CMP        VCB,X
            BNE        NXTVCB
            DEX
            DEY
            BNE        VOLNAM                           ; CHECK ALL CHARACTERS
            PLA                                         ; SINCE A MATCH IS FOUND
            STA        VCBPTR                           ;  SET UP INDEX TO VCB ENTRY
            TAX
            LDA        VCB+VCBSWAP,X                    ;  BRANCH IF
            BEQ        FOUNDVOL                         ;  VOLUME NOT SWAPPED
            JSR        SWAPIN                           ;  IF USER REALLY WANTS IT, THEN BRING IN IF SWAPPED
            BCC        FOUNDVOL                         ;  BRANCH IF SUCCESS
            LDA        #XIOERROR                        ;  USER REFUSES TO MOUNT
            RTS
FOUNDVOL:   CLC                                         ; INDICATE VOLUME FOUND
            RTS
;
NXTVCB:     PLA                                         ; GET CURRENT INDEX AGAIN_
            CLC
            ADC        #VCBSIZE                         ; VCB ENTRY LENGTH_
            BCC        FNDVOL1                          ; BRANCH IF THER IS ANOTHER TO CHECK
            RTS                                         ; RETURN WITH CARRY SET TO SHOW FAILURE_
;PAGE
;
;
LOOKVOL:    LDX        #12                              ; (1) COUNT+(12)DEVICE LIST
LOOKVOL1:   LDA        BLKDLST,X                        ;  EXTRN
            STA        SCRTCH,X                         ;  MY CHANGEABLE COPY
            DEX
            BPL        LOOKVOL1                         ;  WORK BACKWARDS SO
            STA        TOTDEVS                          ;  ENTRY ZERO IS TOTAL DEVICES LISTED
            INX                                         ;  MAKE XREG = ZERO
LOKDEV1:    INX
            STX        SCRTCH
            LDA        SCRTCH,X
            CMP        D_DEV
            BEQ        NXTDEV                           ; DON'T LOOK AGAIN ON A DRIVE THAT HAS BEEN CHECKED
            STA        DEVNUM                           ; CHECK FOR DEVICE ALREADY LOGGED IN A VCB
            JSR        DEVVCB                           ; (CARRY CLEAR IF IT'S THERE)
            BCC        LOKVOL1
            LDA        #0                               ; FIND A FREE VCB TO LOG THIS GUY IN
ENTVCB:     TAX                                         ; INDEX TO NEXT VCB ENTRY
            LDA        VCB,X
            BEQ        FREEVCB                          ; FOUND A FREE SPOT_
            TXA                                         ; NOW INDEX TO NEXT, AND KEEP LOOKIN
            CLC
            ADC        #VCBSIZE                         ; (EACH VCB ENTRY IS 32 BYTES)
            BCC        ENTVCB                           ; BRANCH IF MORE TO FIND
            LDA        #0
ENTVCB2     =          *                                ; SEE IF WE CAN REPLACE A DEVICE
            TAX
            LDA        VCB+VCBSTAT,X                    ; VCB HAS FILES OPEN?
            BEQ        FREEVCB                          ; NO, USE IT!
            TXA
            CLC
            ADC        #VCBSIZE                         ; SEARCH NEXT VCB ENTRY
            BCC        ENTVCB2
            RTS                                         ; FAILED TO FIND A FREE VCB ENTRY
;
CHKVLOG:    LDY        #0                               ; MAKE SURE VOLUME WAS ACTUALLY LOGGED IN
            LDA        (VCBPTR),Y
            BNE        FOUNDVOL                         ; AH, MADE IT___
            LDA        #DUPVOL                          ; WELL, NOT QUITE, THIS VOLUME CAN'T BE LOGGED
            SEC
            RTS
;PAGE
;
FREEVCB:    STX        VCBPTR                           ; NOW THIS IS THE POINTER TO A FREE VCB
            LDA        #2                               ; ROOT DIRECTORIES ALWAYS AT BLOCK 2
            LDX        #0
            BEQ        GETROOT                          ; BRANCH ALWAYS
LOKVOL1:    LDY        #VCBSTAT                         ; MAKE SURE NO FILES ARE ACTIVE ON
            LDA        (VCBPTR),Y                       ;  THE VOLUME BEFORE LOGGING IT IN_
            BMI        SNSWIT                           ;  BRANCH IF FILES ACTIVE
            LDY        #VCBROOT+1                       ; GET ADDRESS OF ROOT DIRECTORY
            LDA        (VCBPTR),Y                       ; HIGH FIRST_
            TAX
            DEY                                         ; THEN LOW_
            LDA        (VCBPTR),Y
GETROOT:    JSR        GETROT0
            BCC        LOKVOL2                          ; BRANCH IF SUCCESSFULLY READ_
            LDA        #0                               ; OTHERWISE, TAKE THIS DEVICE OUT OF VCB
            TAY
            STA        (VCBPTR),Y                       ; (VOLUME 'OFF LINE')
            BEQ        NXTDEV                           ; BRANCH ALWAYS
;
LOKVOL2:    JSR        LOGVCB                           ; GO UPDATE VCB TO INCLUDE CURRENT VOLUME INFO
            BCS        NXTDEV                           ;  IF NOT A SOS DISKETTE, SKIP TO NEXT DEVICE
            JSR        CHKROOT                          ; GO COMPARE TO SEE IF WE FOUND WHAT WE'RE
            BEQ        CHKVLOG                          ;  LOOKING FOR___
;
NXTDEV:     LDX        SCRTCH                           ; LOOK AT OTHER DEVICES?
            CPX        TOTDEVS
            BCC        LOKDEV1                          ; YES_
            LDA        #VNFERR                          ; REPORT VOLUME NOT FOUND_
            RTS
;
SNSWIT      =          *                                ;  SENSE DSWITCH
            LDY        #VCBDEV
            LDA        (VCBPTR),Y
            STA        DEVNUM                           ;  MAKE SURE DEVICE NUMBER IS CURRENT
            JSR        TWRPROT1                         ;  USES DEVNUM
            LDA        DSWGLOB                          ;  DISK SWITCH GLOBAL
            BEQ        NXTDEV                           ;  BRANCH IF NO DISK SWITCH
            JSR        VERFYVOL                         ;  COMPARES VCBPTR VS_ DEVNUM CONTENTS
            BCC        NXTDEV                           ;  BRANCH IF DISK HAS NOT BEEN SWITCHED
            JSR        CHKROOT                          ;  COMPARES PATHNML VS_ GBUF
            BNE        NXTDEV                           ;  IGNORE IF NOT WHAT WE ARE LOOKING FOR
            LDX        #0                               ;  LOOK FOR FREE
            JSR        SNSWIT1
            BCS        NXTDEV                           ;  ANY ERRORS LOGGING IN THE NEW VOLUME
            JMP        CHKVLOG                          ;  MAKE SURE THE NEW VOLUME IS LOGGED
SNSWIT1:    LDA        VCB,X                            ;  VCB ENTRY
            BEQ        SNSWIT2                          ;  BRANCH IF FOUND
            TXA
            CLC
            ADC        #VCBSIZE                         ;  LOOK AT NEXT VCB AREA
            TAX
            BCC        SNSWIT1
            RTS                                         ;  CAN'T BE LOGGED IN!
SNSWIT2:    LDA        #0
            STA        DUPLFLAG                         ;  TURN OFF DUPLICATE VOLUME FLAG
            STX        VCBPTR
            JSR        LOGVCB1                          ;  PARTIALLY LOG IN THE NEW VOLUME
            BCS        NONSOS                           ;  CS MEANS NONSOS ERROR
            LDA        DUPLFLAG                         ;  WAS IT A DUPLICATE VOLUME?
            BNE        SNSWIT6                          ;  BRANCH IF YES
            LDY        #VCBSWAP                         ;  BY MAKING SWAP BYTE NON ZERO
            LDA        #1
            STA        (VCBPTR),Y                       ;  SO SWAPOUT WON'T AFFECT
            LDA        DEVNUM                           ;  A REG PASSES DEVNUM TO SWAPOUT
            JSR        SWAPOUT                          ;  OLD ACTIVE MOUNT MUST BE SWAPPED
            BCC        SNSWIT3
            LDA        #XIOERROR                        ;  USER REFUSED TO REPLACE OLD VOLUME
            RTS
SNSWIT3:    LDY        #VCBSWAP                         ;  NOW LOG IN THE NEW ALL THE WAY
            LDA        #0
            STA        (VCBPTR),Y
SNSWIT4:    JSR        VERFYVOL                         ;  DON'T BOTHER TO ASK IF NEW VOLUME IS ALREADY MOUNTED
            BCC        SNSWIT5                          ;  BRANCH IF NEW VOLUME ON LINE
            JSR        USRREQ                           ;  ASK USER TO REMOUNT NEW VOLUME
            BCC        SNSWIT4                          ;  USER SAYS THEY DID: CHECK IT OUT
            LDA        #VNFERR
SNSWIT5:    RTS
SNSWIT6:    LDA        #DUPVOL
            SEC
            RTS
;PAGE
;
NONSOS:     LDA        #NOTSOS                          ; TELL EM IT'S NOT A SOS DISK (COULD BE PASCAL)
            RTS                                         ;  CARRY SHOULD ALREADY BE SET
;
;
DEVVCB:     LDA        #0                               ; SCAN VCB FOR DEVICE SPECIFIED IN 'DEVNUM'
DVCB1:      TAX                                         ; FIRST TEST FOR VALID VCB_
            LDA        VCB,X
            BEQ        DVCB2
            LDA        VCB+VCBSWAP,X                    ;  SWAPPED VOLUMES DON'T COUNT
            BNE        DVCB2                            ;  AS LOGGED IN
            LDA        VCB+VCBDEV,X                     ; GET DEVICE NUMBER
            CMP        DEVNUM                           ; TEST AGAINST REQUESTED DEVICE
            BEQ        FOUNDEV                          ; YES, SET UP A POINTER TO IT
DVCB2:      TXA                                         ; BUMP TO NEXT VCB
            CLC
            ADC        #VCBSIZE
            BCC        DVCB1                            ; BRANCH IF MORE TO LOOK AT_
            RTS                                         ; RETURN CARRY SET TO INDICATE NOT FOUND
;
TSTDUPVOL:  LDX        VCBPTR                           ; PRESERVE CURRENT ADDR OF FREE VCB
            LDA        #0                               ; LOOK FOR A CURRENTLY LOGGED ON VOLUME OF THE SAME NAME_
TSDUPV1:    STA        VCBPTR
            JSR        CMPVCB
            BCS        TSDUPV2                          ; BRANCH IF NO MATCH_
            LDY        #VCBSTAT                         ; TEST FOR ANY OPEN FILES_
            LDA        (VCBPTR),Y
            BMI        FOUNDDUP                         ; TELL THE SUCKER HE CAN'T LOOK AT THIS VOLUME!
            LDA        #0                               ; TAKE DUPLICATE OFF LINE IF NO OPEN FILES_
            TAY
            STA        (VCBPTR),Y
            BEQ        NODUPVOL                         ; RETURN THAT ALL IS OK TO LOG IN NEW_
TSDUPV2:    LDA        VCBPTR
            CLC
            ADC        #VCBSIZE                         ; BUMP TO NEXT ENTRY_
            BCC        TSDUPV1
NODUPVOL    =          *
FOUNDEV:    CLC
FNDDUP1:    STX        VCBPTR
            RTS
;
FOUNDDUP:   STA        DUPLFLAG                         ; A DUPLICATE HAS BEEN DETECTED_
            SEC                                         ; INDICATE ERROR
            LDA        VCBPTR                           ;  SAVE ADDRESS OF DUPLICATE
            STA        VCBENTRY
            BCS        FNDDUP1                          ; BRANCH ALWAYS TAKEN
;PAGE
;
;
LOGVCB:     LDY        #VCBNML                          ; IS THIS A PREVIOUSLY LOGGED IN VOLUME
            LDA        (VCBPTR),Y                       ; (ACC=0?)
            BEQ        LOGVCB1                          ; NO, GO AHEAD AND PREPARE VCB_
            JSR        CMPVCB                           ; DOES VCB MATCH VOLUME READ?
            BCC        VCBLOGD                          ; YES, DON'T DISTURB IT_
LOGVCB1:    LDA        #0                               ; ZERO OUT VCB ENTRY
            LDY        #VCBSIZE-1
ZERVCB:     STA        (VCBPTR),Y
            DEY
            BPL        ZERVCB
            JSR        TSTSOS                           ; MAKE SURE IT'S A SOS DISKETTE_
            BCS        VCBLOGD                          ; IF NOT, RETURN CARRY SET_
            JSR        TSTDUPVOL                        ; FIND OUT IF A DUPLICATE WITH OPEN FILES ALREADY EXISTS
            BCS        NOTLOG0
            LDA        GBUF+4                           ; MOVE VOLUME NAME TO VCB
            AND        #$F                              ; STRIP ROOT MARKER
            TAY
            PHA
MOVOLNM:    LDA        GBUF+4,Y
            STA        (VCBPTR),Y
            DEY
            BNE        MOVOLNM
            PLA                                         ; GET LENGTH AGAIN
            STA        (VCBPTR),Y                       ; SAVE THAT TOO_
            LDY        #VCBDEV                          ; SAVE DEVICE NUMBER ALSO_
            LDA        DEVNUM
            STA        (VCBPTR),Y
            JSR        CLEARBMS                         ;  MARKS THIS DEVICES OLD BITMAPS AS INVALID (A REG PASSED)
            LDA        GBUF+VTBLK+4                     ; AND TOTOL NUMBER OF BLOCKS ON THIS UNIT,
            LDY        #VCBTBLK
            STA        (VCBPTR),Y
            LDA        GBUF+VTBLK+5
            INY
            STA        (VCBPTR),Y
            LDY        #VCBROOT
            LDA        BLOKNML                          ; AND ADDRESS OF ROOT DIRECTORY
            STA        (VCBPTR),Y
            INY
            LDA        BLOKNMH
            STA        (VCBPTR),Y
            LDY        #VCBDMAP
            LDA        GBUF+VBMAP+4                     ; AND LASTLY, THE ADDRESS
            STA        (VCBPTR),Y                       ;  OF THE FIRST BITMAP
            LDA        GBUF+VBMAP+5
            INY
            STA        (VCBPTR),Y
            CLC                                         ; INDICATE THAT IT WAS LOGGED IF POSIBLE_
VCBLOGD:    RTS
NOTLOG0:    JMP        NOTLOG1
;PAGE
CMPVCB:     LDA        GBUF+4                           ; COMPARE VOLUME NAME IN VCB
            AND        #$F
            LDY        #VCBNML                          ;  WITH NAME IN DIRECTORY
            CMP        (VCBPTR),Y                       ; ARE THEY SAME LENGTH
            BNE        NOTSAME
            TAY
VCBCMP1:    LDA        GBUF+4,Y
            CMP        (VCBPTR),Y
            BNE        NOTSAME
            DEY
            BNE        VCBCMP1
            CLC                                         ; INDICATE MATCH_
            RTS
;
VERFYVOL:   LDX        #0                               ; READ IN ROOT DIRECTORY HEADER_
            LDA        #2
            JSR        GETROT0
            BCS        NOVRFY1                          ; PASS BACK WHATEVER OTHER ERROR OCCURS_
            JSR        CMPVCB                           ; TEST ROOT WITH VOLUME NAME IN VCB_
            BCC        NOVRFY                           ;  BRANCH IF ROOT MATCHES VCB
            LDA        #0                               ;  OTHERWISE, PASS BACK FOREIGN VOLUME ERROR (SOS OR UCSD)
NOVRFY:     RTS                                         ; RETURN RESULTS IN CARRY_
NOVRFY1:    LDA        #VNFERR                          ;  NOTHING IN DRIVE
            RTS
;
GETROT0:    STA        BLOKNML
            STX        BLOKNMH                          ; STORE ADDRESS AND READ IN ROOT
            JSR        RDGBUF
            BCC        RETROT2                          ; BRANCH IF SUCCESSFULLY READ_
NOTSAME     =          *
            SEC                                         ; INDICATE ERROR
RETROT2:    RTS
;
NOTLOG1:    LDX        VCBPTR                           ;  LOAD THE VCB ADDRESS
            LDA        VCBENTRY                         ;  OF THE DUPLICATE VOLUME
            STA        VCBPTR
            STX        VCBENTRY                         ;  AND SAVE THE FREE VCB SPACE ADDR
            LDY        #VCBDEV                          ;  IS DUPLICATE ON SAME DEVICE?
            LDA        DEVNUM
            CMP        (VCBPTR),Y
            BNE        NOTLOG2                          ;  BRANCH IF NOT
            JSR        SWAPIN                           ;  SWAP IN IF NECESSARY
            LDA        #0
            STA        DUPLFLAG                         ;  NO MORE DUPLICATE VOLUME STATUS
            LDA        VCBPTR                           ;  MAKE CHKROOT WORK IN A MOMENT
            STA        PATHNML                          ;  THIS IS INCREDIBLY GROSS
;:  BUT IS A RESULT OF MAKING VOLUME A SPECIAL
;:  CASE OF SEARCHING ALL DEVICES FOR
;:  A KNOWN VOLUME
            CLC
            RTS
NOTLOG2:    LDA        VCBENTRY                         ;  REACH HERE IF REAL DUPLICATE VOLUME
            STA        VCBPTR                           ;  RESOTRE FREE VCB PTR
            CLC
            RTS                                         ;  DUPLICATE VOLUME PRETENDS TO BE NO ERROR
;PAGE
;
TSFRBLK:    LDY        #VCBTFRE+1
            LDA        (VCBPTR),Y                       ; FIND OUT IF ENOUGH FREE BLOCKS
            DEY                                         ; ARE AVAILABLE TO ACCOMODATE REQEST_
            ORA        (VCBPTR),Y                       ; BUT FIRST FIND OUT IF WE GOT A PROPER COUNT FOR THIS VOLUME_
            BNE        CMPFREB                          ; BRANCH IF COUNT IS NON-ZERO
            DEY                                         ; IF ZERO, THEN COUNT MUST BE TAKEN
            LDA        (VCBPTR),Y                       ;  GET HIGH TOTAL BLKS
            TAX                                         ;  SAVE IT
            DEY                                         ;  GET LOW
            LDA        (VCBPTR),Y                       ;  TOTAL BLKS
            BNE        TSFR01
            DEX                                         ;  ADJUST FOR BITMAP BLOCK BOUNDARY
TSFR01:     TXA
            LSR        A                                ; DIVIDE BY 16_ THE RESULT IS THE NUMBER
            LSR        A                                ;  OF BIT MAPS TO BE SEARCHED_
            LSR        A
            LSR        A
            STA        BMCNT                            ; SAVE IT_
            LDA        #0                               ; START COUNT AT ZERO_
            STA        SCRTCH
            STA        SCRTCH+1
            LDA        #$FF                             ; MARK 'FIRST FREE' TEMP AS UNKNOWN
            STA        NOFREE
            LDY        #VCBDEV                          ; MAKE SURE BIT MAP IS UP TO DATE
            LDA        (VCBPTR),Y                       ; GET DEVICE NUMBER
            TAX                                         ; PASS TO 'UPBMAP' IN X
            JSR        UPBMAP                           ; (NOTHING HAPPENS IF IT DON'T HAFTA_)
            BCS        TFBERR                           ; BRANCH IF WE GOT TROUBLE,
            LDY        #VCBDMAP                         ; GET ADDRESS OF FIRST BIT MAP_
            LDA        (VCBPTR),Y
            STA        BLOKNML
            INY                                         ; (FOR HIGH ADDRESS)
            LDA        (VCBPTR),Y
            STA        BLOKNMH
BMAPRD:     JSR        RDGBUF                           ; USE G(ENERAL)BUFF(ER) FOR TEMPORARY
            BCS        TFBERR                           ;  SPACE TO COUNT FREE BLOCKS (BITS)
            JSR        COUNT                            ; GO COUNT EM
            DEC        BMCNT                            ; WAS THAT THE LAST BIT MAP?
            BMI        CHGVCB                           ; IF SO, GO CHANGE FCB TO AVOID DOING THIS AGAIN!
            INC        BLOKNML                          ; NOTE: THE ORGANIZATION OF THE BIT MAPS
            BNE        BMAPRD                           ;  ARE CONTIGUOUS FOR SOS VERSION 0
            INC        BLOKNMH                          ;  IF SOME OTHER ORGANIZATION IS IMPLEMENTED, THIS CODE
            JMP        BMAPRD                           ;  MUST BE CHANGED!
;PAGE
;
CHGVCB:     LDY        #VCBCMAP                         ; MARK WHICH BLOCK HAD FIRST FREE SPACE
            LDA        NOFREE
            BMI        DSKFULL                          ; BRANCH IF NO FREE SPACE WAS FOUND_
            STA        (VCBPTR),Y
            LDY        #VCBTFRE+1                       ; UPDATE THE FREE COUNT_
            LDA        SCRTCH+1                         ; GET HIGH COUNT BYTE
            STA        (VCBPTR),Y                       ; UPDATE VOLUME CONTROL BLOCK_
            DEY
            LDA        SCRTCH
            STA        (VCBPTR),Y                       ; AND LOW BYTE TOO___
CMPFREB:    LDA        (VCBPTR),Y                       ; COMPARE TOTAL AVAILABLE
            SEC
            SBC        REQL                             ;  FREE BLOCKS ON THIS VOLUME_
            INY
            LDA        (VCBPTR),Y
            SBC        REQH
            BCC        DSKFULL
            CLC
            RTS
DSKFULL:    LDA        #OVRERR
            SEC
TFBERR:     RTS
;PAGE
;
COUNT:      LDY        #0                               ; BEGIN AT THE BEGINNING_
FRCONT:     LDA        GBUF,Y                           ; GET BIT PATTERN
            BEQ        FRCNT1                           ; DON'T BOTHER COUNTING NOTHIN'
            JSR        CNTFREE
FRCNT1:     LDA        GBUF+$100,Y                      ; DO BOTH PAGES WITH SAME LOOP
            BEQ        FRCNT2
            JSR        CNTFREE
FRCNT2:     INY
            BNE        FRCONT                           ; LOOP TILL ALL 512 BYTES COUNTED
            BIT        NOFREE                           ; HAS FIRST BLOCK WITH FREE SPACE BEEN FOUND YET?
            BPL        FRCNT3                           ; BRANCH IF IT HAS_
            LDA        SCRTCH                           ; TEST TO SEE IF ANY BLOCKS WERE COUNTED
            ORA        SCRTCH+1
            BEQ        FRCNT3                           ; BRANCH IF NONE COUNTED_
            LDY        #VCBTBLK+1
            LDA        (VCBPTR),Y                       ; SHOW THIS MAP IS FIRST WITH FREE SPACE
            SEC                                         ;  CORRECT FOR EXACT MULTIPLES OF $1000
            SBC        #$01
            LSR        A
            LSR        A
            LSR        A
            LSR        A
            SEC                                         ; SUBTRACT COUNTDOWN FROM TOTAL BIT MAPS
            SBC        BMCNT
            STA        NOFREE
FRCNT3:     RTS
;
CNTFREE:    ASL        A                                ; COUNT THE NUMBER OF BITS IN THIS BYTE_
            BCC        CFREE1
            INC        SCRTCH
            BNE        CFREE1
            INC        SCRTCH+1
CFREE1:     TAX
            BNE        CNTFREE                          ; LOOP UNTIL ALL BITS COUNTED_
            RTS
;
DEALLOC:    STX        BMCNT                            ; SAVE HIGH ORDER ADDRESS OF BLOCK TO BE FREED_
            PHA                                         ; SAVE IT
            LDX        VCBPTR                           ; WHILE THE BITMAP
            LDA        VCB+VCBTBLK+1,X                  ; DISK ADDRESS IS CHECKED
            CMP        BMCNT                            ; TO SEE IF IT MAKES SENSE
            PLA                                         ; RESTORE
            BCC        DEALERR1                         ; BRANCH IF IMPOSSIBLE
            TAX
            AND        #$7                              ; GET THE BIT TO BE OR-ED IN_
            TAY
            LDA        WHICHBIT,Y                       ; (SHIFTING TAKES 7 BYTES, BUT IS SLOWER)
            STA        NOFREE                           ; SAVE BIT PATTERN
            TXA                                         ; GET LOW BLOCK ADDRESS AGAIN_
            LSR        BMCNT
            ROR        A                                ; GET POINTER TO BYTE IN BITMAP THAT REPRESENTS
            LSR        BMCNT                            ; THE BLOCK ADDRESS_
            ROR        A
            LSR        BMCNT
            ROR        A
            STA        BMPTR                            ; SAVE POINTER_
            LSR        BMCNT                            ; NOW TRANSFER BIT WHICH SPECIFIES WHICH PAGE OF BITMAP_
            ROL        HALF
            LDX        BMTAB                            ; (THIS POINTS TO THE TABLE FOR THE BITMAP BUFFER USED)_
            LDA        BMACMAP,X                        ; WHAT IS THE CURRENT MAP
            CMP        BMCNT                            ; IS IN CORE BIT MAP THE ONE WE WANT?
            BEQ        DEALL1                           ; BRANCH IF IN-CORE IS CORRECT_
            JSR        BMAPUP                           ; PUT CURRENT MAP AWAY_
            BCS        DEALERR                          ; PASS BACK ANY ERROR_
            LDA        BMCNT                            ; GET DESIRED MAP NUMBER_
            LDY        #VCBCMAP
            STA        (VCBPTR),Y                       ; AND MAKE IT CURRENT_
            LDX        BMTAB
            LDA        BMADEV,X
            JSR        GTBMAP                           ; READ IT INTO THE BUFFER,
            BCS        DEALERR
DEALL1:     LDY        BMPTR                            ; INDEX TO BYTE_
            LSR        HALF
            BCC        DEALL2                           ; BRANCH IF ON PAGE ONE OF BITMAP_
            INC        BMADR+1
DEALL2:     LDA        NOFREE                           ; THE INDIVIDUAL BIT_
            ORA        (BMADR),Y
            STA        (BMADR),Y
            BCC        DEALL3                           ; BRANCH IF ADDRESS IS PROPER
            DEC        BMADR+1
DEALL3:     LDX        BMTAB                            ; MARK BITMAP AS MODIFIED_
            LDA        #$80
            ORA        BMASTAT,X
            STA        BMASTAT,X
            CLC
DEALERR:    RTS
DEALERR1:   LDA        #BITMAPADR                       ; BIT MAP BLOCK NUMBER IMPOSSIBLE
            SEC                                         ; SAY BIT MAP DISK ADDRESS WRONG (PROBABLY DATA MASQUERADING AS INDEX BLOCK)
            RTS
;
WHICHBIT:   .BYTE      $80,$40,$20,$10
            .BYTE      8,4,2,1
;
;
;PAGE
;
ALCIDXS:    LDA        #0                               ; ALLOCATION OF THE INDEXES ALWAYS FILLS IN
            STA        SAPTR                            ; STARTING AT THE BEGINNING OF THE BLOCK_
            JSR        ALC1BLK                          ; THIS GETS FIRST INDEX AND SETS UP A
            BCS        ERRALC1                          ; POINTER TO THE FREE BLOCKS (TO AVOID
ALIDX1:     LDY        SAPTR                            ; SCANNING THE WHOLE BLOCK EVERY TIME)_
            STA        (TINDX),Y                        ; SAVE INDEX BLOCK ADDRESS (LOW)
            INC        TINDX+1
            LDA        SCRTCH+1                         ; GET HIGH BYTE OF ADDRESS
            STA        (TINDX),Y                        ; (AND SAVE IT)
            DEC        TINDX+1
            DEC        REQL                             ; HAS REQUEST BEEN SATIFIED?
            BEQ        ALDXEND                          ; (CARRY IS CLEAR)
            INC        SAPTR                            ; BUMP INDEX POINTER
            LDY        BMPTR                            ; GET INDEX POINTER TO LAST ACCESSED BIT GROUP
            LDA        HALF                             ; WHICH HALF OF MAP? (BOTH BMPTR & HALF SET UP BY 'ALC1BLK')
            BNE        SECNDHAF
            JSR        GETBITS1                         ; GET NEXT FREE BLOCK ADDRESS_
            BCC        ALIDX1                           ; BRANCH IF NO ERROR
ERRALC1:    RTS
;
SECNDHAF:   JSR        GETBITS2                         ; GET NEXT FREE BLOCK ADDRESS FROM SECOND HALF OF BIT MAP
            BCC        ALIDX1                           ; BRANCH IF NO ERROR_
ALDXEND:    RTS                                         ; RETURN STATUS (CARRY SET INDICATES ERROR)
;
;
ALC1BLK:    JSR        FNDBMAP                          ; GET ADDRESS OF BIT MAP IN 'BMADR'
            BCS        ERRALC1                          ; BRANCH IF ERROR ENCOUNTERED
SRCHFRE:    LDY        #0                               ; START SEARCH AT BEGINNING OF BIT MAP BLOCK
            STY        HALF                             ; INDICATE WHICH HALF (PAGE) WE'RE SEARCHING_
GETBITS1:   LDA        (BMADR),Y
            BNE        BITFOUND                         ; FREE BLOCKS ARE INDICATED BY 'ON' BITS
            INY
            BNE        GETBITS1                         ; CHECK ALL OF 'EM IN FIRST PAGE_
            INC        BMADR+1                          ; BUMP HIGH ADDRESS OF CURRENT BITMAP
            INC        HALF                             ; INDICATE SEARCH HAS PROGRESSED TO PAGE 2
            INC        BASVAL                           ; BASE VALUE= BASE ADDRESS/2048
GETBITS2:   LDA        (BMADR),Y                        ; SEARCH SECOND HALF FOR FREE BLOCK
            BNE        BITFOUND
            INY
            BNE        GETBITS2
            DEC        BMADR+1                          ; RESET BIT MAP ADDRESS TO BEGINNING_
            INC        BASVAL                           ; ADD 2048 OFFSET FOR NEXT PAGE
            JSR        NXTBMAP                          ; GET NEXT BITMAP (IF IT EXISTS) AND UPDATE VCB_
            BCC        SRCHFRE                          ; BRANCH IF NO ERROR ENCOUNTERED_
            RTS                                         ; RETURN ERROR_
;PAGE
;
BITFOUND:   STY        BMPTR                            ; SAVE INDX POINTER TO VALID BIT GROUP
            LDA        BASVAL                           ; SET UP FOR BLOCK ADDRESS CALCULATION
            STA        SCRTCH+1
            TYA                                         ; GET ADDRESS OF BIT PATTERN
            ASL        A                                ; MULTIPLY THIS AND BASVAL BY 8
            ROL        SCRTCH+1
            ASL        A
            ROL        SCRTCH+1
            ASL        A
            ROL        SCRTCH+1
            TAX                                         ; NOW X= LOW ADDRESS WITHIN 7 OF ACTUAL ADDRESS_
            LDA        (BMADR),Y                        ; GET BIT PATTERN AGAIN
            SEC                                         ; MARK RIGHT END OF BYTE_
ADCALC:     ROL        A                                ; FIND LEFT MOST 'ON' BIT
            BCS        BOUNCE                           ; BRANCH IF FOUND_
            INX                                         ; ADJUST LOW ADDRESS
            BNE        ADCALC                           ; BRANCH ALWAYS
BOUNCE:     LSR        A                                ; RESTORE ALL BUT LEFT MOST BIT TO ORIGINAL POSITION
            BCC        BOUNCE                           ; LOOP UNTIL MARK (SET ABOVE) MOVES INTO CARRY
            STA        (BMADR),Y                        ; UPDATE BITMAP TO SHOW ALLOCATED BLOCK IN USE_
            STX        SCRTCH                           ; SAVE LOW ADDRESS_
            LDX        BMTAB                            ; UPDATE BIT MAP BUFFER STATUS
            LDA        #$80                             ; INDICATE MAP HAS BEEN MODIFIED
            ORA        BMASTAT,X                        ; (X IS EITHER 0 OR 6 FOR
            STA        BMASTAT,X                        ; BUFFER 'A' OR 'B' RESPECTIVELY_)
            LDY        #VCBTFRE                         ; SUBTRACT 1 FROM TOTAL FREE
            LDA        (VCBPTR),Y                       ; BLOCKS IN VCB TO ACCOUNT FOR NEWLY
            SBC        #1                               ; ALLOCATED BLOCK (CARRY IS SET FROM 'BOUNCE')
            STA        (VCBPTR),Y
            BCS        RET1BLK                          ; BRANCH IF HI FREE COUNT DOESN'T NEED ADJUSTMENT_
            INY
            LDA        (VCBPTR),Y                       ; ADJUST HIGH COUNT_
            SBC        #0                               ; (CARRY IS CLEAR, SO ACC=ACC-1)
            STA        (VCBPTR),Y
RET1BLK:    CLC                                         ; INDICATE NO ERROR ENCOUNTERED
            LDA        SCRTCH                           ; GET ADDRESS LOW IN ACC_
            LDY        SCRTCH+1                         ; AND HIGH ADDRESS IN Y
            RTS                                         ; RETURN ADDRESS OF NEWLY ALLOCATED BLOCK_
;
;PAGE
;
GTTINDX:    LDY        #VCBDEV                          ; GET DEVICE NUMBER SO WE DON'T
            LDX        #0                               ; ANTICPATE USING BUFFER 'A'_
            LDA        (VCBPTR),Y                       ; USE THE BUFFER USED BY IT!
            CMP        BMADEV                           ; IS IT IN BUFFER 'A'?
            BEQ        FREEBE                           ; IF SO, FREE 'B'!
            CMP        BMBDEV                           ; IF NOT, IS IT IN 'B'?
            BEQ        FREEA                            ; IF SO, FREE UP BUFFER 'A'
            JSR        FNDBMAP                          ; OTHERWISE, FORCE ALLOCATION FOR ONE OF THE BUFFERS
            BCC        GTTINDX                          ; NOW TRY AGAIN_
            RTS                                         ; RETURN ERROR_
;
FREEBE:     LDX        #BMTABSZ                         ; DE-ALLOCATE BUFFER IF NECESSARY
FREEA:      STX        NOFREE                           ; SAVE WHICH BUFFER WE'RE LOOKIN AT_
            LDY        BMASTAT,X                        ; DO WE NEED TO WRITE BUFFER TO FREE IT?
            BPL        USEBUF                           ; NO, THEN USE IT_
            STX        ZPGTEMP                          ; SAVE BM BUFFER ID FOR A BIT
            JSR        WRTBMAP                          ; WRITE BM TO OWNING UNIT
            BCS        SOMERR1                          ; RETURN ANY ERROR (W/O RELEASING BM)
            LDX        ZPGTEMP                          ; FETCH THE BM BUFFER ID
            LDA        #0
            STA        BMASTAT,X                        ; AND MARK BM BUFFER AS FREE
USEBUF:     LDX        NOFREE                           ; GET INDEX TO BUFFER INFO
            LDA        #0                               ; MARK STATUS OF BUFFER AS FREE_
            STA        BMADEV,X                         ; (DEVICE 0 IS NOT ANY DEVICE)
            STA        TINDX
            STA        BMADR
            LDA        A:BMAMADR,X                      ; GET MEMORY ADDRESS OF FREE BUFFER_
            STA        TINDX+1
            TXA                                         ; SET UP PROPER HI ADDRESS OF BIT MAP TOO___
            EOR        #BMTABSZ                         ; SELECT ALTERNATE BIT MAP TABLE_
            STA        BMTAB                            ; (TO INDICATE WHICH IS BITMAP)
            TAX
            LDA        A:BMAMADR,X                      ; GET HIGH ADDRESS OF BIT MAP_
            STA        BMADR+1
            LDA        BMBUFBNK                         ; AND BANK PAIR NUMBER_
            STA        SSTIDXH
            STA        SISBMADR
            CLC                                         ; INDICATE NO ERRORS
SOMERR1:    RTS
;
;PAGE
NXTBMAP:    LDY        #VCBTBLK+1                       ; BEFORE BUMPING TO NEXT MAP,
            LDA        (VCBPTR),Y                       ; CHECK TO BE SURE THERE IS
            LSR        A                                ; INDEED A NEXT MAP!
            LSR        A
            LSR        A
            LSR        A
            LDY        #VCBCMAP
            CMP        (VCBPTR),Y                       ; ARE THERE MORE MAPS?
            BEQ        NOMORBIT                         ; BRANCH IF NO MORE TO LOOK AT_
            LDA        (VCBPTR),Y                       ; ADD 1 TO CURRENT MAP
            CLC
            ADC        #1
            STA        (VCBPTR),Y
            LDY        #VCBDEV
            LDA        (VCBPTR),Y
            TAX                                         ; GO WRITE OUT LAST MAP IF NECESSARY
            JSR        UPBMAP
            JMP        FNDBMAP                          ; READ NEXT BIT MAP INTO BUFFER
;
GETA_BUF:   LDX        #0
            BEQ        FRESHMAP
;
GETB_BUF:   LDX        #BMTABSZ
            BNE        FRESHMAP                         ; BRANCH ALWAYS
;
;
FNDBMAP:    LDY        #VCBDEV                          ; GET DEVICE NUMBER
            LDA        (VCBPTR),Y
            LDX        #0                               ; START WITH MAP 'A'
FNDMAP1:    CMP        BMADEV,X
            BNE        TRYMAP2
FRESHMAP:   STX        BMTAB                            ; SAVE POINTER TO BIT MAP INFO TABLE
            LDY        BMASTAT,X                        ; IS THIS ONE ALREADY MODIFIED?
            BMI        BMFOUND                          ; YES, RETURN POINTER IN 'BMADR'
            JSR        GTBMAP                           ; OTHERWISE READ IN FRESH BIT MAP
            BCC        BMFOUND                          ; BRANCH IF SUCCESSFUL_
            RTS                                         ; OTHERWISE, RETURN ERROR_
;
TRYMAP2:    DEX                                         ; WAS LAST FAILURE MAP 'A'
            BPL        FRBMBUF                          ; NO, MUST FREE UP ONE OF THE BUFFERS
            LDX        #BMTABSZ                         ; TRY BIT MAP BUFFER 'B'_
            JMP        FNDMAP1
;PAGE
;
BMFOUND:    LDX        BMTAB                            ; WHICH TABLE?
            LDY        #VCBCMAP
            LDA        (VCBPTR),Y
            ASL        A
            STA        BASVAL
            LDA        A:BMAMADR,X                      ; GET HIGH ADDRESS
            STA        BMADR+1
            LDA        BMBUFBNK                         ; GET BANK NUMBER OF BUFFER BIT MAP BUFFERS
            STA        SISBMADR
            LDA        #0                               ; BUFFERS ALWAYS FALL ON A PAGE BOUNDARY
            STA        BMADR
            CLC                                         ; INDICATE ALL IS VALID AND GOOD!
            RTS
;
NOMORBIT:   LDA        #OVRERR                          ; INDICATE REQUEST CAN'T BE FILLED_
            SEC                                         ; INDICATE ERROR
            RTS
;
FRBMBUF:    SEC
            LDX        BMTAB                            ; FIND OUT WHICH WAS LAST USED_
            BEQ        CHKBMB                           ; IF 'A' WAS USED CHECK 'B' FIRST
            CLC                                         ; INDICATE 'A' IS CHECKED FIRST
            BIT        BMASTAT                          ; IS BUFFER 'A' FREE (UNMODIFIED)?
            BPL        GETA_BUF                         ; YES, USE IT_
CHKBMB:     BIT        BMBSTAT                          ; IS BUFFER 'B' FREE?
            BCC        FREBUF1                          ; BRANCH IF BOTH ARE USED
            BPL        GETB_BUF                         ; YES___
            BIT        BMASTAT                          ; (CHECK 'A')
            BPL        GETA_BUF
FREBUF1:    LDX        #0
            BCC        FREBUFA                          ; BRANCH IF BUFFER 'A' HAS LEAST PRIORITY_
            LDX        #BMTABSZ
FREBUFA:    STX        ZPGTEMP                          ; SAVE BM BUFF ID FOR A BIT
            JSR        WRTBMAP                          ; XREG PASSES BM BUFF ID
            BCS        NOGO                             ; ERROR ENCOUNTERED ON WRITING
            LDX        ZPGTEMP                          ; FETCH BM BUFF ID
            LDA        #0
            STA        BMASTAT,X                        ; AND MARK BM BUFFER AS FREE
            BCC        FNDBMAP                          ; LOOK AGAIN FOR FRRE BIT MAP BUFFER SPACE
NOGO:       RTS                                         ; RETURN ERROR ON WRITING BM
;
UPBMAP:     CPX        BMADEV                           ; UPDATE BIT MAP OF DEVICE X
            BNE        UPBM1
            CLC                                         ; FREE BUFFER 'A' IF NEEDED_
            BIT        BMASTAT
            BMI        FREBUF1                          ; (CARRY CLEAR FOR BUFFER 'A')
            RTS
;PAGE
;
UPBM1:      CPX        BMBDEV
            BNE        NOUPDAT                          ; DON'T UPDATE IF NOT NECESSARY_
            BIT        BMBSTAT
            BMI        FREBUF1                          ; (CARRY IS SET)
NOUPDAT:    CLC
            RTS                                         ; RETURN 'NO ERROR'
;
CLEARBMS    =          *                                ; MAKE SURE ALL BIT MAPS ASSOCIATED
; WITH A DEVICE ARE MARKED INVALID
; IF A NEW VOLUME IS LOGGED IN ON IT.
; INPUT ARG: A REG = DEVNUM
; X REG PRESERVED
            LDY        #0
            CMP        BMADEV
            BNE        CLRBM1                           ; BRANCH IF BIT MAP A NOT OWNED
            BIT        BMASTAT
            BMI        CLRBM2                           ; BRANCH IF BITMAP A BUSY
            STY        BMADEV                           ; ELSE, CLEAR IT
CLRBM2:     RTS                                         ; NEED ONLY CLEAR ONE
CLRBM1:     CMP        BMBDEV                           ; BIT MAP B?
            BNE        CLRBM2                           ; BRANCH IF BIT MAP B NOT OWNED BY DEVNUM
            BIT        BMBSTAT
            BMI        CLRBM2                           ; BRANCH IF BITMAP B BUSY
            STY        BMBDEV                           ; ELSE CLEAR IT
            RTS                                         ; AND RETURN TO CALLER (NO ERRORS)
;
GTBMAP:     STA        BMADEV,X                         ; SAVE ACC AS CURRENT DEVICE FOR BUFFER
            LDA        A:BMAMADR,X                      ; GET HIGH ORDER ADDRESS OF BUFFER
            STA        BMADR+1                          ; SELECTED BY X
            LDA        BMBUFBNK                         ; AND GET BANK PAIR NUMBER
            STA        SISBMADR                         ; OF BOTH BIT MAP BUFFERS 'A' AND 'B'
            LDY        #VCBCMAP                         ; GET LOWEST MAP NUMBER WITH FREE BLOCKS IN IT_
            LDA        (VCBPTR),Y
            STA        BMACMAP,X                        ; ASSOCIATE THE OFFSET WITH THE BITMAP CONTROL BLOCK
            CLC
            LDY        #VCBDMAP                         ; ADD THIS NUMBER TO THE BASE
            ADC        (VCBPTR),Y                       ; ADDRESS OF FIRST BIT MAP
            STA        BMADADR,X                        ; SAVE LOW ADDRESS OF BIT MAP TO BE USED_
            INY                                         ; NOW GET HIGH DISK ADDRESS OF MAP
            LDA        (VCBPTR),Y                       ; ADD TO THIS THE STATE OF THE CARRY
            ADC        #0
            STA        BMADADR+1,X                      ; SAVE HIGH DISK ADDRESS TOO_
;: DROP INTO 'RDBMAP'
;
;PAGE
;
            LDA        #RDCMD                           ; (X CONTAINS AN INDEX TO DETERMINE WHICH BUFFER)
DOBMAP:     STA        DHPCMD                           ; SAVE DEVICE COMMAND
            LDA        DEVNUM                           ; FIX THE 'BIT MAP TRASH BUG'
            PHA                                         ; BY NOT MUNGING DEVNUM
            LDA        BMADEV,X                         ; GET DEVICE NUMBER_
            STA        DEVNUM
            LDA        BMADADR,X                        ; AND MAP'S DISK ADDRESS
            STA        BLOKNML
            LDA        BMADADR+1,X
            STA        BLOKNMH
            LDA        A:BMAMADR,X                      ; LASTLY GET THE ADDRESS OF THE BUFFER
            LDX        BMBUFBNK                         ; AND BANK NUMBER_
            JSR        DOBITMAP                         ; (NOTE: LOW ADDRESS IS FIXED TO ZERO AS THIS IS A BUFFER)
            PLA                                         ; RESTORE
            STA        DEVNUM                           ; THE DEVNUM WE CAME IN WITH!
            RTS
;
WRTBMAP:    LDA        #WRTCMD                          ; WRITE BIT MAP POINTED TO BY X
            JMP        DOBMAP
;
WRTGBUF:    LDA        #WRTCMD                          ; SET CALL FOR WRITE_
            BNE        SVGCMD                           ; BRANCH ALWAYS_
RDGBUF:     LDA        #RDCMD                           ; SET CALL FOR READ_
SVGCMD:     STA        DHPCMD                           ; PASSED TO DEVICE HANDLER_
            LDA        BLOKNML                          ; SAVE CURRENT
            STA        TTLINK                           ;   GBUF BLOCK
            LDA        BLOKNMH                          ; ADDRESS
            STA        TTLINK+1                         ; FOR DIRECTORY EXTEND
            LDA        #GBUF/256                        ; GET HIGH ADDRESS OF GENERAL BUFFER
            LDX        #0                               ; TO FORCE ACCESS TO NON BANK MEMORY_
DOBITMAP    =          *
DOIDX:      STA        DBUFPH
            STX        SISBPH                           ; SELECT BANK
            LDA        #0                               ; GENERAL PURPOSE BUFFERS ALWAYS
            STA        DBUFPL                           ; START ON A PAGE BOUNDARY_
            JMP        FILEIO2                          ; END VIA DEVICE DISPATCHER_
;
TTLINK:     .RES       2                                ; GBUF CURRENT ADDRESS
;
WRTINDX:    LDA        #WRTCMD
            LDX        IDXADRL                          ; GET BLOCK ADDRESS OF INDEX BLOCK
            LDY        IDXADRH
DOFRST:     STA        DHPCMD                           ; (ENTRY USED BY RD/WRTDFRST)
            STX        BLOKNML
            STY        BLOKNMH
            LDA        TINDX+1                          ; HIGH RAM ADDRESS OF INDEX BLOCK
            LDX        SSTIDXH                          ; AND BANK NUMBER_
            JMP        DOIDX                            ; AND GO DO REQUESTED OPERATION_
;
WRTDFRST:   LDA        #WRTCMD                          ; WRITE FILE'S FIRST BLOCK (USED
            BNE        FADDR                            ; BY CREATE, SO ADDRESS IN 'D_' STUFF)_
RDFRST:     LDA        #RDCMD
FADDR:      LDX        A:DFIL+D_FRST                    ; (BUFFER ADDRESS IS IN 'TINDX')
            LDY        A:DFIL+D_FRST+1
            JMP        DOFRST
;
;
            .INCLUDE   "POSNOPEN.S"
            .INCLUDE   "READWRITE.S"
            .INCLUDE   "CLOSEEOF.S"
            .INCLUDE   "DESTROY.S"
            .INCLUDE   "SWAPOUTIN.S"

