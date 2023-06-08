; Update sysglob.s
; - Update the displayed SOS version
;
; Updates by Robert Justice
;

;SBTL "SOS 1.1  GLOBAL EQUATES"
;.RELOC
              .SEGMENT   "CODE"
              .ORG       $18FC
;MSB OFF
;***************************************************************************************************
;          COPYRIGHT (C) APPLE COMPUTER INC. 1980 
;                    ALL RIGHTS RESERVED 
;***************************************************************************************************
;
;  SOS SYSTEM GLOBAL DATA & EQUATES
;
;  THIS MODULE CONTAINS THE SOS JUMP TABLE, AND ALL GLOBAL
;  DATA AND EQUATES.  THE JUMP TABLE, AND ALL DATA THAT IS 
;  TO BE REFERENCED BY DEVICE HANDLERS, ARE ASSIGNED FIXED 
;  ADDRESSES AT THE BEGINNING OF MEMORY PAGE $19.  DATA 
;  THAT IS ONLY REFERENCED BY SOS BEGINS $1980, BUT MAY BE 
;  MOVED WHENEVER SOS IS RELINKED.
;
;***************************************************************************************************
;
              .IMPORT    ALLOCSIR
              .IMPORT    DEALCSIR
              .IMPORT    NMIDSBL
              .IMPORT    NMIENBL
              .IMPORT    QUEEVENT
              .IMPORT    SELC800
              .IMPORT    SYSDEATH
              .IMPORT    SYSERR
              .IMPORT    REQBUF
              .IMPORT    GETBUFADR
              .IMPORT    RELBUF
              .IMPORT    NMIDBUG
              .IMPORT    NMICONT
              .IMPORT    COLDSTRT
;
;
              .EXPORT    MEMSIZE
              .EXPORT    SYSBANK
              .EXPORT    SUSPFLSH
              .EXPORT    NMIFLAG
              .EXPORT    NMIVECT          ;(S) add nmivect
              .EXPORT    SCRNMODE
              .EXPORT    GRSIZE
;
              .EXPORT    SERR
              .EXPORT    DBUGBRK
              .EXPORT    KYBDNMI
              .EXPORT    NMISPSV
              .EXPORT    SDEATH_REGS
;
              .EXPORT    SOSVER
              .EXPORTZP  SOSVERL
;
              .EXPORT    SZPAGE
              .EXPORT    SXPAGE
              .EXPORT    SSPAGE
;
              .EXPORT    CZPAGE
              .EXPORT    CXPAGE
              .EXPORT    CSPAGE
              .EXPORT    CEVPRI
;
              .EXPORT    SIRTEMP
              .EXPORT    SIRARGSIZ
              .EXPORT    IRQCNTR
              .EXPORT    NMICNTR
              .EXPORT    QEVTEMP
              .EXPORT    QEV_THIS
              .EXPORT    QEV_LAST
;
              .EXPORTZP  BADBRK
              .EXPORT    BADINT1
              .EXPORT    BADINT2
              .EXPORT    NMIHANG
              .EXPORT    EVQOVFL
              .EXPORT    STKOVFL
              .EXPORT    BADSYSCALL
              .EXPORT    DEV_OVFLOW
              .EXPORT    MEM2SML
              .EXPORT    VCBERR
              .EXPORT    FCBERR
              .EXPORT    ALCERR
              .EXPORT    DIRERR
              .EXPORT    TOOLONG
              .EXPORT    BADBUFNUM
              .EXPORT    BADBUFSIZ
              .EXPORT    BITMAPADR
;
              .EXPORTZP  BADSCNUM
              .EXPORTZP  BADCZPAGE
              .EXPORTZP  BADXBYTE
              .EXPORTZP  BADSCPCNT
              .EXPORTZP  BADSCBNDS
;
              .EXPORTZP  NODNAME
              .EXPORTZP  BADDNUM
;
              .EXPORTZP  BADPATH
              .EXPORTZP  CFCBFULL
              .EXPORTZP  FCBFULL
              .EXPORTZP  BADREFNUM
              .EXPORT    PATHNOTFND
              .EXPORT    VNFERR
              .EXPORT    FNFERR
              .EXPORT    DUPERR
              .EXPORT    OVRERR
              .EXPORT    DIRFULL
              .EXPORT    CPTERR
              .EXPORT    TYPERR
              .EXPORT    EOFERR
              .EXPORT    POSNERR
              .EXPORT    ACCSERR
              .EXPORT    BTSERR
              .EXPORT    FILBUSY
              .EXPORT    NOTSOS
              .EXPORT    BADLSTCNT
              .EXPORT    OUTOFMEM
              .EXPORT    BUFTBLFULL
              .EXPORT    BADSYSBUF
              .EXPORT    DUPVOL
              .EXPORT    NOTBLKDEV
              .EXPORT    LVLERR
;
              .EXPORT    BADJMODE
;
              .EXPORT    BADBKPG
              .EXPORT    SEGRQDN
              .EXPORT    SEGTBLFULL
              .EXPORT    BADSEGNUM
              .EXPORT    SEGNOTFND
              .EXPORT    BADSRCHMODE
              .EXPORT    BADCHGMODE
              .EXPORT    BADPGCNT
;
              .EXPORT    XREQCODE
              .EXPORT    XCTLCODE
              .EXPORT    XCTLPARM
              .EXPORT    XNOTOPEN
              .EXPORT    XNOTAVAIL
              .EXPORT    XNORESRC
              .EXPORT    XBADOP
              .EXPORT    XIOERROR
              .EXPORT    XNODRIVE
              .EXPORT    XNOWRITE
              .EXPORT    XBYTECNT
              .EXPORT    XBLKNUM
              .EXPORT    XDISKSW
              .EXPORT    BACKMASK                                 ; MASK BYTE FOR BACKUP BIT.
;
              .EXPORT    E1908                                    ; DISK DRIVER IS READING/WRITING (SET) ELSE NOT (RESET)
;
              .EXPORT    SELCLDST         ;(S)  SELECTOR COLD START
;
;PAGE
              .WORD      SYSGLOB                                  ;SYSGLOB TARGET ADDRESS
              .WORD      $0100                                    ;  AND LENGTH
;
;  SYSTEM GLOBAL DATA
;    (ACCESSIBLE TO SOS AND DEVICE HANDLERS)
;
SYSGLOB:
;
MEMSIZE:      .BYTE      $08                                      ;MEMORY SIZE = 128K
SYSBANK:      .BYTE      $02                                      ;SYSTEM BANK = 2 
SUSPFLSH:     .BYTE      $00                                      ;SYSOUT SUSPEND/FLUSH FLAG
NMIFLAG:      .BYTE      $00                                      ;NMI PENDING FLAG 
NMIVECT:      .WORD      NMIEXIT           ;(S) add nmivect label ;DEFAULT NMI VECTOR
SCRNMODE:     .BYTE      $80                                      ;CURRENT SCREEN MODE
GRSIZE:       .BYTE      $00
;
;
;  SOS JUMP TABLE
;
              .RES       SYSGLOB+$10-*,$00                        ; USED BY THE MOUSE DRIVER 
USERNMI:      JMP        NMIEXIT                                  ;KEYBOARD NMI VECTOR
              JMP        ALLOCSIR                                 ;ALLOCATE A SIR
              JMP        DEALCSIR                                 ;DEALLOCATE A SIR
              JMP        NMIDSBL                                  ;DISABLE NMI
              JMP        NMIENBL                                  ;ENABLE NMI
              JMP        QUEEVENT                                 ;QUEUE AN EVENT
              JMP        SELC800                                  ;SELECT I/O EXPANSION ROM
              JMP        SYSDEATH                                 ;SYSTEM DEATH
              JMP        SYSERR                                   ;SOS ERROR
              JMP        REQBUF                                   ;REQUEST BUFFER
              JMP        GETBUFADR                                ;GET BUFFER'S ADDRESS
              JMP        RELBUF                                   ;RELEASE BUFFER
              JMP        CLRBMASK                                 ;VECTOR TO CLRBMASK 
;PAGE
;
              .WORD      $0000              ;(S)  
              .WORD      $1A00              ;(S)
              .WORD      $F938              ;(S)
;
; SELECTOR NEW COLD START
;
SELCLDST:     JMP        COLDSTRT           ;(S)
              LDX        #$03               ;(S)
SEL001:       LDA        $1935,X            ;(S)
              STA        CSPAGE + $FC,X     ;(S)    CALLER'S STACK PAGE
              LDA        $1939,X            ;(S)
              STA        SSPAGE + $FC,X     ;(S)    SYSTEM STACK PAGE
              DEX                           ;(S)
              BPL        SEL001             ;(S)
              LDA        #$30               ;(S)
              STA        CSPAGE +$FD        ;(S)
              RTS                           ;(S)

;  SOS DATA AND EQUATES
;    (ACCESSIBLE ONLY TO SOS)
;
              .RES       SYSGLOB+$80-*,$00
SERR:         .BYTE      $00                                      ;SYSTEM ERROR CODE
;
DBUGBRK:      NOP                                                 ;TO ENABLE DEBUG BREAK POINTS,
              NOP   ;(S) nop these out PLA                        ;  PATCH THESE BYTES TO
              NOP   ;(S)               PLA                        ;  JMP TO THE DEBUGGER
              NOP   ;(S)               RTS
;
KYBDNMI:      JMP        USERNMI
              JMP        NMIDBUG
NMISPSV:      .BYTE      0
              JMP        NMICONT
NMIEXIT:
              RTS
;
;
SOSVER:       .BYTE      "SOS 1.5A1  16-APR-23"                    ;(S) add the (S) to the version
SOSVERL       =          *-SOSVER
;
              .BYTE      "COPYRIGHT APPLE COMPUTER INC.1980-84"   ;(S) and change the last year
                                                                  ;this message seems not used
;
E1908         =          $1908                                    ; ALLOCATED TO STEPHEN SMITH (MOUSE DRIVER) 
; ABOVE SET AND RESET IN DISK DRIVER
SZPAGE        =          $1800                                    ;SYSTEM ZERO PAGE
SXPAGE        =          $1400                                    ;SYSTEM EXTEND PAGE
SSPAGE        =          $0100                                    ;SYSTEM STACK PAGE
;
CZPAGE        =          $1A00                                    ;CALLER'S ZERO PAGE
CXPAGE        =          $1600                                    ;CALLER'S EXTEND PAGE
CSPAGE        =          $1B00                                    ;CALLER'S STACK PAGE
CEVPRI:       .BYTE      $00                                      ;CALLER'S EVENT PRIORITY
;
SIRTEMP:      .BYTE      $00                                      ;TEMP FOR ALLOCSIR & DEALCSIR
SIRARGSIZ:    .BYTE      $00                                      ;ARGUMENT COUNT FOR ALLOCSIR & DEALCSIR
IRQCNTR:      .WORD      $0000                                    ;FALSE IRQ COUNTER
NMICNTR:      .WORD      $0000                                    ;COUNTER FOR NMILOCK
QEVTEMP:      .BYTE      $00                                      ;TEMP FOR QUEEVENT
QEV_THIS:     .BYTE      $00                                      ;POINTER FOR QUEEVENT
QEV_LAST:     .BYTE      $00                                      ;POINTER FOR QUEEVENT
;
SOSQUIT       =          COLDSTRT
BACKMASK:     .BYTE      BACKBIT                                  ; MASK USED BY BFM TO UPDATE BACKUP BIT
;
; TO CLEAR THE BACKUP BIT, A PROGRAM MUST JSR TO CLRBMASK THRU 1934 THEN DO A
; SET-FILE-INFO WITH NO INTERVENING SOS CALLS.  ANY SOS CALL WILL
; SET BACKMASK AGAIN.  THIS FEATURE IS INTENTIONALLY LEFT UNDOCUMENTED.
;
CLRBMASK:     AND        #BACKBIT                                 ; PURIFY 
              STA        BACKMASK                                 ; SET THE MASK
              RTS                                                 ; AND BACK TO THE CALLER
;PAGE
;
;  SYSTEM DEATH REGISTER SAVE AREA
; (SYSTEM STACK MOVED TO $1700-$17FF) 
;
              .RES       SYSGLOB+$F6-*,$00
SDEATH_REGS:
              .BYTE      $00                                      ;BANK
              .BYTE      $00                                      ;ZERO PAGE
              .BYTE      $00                                      ;ENVIRONMENT
              .BYTE      $00                                      ;Y
              .BYTE      $00                                      ;X
              .BYTE      $00                                      ;A
              .BYTE      $00                                      ;STATUS
              .WORD      $00                                      ;PROGRAM COUNTER
              .BYTE      $00                                      ;STACK POINTER
;
;  SYSTEM DEATH ERROR NUMBERS
;
BADBRK        =          $01                                      ;BRK FROM SOS
BADINT1       =          $02                                      ;INTERRUPT NOT FOUND
BADINT2       =          $03                                      ;BAD ZERO PAGE ALLOCATION
NMIHANG       =          $04                                      ;UNABLE TO LOCK NMI
EVQOVFL       =          $05                                      ;EVENT QUEUE OVERFLOW
STKOVFL       =          $06                                      ;STACK OVERFLOW
;
BADSYSCALL    =          $07                                      ;DMGR DETECTED INVALID REQUEST CODE
DEV_OVFLOW    =          $08                                      ;DMGR - TOO MANY DEVICE HANDLERS
MEM2SML       =          $09                                      ;MEMORY SIZE < 64K
VCBERR        =          $0A                                      ;VOLUME CONTROL BLOCK NOT USABLE (BFMGR)
FCBERR        =          $0B                                      ;FILE CONTROL BLOCK CRASHED
ALCERR        =          $0C                                      ;ALLOCATION BLOCKS INVALID
TOOLONG       =          $0E                                      ;PATHNAME BUFFER OVERFLOW
BADBUFNUM     =          $0F                                      ;INVALID BUFFER NUMBER
BADBUFSIZ     =          $10                                      ;INVALID BUFFER SIZE (=0 OR >16K)
;PAGE
;
;  SYSTEM ERROR NUMBERS
;
; - SYSTEM CALL MANAGER
;
BADSCNUM      =          $01                                      ;BAD SYSTEM CALL NUMBER
BADCZPAGE     =          $02                                      ;BAD CALLER'S ZPAGE (MUST=$1A)
BADXBYTE      =          $03                                      ;BITS	 6..4 <> 0
BADSCPCNT     =          $04                                      ;BAD SYSTEM CALL PARM COUNT
BADSCBNDS     =          $05                                      ;SYS CALL PARM ADR
;
; - DEVICE MANAGER
;
NODNAME       =          $10                                      ;DEVICE NAME NOT FOUND
BADDNUM       =          $11                                      ;INVALID DEV.NUM PARM
;
; - DEVICE HANDLERS (STANDARD ERRORS)
;
XREQCODE      =          $20                                      ;INVALID REQUEST CODE
XCTLCODE      =          $21                                      ;INVALID CONTROL/STATUS CODE
XCTLPARM      =          $22                                      ;INVALID CONTROL/STATUS PARM
XNOTOPEN      =          $23                                      ;DEVICE NOT OPEN
XNOTAVAIL     =          $24                                      ;DEVICE NOT AVAILABLE
XNORESRC      =          $25                                      ;UNABLE TO OBTAIN RESOURCE
XBADOP        =          $26                                      ;INVALID OPERATION
XIOERROR      =          $27                                      ;I/O ERROR
;     
XNODRIVE      =          $28                                      ;NO DRIVE CONNECTED
XNOWRITE      =          $2B                                      ;DEVICE WRITE PROTECTED
XBYTECNT      =          $2C                                      ;BYTE COUNT <> A MULTIPLE OF 512
XBLKNUM       =          $2D                                      ;BLOCK NUMBER TOO LARGE
XDISKSW       =          $2E                                      ;DISK MEDIA HAS BEEN SWITCHED
;
; - NOTE: ERROR CODES $30-$3F HAVE BEEN RESERVED FOR DEVICE 
;   HANDLER SPECIFIC ERRORS
;
;
; - FILE MANAGER
;
BADPATH       =          $40                                      ;PATHNAME, INVALID SYNTAX
CFCBFULL      =          $41                                      ;CHAR FILE CTRL BLOCK TABLE FULL
FCBFULL       =          $42                                      ;BLOCK FILE CTRL BLOCK TABLE FULL
BADREFNUM     =          $43                                      ;INVALID REF.NUM PARM
PATHNOTFND    =          $44                                      ;PATHNAME NOT FOUND
VNFERR        =          $45                                      ;VOLUME NOT FOUND
FNFERR        =          $46                                      ;FILE NOT FOUND
DUPERR        =          $47                                      ;DUPLICATE FILE NAME ERROR
OVRERR        =          $48                                      ;NOT ENOUGH DISK SPACE FOR PREALLOCATION
DIRFULL       =          $49                                      ;DIRECTORY FULL ERROR
CPTERR        =          $4A                                      ;FILE INCOMPATIBLE SOS VERSION
TYPERR        =          $4B                                      ;NOT CURRENTLY SUPPORTED FILE TYPE
EOFERR        =          $4C                                      ;POSITION ATTEMPTED BEYOND END OF FILE
POSNERR       =          $4D                                      ;ILLEGAL POSITION (L.T. 0 OR G.T. $FFFFFF)
ACCSERR       =          $4E                                      ;FILE ACCESS R/W REQUEST CONFLICTS WITH ATTRIBUTES
BTSERR        =          $4F                                      ;USER SUPPLIED BUFFER TOO SMALL
FILBUSY       =          $50                                      ;EITHER WRITE WAS REQUESTED OR WRITE ACCESS ALREADY ALLOCATED
DIRERR        =          $51                                      ;DIRECTORY ERROR
NOTSOS        =          $52                                      ;NOT A SOS DISKETTE
BADLSTCNT     =          $53                                      ;INVALID VALUE IN LIST PARAMETER
OUTOFMEM      =          $54                                      ;OUT OF FREE MEMORY FOR BUFFER
BUFTBLFULL    =          $55                                      ;BUFFER TABLE FULL
BADSYSBUF     =          $56                                      ;INVALID SYSBUF PARAMETER
DUPVOL        =          $57                                      ;SON A BITCH GOT TWO VOLUMES OF SAME ROOT NAME!!!
NOTBLKDEV     =          $58
LVLERR        =          $59                                      ;INVALID FILE LEVEL
BITMAPADR     =          $5A
BACKBIT       =          $20                                      ; MASK FOR BACKUP BIT
;
; - UTILITY MANAGER
;
BADJMODE      =          $70                                      ;INVALID JOYSTICK REQUEST
; 
; - MEMORY MANAGER
;
BADBKPG       =          $E0                                      ;INVALID BANK/PAGE PAIR
SEGRQDN       =          $E1                                      ;SEGMENT REQUEST DENIED
SEGTBLFULL    =          $E2                                      ;SEGMENT TABLE FULL
BADSEGNUM     =          $E3                                      ;INVALID SEGMENT NUMBER
SEGNOTFND     =          $E4                                      ;SEGMENT NOT FOUND
BADSRCHMODE   =          $E5                                      ;INVALID SEARCH MODE PARM
BADCHGMODE    =          $E6                                      ;INVALID CHANGE MODE PARM
BADPGCNT      =          $E7                                      ;INVALID PAGE COUNT PARM
;ORG SYSGLOB+$100
              .WORD      $B800                                    ;KERNEL TARGET ADDRESS
              .WORD      $47C0                                    ;  AND LENGTH

