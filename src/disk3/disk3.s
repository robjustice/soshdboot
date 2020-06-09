;           .TITLE       "SOS Disk /// Driver"
;-----------------------------------------------------------------------
;
;
;               SOS Disk /// Driver
;
;
;       Revisions:
;
;       1.2     16-Aug-18
;               Based on the the disk driver code from the SOS Kernel, 
;               this is repackaged as a normal driver
;               support for rom0 rev removed RJ
;
;               15-Oct-19
;               convert for use with a3driverutil RJ
;
;
;-----------------------------------------------------------------------

            .PROC   DISK3
			.setcpu "6502"
			.reloc

			.SEGMENT "TEXT"
;
; Comment Field of driver
;
            .WORD   $FFFF                  ; Signal that we have a comment
            .WORD   COMMENT_END - COMMENT  ; Length of comment field
COMMENT:    .BYTE   "SOS Disk /// Driver"
COMMENT_END:

			.SEGMENT	"DATA"

;
; Equates
;
; SOS Error Codes
XREQCODE    =          $20
XBADOP      =          $26
XNODRIVE    =          $28
XIOERROR    =          $27
XNOWRITE    =          $2B
XBYTECNT    =          $2C
XBLKNUM     =          $2D
XDISKSW     =          $2E
XCTLCODE    =          $21
;
;
E1908       =          $1908                                 ;GLOBAL FLAG FOR MOUSE DRIVER
                                                             ;TO SAY WE CANNOT BE INTERRUPTED
; SOS Global Subroutines
SYSERR      =          $1928                                 ;SOS error return
;
; ROM Addresses
RDADR       =          $F1B9                                 ;RDADR
READ        =          $F148                                 ;READ
WRITE       =          $F216                                 ;WRITE
SEEK        =          $F400                                 ;SEEK
MSWAIT      =          $F456                                 ;MSWAIT
PRENIB      =          $F2C4                                 ;PRENIB
POSTNIB     =          $F30F                                 ;POSTNIB
;
; MOTOR-UP TIMES PER COMMAND
T50MS       =          $02                                   ; 50MS FOR MONTIMEH
T200MS      =          $08                                   ;200 MS FOR MONTIMEH
T1SEC       =          $27                                   ;1-SEC FOR MONTIMEH

;
;PAGE
; DISK /// CONTROLLER EQUATES:
;
;       MOTOR SELECT BITS:
;
;    DRIVE    INT   EXT1   EXT2
;    -----    ---   ----   ----
;     .D1      1     X      X
;     .D2      X     0      1
;     .D3      X     1      0
;     .D4      X     1      1
;
MS_INT      =          $C0D4                                 ;MOTOR   SELECT:INTERNAL DRIVE
MD_INT      =          $C0D5                                 ;MOTOR DESELECT:INTERNAL DRIVE
;
MS_EXT1     =          $C0D3                                 ;MOTOR   SELECT:EXTERNAL DRIVE
MS_EXT2     =          $C0D1                                 ;MOTOR   SELECT:EXTERNAL DRIVE
MD_EXT1     =          $C0D2                                 ;MOTOR DESELECT:EXTERNAL DRIVE
MD_EXT2     =          $C0D0                                 ;MOTOR DESELECT:EXTERNAL DRIVE
;
IS_INT      =          $C0EA                                 ;I/O SELECT:INTERNAL DRIVE
IS_EXT      =          $C0EB                                 ;I/O SELECT:EXTERNAL DRIVE
;
NOSCROLL    =          $C0D8                                 ;SMOOTHSCROLL OFF
;
MOTOROFF    =          $C0E8                                 ;MOTOR(S) START POWEROFF T/O
MOTORON     =          $C0E9                                 ;MOTOR(S) POWER ON
Q6L         =          $C08C                                 ;Q7L,Q6L=READ
Q6H         =          $C08D                                 ;Q7L,Q6H=SENSE WPROT
Q7L         =          $C08E                                 ;Q7H,Q6L=WRITE
Q7H         =          $C08F                                 ;Q7H,Q6H=WRITE STORE
;
; OTHER EQUATES:
;
E_REG       =          $FFDF                                 ;ENVIRONMENT REGISTER
E_IER       =          $FFEE                                 ;INTERRUPT ENABLE REGISTER
;
; RETRY COUNTERS:
;
R_RECAL     =          1                                     ;MAX RECALIBRATES
; R_RECAL MUST NOT BECOME ZERO! (MOUSE WILL BE LOCKED OUT)
; SEE DISK3.SIO.SRC LINE 14 FOR DETAIL
R_FIND      =          3                                     ;MAX REVS TO FIND A SECTOR
R_IOERR     =          4                                     ;MAX RETRIES ON READ ERROR
R_IRQ       =          6                                     ;MAX IRQ'S TOLERATED BEFORE SEI
;PAGE
;
; ZPAGE EQUATES FOR CORE ROUTINES:
;
; DSECT
; .ORG 081                                                   ; modified for ca65
IBSLOT      =          $81                                   ;.RES 1 ;SLOT=060 FOR RTNS
                                                             ;.RES 7 ;N/A
                                                             ;.RES 1 ;RDADR:CHECKSUM
                                                             ;.RES 1 ;N/A
IMASK       =          IBSLOT+10                             ;.RES 1 ;BIT7 SET IF IRQ ALLOWED
CURTRK      =          IMASK+1                               ;.RES 1 ;SEEK:CURRENT TRACK
                                                             ;.RES 2 ;N/A
INTRTRY     =          CURTRK+3                              ;.RES 1 ;READ: IRQ RETRY COUNT
                                                             ;.RES 5 ;N/A
                                                             ;.RES 1 ;RDADR:'MUST FIND' COUNT
                                                             ;.RES 1 ;READ,WRITE: CHECKSUM
CSSTV       =          INTRTRY+8                             ;.RES 4 ;RDADR:CKSUM,SEC,TRK,VOL
MONTIMEL    =          CSSTV+2                               ;MSWAIT:MOTOR-ON TIME
MONTIMEH    =          MONTIMEL+1
BUF         =          CSSTV+4                               ;.RES 2 ;PRENIB,POSTNIB:USER BUFFER
                                                             ;.RES 1 ;SEEK:PRIOR PHASE
TRKN        =          BUF+3                                 ;.RES 1 ;SEEK:TARGET TRACK
;
; LOCAL TEMPS:
;
; .ORG 0D0 ;WE'RE ALLOWED TO 0FF
BLKTEMP     =          $D0                                   ;.RES 2 ;LOCAL TEMP FOR BLKNUMBER
BUFTEMP     =          BLKTEMP+2                             ;.RES 2 ;LOCAL TEMP FOR BUFFER ADDRESS
TRACK       =          BUFTEMP+2                             ;.RES 1 ;LOCAL TEMP FOR TRACK
SECTOR      =          TRACK+1                               ;.RES 1 ;LOCAL TEMP FOR SECTOR
RETRYADR    =          SECTOR+1                              ;.RES 1 ;LOCAL TEMP FOR SECTOR-FIND RETRIES
RETRYCNT    =          RETRYADR+1                            ;.RES 1 ;LOCAL TEMP FOR I/O RETRIES
RECALCNT    =          RETRYCNT+1                            ;.RES 1 ;LOCAL TEMP FOR RECAL COUNT
BLKCOUNT    =          RECALCNT+1                            ;.RES 1 ;BLKS REQD TO SATISFY BYTECOUNT
SEEKWAIT    =          BLKCOUNT+1                            ;.RES 1 ;<>0 IF SEEK DELAY NEEDED
IRQMASK     =          SEEKWAIT+1                            ;.RES 1 ;ENTRY 'I' BIT
TEMP        =          IRQMASK+1                             ;.RES 1 ;JUST A TEMP
;
;PAGE
; DRIVER INTERFACE AREA:
;
; .ORG 0C0
D_COMMAND   =          $C0                                   ;.RES 1 ;COMMAND CODE
D_UNITNUM   =          D_COMMAND+1                           ;.RES 1 ;UNIT NUMBER
D_BUFL      =          D_UNITNUM+1                           ;.RES 2 ;BUFFER ADDRESS
D_BUFH      =          D_BUFL+1
D_STATCODE  =          D_BUFL                                ;DSTATUS CODE
D_STATBUF   =          D_BUFH                                ;^DSTATUS LIST
D_BYTES     =          D_BUFH+1                              ;.RES 2 ;BYTECOUNT
D_BLOCK     =          D_BYTES+2                             ;.RES 2 ;REQUESTED BLOCKNUM
D_BYTRD     =          D_BLOCK+2                             ;.RES 2 ;BYTES READ (READ)
                                                             ;.RES 6 ;SPARES (OK AS TEMPS)
;DEND 
;
;PAGE

;-----------------------------------------------------------------------
;
;       Device Handler Identification Block
;
;-----------------------------------------------------------------------
DIB1:                                                        ;DIB FOR .D1
            .WORD      DIB2                                  ;FLINK
            .WORD      MAIN                                  ;ENTRY POINT
            .BYTE      3                                     ;NAME LENGTH
            .BYTE       ".D1            "
            .BYTE      $80                                   ;DEVNUM: ACTIVE
            .BYTE      0                                     ;SLOT
            .BYTE      0                                     ;UNIT NUMBER
            .BYTE      $E1,$01,$00                           ;TYPE,SUB,FILLER
            .WORD      280                                   ;BLOCKCOUNT
            .WORD      1                                     ;MANUFACTURER=APPLE
            .WORD      $1200                                 ;VERSION=1.2
;
DIB2:                                                        ;DIB FOR .D2
            .WORD      DIB3                                  ;FLINK
            .WORD      MAIN                                  ;ENTRY POINT
            .BYTE      3                                     ;NAME LENGTH
            .BYTE       ".D2            "
            .BYTE      $80                                   ;DEVNUM: ACTIVE
            .BYTE      0                                     ;SLOT
            .BYTE      1                                     ;UNIT NUMBER
            .BYTE      $E1,$01,$00                           ;TYPE,SUB,FILLER
            .WORD      280                                   ;BLOCKCOUNT
            .WORD      1                                     ;MANUFACTURER=APPLE
            .WORD      $1200                                 ;VERSION=1.2
;
DIB3:                                                        ;DIB FOR .D3
            .WORD      DIB4                                  ;FLINK
            .WORD      MAIN                                  ;ENTRY POINT
            .BYTE      3                                     ;NAME LENGTH
            .BYTE       ".D3            "
            .BYTE      $80                                   ;DEVNUM: ACTIVE
            .BYTE      0                                     ;SLOT
            .BYTE      2                                     ;UNIT NUMBER
            .BYTE      $E1,$01,$00                           ;TYPE,SUB,FILLER
            .WORD      280                                   ;BLOCKCOUNT
            .WORD      1                                     ;MANUFACTURER=APPLE
            .WORD      $1200                                 ;VERSION=1.2
;
DIB4:                                                        ;DIB FOR .D4
            .WORD      0                                     ;NO FLINK
            .WORD      MAIN                                  ;ENTRY POINT
            .BYTE      3                                     ;NAME LENGTH
            .BYTE       ".D4            "
            .BYTE      $80                                   ;DEVNUM: ACTIVE
            .BYTE      0                                     ;SLOT
            .BYTE      3                                     ;UNIT NUMBER
            .BYTE      $E1,$01,$00                           ;TYPE,SUB,FILLER
            .WORD      280                                   ;BLOCKCOUNT
            .WORD      1                                     ;MANUFACTURER=APPLE
            .WORD      $1100                                 ;VERSION=1.1
            .WORD      1                                     ;MANUFACTURER=APPLE
            .WORD      $1200                                 ;VERSION=1.2
;PAGE
; GENERAL DATA:
;
PREVUNIT:   .RES       1                                     ;PRIOR UNIT ACCESSED (FOR REPEAT)
PREVCMD:    .RES       1                                     ;PRIOR CMD (FOR REPEAT)
;
ESAVE:      .RES       1                                     ;SAVED E.REG
VBLSAVE:    .RES       1                                     ;SAVED E.IER
INITFLAG:   .BYTE      0                                     ;<0 IS INITTED
;
MTIMES:     .BYTE      T200MS,T1SEC,T50MS                    ;READ,WRITE,SENSE
;
;***************************************************************************************************
; DRIVE TABLES:
;
DRIVESEL:   .RES     4                                     ;NONZERO IF SELECTED
;
UPTIME:     .RES     4                                     ;MOTOR RUNTIME SINCE STARTED
DRVTRACK:   .RES     4                                     ;CURRENT HEAD POSITION
;PAGE

; MAIN ENTRY POINT:
;
; DISABLE NMI/RESET AND ENABLE ROM/IO SPACE
;
MAIN:
            LDA        E_REG                                 ;SAVE CALLER'S
            AND        #$FF-$20                              ;DROP SCREEN BIT
            STA        ESAVE                                 ; ENVIRONMENT
            LDA        E_REG                                 ;GET EREG AGAIN
            AND        #$FF-$10                              ;DISABLE NMI/RESET

            ORA        #$03                                  ;ENABLE ROM/IO SPACE
            STA        E_REG
;
            LDA        NOSCROLL                              ;DISABLE SMOOTHSCROLL
;
            PHP                                              ;IF ALREADY SEI'D, THEN WE
            PLA                                              ; STAY THAT WAY...
            ROR        A
            ROR        A
            ROR        A
            ROR        A
            STA        IRQMASK                               ;'I' BIT INTO BIT7
;
; MAKE SURE WE HAVE A VALID COMMAND:
;
            LDA        D_COMMAND                             ;GET IT
            BMI        BADCMD                                ;=>WOW!
            BEQ        IOSETUP                               ;=>ZERO IS A READ
            CMP        #10                                   ;OFF THE END?
            BCS        BADCMD                                ;=>YES
            CMP        #9                                    ;REPEAT?
            BNE        CMD1                                  ;=>NOPE
;
; REPEAT. SIMPLY GET PRIOR COMMAND:
;
            LDA        PREVUNIT                              ;IS THIS REPEAT FOR
            CMP        D_UNITNUM                             ; SAME UNIT?
            BNE        BADOP                                 ;=>NO? ILLEGAL!
            LDA        PREVCMD                               ;YES, SET COMMAND
            BEQ        RPTOK                                 ;=>REPEAT'ED READ IS OK
            CMP        #1                                    ;IF NOT, IS IT REPEAT'ED WRITE?
            BNE        BADOP                                 ;=>CAN'T REPEAT OTHER COMMANDS
RPTOK:
            STA        D_COMMAND                             ;SAME AS BEFORE
            CMP        #0                                    ;READ?
            BEQ        IOSETUP                               ;=>YES
; NOW REPEAT GOES LIKE OTHERS:
;
;
CMD1:
            CMP        #1                                    ;WRITE?
            BNE        CMD2                                  ;=>NOPE
            JMP        IOSETUP                               ;=>YES
CMD2:
            CMP        #2                                    ;STATUS?
            BNE        CMD3                                  ;=>NOT STATUS
            LDA        D_STATCODE                            ;IS IT 'SENSE'?
            BEQ        GOSTAT                                ;=>YES
            LDA        #XCTLCODE                             ;ILLEGAL CODE
            JMP        EXIT
GOSTAT:
            JMP        DRVSETUP                              ;=>YES
;
CMD3:
            CMP        #8                                    ;INIT?
            BNE        BADOP                                 ;=>NOPE
            JMP        INIT                                  ;=>YES, DO INIT
;
BADOP:
            LDA        #XBADOP                               ;ILLEGAL COMMAND
            JMP        EXIT                                  ;BACK TO YOU
;
BADCMD:
            LDA        #XREQCODE                             ;INVALID COMMAND
            JMP        EXIT                                  ;BACK TO YOU
;PAGE
; SETUP WHAT WE HAVE TO BEFORE
;  PERFORMING THE I/O OPERATION:
;
IOSETUP:
            LDA        D_BLOCK+1                             ;VALIDATE BLOCKNUM
            BEQ        CHKBYTE                               ;=> IF <256, IT'S OK
            CMP        #2                                    ;IS IT <512?
            BCS        BADBLOCK                              ;=>BAD BOY!
            LDA        D_BLOCK                               ;YES, CHECK LO HALF
            CMP        #280-256                              ; FOR RANGE
            BCC        CHKBYTE                               ;=>IT'S OK
BADBLOCK:
            LDA        #XBLKNUM                              ;BAD BLOCK NUMBER
            JMP        EXIT                                  ;RETURN BAD NEWS
;
CHKBYTE:
            LDA        D_BYTES                               ;GET LO COUNT
            BNE        BADCOUNT                              ;=>ERR, NOT INTEGRAL BLOCK(S)
            LDA        D_BYTES+1                             ;GET HI COUNT
            LSR        A                                     ;MAKE BLOCK COUNT
            BCS        BADCOUNT                              ;=>BAD IF HALF-BLOCK COUNT
            STA        BLKCOUNT                              ;SAVE COUNT OF BLOCKS
;
; DOES REQUESTED BYTECOUNT CAUSE US
;  TO RUN OFF END OF DISK?
;
            LDA        BLKCOUNT                              ;NO. ADD STARTBLOCK
            CLC                                              ; AND BLKCOUNT AND SEE
            ADC        D_BLOCK                               ;  IF WE'RE TOO BIG
            LDX        D_BLOCK+1                             ;DID IT START OUT > 255?
            BNE        BLKG255                               ;=>YES
            BCC        DRVSETUP                              ;=>DEFINITELY < 256
            BCS        CHKLO                                 ;=>IF CARRY,THEN >256
BLKG255:
            BCS        BADCOUNT                              ;>255+CARRY IS NOW >511
CHKLO:
            CMP        #280-256+1                            ;281..511 ?
            BCC        DRVSETUP                              ;=>NO, WE ARE OK
BADCOUNT:
            LDA        #XBYTECNT                             ;ILLEGAL BYTECOUNT
            JMP        EXIT                                  ;SORRY...
;PAGE
;
; SELECT THE APPROPRIATE DRIVE:
;
DRVSETUP:
            LDA        D_COMMAND                             ;SAVE THIS COMMAND
            STA        PREVCMD                               ; AND DEVICE FOR
            LDA        D_UNITNUM                             ;  SUBSEQUENT
            STA        PREVUNIT                              ;   'REPEAT' CALL
            LDA        E_REG                                 ;DOWNSHIFT TO
            ORA        #$80                                  ; 1MHZ FOR REMAINDER
            STA        E_REG                                 ;  OF DRIVER EXECUTION
            JSR        UNITSEL                               ;SELECT & START IT
;
; SEE IF THE MOTOR STARTED. IF NOT,
;  THEN IT'S EITHER DISKSWITCH OR NODRIVE.
;
            JSR        CHKDRV                                ;MOTOR RUNNING?
            BNE        DOIO                                  ;=>YES, GREAT.
;
; IF WE GET A MOTOR WHEN WE MOVE
;  THE HEAD, THEN IT'S DISKSWITCH.
;
            LDX        D_UNITNUM                             ;FORCE HEAD MOTION
            INC        DRVTRACK,X                            ; EVEN IF ALREADY ON ZERO
            INC        DRVTRACK,X                            ;GIVE HIM A FIRM KNOCKER
            LDA        #0                                    ;SEEK TO TRACK ZERO
            JSR        MYSEEK                                ; FOR BFM DIR READ
            JSR        CHKDRV                                ;RUNNING NOW?
            BNE        DSWITCH                               ;=>YES, A SWITCHEROO
            LDA        #0
            LDY        D_UNITNUM                             ;FORGET THAT THIS
            STA        DRIVESEL,Y                            ; DRIVE WAS 'SELECTED'
            LDA        #XNODRIVE                             ;NO, A MISSING DRIVE!
            JMP        EXIT
;
DSWITCH:
            LDA        #XDISKSW                              ;USER PULLED A FAST ONE
            JMP        EXIT                                  ; BUT HE CAN'T FOOL US.
;PAGE
; PREPARE TO DO THE OPERATION:
;
DOIO:
            LDA        D_BUFL                                ;COPY USER BUFFER
            STA        BUFTEMP                               ; AND BLOCK NUMBER
            LDA        D_BUFH                                ;  TO OUR WORKSPACE
            STA        BUFTEMP+1
            LDA        $1400+D_BUFH
            STA        $1400+BUFTEMP+1
            LDA        D_BLOCK
            STA        BLKTEMP
            LDA        D_BLOCK+1
            STA        BLKTEMP+1
;
; IF CALLER GAVE US A COUNT OF ZERO BYTES,
;  THEN WE'RE ALL DONE!
;
            LDA        D_COMMAND                             ;IS IT STATUS?
            CMP        #2                                    ;IF SO, THEN BYTECOUNT
            BNE        DOIO2                                 ; IS MEANINGLESS
            JMP        STATUS
DOIO2:
            LDY        BLKCOUNT                              ;BLKS=0?
            BEQ        READOK                                ;=>YES, YOU GET GOOD RETURN
            CMP        #0                                    ;READ COMMAND?
            BEQ        READREQ                               ;=>YES
            JMP        WRITEREQ
;PAGE
;***************************************************************************************************
;  -- READ --
;***************************************************************************************************
READREQ:
            LDA        #0                                    ;CLEAR COUNT OF
            LDY        #0
            STA        (D_BYTRD),Y                           ; BYTES READ
            INY
            STA        (D_BYTRD),Y
READREQ2:
            JSR        BLK2SECT                              ;COMPUTE TRK/SECTOR THIS BLOCK
;
            JSR        SECTORIO                              ;READ IT PLEASE
            BCS        READERR                               ;=>WE LOSE.
            INC        SECTOR                                ;BUMP TO NEXT
            INC        SECTOR                                ; LOGICAL SECTOR
            INC        BUF+1                                 ;BUMP SECTOR BUFFER
            JSR        SECTORIO                              ;READ IT TOO
            BCS        READERR                               ;=>WE LOSE.
            LDY        #1
            LDA        (D_BYTRD),Y                           ;BUMP COUNT OF
            CLC
            ADC        #2
            STA        (D_BYTRD),Y                           ; BYTES READ
;
; MORE BLOCKS TO GO?
;
            JSR        MOREBLKS                              ;SETUP FOR NEXT BLOCK
            BNE        READREQ2                              ;=>MORE TO READ...
READOK:
            LDA        #0                                    ;GOOD RETURN
            JMP        EXIT                                  ;TELL HAPPY USER
;
READERR:
            JMP        EXIT                                  ;RETURN ERROR CODE
;PAGE
;***************************************************************************************************
; --- WRITE ---
;***************************************************************************************************
;
WRITEREQ:
            JSR        BLK2SECT                              ;COMPUTE TRK/SECTOR THIS BLOCK
            LDA        E_REG                                 ;SET 2 MHZ
            AND        #$7F
            STA        E_REG
            JSR        PRENIB                                ;PRENIBBLIZE FOR WRITE
            JSR        SECTORIO                              ;WRITE IT OUT...
            BCS        WRITERR                               ;=>SOMETHING'S WRONG
;
            INC        SECTOR                                ;BUMP TO NEXT
            INC        SECTOR                                ; LOGICAL SECTOR
            INC        BUF+1                                 ;BUMP SECTOR BUFFER ADDRESS
            LDA        E_REG                                 ;SET 2 MHZ
            AND        #$7F
            STA        E_REG
            JSR        PRENIB                                ;PRENIBBLIZE FOR WRITE
            JSR        SECTORIO                              ;WRITE IT OUT
            BCS        WRITERR                               ;=>SOMETHING'S WRONG
;
; MORE BYTES TO DO?
;
            JSR        MOREBLKS                              ;SETUP FOR NEXT
            BNE        WRITEREQ                              ;=>MORE TO DO
            LDA        #0                                    ;GOOD RETURN
            JMP        EXIT
;
WRITERR:
            JMP        EXIT                                  ;RETURN ERROR CODE
;PAGE
;***************************************************************************************************
;  --- STATUS ---
;***************************************************************************************************
;
STATUS:
            LDX        #$60                                  ;DUMMY SLOT
            LDA        Q6H,X                                 ;SENSE WRITE PROTECT
            LDA        Q7L,X
            ASL        A                                     ;PRESERVE IT IN CARRY
            LDA        Q6L,X                                 ;BACK TO READ MODE
            LDA        #0                                    ;NOW MOVE BIT TO
            ROL        A                                     ; PROPER POSITION
            ROL        A                                     ; (002)
            LDY        #0
            STA        (D_STATBUF),Y                         ;RETURN IT
            LDA        #0                                    ;GOOD RETURN
            JMP        EXIT                                  ;DONE
;PAGE
;***************************************************************************************************
; --- INIT ---
;***************************************************************************************************
;
INIT:
            LDA        INITFLAG                              ;INIT'ED YET?
            BMI        GOODINIT                              ;=>YES, DONE
;
            LDA        #$60                                  ;SETUP SLOT FOR
            STA        IBSLOT                                ; CORE ROUTINES
            LDA        #$FF                                  ;PREVENT SECOND
            STA        INITFLAG                              ; INIT
            LDA        #0                                    ;CLEAR STUFF OUT
            STA        PREVUNIT                              ;SOSBOOT JUST USED .D1
            LDY        #4
CLRDRVS:
            LDA        #0
            STA        DRIVESEL-1,Y                          ;NOBODY SELECTED
            STA        UPTIME-1,Y                            ;ALL OFF
            STA        DRVTRACK-1,Y
            DEY
            BNE        CLRDRVS
;
; SET UP .D1 SINCE LOADER'S USING IT:
;
            LDA        E_REG                                 ;SET 1MHZ FOR THE
            ORA        #$80                                  ; STATEMACHINE I/O
            STA        E_REG
            JSR        CHKDRV                                ;IS .D1 MOTOR SPINNING?
            BEQ        INIT2                                 ;=>NO, MOTOR'S OFF
            LDA        #T200MS                               ;UPTIME GOOD FOR READS
            STA        UPTIME+0
INIT2:
            LDA        #1
            STA        DRIVESEL+0                            ;.D1 IS THE CURRENT DRIVE
            LDA        $0300+CURTRK                          ;RETRIEVE CURRENT TRACK
            STA        DRVTRACK+0                            ;REMEMBER IT
;
; SET UP JMP TABLE FOR CORRECT ROM:
;
GOODINIT:
            LDA        #0                                    ;RETCODE=GOOD, IF YOU CARE
            CLC                                              ;SAY 'GOOD INIT'
            BCC        EXIT                                  ;(ALWAYS TAKEN)
;PAGE
;***************************************************************************************************
; -- EXIT PATH --
;***************************************************************************************************
;
EXIT:
            PHA                                              ;SAVE RETURN CODE
;
; UPDATE UPTIME BY 50 MS (3 SECTOR-TIMES)
;  TO ACCOUNT FOR READ/WRITE TIME:
;
            LDA        D_COMMAND                             ;GET COMMAND
            CMP        #2                                    ;SENSE OR INIT?
            BCS        EXIT2                                 ;=>YES, NO TIME USED UP
            LDA        #2                                    ;TIME=50 MS (2 UNITS)
            JSR        ADDTIME                               ;BUMP UPTIME(S)
;
; RESTORE CALLER ENVIRONMENT:
;
EXIT2:
            LDA        E_REG                                 ;GET CURRENT STATE
            AND        #$20                                  ; OF THE SCREEN
            ORA        ESAVE                                 ;MERGE WITH CALLER STATE
            STA        E_REG
            JSR        FIXIRQ                                ;RE-ENABLE IRQ IF OK
            LDA        MOTOROFF                              ;START MOTOR-OFF TIMEOUT
            PLA                                              ;RESTORE RETURN CODE
            BNE        GOERR                                 ;=>ERROR RETURN VIA SYSERR
            CLC
            RTS                                              ;GOOD RETURN W/CARRY CLEAR
GOERR:
            JSR        SYSERR                                ;RETURN VIA SYSERR
;PAGE
;***************************************************************************************************
; NAME    : SECTORIO
; FUNCTION: READ OR WRITE A SECTOR
; INPUT   : IBSTRK, IBSECT, MONTIME,
; RETURNS : CARRY CLEAR IF OK (AC=00)
;         : CARRY SET   IF ERROR (AC=ERRCODE)
;         : SEEKWAIT  ALL SETUP
; DESTROYS: ALL REGISTERS
;***************************************************************************************************
;
SECTORIO:
            LDA        #R_RECAL                              ;SETUP THE
; R_RECAL MUST BE NON-ZERO!! (SEE BELOW)
            STA        RECALCNT                              ; RECAL TRIES
            NOP                                              ; PAD ONE BYTE
            STA        E1908                                 ; A-REG MUST BE NON-ZERO !!!
; E1908 = NON-ZERO LOCKOUT MOUSE
;
            LDY        D_UNITNUM                             ;ARE WE ON-TRACK?
            LDA        TRACK
            CMP        DRVTRACK,Y
            BEQ        SOUGHT                                ;=>IF SO, FORGET SEEK & DELAY!
;
; WAIT BEFORE STEPPING:
;
            LDA        SEEKWAIT                              ;SEEK DELAY NEEDED?
            BEQ        GOSEEK                                ;=>NAW...
            LDA        #0
            STA        SEEKWAIT                              ;CLEAR THE FLAG
            LDA        #4                                    ;ADD SEEKDELAY TO
            JSR        ADDTIME                               ; THE TOTAL UPTIME(S)
            TAY                                              ;4*25 MS DELAY
SEEKDEL:
            LDA        #0
            JSR        MSWAIT
            DEY
            BNE        SEEKDEL
;
; ISSUE THE SEEK:
;
GOSEEK:
            LDA        TRACK                                 ;GET DESTINATION TRACK
            JSR        MYSEEK                                ;=>..AND YOU SHALL FIND...
;
SOUGHT:
            LDA        IRQMASK                               ;SET IRQ MASK FOR
            STA        IMASK                                 ; CORE ROUTINES
            LDA        #R_IRQ                                ;SETUP IRQ RETRIES
            STA        INTRTRY
            LDA        #R_IOERR                              ; AND ERROR RETRIES
            STA        RETRYCNT
;
; DELAY FOR ANY REMAINING MOTOR-UP TIME:
;
MDELAY:
            LDA        MONTIMEH                              ;ANY TIME REMAINING?
            BPL        FINDIT                                ;=>NO, WE'RE UP TO SPEED.
            LDA        #1                                    ;YES, SO BUMP A SLICE OF
            JSR        ADDTIME                               ; UPTIME WHILE WE WAIT
            LDA        #0
            JSR        MSWAIT
            JMP        MDELAY                                ;=>GO TILL ENOUGH
;
; FIND THE DESIRED SECTOR:
;
; NOTE: FINDSECT RETURNS WITH
;       IRQ INHIBITED!
;
FINDIT:
            PHP                                              ;INHIBIT IRQ WHILE
            SEI                                              ; MESSING WITH VBL FLAGS
            LDA        E_IER                                 ;DISABLE VBL IRQ
            AND        #$18                                  ; DURING SECTOR I/O
            STA        E_IER
            ORA        #$80                                  ;FOR 'SET' LATER
            STA        VBLSAVE
            PLP                                              ;RESTORE IRQ STATUS
            JSR        FINDSECT                              ;FIND ME PLEASE
            BCS        TRYRECAL                              ;=>NO? RECAL OR GIVE UP!
            LDX        #$60                                  ;SET UP SLOT FOR CORE RTNS
            LDA        D_COMMAND                             ;WHAT'S YOUR PLEASURE?
            BNE        SIOWRITE                              ;=>WRITE
;
;***************************************************************************************************
; READ A SECTOR:
;
            JSR        READ                                  ;READ THAT SECTOR
            JSR        FIXIRQ                                ;ENABLE IRQ IF OK
            LDA        VBLSAVE                               ;ALLOW VBL DURING
            STA        E_IER                                 ; POSTNIB
            BCS        BADIO                                 ;=>I/O ERR OR IRQ
            LDA        E_REG                                 ;SET 2MHZ FOR POSTNIB
            AND        #$7F
            STA        E_REG
            JSR        POSTNIB                               ;POSTNIB/CHECKSUM
            BCS        IORETRY                               ;=>I/O ERR:BAD CHKSUM
            JMP        SIOGOOD                               ;=>GOOD READ
;
;***************************************************************************************************
; WRITE A SECTOR:
;
SIOWRITE:
            JSR        WRITE                                 ;WRITE THE DATA
            JSR        FIXIRQ                                ;RE-ENABLE IRQ IF OK
            LDA        VBLSAVE                               ;RESTORE
            STA        E_IER                                 ; VBL IRQ
            BCC        SIOGOOD                               ;=>GOOD WRITE
            BVC        SIOWPROT                              ;=>WRITE PROTECTED
;
;***************************************************************************************************
; IT DIDN'T GO WELL FOR US:
;
BADIO:
            BVS        FINDIT                                ;=>IRQ. JUST RETRY IT.
;
; RETRY AFTER AN I/O ERROR:
;
IORETRY:
            DEC        RETRYCNT                              ;ANY RETRIES LEFT?
            BNE        FINDIT                                ;=>YEAH, RETRY AFTER ERROR
;
; RETRIES EXHAUSTED. RECALIBRATE:
;
TRYRECAL:
            LDA        VBLSAVE                               ;ALLOW VBL IF RECAL
            STA        E_IER                                 ; OR UNRECOVERABLE ERROR
            DEC        RECALCNT                              ;HAVE WE RECALIBRATED YET?
            BMI        SIOERR                                ;=>YUP. WE'RE DEAD.
            JSR        RECAL                                 ;NO, TRY OUR LUCK
            LDY        D_UNITNUM                             ;ARE WE ON-TRACK?
            LDA        TRACK
            CMP        DRVTRACK,Y
            BNE        NOTSAME
            JMP        SOUGHT                                ;=>IF SO, FORGET RESEEK
NOTSAME:
            JMP        GOSEEK                                ;TRY AGAIN ON TARGET TRACK
;
;***************************************************************************************************
SIOERR:
            LDA        #XIOERROR                             ;RETURN CODE
            SEC                                              ;INDICATE HARD ERROR
            BCS        SIORET
SIOWPROT:
            LDA        #XNOWRITE                             ;RETURN CODE
            SEC                                              ;INDICATE HARD ERROR
            BCS        SIORET
SIOGOOD:
            LDA        #0
            CLC                                              ;INDICATE GOOD COMPLETION
SIORET:     LDX        #0                                    ; SAY OK TO MOUSE
            STX        E1908                                 ; WITH THIS GLOBAL 01908
            RTS
;PAGE
;***************************************************************************************************
; NAME    : FINDSECT
; FUNCTION: LOCATE A DESIRED SECTOR
; INPUT   : IBTRK, IBSECT SETUP
; RETURNS : CARRY CLEAR IF OK,
;         : CARRY SET   IF ERROR.
; DESTROYS: ALL REGISTERS & 'TEMP'
; NOTE    : RETURNS WITH IRQ DISABLED IF NO ERROR!
;***************************************************************************************************
;
FINDSECT:
            LDA        #R_FIND*$16                            ;SETUP NUMBER OF REVS
            STA        RETRYADR                              ; ALLOWED TO FIND SECTOR
            LSR        TEMP                                  ;COMPUTE LATENCY FIRST TIME THRU
FINDSEC2:
            LDX        #$60                                  ;FAKE SLOT FOR CORE ROUTINES
            JSR        RDADR                                 ;GET NEXT ADDRESS FIELD
            BCS        RDADERR                               ;=>UGH! AN ERROR!
;
; MAKE SURE WE'RE ON THE CORRECT TRACK:
;
            LDA        TRACK                                 ;IS IT
            CMP        CSSTV+2                               ; CORRECT TRACK?
            BNE        FINDERR                               ;=>NO?!? IT'S USELESS!
            LDA        SECTOR                                ;IS IT
            CMP        CSSTV+1                               ; DESIRED SECTOR?
            BEQ        FINDGOOD                              ;=>YEAH. GOT IT!
;
; COMPUTE LATENCY. EACH TWO-SECTOR
;  DISTANCE IS 25 MS OF UPTIME.
;
            LDA        TEMP                                  ;LATENCY ALREADY COMPUTED?
            BMI        RDADERR                               ;=>YES.
            LDA        SECTOR                                ;HOW FAR AWAY IS OUR
            SEC                                              ; DESIRED SECTOR?
            ROR        TEMP                                  ;PREVENT RECOMPUTATION
            SBC        CSSTV+1
            AND        #$0F
            LSR        A                                     ;EACH 2-SECTORS IS 25 MS
            JSR        ADDTIME
;
; KEEP LOOKING TILL WE FIND IT:
;
RDADERR:
            JSR        FIXIRQ                                ;ENABLE IRQ IF APPROPRIATE
            DEC        RETRYADR                              ;ANY RETRIES LEFT?
            BEQ        FINDERR                               ;=>NO, WE CAN'T FIND IT.
;
; COMPENSATE FOR A BUG IN RDADR: IF WE TRY
;  TO CALL RDADR AGAIN BEFORE THE DATA MARK
;  GOES BY, THEN RDADR WILL ACCIDENTALLY CALL
;  THAT AN ERROR. WE CAN AVOID THIS 'FAKE'
;  ERROR BY DELAYING PAST THE DATA MARK.
            LDY        #200                                  ;1 MS IS PLENTY
ADRDELAY:
            DEY
            BNE        ADRDELAY
            JMP        FINDSEC2                              ;=>NOW TRY LOOKING AGAIN
;
;***************************************************************************************************
FINDGOOD:
            LDA        #0                                    ;CLEAR VOLNUM OUT OF
            STA        MONTIMEH                              ; MOTORTIME!
            CLC                                              ;INDICATE NO ERROR
            RTS
;
FINDERR:
            JSR        FIXIRQ                                ;ENABLE IRQ IF APPROPRIATE
            LDA        #0                                    ;CLEAR VOLNUM OUT OF
            STA        MONTIMEH                              ; MOTORTIME!
            SEC                                              ;INDICATE THE ERROR
            RTS
;PAGE
;***************************************************************************************************
; NAME    : UNITSEL
; FUNCTION: SELECT & START A DRIVE,
;           SET UP MOTOR & SEEK DELAYS
; INPUT   : NONE
; OUTPUT  : MONTIME,SEEKTIME
; DESTROYS: ALL REGISTERS
;***************************************************************************************************
;
UNITSEL:
            LDY        D_UNITNUM                             ;GET DRIVENUM
            LDA        #0                                    ;ASSUME NO SEEKWAIT
            STA        SEEKWAIT                              ; WILL BE NEEDED
            STA        MONTIMEL                              ;CLEAR MONTIME
            STA        MONTIMEH
;
; SEE IF MOTOR(S) STILL SPINNING:
;
            JSR        CHKDRV                                ;MOTOR(S) POWERED UP?
            BNE        SPINNING                              ;=>YES. WHO IS IT?
;
; NO MOTOR(S) SPINNING. DESELECT
;  ALL MOTORS AND START AFRESH:
;
            LDX        MD_INT                                ;DESELECT ALL
            LDA        #0                                    ;SHOW INTERNAL AS
            STA        DRIVESEL+0                            ; NOT SELECTED
            STA        UPTIME+0                              ;INDICATE DRIVE IS FULLY STOPPED
            JSR        EXTDESEL                              ;DESELECT ALL EXTERNALS TOO
            JMP        SETTIME                               ;GO SETUP MOTOR DELAY
;***************************************************************************************************
; MOTOR(S) SPINNING: OURS?
;
SPINNING:
            LDA        DRIVESEL,Y                            ;HAD WE BEEN SELECTED?
            BNE        GOFORIT                               ;=>YES, GO FOR IT RIGHT AWAY.
;
; WE AREN'T SPINNING. SHUTDOWN ANOTHER
;  DRIVE, IF NECESSARY, TO GET GOING:
;
            CPY        #0                                    ;ARE WE THE INTERNAL DRIVE?
            BEQ        SETTIME                               ;=>YES, LEAVE EXT MOTOR ALONE
;
; WE'RE AN EXTERNAL DRIVE. STOP ALL EXTERNAL MOTORS
;  UNCONDITIONALLY, BUT LEAVE THE INTERNAL MOTOR ALONE.
; IF WE *DID* HAVE TO STOP ANOTHER EXTERNAL, THEN
;  MAKE SURE WE SET THE CORRECT PRE-SEEK DELAY!
;
            LDA        #0                                    ;SEE IF ANOTHER EXTERNAL
            ORA        DRIVESEL+3                            ; HAD BEEN
            ORA        DRIVESEL+2                            ;  SELECTED
            ORA        DRIVESEL+1                            ;   BEFORE...
            BEQ        SETTIME                               ;=>NO, SEEK DELAY IS UNNECESSARY
            INC        SEEKWAIT                              ;YES, DELAY BEFORE STEPPING
            JSR        EXTDESEL                              ;DESELECT ALL EXTERNALS
            JMP        SETTIME                               ;=>GO SETUP MOTOR DELAY
;PAGE
;***************************************************************************************************
; OUR DRIVE IS SPINNING. GO FOR IT!
; DEPENDING OF HOW LONG THE MOTOR'S BEEN ON,
;  THIS COMMAND MAY REQUIRE A MOTOR DELAY.
;
GOFORIT:
            LDX        D_COMMAND                             ;GET CURRENT COMMAND
            LDA        MTIMES,X                              ;GET REQUIRED UPTIME FOR IT
            SEC
            SBC        UPTIME,Y                              ;DRIVE RUNNING LONG ENOUGH?
            BCS        SELECT                                ;=>NO, AC NOW HAS DELTA-T
            LDA        #0                                    ;OTHERWISE, WAIT=0
            JMP        SELECT                                ;SET MONTIME & SELECT DRIVE
;***************************************************************************************************
;
; ALL MOTORS WERE OFF. CHOOSE THE
;  APPROPRIATE MOTOR-ON TIME:
;
SETTIME:
            LDA        #0                                    ;INDICATE THAT
            STA        UPTIME,Y                              ; THE DRIVE WAS OFF
            LDX        D_COMMAND                             ;GET CURRENT COMMAND
            LDA        MTIMES,X                              ;GET CORRECT DELAY TIME
;***************************************************************************************************
;
; SELECT THE DRIVE & START IT:
;
SELECT:
            STA        MONTIMEH                              ;NEGATE IT BECAUSE
            LDA        #0                                    ; IT GETS INCREMENTED
            SEC                                              ;  INSTEAD OF
            SBC        MONTIMEH                              ;   DECREMENTED
            STA        MONTIMEH                              ;STUFF MOTOR DELAY
            CPY        #1                                    ;ARE WE THE INTERNAL DRIVE?
            BCS        SELEXT                                ;=>NO, AN EXTERNAL
            LDA        IS_INT                                ;I/O SELECT INTERNAL
            LDA        MS_INT                                ;MOTOR SELECT INTERNAL
            JMP        UNITRET                               ;=>ALL DONE!
;
SELEXT:
            LDA        IS_EXT                                ;I/O SELECT EXTERNAL
            CPY        #2                                    ;ARE WE 2, 3, OR 4 ?
            BCS        NOTD2                                 ;=>DEFINITELY 3 OR 4
            LDA        MD_EXT1                               ;MOTOR SELECT
            LDA        MS_EXT2                               ; ONLY .D2
            JMP        UNITRET                               ;=>ALL DONE!
;
NOTD2:
            BNE        ISD4                                  ;=>DEFINITELY NOT 3
            LDA        MS_EXT1                               ;MOTOR SELECT
            LDA        MD_EXT2                               ; ONLY .D3
            JMP        UNITRET                               ;=>ALL DONE!
;
ISD4:
            LDA        MS_EXT1                               ;MOTOR SELECT
            LDA        MS_EXT2                               ; ONLY .D4
;
;
UNITRET:
            LDA        MOTORON                               ;PROVIDE MOTOR POWER
            LDA        #1                                    ;SAY WE'VE SELECTED
            STA        DRIVESEL,Y                            ; THIS DRIVE
;
; IF WE HAVE MOTORTIME TO BURN,
;  THEN DELAY 50 MS. THIS ENSURES
;  A GOOD SOLID CHKDRV AFTER
;  TURNING ON THE MOTOR.
;
            LDA        MONTIMEH                              ;ANY MOTORTIME?
            BPL        UNITRTS                               ;=>NO, WE GO FOR IT.
            LDY        #5                                    ;5*10 MS
UNITDEL:
            LDA        #100                                  ;100*100US IS 10MS
            JSR        MSWAIT
            DEY
            BNE        UNITDEL
            LDA        #2                                    ;INCLUDE THE 50MS
            JSR        ADDTIME                               ; IN MOTOR UPTIME(S)
UNITRTS:
            RTS
;SKP 5
;***************************************************************************************************
; NAME    : EXTDESEL
; FUNCTION: DESELECT ALL EXTERNAL DRIVE MOTORS
; INPUT   : NONE
; DESTROYS: AC,X
;***************************************************************************************************
;
EXTDESEL:
            LDA        MD_EXT1                               ;DESELECT ALL EXTERNAL
            LDA        MD_EXT2                               ; DRIVE MOTORS
            LDX        #3                                    ;SHOW THAT THEY ARE
            LDA        #0                                    ; ARE ALL DEAD DUCKS
EDS1:       STA        DRIVESEL,X
            STA        UPTIME,X                              ;DRIVE MOTORS ARE OFF
            DEX
            BNE        EDS1
            RTS
;PAGE
;***************************************************************************************************
; NAME    : CHKDRV
; FUNCTION: CHECK IF MOTOR(S) RUNNING
; INPUT   : NONE
; RETURNS : 'BNE' IF RUNNING
;         : 'BEQ' IF NOT
; DESTROYS: AC,X
;***************************************************************************************************
; NOTES: DUE TO A FLOATING PIN, THERE
;  COULD BE A GLITCH WHICH CAUSES THE
;  SHIFTER TO 'FLASH' ONTO THE BUS
;  INSTEAD OF ALWAYS BEING TRISTATED.
;  THIS COULD CAUSE CHKDRV TO THINK
;  THAT THE MOTOR IS SPINNING WHEN IT
;  IS NOT. THUS WE WILL SAMPLE THE SHIFTER
;  FOR 40 US AT 6-US INTERVALS. IF, AFTER
;  THREE (3) CONSECUTIVE PASSES, ANY OF
;  THE PASSES SEES A 'LOCKED' SHIFTER,
;  THEN WE SAY THE DRIVE IS STOPPED.
;
;
CHKDRV:
            LDX        #3                                    ;CHECK SHIFTER SEVERAL TIMES
CHKD1:
            LDA        Q6L+$60                               ;GET DATA
            CMP        Q6L+$60                               ;HAS IT CHANGED?
            BNE        CHANGED                               ;=>YES
            CMP        Q6L+$60                               ;HAS IT CHANGED?
            BNE        CHANGED                               ;=>YES
            CMP        Q6L+$60                               ;HAS IT CHANGED?
            BNE        CHANGED                               ;=>YES
            CMP        Q6L+$60                               ;HAS IT CHANGED?
            BNE        CHANGED                               ;=>YES
            CMP        Q6L+$60                               ;HAS IT CHANGED?
            BNE        CHANGED                               ;=>YES
            CMP        Q6L+$60                               ;HAS IT CHANGED?
            BNE        CHANGED                               ;=>YES
            CMP        Q6L+$60                               ;HAS IT CHANGED?
            BNE        CHANGED                               ;=>YES
            RTS                                              ;IF EVER LOCKED, IT'S STOPPED
;
CHANGED:
            DEX
            BNE        CHKD1                                 ;TRY SEVERAL TIMES
            DEX                                              ;SET CC=BNE
            RTS                                              ;RETURN ZFLAG APPROPRIATELY
;PAGE
;***************************************************************************************************
; NAME    : ADDTIME
; FUNCTION: ADD TO MOTOR UPTIME(S)
; INPUT   : AC=NO. OF 25 MS INCREMENTS
; DESTROYS: Y
;***************************************************************************************************
;
ADDTIME:
            PHA                                              ;PRESERVE AC
            LDY        #4                                    ;TABLE INDEX/COUNT
ADD2:
            LDA        DRIVESEL-1,Y                          ;IS IT SELECTED?
            BEQ        ADD3                                  ;=>NOPE
            PLA
            PHA                                              ;RECOVER DELTA-T
            CLC
            ADC        UPTIME-1,Y                            ;ADD TO MOTOR UPTIME
            CMP        #T1SEC+2                              ;IS IT AT MAX TIME?
            BCC        ADD2A                                 ;=>NO, STORE NEW TIME
            LDA        #T1SEC+1                              ;YES, SET TO >1 SEC
ADD2A:
            STA        UPTIME-1,Y
ADD3:
            DEY
            BNE        ADD2                                  ;=>DO ALL 4 DRIVES
;
            PLA                                              ;RESTORE AC
            RTS
;PAGE
;***************************************************************************************************
; NAME    : RECAL
; FUNCTION: RECALIBRATE DRIVE HEAD
; INPUT   : NONE
; DESTROYS: ALL REGISTERS
; NOTE    : A 'QUIET' RECALIBRATE IS DONE
;         : USING TWO ITERATIONS. IF WE ARE
;         : LOST, THEN SEEK 48-TRACKS
;         : TOWARD TRACK ZERO. IF WE KNOW
;         : WHAT TRACK WE'RE CURRENTLY
;         : ON (+- 1/2 TRACK), THEN JUST
;         : ADD A LITTLE EXTRA AND SEEK
;         : TO TRACK ZERO. A 48-TRACK
;         : SEEK WILL ALWAYS GET US BACK
;         : ONTO THE MEDIA, EVEN IF WE
;         : WERE "OFF THE CAM". FROM THAT
;         : POINT, THE 2ND SEEK GETS US
;         : BACK TO TRACK ZERO QUIETLY.
;***************************************************************************************************
;
RECAL:
            LDA        #2                                    ;TWO ITERATIONS, PLEASE
RECAL1:
            PHA                                              ;SAVE LOOPCOUNT
            LDX        #$60                                  ;SETUP SLOT FOR CORE RTNS
            JSR        RDADR                                 ;WHERE ARE WE?
            BCC        RECAL2                                ;=>NOW WE KNOW
            JSR        RDADR                                 ;GIVE SECOND SHOT
            BCC        RECAL2                                ;=>THAT GOT IT
            LDA        #48                                   ;LOST? TRY 48-TRACK SEEK
            JMP        RECAL3
RECAL2:
            LDA        CSSTV+2                               ;HERE'S WHERE WE ARE
            CLC                                              ;ADD SOME SO WE GET A
            ADC        #3                                    ; HARDER SEEK TO ZERO
RECAL3:
            LDY        D_UNITNUM                             ;THIS IS NOW WHERE
            STA        DRVTRACK,Y                            ; WE ARE
            JSR        FIXIRQ                                ;ENABLE IRQ IF OK
;
            LDA        #0                                    ;DESTINATION TRACK IS 00
            STA        MONTIMEH                              ;CLEAR MOTOR-UP TIME SO
            STA        MONTIMEL                              ; SEEK KNOWS HOW LONG RECAL TAKES
            JSR        MYSEEK                                ;=>SLAM IT BACK!
            PLA                                              ;HAVE WE DONE IT TWICE?
            TAY
            DEY
            TYA
            BNE        RECAL1                                ;=>DO TWO ITERATIONS
            RTS
;PAGE
;***************************************************************************************************
; NAME    : SEEKDSK3
; FUNCTION: SEEK CURRENT DRIVE
; INPUT   : AC=DESTINATION TRACK
; OUTPUT  : NONE
; DESTROYS: ALL REGISTERS
; NOTE    : MUST BE CALLED WHILE
;         :  MOTOR IS RUNNING, IN
;         :  1MHZ+ROM+IO MODE
;***************************************************************************************************
SEEKDSK3:
            LDY        PREVUNIT                              ;GET DRIVENUM
            STY        D_UNITNUM                             ;SET IT UP
            JSR        MYSEEK                                ;MOVE IT!
            RTS
;***************************************************************************************************
; NAME    : MYSEEK
; FUNCTION: SEEK TO DESIRED TRACK
; INPUT   : AC=DESTINATION TRACK
; DESTROYS: ALL REGISTERS
;***************************************************************************************************
MYSEEK:
            STA        TRKN                                  ;TEMP HOLD OF AC
            LDY        D_UNITNUM                             ;GET DRIVENUM
            LDA        DRVTRACK,Y                            ;SETUP CURRENT TRACK
            ASL        A                                     ;SET IN HALFTRACKS FOR SEEK
            STA        CURTRK                                ; FOR SEEK ROUTINE
            LDX        #$60                                  ;SET UP SLOT FOR CORE RTNS
            LDA        MONTIMEH                              ;GET STARTING MOTOR TIME
            STA        TEMP
;
; NOTE: IRQ'S WHICH SUSPEND SEEK MAY CAUSE A
;  SEEK FAILURE. WE WILL HAVE TO RECALIBRATE
;  SINCE WE WON'T BE ON-TRACK. WE CAN NOT GET
;  ON A HALFTRACK SINCE SEEK ALLOWS SETTLING
;  TIME OF THE PHASE. BECAUSE VBL IS A SERIOUS
;  OFFENDER, WE INHIBIT HIM.
;
            PHP                                              ;INHIBIT IRQ WHILE
            SEI                                              ; MESSING WITH VBL FLAGS
            LDA        E_IER
            AND        #$18
            STA        VBLSAVE
            STA        E_IER
            PLP                                              ;RESTORE IRQ STATUS
            LDA        TRKN                                  ;RESTORE DESTINATION TRACK
            STA        DRVTRACK,Y                            ;DEST IS NOW CURRENT
            ASL        A                                     ;MAKE IT IN HALFTRACKS
            JSR        SEEK                                  ;GO MOVE THE HEAD...
            LDA        VBLSAVE                               ;NOW ALLOW THAT
            ORA        #080                                  ; NASTY
            STA        E_IER                                 ;  VBL INTERRUPT
;
; COMPUTE THE TIME USED BY SEEK:
;
            LDA        MONTIMEH                              ;INCLUDE SEEKTIME IN
            SEC
            SBC        TEMP
            JSR        ADDTIME                               ; TOTAL MOTOR UPTIME(S)
            RTS
;PAGE
;***************************************************************************************************
; NAME    : BLK2SECT
; FUNCTION: COMPUTE TRACK/SECTOR FOR A BLOCK
;           AND ADJUST BUFFER ADDRESS
; INPUT   : D_BLOCK, D_BUF
; OUTPUT  : TRACK, SECTOR, D.BUF
; DESTROYS: AC,Y
;***************************************************************************************************
;
BLK2SECT:
            LDA        BLKTEMP+1                             ;GET HI BLK HALF
            ROR        A                                     ;MOVE LO BIT TO CARRY
            LDA        BLKTEMP                               ;GET LO HALF
            ROR        A                                     ;COMBINE WITH HI BIT
            LSR        A
            LSR        A                                     ;FINISH OFF DIVIDE-BY-8
            STA        TRACK                                 ;THAT'S THE TRACK
            LDA        BLKTEMP                               ;GET LO HALF AGAIN
            AND        #7
            TAY
            LDA        SECTABLE,Y                            ;GET START SECTOR
            STA        SECTOR
;
; ADJUST BUFFER ADDRESS SO THAT I/O
;  WON'T WRAPAROUND IN THE BANK:
; (THIS ALGORITHM RIPPED OFF FROM 1.0)
;
            LDA        BUFTEMP+1                             ;GET BUFFER HI ADDRESS
            LDY        $1400+BUFTEMP+1                       ; AND XTND BYTE
            CMP        #$82                                  ;IF RAM ADDR >=8200 THEN BUMP TO
            BCC        NOADJ                                 ; NEXT BANK PAIR
            CPY        #$80
            BCC        NOADJ                                 ;=>NOT USING BANKPAIR
            CPY        #$8F                                  ;SPECIAL BANK 0?
            BEQ        NOADJ                                 ;=>YES
            AND        #$7F                                  ;DROP HI ADDRESS AND
            STA        BUFTEMP+1                             ; BUMP BANK NUMBER
            INC        $1400+BUFTEMP+1
;
NOADJ:
            LDA        BUFTEMP+1                             ;COPY BUFFER ADDRESS
            STA        BUF+1                                 ; FOR PRE & POSTNIB
            LDA        BUFTEMP
            STA        BUF
            LDA        $1400+BUFTEMP+1
            STA        $1400+BUF+1
            RTS
;
SECTABLE:   .BYTE      $00,$04,$08,$0C,$01,$05,$09,$0D
;PAGE
;***************************************************************************************************
; NAME    : MOREBLKS
; FUNCTION: SETUP TO DO NEXT BLOCK
; INPUT   : NONE
; RETURNS : 'BNE' IF MORE TO DO
;         : 'BEQ' IF NO MORE TO DO
; DESTROYS:NOTHING
;***************************************************************************************************
;
MOREBLKS:
            INC        BUFTEMP+1                             ;BUMP BUFFER ADDRESS
            INC        BUFTEMP+1
            INC        BLKTEMP                               ;BUMP BLOCK NUMBER
            BNE        MORE2
            INC        BLKTEMP+1
MORE2:
            DEC        BLKCOUNT                              ;MORE BLOCKS TO GO?
            RTS                                              ;RETURN RESULT OF DEC
                                                             ;SKP 4
;***************************************************************************************************
; NAME    : FIXIRQ
; FUNCTION: ENABLE IRQ IF APPROPRIATE
; INPUT   : NONE
; DESTROYS: NOTHING
;***************************************************************************************************
;
FIXIRQ:
            PHA
            LDA        IRQMASK                               ;SHOULD IRQ BE ENABLED?
            BMI        FIXRET                                ;=>NO, LEAVE IT ALONE
            CLI                                              ;ENABLE IRQ
FIXRET:
            PLA
            RTS
;PAGE

			.ENDPROC
            .END

