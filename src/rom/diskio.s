;*********************************************************
;* APPLE /// ROM - DISK I/O ROUTINES
;* COPYRIGHT 1979 BY APPLE COMPUTER, INC.
;*********************************************************

           .setcpu "6502"
		   .segment "CODE"
		   
;           .PROC   DISKIO
           .ORG    $F000

;**************************
;     CRITICAL TIMING     *
;   REQUIRES PAGE BOUND   *
;   CONSIDERATIONS FOR    *
;   CODE AND DATA         *
;   -----CODE-----        *
;   VIRTUALLY THE ENTIRE  *
;     'WRITE' ROUTINE     *
;      MUST NOT CROSS     *
;     PAGE BOUNDARIES     *
;   CRITICAL BRANCHES IN  *
;   THE 'WRITE', 'READ',  *
;   AND 'READ ADR' SUBRS  *
;   WHICH MUST NOT CROSS  *
;   PAGE BOUNDARIES ARE   *
;   NOTED IN COMMENTS     *
;                         *
;**************************
;                         *
;   EQUATES               *
;                         *
NBUF1      =    $0200
NBUF2      =    $0302        ; (ZERO PAGE AT $300)

HRDERRS    =    $80
DVMOT      =    $E0

IBSLOT     =    $81
IBDRVN     =    IBSLOT+1
IBTRK      =    IBSLOT+2
IBSECT     =    IBSLOT+3
IBBUFP     =    IBSLOT+4     ; & 5
IBCMD      =    IBSLOT+6
IBSTAT     =    IBSLOT+7
IBSMOD     =    IBSLOT+8
CSUM       =    IBSMOD       ; USED ALSO FOR ADDRESS HEADER CKSUM
IOBPDN     =    IBSLOT+9
IMASK      =    IBSLOT+$A
CURTRK     =    IBSLOT+$B
DRVOTRK    =    CURTRK-7
; SLOT 4, DRIVE 1
; SLOT 4, DRIVE 2
; SLOT 5, DRIVE 1
; SLOT 5, DRIVE 2
; SLOT 6, DRIVE 1
; SLOT 6, DRIVE 2
RETRYCNT   =    IBSLOT+$12
SEEKCNT    =    IBSLOT+$13
BUF        =    IBSLOT+$1A
ENVTEMP    =    IBSLOT+$1E
; IBSLOT+$1F NOT USED

;**************************
;                         *
;    ----READADR----      *
;                         *
;**************************

COUNT      =    IBSLOT+$14   ; 'MUST FIND' COUNT.
LAST       =    IBSLOT+$14   ; 'ODD BIT' NIBLS.
CKSUM      =    IBSLOT+$15   ; CHECKSUM BYTE.
CSSTV      =    IBSLOT+$16   ; FOUR BYTES
;          CHECKSUM, SECTOR, TRACK, AND VOLUME.
;
;**************************
;                         *
;                         *
;                         *
;   USES ALL NBUFS        *
;   AND 32-BYTE           *
;   DATA TABLE    'NISI,' *
;                         *
;**************************
;
;**************************
;                         *
;                         *
;                         *
;    USES ALL NBUFS       *
;    USES LAST 54 BYTES   *
;    OF A CODE PAGE FOR   *
;    SIGNIFICANT BYTES    *
;    OF DNIBL TABLE.      *
;                         *
;**************************
;
;**************************
;                         *
;    ----SEEK----         *
;                         *
;**************************
;
TRKCNT     =    COUNT        ; HALFTRACKS MOVED COUNT.
PRIOR      =    IBSLOT+$1C
TRKN       =    IBSLOT+$1D

;**************************
;                         *
;     ----MSWAIT----      *
;                         *
;**************************

MONTIMEL   =    CSSTV+2      ; MOTOR-ON TIME
MONTIMEH   =    MONTIMEL+1   ; COUNTERS.

;**************************
;                         *
;    DEVICE ADDRESS       *
;    ASSIGNMENTS          *
;                         *
;**************************

PHASEOFF   =    $C080        ; STEPPER PHASE OFF.
PHASEON    =    $C081        ; STEPPER PHASE ON.
Q6L        =    $C08C        ; Q7L,Q6L=READ
Q6H        =    $C08D        ; Q7L,Q6H=SENSE WPROT
Q7L        =    $C08E        ; Q7H,Q6L=WRITE
Q7H        =    $C08F        ; Q7H,Q6H=WRITE STORE
INTERUPT   =    $FFEF
ENVIRON    =    $FFDF
ONEMEG     =    $80
TWOMEG     =    $7F

;******************************
;
; EQUATES FOR RWTS AND BLOCK
;
;******************************

MOTOROFF   =    $C088
MOTORON    =    $C089
DRVOEN     =    $C08A
DRV1EN     =    $C08B
PHASON     =    $C081
PHSOFF     =    $C080
TEMP       =    CSSTV        ; PUT ADDRESS INFO HERE
CSUM1      =    TEMP
SECT       =    CSUM1+1
TRACK      =    SECT+1
TRKN1      =    TRACK
VOLUME     =    TRACK+1
IBRERR     =    HRDERRS+3
IBDERR     =    HRDERRS+2
IBWPER     =    HRDERRS+1
IBNODRV    =    HRDERRS
;
;**************************
;                         *
;   READ WRITE A          *
;   TRACK AND SECTOR      *
;                         *
;**************************

REGRWTS:   LDY     #01          ; RETRY COUNT
           LDX     IBSLOT       ; GET SLOT # FOR THIS OPERATION
           STY     SEEKCNT      ; ONLY ONE RECALIBRATE PER CALL
           LDA     #05
           STA     $8F
           PHP                  ; DETERMINE INTERRUPT STATUS
           PLA
           ROR     A
           ROR     A            ; GET INTERRUPT FLAG INTO BIT 7
           ROR     A
           ROR     A
           STA     IMASK
           LDA     ENVIRON      ; PRESERVE ENVIRONMENT
           STA     ENVTEMP
           JSR     CHKDRV       ; SET ZERO FLAG IF MOTOR STOPPED
           PHP                  ; SAVE TEST RESULTS
           LDA     IBBUFP       ; MOVE OUT POINTER TO BUFFER INTO ZPAGE
           STA     BUF
           LDA     IBBUFP+1
           STA     BUF+1
           LDA     #DVMOT
           STA     MONTIMEH
           LDA     IBDRVN       ; DETERMINE DRIVE ONE OR TWO
           CMP     IOBPDN       ; SAME DRIVE USED BEFORE
           STA     IOBPDN       ; SAVE IT FOR NEXT TIME
           PHP                  ; KEEP RESULTS OF COMPARE
           ROR     A            ; GET DRIVE NUMBER INTO CARRY
           LDA     MOTORON,X    ; TURN ON THE DRIVE
           BCC     DRIVSEL      ; BRANCH IF DRIVE 1 SELECTED
           INX                  ; SELECT DRIVE 2
DRIVSEL:   LDA     DRVOEN,X
           JSR     SET1MEG      ; INSURE ONE MEGAHERTZ OPERATION
           PLP                  ; WAS IT SAME DRIVE?
           BEQ     OK
           PLP                  ; MUST INDICATE DRIVE OFF BY SETTING ZERO FLAG
           LDY     #07          ; DELAY 150 MS BEFORE STEPPING
DRVWAIT:   JSR     MSWAIT       ; (ON RETURN A=0)
           DEY
           BNE     DRVWAIT
           PHP                  ; NOW ZERO FLAG SET
OK:        LDA     IBTRK        ; GET DESTINATION TRACK
           LDX     IBSLOT       ; RESTORE PROPER X (SLOT*16)
           JSR     MYSEEK       ; AND GO TO IT
; NOW AT THE DESIRED TRACK WAS THE MOTOR ON TO START WITH?
           PLP                  ; WAS MOTOR ON?
           BNE     TRYTRK       ; IF SO, DON'T DELAY, GET IT TODAY!

; MOTOR WAS OFF, WAIT FOR IT TO SPEED UP

MOTOF:     LDY     #$12         ; WAIT EXACTLY 100 US FOR EACH COUNT
CONWAIT:   DEY                  ; IN MONTIME
           BNE     CONWAIT
           INC     MONTIMEL     ; COUNT UP TO 0000
           BNE     MOTOF
           INC     MONTIMEH
           BMI     MOTOF
;
;******************************
; MOTOR SHOULD BE UP TO SPEED
; IF IT STILL LOOKS STOPPED THEN
; THE DRIVE IS NOT PRESENT.
;
;******************************
;
           JSR     CHKDRV       ; IS DRIVE PRESENT?
           BNE     TRYTRK       ; YES, CONTINUE
NODRIVERR: LDA     #IBNODRV     ; NO, GET TELL EM NO DRIVE
           JMP     HNDLERR
;
; NOW CHECK IF IT IS NOT THE FORMAT DISK COMMAND,
; LOCATE THE CORRECT SECTOR FOR THIS OPERATION F069
;
TRYTRK:    LDA     IBCMD        ; GET COMMAND CODE #
           BEQ     ALLDONE      ; IF NULL COMMAND, GO HOME TO BED
           CMP     #03          ; COMMAND IN RANGE?
           BCS     ALLDONE      ; NO, DO NOTHING!
           ROR     A            ; SET CARRY=1 FOR READ, 0 FOR WRITE
           BCS     TRYTRK2      ; MUST PRENIBBLIZE FOR WRITE
           LDA     ENVIRON
           AND     #TWOMEG      ; SHIFT TO HIGH SPEED!
           STA     ENVIRON
           JSR     PRENIB16
TRYTRK2:   LDY     #$7F         ; ONLY 127 RETRIES OF ANY KIND
           STY     RETRYCNT
TRYADR:    LDX     IBSLOT       ; GET SLOT NUM INTO X-REG
           JSR     RDADR16      ; READ NEXT ADDRESS FIELD
           BCC     RDRIGHT      ; IF READ IS RIGHT, HURRAH!
TRYADR2:   JSR     CHKINT       ; BRANCH TO CHECK FOR INTERRUPTS
           DEC     RETRYCNT     ; ANOTHER MISTAKE!!
           BPL     TRYADR       ; WELL, LET IT GO THIS TIME
           DEC     SEEKCNT      ; ONLY RECALIBRATE ONCE!
           BNE     DRVERR       ; TRIED TO RECALIBRATE A SECOND TIME, ERROR!
           LDA     $8F          ; ANOTHER MISTAKE!!
           BMI     TRYTRK2      ; WELL, LET IT GO THIS TIME
           LDA     CURTRK
           PHA                  ; SAVE TRACK WE REALLY WANT
           LDA     #$60         ; RECALIBRATE ALL OVER AGAIN!    ERROR!
           JSR     SETTRK       ; PRETEND TO BE ON TRACK 80
           LDA     #00
           JSR     MYSEEK       ; MOVE TO TRACK 00
GOCAL1:    PLA
GOCAL:     JSR     MYSEEK       ; GO TO CORRECT TRACK THIS TIME!
           BCC     TRYTRK2      ; LOOP BACK, TRY AGAIN ON THIS TRACK FOAC
; HAVE NOW READ AN ADDRESS FIELD CORRECTLY.
; MAKE SURE THIS IS THE TRACK, SECTOR, AND VOLUME DESIRED.
RDRIGHT:   LDY     TRACK        ; ON THE RIGHT TRACK?
           CPY     CURTRK
           BEQ     RTTRK        ; IF SO, GOOD
;
; RECALIBRATING FROM THIS TRACK
;
           LDA     CURTRK       ; PRESERVE DESTINATION TRACK
           PHA
           TYA
           ASL     A
           JSR     SETTRK
           PLA
           JSR     MYSEEK
           BCC     TRYADR2
RTTRK:     LDA     VOLUME       ; GET ACTUAL VOLUME HERE
           STA     IBSMOD       ; TELL OPSYS WHAT VOLUME WAS THERE
CORRECTVOL: LDA     SECT         ; CHECK IF THIS IS THE RIGHT SECTOR
           CMP     IBSECT
TRYAGAIN:  BNE     TRYADR2      ; NO, TRY ANOTHER SECTOR
           LDA     IBCMD        ; READ OR WRITE?
           LSR     A            ; THE CARRY WILL TELL
           BCC     WRIT         ; CARRY WAS SET FOR READ OPERATION,
           JSR     READ16       ; CLEARED FOR WRITE
           BCS     TRYADR2      ; CARRY SET UPON RETURN IF BAD READ
           LDA     ENVIRON
           AND     #TWOMEG
           STA     ENVIRON      ; SET TWO MEGAHERTZ
           JSR     POSTNIB16    ; DO PARTIAL POSTNIBBLE CONVERSION
           LDX     IBSLOT       ; RESTORE SLOTNUM INTO X
           BCS     TRYADR2      ; CHECKSUM ERROR
ALLDONE:   CLC 
           LDA     #00          ; NO ERROR
           BCC     ALDONE1      ; SKIP OVER NEXT BYTE WITH BIT OPCODE
DRVERR:    LDA     #IBDERR      ; BAD DRIVE
HNDLERR:   SEC                  ; INDICATE AN ERROR
ALDONE1:   STA     IBSTAT       ; GIVE HIM ERROR
           LDA     MOTOROFF,X   ; TURN IT OFF
           JSR     CHKINT       ; BRANCH TO CHECK FOR INTERRUPTS
           LDA     ENVTEMP      ; RESTORE ORIGINAL ENVIRONMENT
           STA     ENVIRON
           RTS

WRIT:      JSR     WRITE16      ; WRITE NYBBLES NOW
           BCC     ALLDONE      ; IF NO ERRORS
           LDA     #IBWPER      ; DISK IS WRITE PROTECTED!!
           BVC     HNDLERR      ; TAKEN IF TRUELY WRITE PROTECT ERROR
           BNE     TRYAGAIN     ; OTHERWISE ASSUME AN INTERRUPT MESSED THINGS UP F104
; THIS IS THE 'SEEK' ROUTINE
; SEEKS TRACK 'N' IN SLOT #X/$10
; IF DRIVENO IS NEGATIVE, ON DRIVE 0
; IF DRIVENO IS POSITIVE, ON DRIVE 1 F104
MYSEEK:    ASL     A            ; ASSUME TWO PHASE STEPPER.
SEEK1:     STA     TRKN1        ; SAVE DESTINATION TRACK(*2)
           JSR     ALLOFF       ; TURN ALL PHASES OFF TO BE SURE.
           JSR     DRVINDX      ; GET INDEX TO PREVIOUS TRACK FOR CURRENT DRIVE
           LDA     DRVOTRK,X
           STA     CURTRK       ; THIS IS WHERE I AM
           LDA     TRKN1        ; AND WHERE I'M GOING TO
           STA     DRVOTRK,X
GOSEEK:    JSR     SEEK         ; GO THERE!
ALLOFF:    LDY     #03          ; TURN OFF ALL PHASES BEFORE RETURNING
NXOFF:     TYA                  ; (SEND PHASE IN ACC.)
           JSR     CLRPHASE     ; CARRY IS CLEAR, PHASES SHOULD BE TURNED OFF
           DEY
           BPL     NXOFF
           LSR     CURTRK       ; DIVIDE BACK NOW
           CLC
           RTS
;
; THIS SUBROUTINE SETS THE SLOT DEPENDENT TRACK
; LOCATION
;
SETTRK:    JSR     DRVINDX      ; GET INDEX TO DRIVE NUMBER
           STA     DRVOTRK,X
           RTS
;
;****************************
; SUBR TO TELL IF MOTOR IS STOPPED
;
; IF MOTOR IS STOPPED, CONTROLLER'S
; SHIFT REG WILL NOT BE CHANGING.
;
; RETURN Y=0 AND ZERO FLAG SET IF IT IS STOPPED.
;
;****************************
;
CHKDRV:    LDY     #00          ; INIT LOOP COUNTER
CHKDRV1:   LDA     Q6L,X        ; READ THE SHIFT REG
           JSR     CKDRTS       ; DELAY
           PHA
           PLA
           CMP     Q6L,X        ; HAS SHIFT REG CHANGED?
           BNE     CKDRTS       ; YES, MOTOR IS MOVING
           DEY                  ; NO, DEC RETRY COUNTER
           BNE     CHKDRV1      ; AND TRY 256 TIMES
CKDRTS:    RTS                  ; THEN RETURN F13E
DRVINDX:   PHA                  ; PRESERVE ACC.
           TXA                  ; GET SLOT(*$10)/8
           LSR     A
           LSR     A
           LSR     A
           ORA     IBDRVN       ; FOR DRIVE 0 OR 1
           TAX                  ; INTO X FOR INDEX TO TABLE
           PLA                  ; RESTORE ACC.
           RTS
;
;*****************************
;
; NOTE: FORMATTING ROUTINES
;       NOT INCLUDED FOR SOS
;
;*****************************
;
;*************************
;                        *
;    READ SUBROUTINE     *
;  (16-SECTOR FORMAT)    *
;                        *
;*************************
;                        *
;   READS ENCODED BYTES  *
;  INTO NBUF1 AND NBUF2  *
;                        *
;  FIRST READS NBUF2     *
;          HIGH TO LOW,  *
;  THEN READS NBUF1	     *
;          LOW TO HIGH.  *
;                        *
;   ---- ON ENTRY ----   *
;                        *
;  X-REG: SLOTNUM        *
;         TIMES $10.     *
;                        *
;  READ MODE (Q6L, Q7L   *
;                        *
;   ---- ON EXIT ----    *
;                        *
;  CARRY SET IF ERROR    *
;                        *
;  IF NO ERROR:          *
;     A-REG HOLDS $AA.   *
;     X-REG UNCHANGED.   *
;     Y-REG HOLDS $00.   *
;     CARRY CLEAR.       *
;   ---- CAUTION ---     *
;                        *
;        OBSERVE         *
;    'NO PAGE CROSS'     *
;      WARNINGS ON       *
;    SOME BRANCHES!!     *
;                        *
;   ---- ASSUMES ----    *
;                        *
;   1 USEC CYCLE TIME    *
;                        *
;*************************

READ16:    LDY     #$20         ; 'MUST FIND' COUNT.
RSYNC:     DEY                  ; IF CAN'T FIND MARKS.
           BEQ     RDERR        ; THEN EXIT WITH CARRY SET
RD1:       LDA     Q6L,X        ; READ NIBL.
           BPL     RD1          ; *** NO PAGE CROSS! ***
RSYNC1:    EOR     #$D5         ; DATA MARK1?
           BNE     RSYNC        ; LOOP IF NOT.
           NOP                  ; DELAY BETWEEN NIBLS.
RD2:       LDA     Q6L,X
           BPL     RD2          ; *** NO PAGE CROSS! ***
           CMP     #$AA         ; DATA MARK 2?
           BNE     RSYNC1       ; (IF NOT, IS IT DM1?)
           LDY     #$55         ; INIT NBUF2 INDEX.
;                               ( ADDED NIBL DELAY)
           NOP                  ; DELAY BETWEEN NIBLS.
RD3:       LDA     Q6L,X
           BPL     RD3          ; *** NO PAGE CROSS! ***
           CMP     #$AD         ; DATA MARK 3?
           BNE     RSYNC1       ; (IF NOT, IS IT DM1?)
;                               (CARRY SET IF DM3!)
           NOP                  ; DELAY BETWEEN NIBLS.
           NOP                  ; DELAY BETWEEN NIBLS.
RD4:       LDA     Q6L,X
           BPL     RD4          ; *** NO PAGE CROSS! ***
           STA     NBUF2,Y      ; STORE BYTES DIRECTLY
           LDA     INTERUPT     ; POLL INTERRUPT LINE
           ORA     IMASK        ; (THIS MAY BE USED TO INVALIDATE POLL)
           BPL     GOSERV
           DEY                  ; INDEX TO NEXT
           BPL     RD4
RD5:       INY                  ; (FIRST TIME Y=0)
RD5A:      LDA     Q6L,X        ; GET ENCODED BYTES OF NBUF1
           BPL     RD5A
           STA     NBUF1,Y
           LDA     INTERUPT     ; POLL INTERRUPT LINE
           ORA     IMASK        ; (THIS MAY BE USED TO INVALIDATE POLL)
           BPL     GOSERV
           CPY     #$E4         ; WITHIN 1 MS OF COMPLETION?
           BNE     RD5
           INY 
RD6:       LDA     Q6L,X        ; NO POLL FROM NOW ON
           BPL     RD6
           STA     NBUF1,Y
           INY                  ; FINISH OUT NBUF1 PAGE
           BNE     RD6
RDCKSUM:   LDA     Q6L,X        ; GET CHECKSUM BYTE.
           BPL     RDCKSUM
           STA     CKSUM
           JSR     RDA6         ; CHECK BIT SLIP MARKS
;
; CHECK FOR INTERRUPTS
;
CHKINT:    BIT     IMASK        ; SHOULD INTERRUPTS BE ALLOWED?
           BPL     @10          ; YES, ALLOW THEM.
           BIT     $8F
           BPL     @20
@10:       CLI
@20:       RTS

GOSERV:    JSR     SERVICE      ; GO TO SERVICE INTERRUPT
RDERR:     SEC
           RTS

;***************************
;			   *
;     READ ADDRESS FIELD   *
;         SUBROUTINE	   *
;    (16-SECTOR FORMAT)	   *
;			               *
;***************************
;			               *
;    READS VOLUME, TRACK   *
;        AND SECTOR	       *
;			               *
;   ---- ON ENTRY ----	   *
;                          *
;  XREG: SLOTNUM TIMES $10 *
;			               *
;  READ MODE  (Q6L, Q7L)   *
;			               *
;   ---- ON EXIT ----	   *
;			               *
;  CARRY SET IF ERROR	   *
;			               *
;  IF NO ERROR:		       *
;    A-REG HOLDS $AA.	   *
;    Y-REG HOLDS $00.	   *
;    X-REG UNCHANGED.	   *
;    CARRY CLEAR.	       *
;			               *
;    CSSTV HOLDS CHKSUM,   *
;      SECTOR, TRACK, AND  *
;      VOLUME READ.	       *
;			               *
;    USES TEMPS COUNT,	   *
;      LAST, CSUM, AND	   *
;      4 BYTES AT CSSTV.   *
;			               *
;   ---- EXPECTS ----      *
;			               *
;   ORIGINAL 10-SECTOR	   *
;  NORMAL DENSITY NIBLS	   *
;			               *
;   (4-BIT), ODD BITS,	   *
;   THEN EVEN		       *
;			               *
;    ---- CAUTION ----     *
;			               *
;         OBSERVE	       *
;     'NO PAGE CROSS'	   *
;       WARNINGS ON	       *
;     SOME BRANCHES!!      *
;                          *
;    ---- ASSUMES ----     *
;                          *
;    1 USEC CYCLE TIME     *
;                          *
;***************************
;
RDADR16:   LDY     #$FC
           STY     COUNT        ; 'MUST FIND' COUNT.
RDASYN:    INY
           BNE     RDA1         ; LOW ORDER OF COUNT
           INC     COUNT        ; (2K NIBLS TO FIND
           BEQ     RDERR        ; ADR MARK, ELSE ERR)
RDA1:      LDA     Q6L,X        ; READ NIBL.
           BPL     RDA1         ; *** NO PAGE CROSS! ***
RDASN1:    CMP     #$D5         ; ADR MARK 1?
           BNE     RDASYN       ; (LOOP IF NOT)
           NOP                  ; ADDED NIBL DELAY
RDA2:      LDA     Q6L,X
           BPL     RDA2         ; *** NO PAGE CROSS! ***
           CMP     #$AA         ; ADR MARK 2?
           BNE     RDASN1       ; (IF NOT, IS IT AM1?)
           LDY     #03          ; INDEX FOR 4-BYTE READ
                                ; (ADDED NIBL DELAY)
RDA3:      LDA     Q6L,X
           BPL     RDA3         ; *** NO PAGE CROSS! ***
           CMP     #$96         ; ADR MARK 3?
           BNE     RDASN1       ; (IF NOT IS IT AM1?)
                                ; (LEAVES CARRY SET!)
           SEI                  ; DISABLE INTERRUPT SYSTEM
           LDA     #00          ; INIT CHECKSUM
RDAFLD:    STA     CSUM
RDA4:      LDA     Q6L,X        ; READ 'ODD BIT' NIBBL
           BPL     RDA4         ; *** NO PAGE CROSS! ***
           ROL     A            ; ALIGN ODD BITS, 1' INTO LSB
           STA     LAST         ; (SAVE THEM)
RDAS:      LDA     Q6L,X        ; READ 'EVEN BIT' NIBL
           BPL     RDAS         ; *** NO PAGE CROSS ***
           AND     LAST         ; MERGE ODD AND EVEN BITS
           STA     CSSTV,Y      ; STORE DATA BYTE
           EOR     CSUM
           DEY 
           BPL     RDAFLD       ; LOOP ON 4 DATA BYTES.
           TAY                  ; IF FINAL CHECKSUM
           BNE     RDERR        ; NONZERO, THEN ERROR
RDA6:      LDA     Q6L,X        ; FIRST BIT SLIP NIBBL
           BPL     RDA6         ; *** NO PAGE CROSS! ***
           CMP     #$DE
           BNE     RDERR        ; ERROR IF NONMATCH
           NOP                  ; DELAY
RDA7:      LDA     Q6L,X        ; SECOND BIT-SLIP NIBL
           BPL     RDA7         ; *** NO PAGE CROSS! ***
           CMP     #$AA
           BNE     RDERR        ; ERROR IF NOMATCH
RDEXIT:    CLC                  ; CLEAR CARRY ON
WEXIT:     RTS                  ; NORMAL READ EXITS.
 
;***********************
;                      *
;    WRITE SUBR        *
;  (16-SECTOR FORMAT)  *
;                      *
;***********************
;                      *
;   WRITES DATA FROM   *
;    NBUF1 AND NBUF2   * .
;                      *
;  FIRST NBUF2,        *
;      HIGH TO LOW.    *
;  THEN NBUF1,         *
;      LOW TO HIGH     *
;                      *
;  ---- ON ENTRY ----  *
;                      *
;   X-REG    SLOTNUM   *
;        TIMES $10     *
;                      *
;                      *
;  ---- ON EXIT ----   *
;                      *
;  CARRY SET IF ERROR. *
;   (W PROT VIOLATION) *
;                      *
;  IF NO ERROR:        *
;                      *
;    A-REG UNCERTAIN.  *
;    X-REG UNCHANGED.  *
;    Y-REG HOLDS $00.  *
;    CARRY CLEAR.      *
;		               *
;  ---- ASSUMES ----   *
;		               *
;  1 USEC CYCLE TIME   *
;                      *
;***********************
;
WRITE16:   SEC                  ; ANTICIPATE WPROT ERR.
           CLV                  ; TO INDICATE WRITE PROTECT ERROR INSTEAD OF
                                ; INTERRUPT
           LDA     Q6H,X
           LDA     Q7L,X        ; SENSE WPROT FLAG.
           BMI     WEXIT        ; BRANCH IF WRITE PROTECTED
WRIT1:     LDA     #$FF         ; SYNC DATA.
           STA     Q7H,X        ; (5) GOTO WRITE MODE
           ORA     Q6L,X        ; (4)
           LDY     #04          ; (2) FOR FIVE NIBLS.
           NOP                  ; (2)
           PHA                  ; (4)
           PLA                  ; (3)
WSYNC:     PHA                  ; (4) EXACT TIMING
           PLA                  ; (3)
           JSR     WNIBL7       ; (13,9,6) WRITE SYNC
           DEY                  ; (2)
           BNE     WSYNC        ; (2*) MUST NOT CROSS PAGE!
           LDA     #$D5         ; (2)  1ST DATA MARK
           JSR     WNIBL9       ; (15,9,6)
           LDA     #$AA         ; (2)  2ND DATA MARK
           JSR     WNIBL9       ; (15,9,6)
           LDA     #$AD         ; (2)  3RD DATA MARK
           JSR     WNIBL9       ; (15,9,6)
           LDY     #$55         ; (2) NBUF2 INDEX
           NOP                  ; (2) FOR TIMING
           NOP                  ; (2)
           NOP                  ; (2)
           BNE     VRYFRST      ; (3) BRANCH ALWAYS
WINTRPT:   LDA     INTERUPT     ; (4) POLL INTERRUPT LINE
           ORA     IMASK        ; (3)
           SEC                  ; (2)
           BPL     SERVICE      ; (2) BRANCH IF INTERRUPT HAS OCCURED
VRYFRST:   BMI     WRTFRST      ; (3) FOR TIMING.
WRTFRST:   LDA     NBUF2,Y      ; (4)
           STA     Q6H,X        ; (5) STORE ENCODED BYTE
           LDA     Q6L,X        ; (4) TIME MUST = 32 US PER BYTE!
           DEY                  ; (2)
           BPL     WINTRPT      ; (3) (2 IF BRANCH NOT TAKEN)
           TYA                  ; (2) INSURE NO INTERRUPT THIS BYTE
           BMI     WMIDLE       ; (3) BRANCH ALWAYS.
WNTRPT1:   LDA     INTERUPT     ; (4) POLL INTERRUPT LINE
WMIDLE:    ORA     IMASK        ; (3)
           SEC                  ; (2)
           BMI     WDATA2       ; (3) BRANCH IF NO INTERRUPT
           BPL     SERVICE      ; GO SERVICE INTERRUPT.
WDATA2:    INY                  ; (2)
           LDA     NBUF1,Y      ; (4)
           STA     Q6H,X        ; (5) STORE ENCODED BYTE
           LDA     Q6L,X        ; (4)
           CPY     #$E4         ; (2) WITHIN 1 MS OF COMPLETION?
           BNE     WNTRPT1      ; (3)    (2) NO KEEP WRITTING AND POLLING.
           NOP                  ; (2)
           INY                  ; (2)
WDATA3:    NOP                  ; (2)
           NOP                  ; (2)
           PHA                  ; (4)
           PLA                  ; (3)
           LDA     NBUF1,Y      ; (4) WRITE LAST OF ENCODED BYTES
           STA     Q6H,X        ; (5) WITHOUT POLLING INTERRUPTS.
           LDA     Q6L,X        ; (4)
           LDA     CKSUM        ; (3) NORMALLY FOR TIMING
           INY                  ; (2)
           BNE     WDATA3       ; (3)    (2)
           BEQ     WRCKSUM      ; (3) BRANCH ALWAYS
WRCKSUM:   JSR     WNIBL7       ; (13,9,6)    GO WRITE CHECK SUM!!
           PHA                  ; (3)
           PLA                  ; (4)
WRBITSLMK: LDA     BITSLIPMK,Y  ; (4) LOAD BIT SLIP MARK
           JSR     WNIBL        ; (6,9,6)
           INY                  ; (2)
           CPY     #04          ; (2)
           BNE     WRBITSLMK    ; (2)    (3)
           CLC                  ; (2)
NOWRITE:   LDA     Q7L,X        ; OUT OF WRITE MODE.
           LDA     Q6L,X        ; TO READ MODE.
           RTS                  ; RETURN FROM WRITE.

SERVICE:   BIT     SEV          ; SET VFLAG TO INDICATE INTERRUPT
           JSR     NOWRITE      ; TAKE IT OUT OF WRITE MODE!
           LDA     $8F
           BPL     TEN
           STA     IMASK
TEN:       DEC     $8F
           CLI                  ; COULD NOT HAVE GOT HERE WITHOUT CLI OK
           RTS
;
;****************************
;                           *
;   7-BIT NIBL WRITE SUBRS  *
;                           *
;   A-REG OR'D PRIOR EXIT   *
;       CARRY CLEARED	    *
;                           *
;****************************
;
WNIBL9:    CLC                  ; (2) 9 CYCLES, THEN WRITE
WNIBL7:    PHA                  ; (3) 7 CYCLES, THEN WRITE
           PLA                  ; (4)
WNIBL:     STA     Q6H,X        ; (5) NIBL WRITE SUB
           ORA     Q6L,X        ; (4) CLOBBERS ACC. NOT CARRY
           RTS
;
;****************************
;                           *
;    PRENIBILIZE SUBR       *
;   (16-SECTOR FORMAT)      *
;                           *
;****************************
;                           *
;  CONVERTS 256 BYTES OF    *
;  USER DATA IN (BUF) INTO  *
;  ENCODED BYTES TO BE      *
;  WRITTEN DIRECTLY TO DISK *
;  ENCODED CHECK SUM IN     *
;  ZERO PAGE 'CKSUM'        *
;                           *
;   ---- ON ENTRY ----      *
;                           *
;  BUF IS 2-BYTE POINTER    *
;    TO 256 BYTES OF USER   *
;    DATA.                  *
;                           *
;  A-REG CHECK SUM.         *
;  X-REG UNCERTAIN          *
;  Y-REG HOLDS 0.           *
;  CARRY SET.               *
;                           *
;****************************
;
PRENIB16:  LDX     #02          ; START NBUF2 INDEX.
           LDY     #00          ; START USER BUF INDEX.
PRENIB1:   DEY                  ; NEXT USER BYTE
           LDA     (BUF),Y
           LSR     A            ; SHIFT TWO BITS OF
           ROL     NBUF2-1,X    ; CURRENT USER BYTE
           LSR     A            ; INTO CURRENT NBUF2
           ROL     NBUF2-1,X    ; BYTE.
           STA     NBUF1+1,Y    ; (6 BITS LEFT).
           INX                  ; FROM 0 TO $55
           CPX     #$56
           BCC     PRENIB1      ; BR IF NO WRAPAROUND
           LDX     #00          ; RESET NBUF2 INDEX
           TYA                  ; USER BUF INDEX
           BNE     PRENIB1      ; (DONE IF ZERO)
           LDY     #$56         ; (ACC=0 FOR CHECK SUM)
PRENIB3:   EOR     NBUF2-2,Y    ; COMBINE WITH PREVIOUS
PRENIB2:   AND     #$3F         ; STRIP GARBAGE BITS
           TAX                  ; TO FORM RUNNING CHECK SUM
           LDA     NIBL,X       ; GET ENCODED EQUIV.
           STA     NBUF2-1,Y    ; REPLACE PREVIOUS
           LDA     NBUF2-2,Y    ; RESTORE ACTUAL PREVIOUS
           DEY
           BNE     PRENIB3      ; LOOP UNTIL ALL OF NBUF2 IS CONVERTED.
           AND     #$3F
PRENIB4:   EOR     NBUF1+1,Y    ; NOW DO THE SAME FOR
           TAX                  ; NIBBLE BUFFER 1
           LDA     NIBL,X       ; TO DO ANY BACK TRACKING (NBUF1-1)
           STA     NBUF1,Y
           LDA     NBUF1+1,Y    ; RECOVER THAT WHICH IS NOW 'PREVIOUS'
           INY 
           BNE     PRENIB4
           TAX                  ; USE LAST AS CHECK SUM
           LDA     NIBL,X
           STA     CKSUM
           JMP     SET1MEG      ; ALL DONE.
; 
;**************************
;                         *
;    POSTNIBLIZE SUBR     *
;    16-SECTOR FORMAT     *
;                         *
;**************************
;
POSTNIB16: SEC
           LDY     #$55         ; FIRST CONVERT TO 6 BIT NIBBLES
           LDA     #00          ; INIT CHECK SUM
PNIBL1:    LDX     NBUF2,Y      ; GET ENCODED BYTE
           EOR     DNIBL,X
           BMI     SET1MEG      ; SET 1 MHZ
           STA     NBUF2,Y      ; REPLACE WITH 6 BIT EQUIV.
           DEY 
           BPL     PNIBL1       ; LOOP UNTIL DONE WITH NIBBLE BUFFER 2
           INY                  ; NOW Y=0
PNIBL2:    LDX     NBUF1,Y      ; DO THE SAME WITH
           EOR     DNIBL,X
           STA     NBUF1,Y      ; NIBBLE BUFFER 1
           INY                  ; DO ALL 256 BYTES
           BNE     PNIBL2
           LDX     CKSUM        ; MAKE SURE CHECK SUM MATCHES
           EOR     DNIBL,X      ; BETTER BE ZERO
           BNE     POSTERR      ; BRANCH IF IT IS
POST1:     LDX     #$56         ; INIT NBUF2 INDEX
POST2:     DEX                  ; NBUF IDX $55 TO $00
           BMI     POST1        ; WRAPAROUND IF NEG
           LDA     NBUF1,Y
           LSR     NBUF2,X      ; SHIFT 2 BITS FROM
           ROL     A            ; CURRENT NBUF2 NIBL
           LSR     NBUF2,X      ; CURRENT NBUF1
           ROL     A            ; NIBL.
           STA     (BUF),Y      ; BYTE OF USER DATA
           INY                  ; NEXT USER BYTE
           BNE     POST2
           CLC                  ; GOOD DATA
POSTERR    =       *
SET1MEG:   LDA     ENVIRON
           ORA     #ONEMEG      ; SET TO ONE MEGAHERTZ CLOCK RATE
           STA     ENVIRON
SEV:       RTS                  ; (SEV USED TO SET VFLAG)
;
;**************************
;                         *
;     6-BIT TO 7-BIT      *
;  NIBL CONVERSION TABLE  *
;                         *
;**************************
;                         *
;   CODES WITH MORE THAN  *
;   ONE PAIR OF ADJACENT  *
;    ZEROES OR WITH NO    *
;   ADJACENT ONES (EXCEPT *
;     B7) ARE EXCLUDED.   *
;                         *
;**************************
;
NIBL:      .BYTE   $96,$97,$9A,$9B,$9D,$9E,$9F,$A6,$A7,$AB,$AC,$AD,$AE,$AF,$B2,$B3,$B4,$B5
           .BYTE   $B6,$B7,$B9,$BA,$BB,$BC,$BD,$BE,$BF,$CB,$CD,$CE,$CF,$D3,$D6,$D7
           .BYTE   $D9,$DA,$DB,$DC,$DD,$DE,$DF,$E5,$E6,$E7,$E9,$EA,$EB,$EC,$ED,$EE
           .BYTE   $EF,$F2,$F3,$F4,$F5,$F6,$F7,$F9,$FA,$FB,$FC,$FD,$FE,$FF


;***************************
;                          *
;     7-BIT TO 6-BIT       *
;    'DENIBLIZE' TABL      *
;    (16-SECTOR FORMAT)    *
;                          *
;       VALID CODES        *
;      $96 TO $FF ONLY.    *
;                          *
;                          *
;    CODES WITH MORE THAN  *
;    ONE PAIR OF ADJACENT  *
;     ZEROES OR WITH NO    *
;    ADJACENT ONES (EXCEPT *
;    BIT 7) ARE EXCLUDED   *
;***************************
;
DNIBL       =      REGRWTS + $300
           .BYTE   $01,$00,$01
           .BYTE   $98,$99,$02,$03,$9C,$04,$05,$06,$A0,$A1,$A2,$A3,$A4,$A5,$07,$08,$A8
           .BYTE   $A9,$AA,$09,$0A,$0B,$0C,$0D,$B0,$B1,$0E,$0F,$10,$11,$12,$13,$B8,$14,$15
           .BYTE   $16,$17,$18,$19,$1A

BITSLIPMK: .BYTE   $DE,$AA,$EB,$FF,$C4,$C5,$C6,$C7,$C8,$C9,$CA,$1B,$CC,$1C,$1D,$1E
           .BYTE   $D0,$D1,$D2,$1F,$D4,$D5,$20,$21,$D8,$22,$23,$24,$25,$26,$27,$28,$E0,$E1
           .BYTE   $E2,$E3,$E4,$29,$2A,$2B,$E8,$2C,$2D,$2E,$2F,$30,$31,$32,$F0,$F1,$33,$34
           .BYTE   $35,$36,$37,$38,$F8,$39,$3A,$3B,$3C,$3D,$3E,$3F




;*************************
;                        *
;  FAST SEEK SUBROUTINE  *
;                        *
;*************************
;                        *
;   ---- ON ENTRY ----   *
;                        *
;  X-REG HOLDS SLOTNUM   *
;         TIMES $10      *
;                        *
;  A-REG HOLDS DESIRED   *
;         HALFTRACK.     *
;                        *
;  CURTRK HOLDS DESIRED  *
;          HALFTRACK.    *
;                        *
;    ---- ON EXIT ----   *
;                        *
;  A-REG UNCERTAIN.      *
;  Y-REG UNCERTAIN.      *
;  X-REG UNDISTURBED.    *  
;                        *
;  CURTRK AND TRKN HOLD  *
;      FINAL HALFTRACK.  *
;                        *
;  PRIOR HOLDS PRIOR     *
;    HALFTRACK IF SEEK   *
;    WAS REQUIRED.       *
;                        *
;  MONTIMEL AND MONTIMEH *
;    ARE INCREMENTED BY  *
;    THE NUMBER OF       *
;    100 USEC QUANTUMS   *
;    REQUIRED BY SEEK    *
;    FOR MOTOR ON TIME   *
;    OVERLAP.            *
;                        *
; --- VARIABLES USED --- *
;                        *
;  CURTRK, TRKN, COUNT,  *
;    PRIOR, SLOTTEMP     *
;    MONTIMEL, MONTIMEH  *
;                        *
;*************************
;
SEEK:      STA     TRKN         ; SAVE TARGET TRACK
           CMP     CURTRK       ; ON DESIRED TRACK?
           BEQ     SETPHASE     ; YES, ENERGIZE PHASE AND RETURN
           LDA     #00
           STA     TRKCNT       ; HALFTRACK COUNT.
SEEK2:     LDA     CURTRK       ; SAVE CURTRK FOR
           STA     PRIOR        ; DELAYED TURN OFF. SEC
           SEC 
           SBC     TRKN         ; DELTA-TRACKS.
           BEQ     SEEKEND      ; BR IF CURTRK=DESTINATION
           BCS     OUT          ; (MOVE OUT, NOT IN)
           EOR     #$FF         ; CALC TRKS TO GO.
           INC     CURTRK       ; DECR CURRENT TRACK (OUT)
           BCC     MINTST       ; (ALWAYS TAKEN).
OUT:       ADC     #$FE         ; CALC TRACKS TO GO.
           DEC     CURTRK       ; DECR CURRENT TRACK (OUT)
MINTST:    CMP     TRKCNT
           BCC     MAXTST       ; AND 'TRKS MOVED'
           LDA     TRKCNT
MAXTST:    CMP     #09
           BCS     STEP2        ; IF TRKCNT>$08 LEAVE Y ALONE (Y=$08)
STEP:      TAY                  ; ELSE SET ACCELERATION INDEX IN Y SEC
           SEC
STEP2:     JSR     SETPHASE
           LDA     ONTABLE,Y    ; FOR 'ONTIME'
           JSR     MSWAIT       ; (100 USEC INTERVALS)
           LDA     PRIOR
           CLC                  ; FOR PHASE OFF
           JSR     CLRPHASE     ; TURN OFF PRIOR PHASE
           LDA     OFFTABLE,Y   ; THEN WAIT 'OFFTIME'
           JSR     MSWAIT       ; (100 USEC INTERVALS)
           INC     TRKCNT       ; 'TRACKS MOVED' COUNT.
           BNE     SEEK2        ; (ALWAYS TAKEN)
SEEKEND:   JSR     MSWAIT       ; SETTLE 25 MSEC
           CLC                  ; SET FOR PHASE OFF
SETPHASE:  LDA     CURTRK       ; GET CURRENT TRACK
CLRPHASE:  AND     #03          ; MASK FOR 1 AND 4 PHASES
           ROL     A            ; DOUBLE FOR PHASE ON/OFF INDEX
           ORA     IBSLOT 
           TAX
           LDA     PHASEOFF,X   ; TURN ON/OFF ONE PHASE IBSLOT
           LDX     IBSLOT       ; RESTORE X-REG
SEEKRTS:   RTS                  ; AND RETURN
;
;***************************
;                          *
;    MSWAIT SUBROUTINE     *
;***************************
;                          *
;   DELAYS A SPECIFIED     *
;    NUMBER OF 100 USEC    *
;    INTERVALS FOR MOTOR   *
;    ON TIMING             *
;                          *
;    ---- ON EXIT ----     *
;                          *
;   A-REG HOLDS $00        *
;   X-REG HOLDS $00        *
;   Y-REG UNCHANGED        *
;   CARRY SET              *
;                          *
;   MONTIMEL, MONTIMEH     *
;    ARE INCREMENTED ONCE  *
;    PER 100 USEC INTERVAL *
;    FOR MOTOR ON TIMING   *
;                          *
;     ---- ASSUMES ----    *
;                          *
;    1 USEC CYCLE TIME     *
;                          *
;***************************
;
MSWAIT:    LDX     #$11
MSW1:      DEX                  ; DELAY 86 USEC
           BNE     MSW1
           INC     MONTIMEL   
           BNE     MSW2         ; DOUBLE BYTE INCREMENT
           INC     MONTIMEH
MSW2:      SEC
           SBC     #01          ; DONE IN INTERVALS 
           BNE     MSWAIT       ; (A-REG COUNTS)
           RTS
;
;*************************
;                        *
;  PHASE ON-, OFF-TIME   *
;   TABLES IN 100-USEC   *
;   INTERVALS. (SEEK)    *
;                        *
;*************************
;
ONTABLE:   .BYTE   $01,$30,$28,$24,$20,$1E,$1D,$1C,$1C
OFFTABLE:  .BYTE   $70,$2C,$26,$22,$1F,$1E,$1D,$1C,$1C

BLOCKIO:   STX     IBTRK
           LDY     #05
           PHA
TRKSEC:    ASL     A
           ROL     IBTRK
           DEY
DOFA:      BNE     TRKSEC
           PLA
           AND     #07
           TAY
           LDA     SECTABL,Y
           STA     IBSECT
           JSR     REGRWTS
           BCS     QUIT
           INC     IBBUFP+1
           INC     IBSECT
           INC     IBSECT
           JSR     REGRWTS
           DEC     IBBUFP+1
QUIT:      LDA     IBSTAT
           RTS
;
SECTABL:   .BYTE   $00,$04,$08,$0C,$01,$05,$09,$0D
;*******************************
;                              *
;    JOYSTICK READ ROUTINE     *
;                              *
;*******************************
;  ENTRY  ACC= COUNT DOWN HIGH *
;         X&Y= DON'T CARE      *
;                              *
;   EXIT  ACC= TIMER HIGH BYTE *
;           Y= TIMER LOW BYTE  *
;         CARRY CLEAR          *
;                              *
;    IF CARRY SET, ROUTINE     *
;       WAS INTERRUPTED &      *
;     ACC & Y ARE INVALID      *
;******************************* 
;
TIMLATCH   =       $FFD9
TIMER1L    =       $FFD8
TIMER1H    =       $FFD9
JOYRDY     =       $C066

ANALOG     =       *         ; CARRY SHOULD BE SET!
           STA     TIMLATCH     ; START THE TIMER!
ANLOG1:    LDA     INTERUPT
           AND     JOYRDY       ; WAIT FOR ONE OR THE OTHER TO GO LOW
           BMI     ANLOG1
           LDA     JOYRDY       ; WAS IT REALLY THE JOPYSTICK?
           BMI     GOODTIME     ; NOPE, WHAT TIME IS IT?
           CLC                  ; TIME'S A SLIP SLIDIN AWAY
           LDA     TIMER1H      ; NOW, WHAT TIME IS IT?
           LDY     TIMER1L
           BPL     GOODTIME     ; TIME WAS VALID!
           LDA     TIMER1H      ; HI BYTE CHANGED
GOODTIME:  RTS

;           .END
