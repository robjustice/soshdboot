;SBTL "SOS 1.1  INTRPTS_ & PROC. LAUNCH"
;.RELOC
            .SEGMENT   "CODE"
            .INCLUDE   "SOSORG"
            .ORG       ORGIPL
ZZORG       =          *
;MSB OFF
;***************************************************************************************************
;          COPYRIGHT (C) APPLE COMPUTER INC. 1980
;                    ALL RIGHTS RESERVED
;***************************************************************************************************
;
;  THIS MODULE IS RESPONSIBLE FOR FIELDING ALL INTERRUPTS
;  AND RELAUNCHING THE INTERRUPTED CODE AFTER THE INTERRUPTS
;  HAVE BEEN PROCESSED_  THE MAJOR FUNCTIONAL AREAS ARE:
;
;       GENERAL INTERRUPT RECEIVER
;       NMI INTERRUPT RECEIVER
;       DISPATCHER
;       INTERRUPT ALLOCATION & DEALLOCATION
;       EVENT QUEUE MANAGER
;       TABLE INITIALIZATION
;
;***************************************************************************************************
;
;  SUBROUTINE ENTRY POINTS
;
            .EXPORT    IRQ_RCVR                            ;GENERAL INTERRUPT RECEIVER
            .EXPORT    NMI_RCVR                            ;NON-MASKABLE INTRPT RCVR
            .EXPORT    DISPATCH                            ;DISPATCHER
            .EXPORT    ALLOCSIR                            ;SIR ALLOCATION
            .EXPORT    DEALCSIR                            ;SIR DEALLOCATION
            .EXPORT    SELC800                             ;SELECT I/O EXPANSION ROM
            .EXPORT    NMIDSBL                             ;DISABLE NMI
            .EXPORT    NMIENBL                             ;ENABLE NMI
            .EXPORT    NMIDBUG                             ;NMI DEBUG ENTRY
            .EXPORT    NMICONT                             ;NMI DEBUG CONTINUATION
            .EXPORT    QUEEVENT                            ;QUEUE AN EVENT
;
;  EXTERNAL SUBROUTINES & DATA
;
            .IMPORT    SCMGR
            .IMPORT    CHKBUF
;
;  SYSTEM DEATH ERRORS
;
            .IMPORT    SYSDEATH
            .IMPORTZP  BADBRK
            .IMPORTZP  BADINT1
            .IMPORTZP  BADINT2
            .IMPORTZP  NMIHANG
            .IMPORTZP  EVQOVFL
            .IMPORTZP  STKOVFL
;
;  LINKAGE DATA FOR INITIALIZATION ROUTINES
;
            .EXPORT    EV_QUEUE
            .EXPORT    EVQ_CNT
            .EXPORT    EVQ_SIZ
            .EXPORT    EVQ_LEN: ABSOLUTE
            .EXPORT    EVQ_FREE
            .EXPORT    EVQ_LINK
            .EXPORT    SIRTABLE
            .EXPORT    SIRTBLSIZ
            .EXPORT    ZPGSTACK
            .EXPORTZP  ZPGSTART
;
;  SYSGLOB DATA
;
            .IMPORT    SERR
            .IMPORT    CEVPRI                              ;CALLER'S EVENT PRIORITY
            .IMPORT    SYSBANK                             ;SYSTEM BANK
            .IMPORT    KYBDNMI
            .IMPORT    NMISPSV
            .IMPORT    NMIFLAG                             ;NMI PENDING FLAG
            .IMPORT    SCRNMODE                            ;CURRENT SCREEN MODE
            .IMPORT    SIRTEMP                             ;FOR ALLOCSIR & DEALCSIR
            .IMPORT    SIRARGSIZ
            .IMPORT    IRQCNTR                             ;FLASE IRQ COUNTER
            .IMPORT    NMICNTR                             ;TWO BYTE COUNTER
            .IMPORT    QEVTEMP
            .IMPORT    QEV_THIS
            .IMPORT    QEV_LAST
            .IMPORT    BACKMASK
;
;  CONSTANT DECLARATIONS
;
FALSE       =          $00
BITON0      =          $01
BITON1      =          $02
BITON2      =          $04
BITON4      =          $10
BITON5      =          $20
BITON6      =          $40
BITON7      =          $80
BITOFF3     =          $F7
BITOFF4     =          $EF
BITOFF5     =          $DF
BITOFF6     =          $BF
BITOFF7     =          $7F
BACKBIT     =          $20                                 ; BACKUP BIT MASK
;
;  SYSTEM CONTROL REGISTERS
;
B_REG       =          $FFEF                               ;BANK REGISTER
E_REG       =          $FFDF                               ;ENVIRONMENT REGISTER
Z_REG       =          $FFD0                               ;ZERO PAGE REGISTER
;
;  6522 REGISTERS
;
D_IFR       =          $FFDD
D_IER       =          $FFDE
E_IORB      =          $FFE0
E_IFR       =          $FFED
E_IER       =          $FFEE
E_IORA      =          $FFEF
;PAGE
;
;  REGISTER PRESERVATION EQUATES
;  FOR USE DURING INTERRUPT PROCESSING
;
A_SAVE      =          $103
S_SAVE      =          $104
SP_SAVE     =          $1FF
E_SAVE      =          $1FE
Z_SAVE      =          $1FD
B_SAVE      =          $1FC
EXPNSLOT:   .BYTE      $00                                 ;CURRENT I/O EXPANSION SLOT
;
;  STATUS LOCATIONS FOR INTERRUPT POLLING
;
ACIASTAT    =          $C0F1
ANYSLOT:    .BYTE      BITON1
SLOT1       =          $C065
SLOT2       =          $C064
SLOT3:      .BYTE      BITON5
SLOT4:      .BYTE      BITON4
;
;  INTERRUPT ZERO PAGE STORAGE & EQUATES
;
SIRARGS     =          $F9                                 ;AND $FA
QEVARGS     =          $FB                                 ;AND $FC
IRQADDR     =          $FD                                 ;AND $FE
ZPGSP       =          $FF
ZPGSTART    =          $F8
ZPGSTOP     =          $28
ZPGSPACE    =          $20
ZPGSTACK:   .BYTE      ZPGSTART
;
;  SYSTEM INTERNAL RESOURCE
;  TABLE STORAGE AND EQUATES
;
SIRTBLSIZ   =          $18
SIRTABLE:   .RES       SIRTBLSIZ
SIRADR_L:   .RES       SIRTBLSIZ
NMIADR_L:   .RES       1                                   ;MUST PRECEED SIRADR_H
SIRADR_H:   .RES       SIRTBLSIZ
SIRADR_B:   .RES       SIRTBLSIZ
;
;  EVENT QUEUE STORAGE AND EQUATES
;
EVQ_SIZ     =          6                                   ;ENTRY SIZE
EVQ_CNT     =          $07                                 ;ENTRY COUNT
EVQ_LEN     =          $2A                                 ;(EVQ_SIZ*EVQ_CNT)
EV_QUEUE:   .RES       EVQ_LEN
EVQ_FREE    =          EV_QUEUE+2                          ;FIRST FREE ENTRY INDEX
EVQ_LINK    =          EV_QUEUE+0                          ;NEXT ACTIVE ENTRY INDEX
EVQ_PRI     =          EV_QUEUE+1                          ;EVENT PRIORITY
EVQ_ID      =          EV_QUEUE+2                          ;EVENT IDENTIFICATION
EVQ_ADRL    =          EV_QUEUE+3                          ;EVENT ADDRESS:  LOW BYTE
EVQ_ADRH    =          EV_QUEUE+4                          ;EVENT ADDRESS:  HIGH BYTE
EVQ_BANK    =          EV_QUEUE+5                          ;EVENT ADDRESS:  BANK
;SBTL "GENERAL INTERRUPT RECEIVER"
;***************************************************************************************************
;
;  THIS IS THE GENERAL INTERRUPT RECEIVER_  WHEN AN
;  INTERRUPT OCCURS, THE CPU PASSES CONTROL TO THE GIR
;  THROUGH THE IRQ VECTOR_  THE GIR IS RESPONSIBLE FOR
;  SAVING THE CURRENT ENVIRONMENT, SETTING UP THE SOS
;  ENVIRONMENT, AND CALLING THE APPROPRIATE CODE MODULE_
;  IF THE INTERRUPT WAS CAUSED BY A BRK, THE GIR CALLS
;  THE SYSTEM CALL MANAGER_  OTHERWISE, THE GIR POLLS THE
;  I/O DEVICES AND CALLS THE APPROPRIATE MASTER INTERRUPT
;  HANDLER_  WHEN THE SCM OR MIH RETURNS, THE GIR PASSES
;  CONTROL TO THE DISPATCHER_
;
;***************************************************************************************************
;
IRQ_RCVR    =          *
;
;  SAVE CPU REGISTERS A, X, & Y ON CURRENT STACK
;
            PHA
            TXA
            PHA
            TYA
            PHA
;
;  CHECK FOR STACK OVERFLOW AND
;  SAVE INTERRUPTED STATUS IN Y REGISTER_
;
            TSX
            CPX        #$FA
            BCC        GIR005
            LDA        #STKOVFL
            JSR        SYSDEATH
GIR005:     LDY        S_SAVE,X
;
;  SET UP INTERRUPT ENVIRONMENT:
;    BINARY ARITHMETIC, 2 MHZ, I/O ENABLED,
;    RAM WRITE ENABLED, PRIMARY STACK,
;    AND $F000 RAM SELECTED_  PRESERVE
;    USER STATE OF SCREEN AND RESET LOCK.
;
            CLD
            LDA        E_REG
            TAX
            AND        #BITON5+BITON4
            ORA        #BITON6+BITON2
            STA        E_REG
;
;  IF NOT ALREADY ON PRIMARY STACK, SAVE USER'S STACK
;  POINTER AND SET UP SOS STACK POINTER_
;
            TXA
            AND        #BITON2
            BNE        GIR010
            TXA
            TSX
            STX        SP_SAVE
            LDX        #<E_SAVE
            TXS
            TAX
;
;  SAVE E, Z, B, & I/O EXPANSION SLOT ON SOS STACK
;  IF BRK THEN CALL SCMGR ELSE POLL I/O DEVICES
;
GIR010:
            TXA
            PHA
            LDA        Z_REG
            PHA
            LDA        B_REG
            PHA
            LDA        EXPNSLOT
            PHA
            BIT        $CFFF
            BIT        $C020                               ;RESET I/O SPACE
            LDA        #$00
            STA        EXPNSLOT
            TYA
            AND        #BITON4
            BEQ        POLL_IO
;
;  CALL SYSTEM CALL MANAGER; ON RETURN, PUT ERROR CODE IN
;  USER'S A REGISTER AND SET RETURN STATUS, THEN DISPATCH.
;
            TSX                                            ;CHECK FOR
            CPX        #<(B_SAVE-2)                        ;  REENTRANT
            BEQ        GIR020                              ;  SYSTEM CALL
            LDA        #BADBRK
            JSR        SYSDEATH
GIR020:     LDA        E_REG                               ;SELECT $C000 RAM
            AND        #BITOFF6
            STA        E_REG
            CLI                                            ;ENABLE INTERRUPTS
            JSR        SCMGR                               ;CALL THE SYSTEM CALL MGR
            LDA        #BACKBIT                            ; GET THE MASK
            STA        BACKMASK                            ; SET IT IN SYSGLOB
            JSR        CHKBUF
            SEI
            LDX        SP_SAVE
            LDA        Z_SAVE
            EOR        #BITON0                             ;SET ZERO PAGE TO
            STA        Z_REG                               ;  CALLER'S STACK
            LDA        SERR
            STA        <A_SAVE,X
            PHP
            LDA        <S_SAVE,X
            AND        #$7D
            STA        <S_SAVE,X
            PLA
            AND        #$82
            ORA        <S_SAVE,X
            STA        <S_SAVE,X
            JMP        DISPATCH
;PAGE
;
;  SET INTERRUPT ZERO PAGE AND SOS BANK
;    THEN POLL I/O DEVICES
;
POLL_IO:    BIT        E_IORA                              ;VERIFY THAT 'IRQ IS LOW
            BPL        PIO006
            INC        IRQCNTR                             ;BUMP FALSE IRQ COUNTER
            BNE        PIO004
            INC        IRQCNTR+1
PIO004:     JMP        DISPATCH
PIO006:     LDA        #0                                  ;SET INTERRUPT ZERO PAGE
            STA        Z_REG
            LDA        E_REG
            ORA        #BITON7                             ;FORCE 1 MHZ FOR
            STA        E_REG                               ;  READING ACIA STATUS
            AND        #BITOFF7
            LDX        #$01
            LDY        ACIASTAT                            ;ANY INTERRUPT ON ACIA?
            STA        E_REG
            BMI        PIO070
            LDA        E_IFR                               ;ANY INTERRUPT ON E-6522?
            BPL        PIO020                              ;  NO
            AND        E_IER
            LDY        #7
            LDX        #$02
PIO010:     LSR        A                                   ;CHECK FLAG BITS
            BCS        PIO070
            INX
            DEY
            BNE        PIO010
            BEQ        PIO035
PIO020:     LDA        D_IFR                               ;ANY INTERRUPT ON D-6522?
            BPL        PIO035
            AND        D_IER
            BIT        ANYSLOT                             ;ANY SLOT INTERRUPT?
            BNE        PIO040                              ;  YES
            LDY        #7
            LDX        #$09
PIO030:     LSR        A                                   ;CHECK FLAG BITS
            BCS        PIO070
            INX
            DEY
            BNE        PIO030
PIO035:     LDX        #$10                                ;INTERRUPT NOT FOUND
            BNE        PIO050
PIO040:     LDX        #$11
            BIT        SLOT1                               ;SLOT 1?
            BPL        PIO070
            INX
            BIT        SLOT2                               ;SLOT 2?
            BPL        PIO070
            LDA        E_IORA
            INX
            BIT        SLOT3                               ;SLOT 3?
            BEQ        PIO070
            INX
            BIT        SLOT4                               ;SLOT 4?
            BEQ        PIO070
            LDX        #$0A
;
;  BAD INTERRUPT -- SYSTEM DEATH
;
PIO050:     LDA        #BADINT1                            ;INTERRUPT NOT FOUND
            JSR        SYSDEATH
PIO060:     LDA        #BADINT2                            ;BAD ZERO PAGE ALLOCATION
            JSR        SYSDEATH
;
;  INTERRUPTING DEVICE FOUND
;    ALLOCATE ZERO PAGE AND CALL MASTER INTERRUPT HANDLER
;
;  NOTE:
;    SINCE READING THE ACIA'S STATUS REGISTER RESETS THE
;    DSR AND DCD BITS, THE STATUS READ BY THE POLLING
;    ROUTINE MUST BE PASSED TO THE INTERRUPT HANDLER;
;    THE Y REGISTER HAS BEEN SELECTED FOR THIS PURPOSE_
;    THE CURRENT IMPLEMENTATION DOES NOT USE Y IN CALLING
;    THE INTERRUPT HANDLER_  IF SUBSEQUENT REVISIONS
;    NEED TO USE Y, THE STATUS MUST BE PRESERVED AND
;    RESTORED BEFORE CALLING THE INTERRUPT HANDLER_
;
CALLMIH:    JMP        (IRQADDR)
;
PIO070:     LDA        SIRTABLE,X                          ;INTERRUPT ALLOCATED?
            BPL        PIO050                              ;  NO
            LDA        SIRADR_L,X                          ;GET INTERRUPT ADDRESS
            STA        IRQADDR
            ORA        SIRADR_H,X                          ;CHECK FOR ADDRESS = $00
            BEQ        PIO050                              ;  BAD ADDRESS
            LDA        SIRADR_H,X
            STA        IRQADDR+1
            LDA        SIRADR_B,X
            STA        B_REG
            LDA        ZPGSTACK                            ;ALLOCATE MIH ZERO PAGE
            CMP        #ZPGSTOP+ZPGSPACE
            BCC        PIO060                              ;TOO MANY NESTED INTERRUPTS
            SBC        #ZPGSPACE
            STA        ZPGSTACK
            STA        ZPGSP
            TAX
            JSR        CALLMIH                             ;CALL INTERRUPT HANDLER
            SEI
            LDA        #$00
            STA        Z_REG
            CLC
            LDA        ZPGSTACK                            ;DEALLOCATE MIH ZERO PAGE
            ADC        #ZPGSPACE
            STA        ZPGSTACK
            STA        ZPGSP
            LDA        #BITON1
            STA        D_IFR                               ;CLEAR ANY SLOT INTERRUPT
            JMP        DISPATCH
;SBTL "NON-MASKABLE INTERRUPT RECEIVER"
;***************************************************************************************************
;
;  THIS IS THE NON-MASKABLE INTERRUPT RECEIVER_  WHEN AN
;  NMI OCCURS, THE CPU PASSES CONTROL TO THE NMI RECEIVER
;  THROUGH THE NMI VECTOR_  THE OPERATION OF THE NMI
;  RECEIVER IS ESSENTIALLY THE SAME AS THE GIR EXCEPT
;  THAT IT IS NOT CONCERNED WITH BRK, AND THE ONLY VALID
;  SOURCE OF AN NMI IS THE KEYBOARD OR THE I/O DEVICE THAT
;  HAS ALLOCATED THE NMI RESOURCE_
;
;***************************************************************************************************
;
;
NMI_RCVR    =          *
;
;  SAVE CPU REGISTERS A, X, & Y ON CURRENT STACK
;
            PHA
            TXA
            PHA
            TYA
            PHA
;
;  CHECK FOR STACK OVERFLOW
;
            TSX
            CPX        #$FA
            BCC        NMI005
            LDA        #STKOVFL
            JSR        SYSDEATH
;
;  SET UP INTERRUPT ENVIRONMENT:
;    BINARY ARITHMETIC, 2 MHZ, I/O ENABLED,
;    RAM WRITE ENABLED, PRIMARY STACK,
;    AND $F000 RAM SELECTED_  PRESERVE
;    USER STATE OF SCREEN AND RESET LOCK.
;
NMI005:
            CLD
            LDA        E_REG
            TAX
            AND        #BITON5+BITON4
            ORA        #BITON6+BITON2
            STA        E_REG
;
;  IF NOT ALREADY ON PRIMARY STACK, SAVE USER'S
;  STACK POINTER AND SET UP SOS STACK POINTER_
;
            TXA
            AND        #BITON2
            BNE        NMI010
            TXA
            TSX
            STX        SP_SAVE
            LDX        #<E_SAVE
            TXS
            TAX
;
;  SAVE SYSTEM CONTROL REGISTERS E, Z, & B ON SOS STACK
;
NMI010:
            TXA
            PHA
            LDA        Z_REG
            PHA
            LDA        B_REG
            PHA
            LDA        EXPNSLOT
            PHA
            BIT        $CFFF
            BIT        $C020                               ;RESET I/O SPACE
            LDA        #$00
            STA        EXPNSLOT
;
;  SET INTERRUPT ZERO PAGE
;
            LDA        #0
            STA        Z_REG
;
;  SEE IF NMI IS FROM KEYBOARD OR I/O DEVICE
;
            LDA        E_IORB
            BMI        NMI030
;
;  NMI IS FROM I/O DEVICE
;
            LDA        SIRTABLE                            ;NMI ALLOCATED?
            BPL        NMI020
            JSR        CALLNMI
            SEI
            JMP        DISPATCH
CALLNMI:    LDA        SIRADR_L
            STA        NMIADR_L
            LDA        SIRADR_B
            STA        B_REG
            JMP        (NMIADR_L)
;
;  BAD INTERRUPT -- SYSTEM DEATH
;
NMI020:     LDA        #BADINT1                            ;NMI NOT ALLOCATED
            JSR        SYSDEATH
;
;  NMI IS FROM THE KEYBOARD
;
NMI030:     LDA        SYSBANK
            STA        B_REG
            JSR        KYBDNMI
            SEI
            JMP        DISPATCH
;SBTL "DISPATCHER"
;***************************************************************************************************
;
;  THIS IS THE DISPATCHER_  UPON COMPLETION, ALL SOS CALLS
;  AND INTERRUPT HANDLERS RETURN CONTROL TO THE DISPATCHER_
;  ITS PURPOSE IS TO SET UP THE APPROPRIATE ENVIRONMENT AND
;  PASS CONTROL TO WHATEVER CODE SHOULD RUN NEXT.
;
;  WHEN SOS IS INTERRUPTED, CONTROL ALWAYS RETURNS TO THE
;  INTERRUPTED CODE_  HOWEVER, WHEN THE USER IS INTERRUPTED,
;  BY EITHER A SOS CALL OR AN INTERRUPT, THE DISPATCHER
;  MUST CHECK THE EVENT QUEUE_  IF THERE IS AN ACTIVE EVENT
;  WITH A PRIORITY HIGHER THAN THE CURRENT EVENT FENCE,
;  CONTROL IS PASSED TO THE EVENT CODE_  OTHERWISE, CONTROL
;  RETURNS TO THE INTERRUPTED CODE_
;
;***************************************************************************************************
;
DISPATCH    =          *
;
;  DISABLE INTERRUPTS AND RESTORE
;  SYSTEM CONTROL REGISTERS B & Z
;
            SEI
            LDA        E_REG
            ORA        #BITON6                             ;ENABLE I/O
            STA        E_REG
            PLA
            JSR        SELC800                             ;RESTORE I/O SPACE
            PLA
            STA        B_REG
            PLA
            STA        Z_REG
;
;  CHECK SAVED ENVIRONMENT REGISTER
;  IF RETURNING TO PRIMARY STACK
;    THEN RESTORE E REG AND RELAUNCH SOS
;    ELSE RESET STACK POINTER & RESTORE E REG
;
            PLA
            ORA        #BITON5                             ;SET SCREEN STATE TO
            BIT        SCRNMODE                            ;  CURRENT SCREEN MODE
            BMI        DSP005
            AND        #BITOFF5
DSP005:
            TAY
            AND        #BITON2
            BEQ        DSP010
            STY        E_REG
            BNE        DSP030
DSP010:
            PLA
            TAX
            TXS
            STY        E_REG
;
;  CHECK FOR ACTIVE EVENT WITH PRIORITY > FENCE
;
DSP020:     LDA        CEVPRI
            LDX        EVQ_LINK
            CMP        EVQ_PRI,X
            BCS        DSP030
;
;  PROCESS ACTIVE EVENT TRAP
;  SAVE E, Z, B, & CALLER'S PRIORITY ON STACK THEN CALL
;  EVENT.  UPON RETURN, RESTORE PRIORITY, B, Z, & E THEN
;  CHECK FOR MORE EVENTS_
;
            LDA        E_REG
            PHA
            LDA        Z_REG
            PHA
            LDA        B_REG
            PHA
            LDA        CEVPRI
            PHA
            JSR        DO_EVENT
            SEI
            PLA
            STA        CEVPRI
            PLA
            STA        B_REG
            PLA
            STA        Z_REG
            PLA
            ORA        #BITON5                             ;SET SCREEN STATE TO
            BIT        SCRNMODE                            ;  CURRENT SCREEN MODE
            BMI        DSP025
            AND        #BITOFF5
DSP025:     STA        E_REG
            JMP        DSP020
;
;  RESTORE CPU REGISTERS Y, X, & A AND LAUNCH
;
DSP030:
            PLA
            TAY
            PLA
            TAX
            PLA
            RTI
;PAGE
;***************************************************************************************************
;
;  THIS SUBROUTINE CALLS THE HIGHEST PRIORITY ACTIVE EVENT.
;  FIRST, IT DELINKS THE FIRST ENTRY ON THE ACTIVE LIST AND
;  LINKS IT TO THE FREE LIST.  THEN, IT SETS UP THE BANK,
;  ADDRESS, ID, & STATUS AND CALLS THE EVENT VIA AN RTI_
;
;***************************************************************************************************
;
DO_EVENT    =          *
;
;  WRITE ENABLE RAM
;
            LDY        E_REG
            TYA
            AND        #BITOFF3
            STA        E_REG
;
;  DELINK ENTRY FROM ACTIVE LIST AND RELINK IT TO FREE LIST
;
            LDX        EVQ_LINK
            LDA        EVQ_LINK,X
            STA        EVQ_LINK
            LDA        EVQ_FREE
            STA        EVQ_LINK,X
            STX        EVQ_FREE
;
;  SET FENCE TO EVENT PRIORITY THEN RESTORE E REG
;
            LDA        EVQ_PRI,X
            STA        CEVPRI
            STY        E_REG
;
;  SET UP B, Z, E, ADDRESS, ID, & STATUS
;
            LDA        EVQ_BANK,X
            STA        B_REG
            LDA        EVQ_ADRH,X
            PHA
            LDA        EVQ_ADRL,X
            PHA
            LDY        EVQ_ID,X
            PHP
            PLA
            AND        #$82
            PHA
            TYA
            RTI
;SBTL "SYSTEM INTERNAL RESOURCES"
;***************************************************************************************************
;
;  SYSTEM INTERNAL RESOURCE NUMBERS
;
;
;  SIR  RESOURCE
;
;   0   SOUND PORT / I/O NMI
;   1   ACIA
;   2   E_CA2 -- KEYBOARD
;   3   E_CA1 -- CLOCK
;   4   E_SR
;   5   E_CB2 -- VBL +
;   6   E_CB1 -- VBL -
;   7   E_T2
;   8   E_T1
;   9   D_CA2 -- CSP INPUT FLAG / INPUT SWITCH 1
;   A   D_CA1 -- ANY SLOT (RESERVED FOR SOS)
;   B   D_SR  -- CSP DATA REGISTER
;   C   D_CB2 -- CSP DATA I/O / ENSIO
;   D   D_CB1 -- CSP CLOCK / ENSEL / A/D SELECT / INPUT SW3
;   E   D_T2
;   F   D_T1
;  10   DISK STEPPER / GRAPHICS SCROLL / CHARACTER DOWNLOAD
;  11   SLOT 1
;  12   SLOT 2
;  13   SLOT 3
;  14   SLOT 4
;  15   (UNASSIGNED)
;  16   (UNASSIGNED)
;  17   (UNASSIGNED)
;
;***************************************************************************************************
;SBTL "RESOURCE ALLOCATION & DEALLOCATION"
;***************************************************************************************************
;
;  RESOURCE ALLOCATION AND DEALLOCATION
;
;  SIRS ARE ALLOCATED AND DEALLOCATED BY THE SUBROUTINES
;  'ALLOCSIR' AND 'DEALCSIR'.  THE RESOURCE PARAMETERS ARE
;  PASSED IN A TABLE THAT CONTAINS ONE FIVE-BYTE ENTRY FOR
;  EACH SIR THAT IS TO BE ALLOCATED OR DEALLOCATED_  THE
;  TABLE ENTRY FORMAT IS SHOWN BELOW:
;
;           0       1       2       3       4
;       +-------+-------+-------+-------+-------+
;       | SIR # |  ID   | ADR_L | ADR_H | ADR_B |
;       +-------+-------+-------+-------+-------+
;
;  SIR # -- SYSTEM INTERNAL RESOURCE NUMBER
;  ID    -- IDENTIFICATION BYTE
;           SUPPLIED BY ALLOCSIR, CHECKED BY DEALCSIR
;  ADR   -- INTERRUPT ADDRESS (LOW, HIGH, BANK)
;           ZERO IF NO INTERRUPT HANDLER
;
;
;  ALLOCSIR -- ALLOCATE SYSTEM INTERNAL RESOURCES
;
;    PARAMETERS:
;      A:  NUMBER OF BYTES IN TABLE
;      X:  TABLE ADDRESS (LOW BYTE)
;      Y:  TABLE ADDRESS (HIGH BYTE)
;
;    NORMAL EXIT -- SIRS ALLOCATED
;      CARRY:  CLEAR
;      A, X, Y:  UNDEFINED
;
;    ERROR EXIT -- SIRS NOT ALLOCATED
;      CARRY:  SET
;      X:  SIR NUMBER
;      A, Y:  UNDEFINED
;
;
;  DEALCSIR -- DEALLOCATE SYSTEM INTERNAL RESOURCES
;
;    PARAMETERS:
;      A:  NUMBER OF BYTES IN TABLE
;      X:  TABLE ADDRESS (LOW BYTE)
;      Y:  TABLE ADDRESS (HIGH BYTE)
;
;    NORMAL EXIT -- SIRS DEALLOCATED
;      CARRY:  CLEAR
;      A, X, Y:  UNDEFINED
;
;    ERROR EXIT -- SIRS NOT DEALLOCATED
;      CARRY:  SET
;      X:  SIR NUMBER
;      A, Y:  UNDEFINED
;
;***************************************************************************************************
;PAGE
;
IDBYTE:     .BYTE      $81
;
ALLOCSIR    =          *
            CLC
            PHP
            SEI
            STA        SIRARGSIZ                           ;SAVE TABLE SIZE
            LDA        E_REG
            STA        SIRTEMP
            ORA        #BITON2                             ;FORCE PRIMARY STACK
            AND        #BITOFF3                            ;  AND WRITE ENABLE
            STA        E_REG
            LDA        SIRTEMP
            PHA
            LDA        Z_REG
            PHA
            LDA        #$00
            STA        Z_REG                               ;SET ZERO PAGE := $00
            STX        SIRARGS
            STY        SIRARGS+1                           ;SET POINTER TO TABLE
;
            LDY        #$00
ASIR010:    LDA        (SIRARGS),Y                         ;GET SIR NUMBER
            CMP        #SIRTBLSIZ
            TAX
            BCS        ASIR020
            LDA        SIRTABLE,X                          ;CHECK ALLOCATION
            BMI        ASIR020
            LDA        IDBYTE
            STA        SIRTABLE,X                          ;ALLOCATE SIR
            INY
            STA        (SIRARGS),Y                         ;RETURN ID BYTE
            INY
            LDA        (SIRARGS),Y
            STA        SIRADR_L,X                          ;SAVE INTERRUPT ADDRESS
            INY
            LDA        (SIRARGS),Y
            STA        SIRADR_H,X
            INY
            LDA        (SIRARGS),Y
            STA        SIRADR_B,X
            INY
            CPY        SIRARGSIZ
            BCC        ASIR010
;
            CLC
            INC        IDBYTE                              ;BUMP ID BYTE
            BMI        SIREXIT
            LDA        #$81
            STA        IDBYTE
            BMI        SIREXIT
;
ASIR020:    STX        SIRTEMP                             ;SAVE BAD SIR NUMBER
ASIR030:
            SEC
            TYA
            SBC        #5
            TAY
            BCC        ASIR040
            LDA        (SIRARGS),Y                         ;GET SIR NUMBER
            TAX
            LDA        #FALSE
            STA        SIRTABLE,X                          ;RELEASE ALLOCATED SIRS
            BEQ        ASIR030
;
ASIR040:    LDX        SIRTEMP                             ;RETURN BAD SIR
            SEC
;
;
;
SIREXIT:
            PLA
            STA        Z_REG                               ;RESTORE Z REGISTER
            PLA
            STA        E_REG                               ;RESTORE E REGISTER
            BCC        SIREXIT1
            PLA
            ORA        #BITON0
            PHA
SIREXIT1:
            PLP
            RTS
;
;
;
DEALCSIR    =          *
            CLC
            PHP
            SEI
            STA        SIRARGSIZ                           ;SAVE TABLE SIZE
            LDA        E_REG
            STA        SIRTEMP
            ORA        #BITON2                             ;FORCE PRIMARY STACK
            AND        #BITOFF3                            ;  AND WRITE ENABLE
            STA        E_REG
            LDA        SIRTEMP
            PHA
            LDA        Z_REG
            PHA
            LDA        #$00
            STA        Z_REG                               ;SET ZERO PAGE := $00
            STX        SIRARGS
            STY        SIRARGS+1                           ;SET POINTER TO TABLE
;
            LDY        #$00
DSIR010:    LDA        (SIRARGS),Y                         ;GET SIR NUMBER
            TAX
            CPX        #SIRTBLSIZ
            BCS        DSIR030
            INY
            LDA        SIRTABLE,X
            BPL        DSIR030                             ;VERIFY ALLOCATION
            CMP        (SIRARGS),Y
            BNE        DSIR030
            INY
            INY
            INY
            INY
            CPY        SIRARGSIZ
            BCC        DSIR010
;
            LDY        SIRARGSIZ
DSIR020:
            SEC
            TYA
            SBC        #5
            TAY
            BCC        SIREXIT
            LDA        (SIRARGS),Y                         ;GET SIR NUMBER
            TAX
            LDA        #FALSE
            STA        SIRTABLE,X
            BEQ        DSIR020
;
DSIR030:
            SEC
            BCS        SIREXIT
;SBTL "SELECT I/O EXPANSION ROM"
;***************************************************************************************************
;
;  SUBROUTINE 'SELC800' IS CALLED TO SELECT THE C800 I/O EX-
;  PANSION ADDRESS SPACE FOR A PERIPHERAL SLOT.  ON ENTRY,
;  THE SLOT NUMBER IS PASSED IN THE ACCUMULATOR_  IF NO
;  ERROR OCCURS, CARRY IS CLEARED; OTHERWISE, CARRY IS SET
;  AND THE PREVIOUS SLOT REMAINS SELECTED_
;
;  PARAMETERS:
;    A:  SLOT NUMBER
;
;  NORMAL EXIT -- NEW SLOT SELECTED
;    CARRY:  CLEAR
;    A:  UNDEFINED
;    X, Y:  UNCHANGED
;
;  ERROR EXIT -- SLOT NOT CHANGED
;    CARRY:  SET
;    A, X, Y:  UNCHANGED
;
;  WARNING !!!
;    'SELC800' USES SELF-MODIFYING CODE!
;
;***************************************************************************************************
;
SELC800     =          *
            CMP        #$05                                ;CHECK SLOT NUMBER
            BCS        SC8EXIT                             ;  INVALID
            PHP
            SEI
            STA        EXPNSLOT
            ORA        #$C0                                ;MAKE SLOT INTO $CN00
            STA        CNADDR+2                            ;  AND MODIFY BIT ADDRESS
            BIT        $C020
            BIT        $CFFF                               ;DESELECT PREVIOUS SLOT
CNADDR:     BIT        $C0FF                               ;  AND SELECT CURRENT SLOT
            PLP
SC8EXIT:
            RTS
;SBTL "NMI DISABLE / ENABLE"
;***************************************************************************************************
;
;  THE SUBROUTINES NMIDSBL AND NMIENBL ARE CALLED TO
;  DISABLE AND ENABLE NMI, RESPECTIVELY.  THERE ARE NO
;  INPUT PARAMETERS_  ON EXIT, THE REGISTERS ARE UN-
;  DEFINED_  NMIDSBL CLEARS THE CARRY FLAG IF NMI WAS
;  SUCCESSFULLY DISABLED; OTHERWISE, CARRY IS SET.
;
;***************************************************************************************************
;
NMIDSBL     =          *
            LDX        E_REG
            BIT        NMIFLAG
            BPL        NDS020
            TXA
            ORA        #BITON7
            STA        E_REG                               ;SET 1MHZ
            LDA        #$00
            STA        NMICNTR
            STA        NMICNTR+1
NDS010:     BIT        NMIFLAG                             ;NMI PENDING?
            BPL        NDS020                              ;  NO
            INC        NMICNTR                             ;BUMP NMI COUNTER
            BNE        NDS010                              ;  AND RECHECK NMI FLAG
            INC        NMICNTR+1
            BNE        NDS010
            LDA        #NMIHANG                            ;CAN'T LOCK NMI
            JSR        SYSDEATH
NDS020:     TXA                                            ;GET E_REG
            AND        #BITOFF4                            ;DISABLE NMI
            STA        E_REG
            RTS
;
;
;
NMIENBL     =          *
            LDA        E_REG
            ORA        #BITON4                             ;ENABLE NMI
            STA        E_REG
            RTS
;SBTL "KEYBOARD NMI HANDLER"
;***************************************************************************************************
;
;  BY DEFAULT, KEYBOARD NMI IS IGNORED_  THE USER MAY
;  PROCESS NMI BY CHANGING THE ADDRESS IN SYSTEM GLOBAL.
;
;***************************************************************************************************
;
NMIDBUG     =          *
            TSX                                            ;SAVE THE STACK POINTER
            STX        NMISPSV
            LDA        #$03                                ;SELECT MONITOR'S ZERO PAGE
            STA        Z_REG
            LDA        E_REG
            ORA        #$03                                ;SELECT MONITOR ROM
            STA        E_REG
            JSR        $F901                               ;CALL THE MONITOR
;
NMICONT     =          *
            LDA        E_REG
            ORA        #BITON2                             ;FORCE PRIMARY STACK
            STA        E_REG
            LDX        NMISPSV
            TXS                                            ;RESTORE STACK POINTER
            RTS
;SBTL "EVENT QUEUE MANAGER"
;***************************************************************************************************
;
;  THE EVENT QUEUE IS USED TO HOLD THE PARAMETERS OF EVENTS
;  THAT HAVE BEEN DETECTED BUT NOT YET RECOGNIZED_  EVENT
;  QUEUE ENTRIES ARE ORGANIZED INTO TWO LINKED LISTS; A FREE
;  LIST AND AN ACTIVE LIST.  EACH ENTRY IS SIX BYTES LONG,
;  WITH THE FIRST BYTE (BYTE 0) USED AS A LINK.  THE LINK
;  BYTE CONTAINS THE TABLE INDEX OF THE NEXT ENTRY IN THE
;  LIST.  BECAUSE OF THE INDEXING METHOD, THE EVENT QUEUE
;  MUST NOT EXCEED 256 BYTES_
;
;  ENTRY ZERO IS A SPECIAL ENTRY.  BYTE 0 IS THE INDEX OF
;  THE FIRST ACTIVE ENTRY; BYTE 1 CONTAINS A ZERO, ALLOWING
;  ENTRY 0 TO BE USED AS THE ACTIVE EVENT LIST TERMINATER;
;  BYTE 2 CONTAINS THE INDEX OF THE FIRST FREE ENTRY; AND
;  BYTES 4 THROUGH 6 ARE UNUSED_
;
;  THE FREE LIST IS LINKED LIFO.  THE ONLY VALID BYTE IN A
;  FREE ENTRY IS THE LINK BYTE; THE REMAINING BYTES ARE
;  UNDEFINED_  THE FREE LIST IS TERMINATED BY A LINK BYTE
;  CONTAINING A ZERO.
;
;  THE ACTIVE LIST IS LINKED IN DECREASING PRIORITY ORDER
;  WITH ENTRIES OF EQUAL PRIORITY LINKED FIFO.  BYTES 1
;  THROUGH 5 CONTAIN THE EVENT PRIORITY, EVENT ID, LOW BYTE
;  OF THE EVENT ADDRESS, HIGH BYTE OF THE EVENT ADDRESS, AND
;  THE ADDRESS BANK.  THE ACTIVE LIST IS TERMINATED BY AN
;  ENTRY WITH AN EVENT PRIORITY OF ZERO.
;
;***************************************************************************************************
;PAGE
;***************************************************************************************************
;
;  SUBROUTINE 'QUEEVENT' IS USED TO ENTER AN EVENT INTO THE
;  EVENT QUEUE_  ACTIVE EVENTS ARE LINKED IN DECREASING
;  PRIORITY ORDER WITH EVENTS OF EQUAL PRIORITY LINKED FIFO.
;  EVENTS ARE REMOVED FROM THE QUEUE AS THEY ARE RECOGNIZED
;  BY THE DISPATCHER_
;
;  PARAMETERS:
;    X:  EVENT PARAMETER ADDRESS (LOW BYTE)
;    Y:  EVENT PARAMETER ADDRESS (HIGH BYTE)
;
;    EVENT       0       1       2       3       4
;    PARMS:  +-------+-------+-------+-------+-------+
;            |  PRI  |   ID  | ADR_L | ADR_H | ADR_B |
;            +-------+-------+-------+-------+-------+
;            PRI:  EVENT PRIORITY
;            ID:   EVENT ID BYTE
;            ADR:  EVENT ADDRESS (LOW, HIGH, BANK)
;
;  EXIT CONDITIONS:
;    CARRY:  CLEAR
;    A, X, Y:  UNDEFINED
;
;***************************************************************************************************
;
QUEEVENT    =          *
            CLC
            PHP
            SEI
            LDA        E_REG
            STA        QEVTEMP
            ORA        #BITON2                             ;FORCE PRIMARY STACK
            AND        #BITOFF3                            ;  AND WRITE ENABLE
            STA        E_REG
            LDA        QEVTEMP
            PHA
            LDA        Z_REG
            PHA
            LDA        #0
            STA        Z_REG                               ;SET ZERO PAGE := 0
;
            STX        QEVARGS
            STY        QEVARGS+1                           ;SET ARGUMENT POINTER
            LDY        #0
            LDA        (QEVARGS),Y                         ;GET PRIORITY
            BEQ        Q_EXIT                              ;  IGNORE IF ZERO
;
            LDX        EVQ_FREE
            BEQ        Q_FULL
            STX        QEV_THIS                            ;GET FIRST FREE ENTRY
            LDA        EVQ_LINK,X                          ;  AND DELINK IT
            STA        EVQ_FREE
;
            LDY        #EVQ_SIZ-2
QEV010:     LDA        (QEVARGS),Y                         ;COPY ARGUMENTS
            STA        EVQ_BANK,X                          ;  INTO NEW ENTRY
            DEX
            DEY
            BPL        QEV010
;
            LDX        QEV_THIS
            LDY        #0
QEV020:     STY        QEV_LAST
            LDA        EVQ_LINK,Y
            TAY
            LDA        EVQ_PRI,Y                           ;SCAN EVENT QUEUE
            CMP        EVQ_PRI,X                           ;  FOR PROPER POSITION
            BCS        QEV020
;
            TYA
            STA        EVQ_LINK,X                          ;RELINK EVENT INTO QUEUE
            TXA
            LDY        QEV_LAST
            STA        EVQ_LINK,Y
;
Q_EXIT:
            PLA
            STA        Z_REG                               ;RESTORE Z REGISTER
            PLA
            STA        E_REG                               ;RESTORE E REGISTER
            PLP
            RTS
;
Q_FULL:     LDA        #EVQOVFL                            ;EVENT QUEUE OVERFLOW
            JSR        SYSDEATH
;LST ON
ZZEND       =          *
ZZLEN       =          ZZEND-ZZORG
            .IF        ZZLEN-LENIPL
            .FATAL     "SOSORG FILE IS INCORRECT FOR IPL"
            .ENDIF

