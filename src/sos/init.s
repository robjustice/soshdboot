;SBTL "SOS 1.1 INITIALIZATION"
;.RELOC
             .SEGMENT   "CODE"
             .INCLUDE   "SOSORG"
             .ORG       ORGINIT
ZZORG:
;MSB OFF
;***************************************************************************************************
;          COPYRIGHT (C) APPLE COMPUTER INC.  1981
;                    ALL RIGHTS RESERVED
;***************************************************************************************************
;
; SOS INIT MODULE (VERSION = 1.1O   )
;                 (DATE    = 8/04/81)
;
;***************************************************************************************************
;
             .EXPORT    INT_INIT
             .EXPORT    EVQ_INIT
             .EXPORT    CLK_INIT
             .EXPORT    MMGR_INIT
             .EXPORT    BMGR_INIT
             .EXPORT    DMGR_INIT
             .EXPORT    CFMGR_INIT
             .EXPORT    BFM_INIT
;
;  EXTERNAL SUBROUTINES & DATA
;
             .IMPORT    SXPAGE
             .IMPORT    SYSDEATH
;
;  INTERRUPT SYSTEM INITIALIZATION
;
             .IMPORT    COLDSTRT
             .IMPORT    IRQ_RCVR
             .IMPORT    NMI_RCVR
             .IMPORT    NMIFLAG
             .IMPORT    SIRTABLE
             .IMPORTZP  SIRTBLSIZ
             .IMPORT    ZPGSTACK
             .IMPORTZP  ZPGSTART
;
;  EVENT QUEUE INITIALIZATION
;
             .IMPORT    EV_QUEUE
             .IMPORT    EVQ_LEN
             .IMPORTZP  EVQ_CNT
             .IMPORTZP  EVQ_SIZ
             .IMPORT    EVQ_FREE
             .IMPORT    EVQ_LINK
;
;  CLOCK INITIALIZATION
;
             .IMPORT    PCLOCK
;
;  CHARACTER FILE MANAGER INITIALIZATION
;
             .IMPORTZP  CFCB_MAX
             .IMPORT    CFCB_DEV
;
;  DEVICE MANAGER INITIALIZATION
;
             .IMPORT    DMGR
             .IMPORT    MAX_DNUM
;
;  BUFFER MANAGER INITIALIZATION
;
             .IMPORTZP  BUF_CNT
             .IMPORT    PGCT_T
             .IMPORT    XBYTE_T
             .IMPORT    BUFREF
;
;  MEMORY MANAGER INITIALIZATION
;
             .IMPORTZP  ST_CNT
             .IMPORT    ST_ENTRY
             .IMPORT    ST_FREE
             .IMPORT    ST_FLINK
             .IMPORT    VRT_LIM
             .IMPORT    MEMSIZE
             .IMPORTZP  MEM2SML
;
;  BLOCK FILE MANAGER INITIALIZATION
;
             .IMPORT    FCBZPP
             .IMPORT    PATHBUF
             .IMPORT    VCB
             .IMPORT    WORKSPC
             .IMPORT    PFIXPTR
             .IMPORT    FCBADDRH
             .IMPORTZP  BMAPAGE
             .IMPORTZP  BMBPAGE
             .IMPORT    BMAMADR
             .IMPORT    BMBMADR
             .IMPORTZP  BFMFCB1
             .IMPORTZP  BFMFCB2
;
;  CONSTANT DECLARATIONS
;
TRUE         =          $80
FALSE        =          $00
BITON6       =          $40
BITON7       =          $80
;
;  SYSTEM CONTROL REGISTERS
;
E_REG        =          $FFDF                                ;ENVIRONMENT REGISTER
Z_REG        =          $FFD0                                ;ZERO PAGE REGISTER
;SBTL "INTERRUPT SYSTEM INITIALIZATION"
;
;  6522 REGISTERS
;
D_DDRB       =          $FFD2
D_DDRA       =          $FFD3
D_ACR        =          $FFDB
D_PCR        =          $FFDC
D_IFR        =          $FFDD
D_IER        =          $FFDE
E_IORB       =          $FFE0
E_DDRB       =          $FFE2
E_DDRA       =          $FFE3
E_ACR        =          $FFEB
E_PCR        =          $FFEC
E_IFR        =          $FFED
E_IER        =          $FFEE
ACIASTAT     =          $C0F1
;
;
;***************************************************************************************************
;
;  THIS SUBROUTINE INITIALIZES THE INTERRUPT SYSTEM.
;  ALL HARDWARE INTERRUPTS ARE MASKED AND THE
;  INTERRUPT ALLOCATION TABLE IS CLEARED.
;
;***************************************************************************************************
;
;
INT_INIT:
             SEI                                             ;DISABLE INTERRUPTS
             LDA        #ZPGSTART                            ;SET UP MIH
             STA        ZPGSTACK                             ;  ZERO PAGE STACK POINTER
;
             LDA        E_REG                                ;SELECT $C000 I/O SPACE
             PHA                                             ;  AND SET 1 MHZ
             ORA        #BITON7+BITON6
             STA        E_REG
;
             STA        ACIASTAT                             ;RESET ACIA
;
             LDA        #$FF                                 ;SET UP 6522 D
             STA        D_DDRB
             STA        D_DDRA
             LDA        #$00
             STA        D_ACR
             LDA        #$76
             STA        D_PCR
             LDA        #$7F
             STA        D_IFR
             STA        D_IER
             LDA        #$82
             STA        D_IER
;
             LDA        #$3F                                 ;SET UP 6522 E
             STA        E_DDRB
             LDA        #$0F
             STA        E_DDRA
             LDA        #$00
             STA        E_ACR
             LDA        #$63
             STA        E_PCR
             LDA        #$7F
             STA        E_IFR
             STA        E_IER
;
             LDA        #$FF
             STA        E_IORB                               ;SOUND PORT
             BIT        $C0D8                                ;DISABLE GRAPHICS SCROLL
             BIT        $C0DA                                ;DISABLE CHARACTER DOWNLOAD
             BIT        $C0DC                                ;DISABLE ENSEL
             BIT        $C0DE                                ;SET ENSIO FOR INPUT
;
             PLA                                             ;RESTORE E REGISTER
             STA        E_REG
;
             LDA        #FALSE
             STA        NMIFLAG                              ;CLEAR NMI WAIT FLAG
             LDY        #SIRTBLSIZ-1
INTI010:     STA        SIRTABLE,Y                           ;  ALLOCATION TABLE
             DEY
             BPL        INTI010
             LDA        #TRUE
             STA        SIRTABLE+$0A                         ;LOCK DOWN ANY SLOT SIR
;
             LDX        #$05
INTI020:     LDA        RAMVECT,X                            ;SET UP VECTORS
             STA        $FFFA,X                              ;  AT $FFFA - $FFFF
             LDA        RAMJMPS,X                            ;SET UP JMP INSTRUCTIONS
             STA        $FFCA,X                              ;  AT $FFCA - $FFCF
             DEX
             BPL        INTI020
             RTS
;
RAMVECT:     .WORD      NMI_RCVR
             .WORD      COLDSTRT
             .WORD      IRQ_RCVR
RAMJMPS:     JMP        NMI_RCVR
             JMP        IRQ_RCVR
;SBTL "EVENT QUEUE INITIALIZATION"
;***************************************************************************************************
;
;  THIS SUBROUTINE INITIALIZES THE EVENT QUEUE.  ALL ENTRIES
;  ARE CLEARED AND LINKED INTO THE FREE LIST.  THE ACTIVE
;  LIST IS EMPTY.
;
;***************************************************************************************************
;
;
EVQ_INIT:
;
;  CLEAR ALL ENTRIES
;
             LDY        #<EVQ_LEN
             LDA        #0
EVQI010:     STA        EV_QUEUE-1,Y
             DEY
             BNE        EVQI010
;
;  SET UP FREE LIST
;
             LDX        #EVQ_CNT-2
             LDA        #EVQ_SIZ
             STA        EVQ_FREE
EVQI020:
             TAY
             CLC
             ADC        #EVQ_SIZ
             STA        EVQ_LINK,Y
             DEX
             BNE        EVQI020
             RTS
;SBTL "PSEUDO CLOCK INITIALIZATION"
;***************************************************************************************************
;
;  THIS SUBROUTINE INITIALIZES THE PSEUDO CLOCK.  IF THE
;  RAM BEHIND THE "D" 6522 HAS THE PROPER CHECKSUM, IT
;  IS USED TO INITIALIZE THE PSEUDO CLOCK.  OTHERWISE,
;  THE PSEUDO CLOCK IS SET TO ZERO.
;
; (ADDED 23 OCT 81)
; BOTH THE CLOCK AND PSEUDO CLOCK ARE
; ARE NOW INITIALIZED
;
;***************************************************************************************************
;
PCLK         =          $F0
CKSUM        =          $F2
CLKICR       =          $11                                  ; CLOCK INTERRUPT CONTROL REG
CLKSTBY      =          $16                                  ; CLOCK STANDBY INTERRUPT
CLOCK        =          $C070
;
CLK_INIT:
             LDA        #$D0
             STA        PCLK                                 ;POINT (PCLK) TO 8F:FFD0
             LDA        #$FF
             STA        PCLK+1
             LDA        #$8F
             STA        SXPAGE+PCLK+1
             LDA        #$A5
             STA        CKSUM                                ;INITIALIZE CHECKSUM
;
             LDY        #$00
CLK010:      LDA        (PCLK),Y                             ;COPY SAVED CLOCK DATA
             STA        PCLOCK,Y                             ;  TO PSEUDO CLOCK
             EOR        CKSUM
             STA        CKSUM                                ;UPDATE CHECKSUM
             INY
             CPY        #$0A
             BCC        CLK010
;
             CMP        (PCLK),Y                             ;TEST CHECKSUM
             BEQ        CLK030
;
             LDA        #$00
CLK020:
             DEY
             STA        PCLOCK,Y                             ;ZERO PSEUDO CLOCK
             BNE        CLK020
CLK030:      LDA        E_REG
             PHA
             ORA        #$80                                 ; SET 1 MHZ
             STA        E_REG
             LDA        #$00
             LDY        Z_REG
             LDX        #CLKICR
             STX        Z_REG
             STA        CLOCK                                ; DISABLE CLOCK INTERRUPTS
             LDX        #CLKSTBY
             STX        Z_REG
             STA        CLOCK                                ; DISABLE STANDBY INTERRUPT
             STY        Z_REG
             PLA
             STA        E_REG
             RTS
;SBTL "CHARACTERFILE MANAGER INIT"
;***************************************************************************************************
;
; CHAR FILE MANAGER INITIALIZATION ROUTINE
;
; CFMGR_INIT INITIALIZES ALL ENTRIES IN THE CFCB TABLE TO
; THE "FREE" STATE.
;
;***************************************************************************************************
;
CFMGR_INIT:
             LDA        #$80
             LDX        #CFCB_MAX-1
CFINIT010:   STA        CFCB_DEV,X
             DEX
             BPL        CFINIT010
             RTS
;SBTL "DEVICE MANAGER INITIALIZATION"
;***************************************************************************************************
;
; DEVICE MANAGER INITIALIZATION ROUTINE
;
; INITIALIZES THE SYSTEM DEVICE TABLE (SDT) BY WALKING THE
; DEVICE INFORMATION BLOCK (DIB) LINKS.  CALLED BY SYSLDR.
;
;***************************************************************************************************
;
D_TPARMX     =          $C0
REQCODE      =          D_TPARMX+$00
DNUM         =          D_TPARMX+$01
DNUM_TEMP:   .RES       1
;
;
DMGR_INIT:
             LDX        MAX_DNUM
             INC        MAX_DNUM                             ; MAX_DNUM:=MAX DEV NUMBER IN SYSTEM+1
             STX        DNUM_TEMP
DMI110:      LDA        #8                                   ; INITIALIZE ALL DEVICES IN SYSTEM (D_INIT)
             STA        REQCODE
             LDA        DNUM_TEMP
             STA        DNUM
             JSR        DMGR
             DEC        DNUM_TEMP
             BNE        DMI110
             RTS                                             ; NORMAL EXIT
;SBTL "BUFFER MANAGER INITIALIZATION"
;***************************************************************************************************
;
; BMGR_INIT
;
; THIS ROUTINE INITIALIZES THE BUFFER TABLE'S ENTRIES TO "FREE".
; CALLED DURING SYSTEM BOOT.
;
;***************************************************************************************************
;
BMGR_INIT:
             LDA        #$FF                                 ; USED WHEN FINDING LOWEST BUFFER IN TBL (BUFCOMPACT)
             STA        XBYTE_T
;
             LDX        #BUF_CNT-1
             LDA        #$80
BUFI010:     STA        PGCT_T,X                             ;SET ALL ENTRIES "FREE"
             DEX
             BNE        BUFI010
;
             STX        BUFREF                               ;ZERO COUNT BYTE IN BUFFER REFERENCE TABLE
;
             CLC
             RTS
;SBTL "MEMORY MANAGER INITIALIZATION"
;***************************************************************************************************
;
; MMGR_INIT
;
; THIS ROUTINE INITIALIZES THE MEMORY MANAGER'S SEGMENT TABLE
; TO FREE ENTRIES, AND DETERMINES THE MEMORY SIZE OF THE
; MACHINE (96K,128K,160K,192K,224K,256K,..,512K IN 32K STEPS).
;
;***************************************************************************************************
;
MMGR_INIT:
;
; INIT SEGMENT TABLE
;
             LDA        #0
             STA        ST_ENTRY
             LDA        #$81
             STA        ST_FREE
;
             LDY        #ST_CNT-1
             LDA        #$80                                 ; SET LAST LINK TO NULL
             STA        ST_FLINK,Y
MEMI010:
             TYA
             ORA        #$80
             DEY
             STA        ST_FLINK,Y
             BNE        MEMI010
;
; COMPUTE VIRTUAL LIMIT FROM MEMORY SIZE
; VRT_LIM := NUMBER OF PAGES IN BANK SWITCHED MEMORY - 1
;         := (MEMSIZ-2)*64 - 1
;         := (MEMSIZ-4)*64 + 127
;
             SEC
             LDA        MEMSIZE
             SBC        #4
             BCC        MEMI_ERR
             LSR        A
             LSR        A
             STA        VRT_LIM+1
             LDA        #$FE
             ROR        A
             STA        VRT_LIM
             CLC
             RTS                                             ; NORMAL EXIT
;
MEMI_ERR:    LDA        #MEM2SML                             ; FATAL ERR - MEM < 64K
             JSR        SYSDEATH
;PAGE
;***************************************************************************************************
;
;  BLOCK FILE MANAGER INITIALIZATION
;
;***************************************************************************************************
;
SISTER       =          $1400                                ;BFM XPAGE
BFM_INIT:
             LDA        #BFMFCB1                             ; ADDRESS OF PAGE 1 OF FCB
             STA        <FCBZPP+1
             LDA        #BFMFCB2                             ; AND PAGE 2
             STA        <FCBZPP+3
             LDA        #0
             STA        <FCBZPP                              ; FCB PAGE ALIGNED
             STA        <FCBZPP+2
             STA        SISTER+FCBZPP+1                      ; PREPARE PART OF EXTEND BYTE
             STA        SISTER+FCBZPP+3
             TAY                                             ; MAKE ZERO INTO INDEX
CLRBUFFS:
             STA        PATHBUF,Y                            ; PATHNAME BUFFER PAGE
             STA        VCB,Y                                ; VOLUME CONTROL BLOCK PAGE
             STA        (<FCBZPP),Y                          ; BOTH FILE CONTROL BLOCK PAGES
             STA        (<FCBZPP+2),Y
             INY
             BNE        CLRBUFFS
             LDX        #$3F                                 ; SIZE OF MY ZERO PAGE STUFF
CLRZWRK:     STA        0,X                                  ; ZERO PAGE ZEROED
             STA        WORKSPC,X
             DEX
             BPL        CLRZWRK
             LDA        #>PATHBUF
             STA        PFIXPTR+1
             LDA        #BFMFCB1
             STA        FCBADDRH
             LDA        #BMAPAGE                             ; BIT MAP A PAGE NUMBER
             STA        BMAMADR
             LDA        #BMBPAGE                             ; BIT MAP B PAGE NUMBER
             STA        BMBMADR
             CLC
             RTS
;
;LST ON
ZZEND:
ZZLEN        =          ZZEND-ZZORG
; add padding to make $200 bytes long
PADDING      =          $200-ZZLEN
             .RES       PADDING

             .IF        ZZLEN-LENINIT
             .FATAL     "SOSORG FILE IS INCORRECT FOR INIT"
             .ENDIF

