; Update sos loader for hard disk booting
; - the path names specifiy .PROFILE instead of D1
;    I_PATH  ".PROFILE/SOS.INTERP"
;    D_PATH  ".PROFILE/SOS.DRIVER"
;    PREFX_PATH  ".PROFILE"
;
; - BFMINIT2 is not called, this removes the copy protection check from
;    SOS to speed things up a little. Assume things on the HD are the unprotected versions
;
; - Add support to boot either unit, .profile or .pb2
;    addtional paths included that we copy over as required
;    I_PATH2  ".PB2/SOS.INTERP"
;    D_PATH2  ".PB2/SOS.DRIVER"
;    set K_DRIVES to 2, then don't update from driver file
;
; Updates by Robert Justice
;
; macro to support setting the most significant bit on for ascii strings with EDASM MSB ON
              .macro     ASCMSBON s
              .repeat    .strlen(s), i
              .byte      .strat(s,i) | $80
              .endrepeat
              .endmacro

;SBTL "SOS 1.1 SOS LOADER" 
;.RELOC
              .SEGMENT   "CODE"
              .INCLUDE   "SOSORG"
              .ORG       $1E00
ZZORG         =          *
;MSB OFF
;***************************************************************************************************
;          COPYRIGHT (C) APPLE COMPUTER INC. 1980
;                    ALL RIGHTS RESERVED
;***************************************************************************************************
;
;        SOS KERNEL LOAD & MEMORY POINTS
;
;  MODULE      START  END   I/O  ROM  SOS BLOAD    SIZE
;------------------------------------------------------
;  SOSLDR      1E00 - 28F7            2000         0CF8
;  INIT        28F8 - 2AA9            2AF8        [01B2]
;  SYSGLOB     18FC - 1A03            2CF8
;
;  BFM_INIT2 + BITMAPS
;              B800 - BBFF            2E00         03FF
;  BFM         BC00 - DE62            3200         2263
;  <PATCH>     DE63 - DE6A            5463         0008
;
;  OPRMSG      DE6B - E48A   X        546B         015A
;  IPL         DFC5 - E48F   X    X   55C5         04CB
;  UMGR        E48B - E89D   X    X   5A8B         040E      -- umgr start was incorrect had E490
;
;  DISK3       E899 - EE03   X    X   5E99         056B
;  SYSERR      EE04 - EED8   X        64D9         00D5
;  DEVMGR      EED9 - F05D            64D9         0185
;
;  SCMGR       F05E - F2F3            665E         0296
;  FMGR        F2F4 - F354            68F4         0061
;  CFMGGR      F355 - F551            6955         01FD
;
;  BUFMGR      F552 - F86D            6B52         031C
;  MEMMGR      F86E - FFBE            6E6E         0751
;  <END>       FFBE
;
;***************************************************************************************************
; SOS LOADER  (VERSION = 1.1O   ) 
;             (DATE    = 8/04/81)
;
; SOURCE FILES:  SOSLDR.SRC,   SOSLDR.A.SRC, SOSLDR.B.SRC, SOSLDR.C.SRC, 
;                              SOSLDR.D.SRC, SOSLDR.E.SRC, SOSLDR.F.SRC
;
; FUNCTION:
;    MOVES AND INITIALIZES SOS KERNEL, READS INTERPRETER FROM DISK, READS CHARACTER SET TABLE, 
;    KEYBOARD TABLE AND DRIVERS FROM DISK, INITIALIZES ALL DRIVERS AND THEN JUMPS TO INTERPRETER 
;    ENTRY POINT.
;
; CALLED BY:
;    SOSBOOT 7.0 WITH KERNEL FILE LOADED AT $I:1E00.9FFF(MAX) 
;    WHERE: $I=INTERPRETER BANK (HIGHEST BANK IN SYSTEM)
;
; CALLS:
;    INTERPRETER ENTRY POINT (FIRST BYTE OF INTERPRETER CODE)
;
; DOCUMENTS:
;    SOS ERS APPENDICES - XX/XX/81 
;    APPLE III I/O SYSTEM PROGRAMMERS GUIDE - DEC-15-80
;
; CONSTRAINTS:
;    INTERPRETER FILE:  READ INTO BANK 0 BEGINNING AT $80:LDREND+$400(=BUFSIZE).     
;                       INTERPRETER CODE DOES NOT CONTAIN RELOCATION INFORMATION.
;                       MAX = 38K  ($I:2000..B7FF) 
;                       MIN = .25K ($I:B700..B7FF) 
;
;    DRIVER FILE:  READ INTO BANK 0 BEGINNING AT $80:LDREND+$400(=BUFSIZE). 
;                  DRIVER MODULES ARE RELOCATED AND MOVED TO THE HIGHEST AVAILABLE 32K BANK USING 
;                  A "FIRST FIT" ALGORITHM.  MODULES ARE REMOVED FROM THE FILE BEGINNING AT THE BACK 
;                  AND WORKING TOWARD THE FRONT.  A DRIVER MODULE CANNOT SPAN A BANK BOUNDARY. 
;
;                  DRIVER FILE:  MAX = 60K  (APPROX)       DRIVER MODULE:  MAX = 32K-1 
;                                MIN = .25K                                MIN < .25K 
;
;
; DATA STRUCTURES:
;    SOS.KERNEL FILE FORMAT
;    SOS.INTERP FILE FORMAT
;    SOS.DRIVER FILE FORMAT
;
;***************************************************************************************************
;PAGE
;***************************************************************************************************
;
; NOTATION:
;
;    A, X, Y           ::= 6502 REGISTERS
;
;    C, OV             ::= CARRY, OVERFLOW FLAGS IN 6502 STATUS (P) REGISTER
;    E, Z, B           ::= ENVIRONMENT, ZERO PAGE, BANK REGISTERS (SYSTEM CONTROL REGISTERS)
;
;    (1.I.S.R:W_P.R.R) ::= ENVIRONMENT REGISTER FLAGS.  FROM LEFT TO RIGHT BITS 7..0 
;                          (1MHZ, I/O ENABLE, SCREEN ENABLE, RESET ENABLE, 
;                           WRITE PROTECT, PRIMARY STACK, ROM1, ROM ENABLE)
;
;    "POSITIVE LOGIC"  ::= ALL LOGIC USED IS POSITIVE LOGIC.  FOR EXAMPLE, C="NO DRIVERS LEFT" 
;                          INDICATES THAT NO DRIVERS ARE LEFT WHEN CARRY = SET, AND THAT ONE OR 
;                          MORE DRIVERS ARE LEFT WHEN CARRY = CLEAR.
;
;    TRUE,FALSE        ::= TRUE = SET = ON, WHILE FALSE = CLEAR = OFF.
;
;***************************************************************************************************
;
; ABBREVIATIONS:
;
;    DIB               ::= DEVICE INFORMATION BLOCK.  DEFINES A UNIQUE DEVICE THAT CAN BE LINKED 
;                          INTO THE SYSTEM DEVICE TABLE.  EACH DRIVER MODULE CONTAINS ONE OR MORE 
;                          DIBS (DEVICES) EACH OF WHICH CAN BE "ACTIVE" OR "INACTIVE".
;
;    ADIB              ::= "ACTIVE DIB"
;
;   <VARNAME>.P        ::=  POINTER.  A 3 BYTE ZERO PAGE POINTER.  DON'T FORGET THE X BYTE!
;
;    SDT               ::= SYSTEM DEVICE TABLE.  CONTAINS THE ENTRY POINT AND DIB ADDRESS OF EACH 
;                          DEVICE CONFIGURED INTO THE SYSTEM, (USED BY THE DEVICE MANAGER).
;***************************************************************************************************
;PAGE
;***************************************************************************************************
;
;   $1E00 +---------------+
;         !    SOSLDR     !<-ENTRY        SOS MEMORY MAP
;   $1FFF +---------------+  -----       (128K APPLE ///)
;
;              BANK 0               BANK 1               BANK 2
;   $2000 +---------------+    +---------------+    +---------------+
;         !               !    !               !    !               !
;         !               !    !               !    !    SOSLDR     !
;         !               !    !               !    !      &        !
;         !               !    !               !    !  INIT MODULE  !
;         !               !    !               !    !               !
;         !               !    !               !    ! - - - - - - - !
;         !               !    !               !    !    GLOBALS    !
;         !               !    !               !    ! - - - - - - - !
;         !               !    !               !    !               !
;         !               !    !               !    !               !
;         !               !    !               !    !               !         !
;         !               !    !               !    !               !         !
;         !               !    !               !    !               !         !
;         !               !    !               !    !               !         !
;         !               !    !               !    !               !         !
;         !               !    !               !    !               !         !
;         !               !    !               !    !               !  KERNEL !
;         !               !    !               !    !               !         !
;         !               !    !               !    !               !         !
;         !               !    !               !    !               !         !
;         !               !    !               !    !               !         !
;         !               !    !               !    !               !         !
;         !               !    !               !    !               !         !
;         !               !    !               !    !               !         !
;         !               !    !               !    !               !         !
;         !               !    !               !    !               !         !
;         !               !    !               !    !               !-- EOF --!
;         !               !    !               !    !               !         !
;         !               !    !               !    !               !         !
;         !               !    !               !    !               !         !
;         !               !    !               !    !               !         !
;         !               !    !               !    !               !         !
;   $9FFF +---------------+    +---------------+    +---------------+
;
;
;   $A000 +---------------+
;     .   !    SOSBOOT    !
;     .   +---------------+
;
;
;  FIGURE 1.  SOS KERNEL FILE READ INTO $2:1E00..9FFF BY SOS BOOT IN BLOCKS 0,1.
;             SOS LOADER BEGINS EXECUTION AT THIS POINT.
;
;
;
;
;***************************************************************************************************
;PAGE
;***************************************************************************************************
;
;   $1E00 +---------------+
;         !    SOSLDR     !          SOS MEMORY MAP
;   $1FFF +---------------+          (128K APPLE ///)
;
;               BANK 0               BANK 1               BANK 2
;   $2000 +---------------+    +---------------+    +---------------+
;         !               !    !               !    !               !         !
;         !    SOSLDR     !    !               !    !               !
;         !      &        !    !               !    !               !
;         !  INIT MODULE  !    !               !    !               !
;         !               !    !               !    !               !         !
;  LDREND ! - - - - - - - !    !               !    !               !
;         !  FILE BUFFER  !    !               !    !               !
;         ! - - - - - - - !    !               !    !               !
;         !               !    !               !    !               !         !
;         !               !    !               !    !               !         !
;         !               !    !               !    !               !         !
;         !               !    !               !    !               !         !
;         !               !    !               !    !               !         !
;         !               !    !               !    !               !         !
;         !  INTERPRETER  !    !  INTERPRETER  !    !               !
;         !     FILE      !    !     FILE      !    !               !
;         !               !    !               !    !               !         !
;         !               !    !               !    !               !         !
;         !               !    !               !    !               !         !
;         !               !    !               !    !               !         !
;         !               !    !               !    !               !         !
;         !               !    !               !    !               !         !
;         !               !    !               !    !               !         !
;         !               !    !               !    !               !         !
;         !               !    !               !    !               !         !
;         !               !    !               !    !               !         !
;         !               !    !               !    !               !         !
;         !               !    !               !    !               !         !
;         !               !    !               !    !               !         !
;         !               !    !               !    !               !         !
;         !               !    !               !    !               !         !
;         !               !    !- - - EOF - - -!    !               !
;   $9FFF +---------------+    +---------------+    +---------------+
;
;
;
;
;  FIGURE 2.  SOS INTERPRETER FILE READ INTO BANKS 0 AND 1
;             USING EXTENDED ADDRESSING (X=$80).
;
;
;
;
;
;***************************************************************************************************
;PAGE
;: 

;*******************************************************************************
;
;:    $1E00 +---------------+
;:          !    SOSLDR     !       SOS MEMORY MAP
;:    $1FFF +---------------+       (128K APPLE ///)
;
;:               BANK 0                BANK 1               BANK 2
;:    $2000 +---------------+    +---------------+    +---------------+
;:          !               !    !               !    !               !
;:          !    SOSLDR     !    !               !    !               !
;:          !      &        !    !               !    !               !
;:          !  INIT MODULE  !    !               !    !               !
;:          !               !    !               !    !               !
;:          ! - - - - - - - !    !               !    !               !
;:          !  FILE BUFFER  !    !               !    !               !
;:          ! - - - - - - - !    !               !    !               !
;:          !               !    !               !    !               !
;:          !               !    !               !    !               !             !
;:          !               !    !               !    !               !             !
;:          !               !    !               !    !               !             !
;:          !               !    !               !    !               !             !
;:          !               !    !               !    !               !             !
;:          !               !    !               !    !               !             !
;:          !               !    !               !    !               !             !
;:          !               !    !               !    !               !             !
;:          !               !    !               !    !               !             !
;:          !               !    !               !    !               !             !
;:          !               !    !               !    !               !             !
;:          !               !    !               !    !               ! - - - - - - !
;:          !    DRIVER     !    !    DRIVER     !    !               !
;:          !     FILE      !    !     FILE      !    !               !
;:          !               !    !               !    !               !             !
;:          !               !    !               !    !               !             !
;:          !               !    !               !    !               ! INTERPRETER !
;:          !               !    !               !    !               !     CODE    !
;:          !               !    !               !    !               !             !
;:          !               !    !               !    !               !             !
;:          !               !    !               !    !               !             !
;:          !               !    !               !    !               !             !
;:          !               !    !- - - EOF - - -!    !               !
;:    $9FFF +---------------+    +---------------+    +---------------+
;
;
;
;
;:                FIGURE 3.  SOS DRIVER FILE READ INTO BANKS 0 AND 1
;:                           USING EXTENDED ADDRESSING (X=$80).
;
;
;
;
;*******************************************************************************
;!BITROT: 

;PAGE
;!BITROT: 

;:          !               !    !               !    !        !               !
;:    $9FFF +---------------+    +---------------+    !        +---------------+
;:                                                    !
;:                                                    !
;:                                                    !   (SYSTEM DEVICE TABLE)
;:                                                    !
;:   FIGURE 4. SOS LOADER FINISHED.  JUMP TO         DIB   ADR   BANK  UNIT
;:             FIRST BYTE OF INTERPRETER'S CODE.   !-----!-----!-----!-----!
;:                                                 !     !     !     !     !
;:                                                 !     !     !     !     !
;:                                                 !     !     !     !     !
;:                                                 !     !     !     !     !
;:                                                 !-----!-----!-----!-----!
;
;
;
;*******************************************************************************
;PAGE
;***************************************************************************************************
;
; SUBROUTINES:
;
; SOSLDR                "MAIN PROGRAM" 
;
;    SOSLDR1            "PROCESSES KERNEL/INTERPRETER/DRIVER FILES" 
;
; (1)   MOVE            "MOVES SRC_P..SRC_P+CNT-1 TO DST_P..DST_P+CNT-1" 
;
;       INIT_KRNL       "CALLS KERNEL INITIALIZATION MODULES" 
;
;       WELCOME         "PRINTS WELCOME MESSAGE ("APPLE ///", VERSION, DATE/TIME, COPYRIGHT) 
;
;       ADVANCE         "ADVANCES WRK.PTR TO NEXT INTERP/KERNEL MODULE.  INITS SRC_P, DST_P, CNT FOR MOVE" 
;
;       REVERSE         "REVERSES TITLE/CODE/RELOC COUNTS TO ALLOW DRIVER FILE TO BE PROCESSED FM BACK TO FRONT" 
;
;       DADVANCE        "ADVANCES WORK_P TO NEXT DRIVER MODULE.  INITS SRC_P, CNT, REL_P FOR MOVE" 
;
;          DADD         "ADVANCES WORK_P TO NEXT DRIVER FIELD" 
;
;       FLAGS           "PROCESSES "INACTIVE" & "PAGE ALIGN" FLAGS IN DRIVER MODULE'S DIBS" 
;    
;          NEXT_DIB     "ADVANCES TO NEXT DIB IN DRIVER MODULE" 
;
;       GETMEM          "COMPUTES DESTINATION BASE ADDRESS FOR NEXT DRIVER MODULE" 
;
;          NEWDST       "COMPUTES DESTINATION BASE ADDRESS, ALIGNING ON PAGE BOUNDARY IF REQUESTED" 
;
;          BUILD_DSEG   "COMPUTES # OF PAGES TO ADD TO DRIVER SEGMENT AND WHETHER TO BEGIN A NEW SEGMENT" 
;
;       RELOC           "RELOCATES DRIVER MODULE'S CODE FIELD USING RELOCATION FIELD" 
;
; (1)   LINK            "LINKS FIRST DIB TO PREVIOUS DRIVER'S LAST "ACTIVE" DIB, AND ADDS SDT ENTRY" 
;
;          SET_DRIVES   "INITIALIZES DIB LINKS IN KERNEL'S FLOPPY DRIVER"
;
; (1)      ALLOC_DEV    "ADDS A NEW ENTRY TO THE DEVICE MANAGER'S SYSTEM DEVICE TABLE (SDT)" 
;
;       ALLOC_SEG       "ALLOCATES SEGMENTS FOR KERNEL, INTERPRETER AND SYSTEM WORK AREA" 
;
;          RSEG         "CALLS MEMORY MANAGER TO ALLOCATE SEGMENTS FOR THE KERNEL AND INTERPRTER" 
;
;       ALLOC_DSEG      "ALLOCATES SEGMENTS FOR DRIVER MODULES" 
;
;    ERROR              "DISPLAYS ERROR MESSAGE, SOUNDS BELL AND LOOPS UNTIL CONTROL/RESET PRESSED" 
;
; (1) - INDICATES THAT THE ROUTINE PERFORMS BANK SWITCHING AND MUST(!) BE OUTSIDE THE 32K RAM BANKS.
;***************************************************************************************************
;PAGE
;***************************************************************************************************
;
; SOS.KERNEL FILE FORMAT
;
;  (8)   LABEL                    <---+ 
;          = "SOS KRNL"               ! 
;                                     ! 
;  (2)   HEADER COUNT                 ! 
;        HEADER                       ! 
;          = # OF FLOPPY DRIVES       !    CONTAINED IN THIS LISTING  
;          = INTERPRETER PATHNAME     ! 
;          = DRIVER PATHNAME          ! 
;                                     ! 
;  (4)   ADR & COUNT                  ! 
;        SOSLDR CODE              <---+ 
;
;  (4)   ADR & COUNT 
;        GLOBALS 
;
;  (4)   ADR & COUNT 
;        KERNEL CODE 
; 
;***************************************************************************************************
;
; SOS.INTERP FILE FORMAT
;
;  (8)   LABEL
;           = "SOS NTRP"
;
;  (2)   HEADER COUNT
;
;  (4)   ADR & COUNT
;        INTERPRETER CODE
;
;***************************************************************************************************
;
; SOS.DRIVER FILE FORMAT
;
;  (8)   LABEL
;           = "SOS DRVR"
;
;  (2)   HEADER COUNT
;           = # OF FLOPPY DRIVES
;           = CHARACTER SET TABLE
;           = KEYBOARD TABLE
;        ...
;                                                               +---------------------------------------+ 
;  (2)   DM #N TITLE COUNT        <---+                         !       RELOCATION FIELD FORMAT         ! 
;              TITLE FIELD            !                         !       -----------------------         ! 
;  (2)   DM #N CODE  COUNT            !    DRIVER MODULE #N     ! CONSISTS OF A LIST OF 2 BYTE POINTERS ! 
;              CODE  FIELD            !                         ! WHICH POINT TO THE LOW BYTE OF A TWO  !  
;  (2)   DM #N RELOC COUNT            !                         ! BYTE QUANTITY TO BE RELOCATED.        ! 
;              RELOC FIELD        <---+                         +---------------------------------------+ 
;        ...
;
;        $FFFF = THE END
;
;***************************************************************************************************
;PAGE
;***************************************************************************************************
;
; SOSLDR - EXTERNAL DECLARATIONS 
;
;***************************************************************************************************
              .IMPORT    SYSBANK
              .IMPORT    MEMSIZE
              .IMPORT    SCRNMODE
              .IMPORT    SOSVER
              .IMPORTZP  SOSVERL
;
              .IMPORT    INT_INIT                                    ; (IPL) INTERRUPT INIT 
              .IMPORT    EVQ_INIT                                    ; (IPL) EVENT QUEUE INIT 
              .IMPORT    DMGR_INIT                                   ; DEVICE MANAGER INIT 
              .IMPORT    MAX_DNUM                                    ;        "
              .IMPORTZP  SDT_SIZE
              .IMPORT    SDT_DIBL
              .IMPORT    SDT_DIBH
              .IMPORT    SDT_ADRL
              .IMPORT    SDT_ADRH
              .IMPORT    SDT_BANK
              .IMPORT    SDT_UNIT
              .IMPORTZP  BLKD_SIZE
              .IMPORT    BLKDLST
              .IMPORT    CFMGR_INIT                                  ; CHAR FILE MANAGER INIT 
              .IMPORT    MMGR_INIT                                   ; MEMORY MANAGER INIT 
              .IMPORT    BMGR_INIT                                   ; BUFFER FILE MANAGER INIT 
              .IMPORT    BFM_INIT                                    ; BLOCK FILE MANAGER INIT 
              .IMPORT    BFM_INIT2                                   ; BLOCK FILE MANAGER INIT2
              .IMPORT    CLK_INIT                                    ; CLOCK SYSTEM CALL INIT
; 
              .IMPORT    DIB1                                        ; ON BOARD DISK DRIVER'S DIBS (1-4) 
              .IMPORT    DIB2
              .IMPORT    DIB3
              .IMPORT    DIB4
;
;ENTRY I_BASE_P ; USED BY BFM_INIT2  (HARDWIRED!) 
;PAGE
;***************************************************************************************************
;
; FILE DATA DECLARATIONS 
;
;***************************************************************************************************
; KERNEL FILE
;***************************************************************************************************
K_FILE:       .BYTE      "SOS KRNL"
K_HDR_CNT:    .WORD      LDR_ADR-K_DRIVES
K_DRIVES:     .BYTE      $2                             ;update to allow to boot of either unit (drive)
K_FLAGS:      .BYTE      $0                                          ; RESERVED FOR FUTURE USE 

I_PATH:       .BYTE      $13
              .BYTE      ".PROFILE/SOS.INTERP"
              .RES       $18-$14

I_PATH2:      .BYTE      $0F
              .BYTE      ".PB2/SOS.INTERP"
              .RES       $18-$10

D_PATH:       .BYTE      $13
              .BYTE      ".PROFILE/SOS.DRIVER"
              .RES       $18-$14
              
D_PATH2:      .BYTE      $0F
              .BYTE      ".PB2/SOS.DRIVER"
              .RES       $18-$10
              
LDR_ADR:      .WORD      $0
LDR_CNT:      .WORD      ZZEND-SOSLDR
;***************************************************************************************************
; INTERPRETER/DRIVER FILES    <--+ 
; ERROR MESSAGES                 !    DEFINED IN BACK OF THIS LISTING 
; WELCOME MESSAGES            <--+ 
;***************************************************************************************************
;PAGE
;***************************************************************************************************
; 
; SOSLDR - DATA DECLARATIONS (1) 
;
;***************************************************************************************************
TRUE          =          $80
FALSE         =          $0
;
Z_REG         =          $FFD0
E_REG         =          $FFDF
B_REG         =          $FFEF
;
CZPAGE        =          $1A00
CSPAGE        =          $1B00
CXPAGE        =          $1600
SZPAGE        =          $1800
SXPAGE        =          $1400
SSPAGE        =          $0100
;
ROM_ADR       =          $F1B9
ROM_ID        =          $A0
;PAGE 
;***************************************************************************************************
;
; SOSLDR - DATA DECLARATIONS (2)
;
;***************************************************************************************************
ZPAGE         =          $00
;
K_BASE        =          ZPAGE+$0                                    ; SOSLDR1 SUBROUTINE    +--------------------------------------+ 
I_BASE_P      =          ZPAGE+$2                                    ;                       ! <VARNAME>.P ::= 3 BYTE ZPAGE POINTER ! 
RDBUF_P       =          ZPAGE+$4                                    ;                       +--------------------------------------+ 
SYSBUF_P      =          ZPAGE+$6
TEMP_BANK     =          ZPAGE+$8
TEMP_ADRH     =          ZPAGE+$9
WORK_P        =          ZPAGE+$A
;
REV_SAVE      =          ZPAGE+$C                                    ; REVERSE SUBROUTINE
;
FIRST_ADIB    =          ZPAGE+$10                                   ; FLAGS SUBROUTINE 
PREV_ADIB_P   =          ZPAGE+$12
DIB_P         =          ZPAGE+$14
PG_ALIGN      =          ZPAGE+$16
DIB_FLAGS     =          $14
DIB_DCB       =          $20
;
PREVBANK      =          ZPAGE+$18                                   ; GETMEM SUBROUTINE
PREVDST       =          ZPAGE+$19
;
CODE_P        =          ZPAGE+$1C                                   ; RELOCATION SUBROUTINE 
REL_P         =          ZPAGE+$1E
REL_END       =          ZPAGE+$20
;     
SRC_P         =          ZPAGE+$22                                   ; MOVE SUBROUTINE 
DST_P         =          ZPAGE+$24
CNT           =          ZPAGE+$26
;
DSTBANK       =          ZPAGE+$2A                                   ; LINK SUBROUTINE 
LINK_P        =          ZPAGE+$2C
;
DIB_ENTRY     =          2                                           ; ALLOC_DEV SUBROUTINE
DIB_UNIT      =          4+16+2
DIB_DTYPE     =          4+16+3
;
ETEMP         =          ZPAGE+$2E                                   ; ERROR SUBROUTINE
; 
WTEMP         =          ZPAGE+$2F                                   ; WELCOME SUBROUTINE
;
P_UNIT        =          $343                                        ; PRODOS BLOCK INTERFACE UNIT
                                                                     ; Need to look at page 3xx as Z_REG is updated

;PAGE
;***************************************************************************************************
;
; SOS LOADER - 
;
; (MAIN PROGRAM)
;***************************************************************************************************
SOSLDR        =          *                                           ;                                          +---------------+    
              LDA        #0                                          ; ZERO SOS/USER X, Z AND STACK PAGES       ! SEE FIGURE 1. !              
              TAX                                                    ;                                          +---------------+ 
SLDR010:      STA        CZPAGE,X
              STA        CXPAGE,X
              STA        CSPAGE,X
              STA        SZPAGE,X
              STA        SXPAGE,X
              STA        SSPAGE,X

DEX
              BNE        SLDR010
;                                       ; SETUP SOS CALL ENVIRONMENT (WRITE PROTECT=OFF) 
              LDA        #$30                                        ; E:=( 0.0.1.1:0.0.0.0 ) 
              STA        E_REG                                       ;    ( 1.I.S.R:W_P.R.R ) 
;
              LDX        #$FB                                        ; CONSOLE 1.0 MODIFIES STACK DURING D_INIT CALL 

TXS
              LDA        #>CZPAGE                                    ; ZREG:=CALLER'S Z PAGE 
              STA        Z_REG
;                                       ; +--------------------------------+ 
              JSR        SOSLDR1                                     ; ! PROCESS KRNL/INTERP/DRVR FILES ! 
;                                       ; +--------------------------------+ 
              LDA        E_REG
              AND        #$10                                        ; SETUP SOS CALL ENVIRONMENT (WRITE PROTECT=ON) 
              ORA        #$28                                        ; E:=( 0.0.1.X:1.0.0.0 ) 
              STA        E_REG                                       ;    ( 1.I.S.R:W_P.R.R ) 
;
              LDX        #$FF                                        ; STACK_REG:=$FF 
              TXS
              LDA        #>CZPAGE                                    ; ZREG:=CALLER'S Z PAGE
              STA        Z_REG
;                                                                                  +---------------+ 
              LDA        SYSBANK                                     ; BREG:=SYSBANK                            ! SEE FIGURE 4. ! 
              STA        B_REG                                       ;                                          +---------------+
              JMP        (I_BASE_P)                                  ; SOS LOAD COMPLETE - JMP TO INTERPRETER 
;
;THE END.
;***************************************************************************************************
;PAGE
;*************************************************************************************************** 
;
; MOVE ( IN:   SRC_P
;        IN:   DST_P
;        IN:   A="BANK"
;        IN:   CNT      )
;
;        LOCAL:  END
; (MOVES SRC_P..SRC_P+CNT-1 TO DST_P..DST_P+CNT-1)                "CNT PARM IS DESTROYED" 
;***************************************************************************************************
MOVE          =          *
              TAX
              LDA        B_REG                                       ; SAVE BANK REGISTER 
              PHA
              STX        B_REG                                       ; BREG:=A
              LDA        CNT+1                                       ; IF CNT <> 0 
              ORA        CNT                                         ;    THEN 
              BEQ        MOVE_EXIT
              LDA        CNT                                         ;       CNT:=CNT-1
              BNE        MOVE010
              DEC        CNT+1
MOVE010:      DEC        CNT
              CLC                                                    ;       SRC_P:=SRC_P+PAGE.CNT
              LDA        SRC_P+1
              ADC        CNT+1
              STA        SRC_P+1
              LDA        DST_P+1                                     ;       DST_P:=DST_P+PAGE.CNT
              ADC        CNT+1
              STA        DST_P+1
              INC        CNT+1                                       ;       PAGE.CNT:=PAGE.CNT+1
              LDY        CNT                                         ;       Y:=BYTE.CNT 
              BEQ        MOVE020                                     ;       IF Y=0 THEN M2 
;
MOVE_PAGE:    LDA        (SRC_P),Y                                   ;M1:    DO 
              STA        (DST_P),Y                                   ;          (DST_P),Y:=(SRC_P),Y 
              DEY                                                    ;          Y:=Y-1 
              BNE        MOVE_PAGE                                   ;       UNTIL  Y=0 
MOVE020:      LDA        (SRC_P),Y                                   ;M2:    (DST_P),Y:=(SRC_P),Y 
              STA        (DST_P),Y
              DEY                                                    ;       Y:=Y-1 
              DEC        SRC_P+1                                     ;       SRC_P:=SRC_P-256 
              DEC        DST_P+1                                     ;       DST_P:=DST_P-256 
              DEC        CNT+1                                       ;       PAGE.CNT:=PAGE.CNT-1 
              BNE        MOVE_PAGE                                   ;       IF PAGE.CNT <> 0 THEN M1 
;
              INC        SRC_P+1                                     ; RESTORE SRC_P
              INC        DST_P+1                                     ;    "    DST_P
;
MOVE_EXIT:    PLA                                                    ; RESTORE BANK REGISTER
              STA        B_REG
              RTS
;PAGE
;***************************************************************************************************
;
; LINK ( IN:   DST_P
;        IN:   DSTBANK
;        IN:   PREVBANK
;        IN:   FIRST_ADIB
;        I/O:  SDT_TBL
;        I/O:  BLKDLST
;        OUT:  LINKED DRIVER MODULE )
;
;        OWN:  LINK_P
; (LINKS FIRST DIB TO PREVIOUS DRIVER'S LAST "ACTIVE" DIB, AND ADDS SDT ENTRY) 
;***************************************************************************************************
LINK          =          *
              CLC                                                    ; FIRST_ADIB:=0:DST_P+FIRST_ADIB 
              LDA        DST_P
              ADC        FIRST_ADIB
              STA        FIRST_ADIB
              LDA        DST_P+1
              ADC        FIRST_ADIB+1
              STA        FIRST_ADIB+1
              LDA        #0
              STA        CXPAGE+FIRST_ADIB+1
              LDA        PREVBANK                                    ; BREG:=PREVBANK
              STA        B_REG
              LDY        #0                                          ; (LINK_P):=FIRST_ADIB
              LDA        FIRST_ADIB
              STA        (LINK_P),Y
              INY
              LDA        FIRST_ADIB+1
              STA        (LINK_P),Y
              LDA        DSTBANK                                     ; BREG:=DSTBANK
              STA        B_REG
              LDA        FIRST_ADIB                                  ; LINK_P:=FIRST_ADIB
              STA        LINK_P
              LDA        FIRST_ADIB+1
              STA        LINK_P+1
WALKLINKS:    JSR        ALLOC_DEV                                   ; ALLOC_DEV(LINK_P BREG.IN, SDT_TBL BLKDLST.IO)
LINK010:      LDY        #0                                          ; WHILE (LINK_P) <> 0 AND (LINK_P) <> LINK_P
              LDA        (LINK_P),Y
              INY
              ORA        (LINK_P),Y
              BEQ        LINK100
              LDA        (LINK_P),Y
              CMP        LINK_P+1
              BNE        LINK030
              DEY
              LDA        (LINK_P),Y
              CMP        LINK_P
              BEQ        LINK100
LINK030:      LDY        #0                                          ;    DO  LINK_P:=(LINK_P) 
              LDA        (LINK_P),Y
              TAX
              INY
              LDA        (LINK_P),Y
              STX        LINK_P
              STA        LINK_P+1
              JSR        ALLOC_DEV                                   ;     "  ALLOC_DEV(LINK_P BREG.IN, SDT_TBL BLKDLST.IO)
              JMP        LINK010
; 
LINK100:      LDY        #0                                          ; (LINK_P):=0 
              TYA
              STA        (LINK_P),Y
              INY
              STA        (LINK_P),Y
              DEY                                                    ; BREG:=0 
              STY        B_REG
              RTS
;
;
;
;
; LINK_INIT ( IN:   A=# DRIVES
;             IN:   DIB1..4
;             I/O:  SDT_TBL
;             I/O:  BLKDLST    )
;
LINK_INIT     =          *
              JSR        SET_DRIVES                                  ; SET_DRIVES(A=#DRIVES.IN, DIB1..4.IN)
              LDA        #0
              STA        MAX_DNUM                                    ; MAXDNUM:=0
              STA        BLKDLST                                     ; BLKDLST:=0
              STA        CXPAGE+LINK_P+1                             ; LINK_P:=0:DIB1 
              LDA        #<DIB1
              STA        LINK_P
              LDA        #>DIB1
              STA        LINK_P+1
              JMP        WALKLINKS
;PAGE
;***************************************************************************************************
;
; ALLOC_DEV ( IN:   LINK_P
;             IN:   B_REG
;             I/O:  SDT_TBL                                                   (SYSTEM DEVICE TABLE) 
;                      IN:   SDT_SIZE  = CONSTANT
;                      IN:   DIB_ENTRY = CONSTANT                       DEV   DIB   ADR  BANK  UNIT 
;                      IN:   DIB_UNIT  = CONSTANT                           !-----!-----!-----!-----! 
;                      IN:   DIB_DTYPE = CONSTANT                        1  !     !     !     !     ! 
;                      I/O:  MAX_DNUM                                    2  !     !     !     !     ! 
;                      OUT:  SDT_BANK                                    .  !     !     !     !     ! 
;                      OUT:  SDT_DIB                                     .  !     !     !     !     ! 
;                      OUT:  SDT_ADR                                     .  !-----!-----!-----!-----! 
;                      OUT:  SDT_UNIT                                 MAX_DNUM 
;             I/O:  BLKDLST 
;                      IN:   BLKD_SIZE = CONSTANT
; (ADDS A NEW ENTRY TO THE DEVICE MANAGER'S SYSTEM DEVICE TABLE (SDT)) 
;***************************************************************************************************
ALLOC_DEV     =          *
              INC        MAX_DNUM                                    ; MAX_DNUM:=MAX_DNUM+1 
              LDX        MAX_DNUM                                    ; IF MAX_DNUM >= SDT_SIZE 
              CPX        #SDT_SIZE                                   ;    THEN 
              BCC        ADEV010
              LDX        #ERR8X                                      ;       ERROR("TOO MANY DEVICES") 
              LDY        #ERR8L
              JSR        ERROR
ADEV010:      LDA        B_REG                                       ; SDT_BANK,X:=BREG
              STA        SDT_BANK,X
              CLC                                                    ; SDT_DIB,X:=LINK_P+4
              LDA        LINK_P
              ADC        #4
              STA        SDT_DIBL,X
              LDA        LINK_P+1
              ADC        #0
              STA        SDT_DIBH,X
              SEC                                                    ; SDT_ADR,X:=(LINK_P),DIB_ENTRY-1 
              LDY        #DIB_ENTRY
              LDA        (LINK_P),Y
              SBC        #1
              STA        SDT_ADRL,X
              INY
              LDA        (LINK_P),Y
              SBC        #0
              STA        SDT_ADRH,X
              LDY        #DIB_UNIT                                   ; SDT_UNIT,X:=(LINK_P),DIB_UNIT
              LDA        (LINK_P),Y
              STA        SDT_UNIT,X
              LDY        #DIB_DTYPE                                  ; IF (LINK_P),DIB_DTYPE = "BLOCK DEVICE" 
              LDA        (LINK_P),Y
              BPL        ADEV_EXIT
              TXA                                                    ;    THEN 
              INC        BLKDLST                                     ;       BLKDLST:=BLKDLST+1
              LDX        BLKDLST                                     ;       IF BLKDLST >= BLKD_SIZE 
              CPX        #BLKD_SIZE                                  ;          THEN 
              BCC        ADEV020
              LDX        #ERR9X                                      ;             ERROR("TOO MANY BLOCK DEVICES")
              LDY        #ERR9L
              JSR        ERROR
ADEV020:      STA        BLKDLST,X                                   ;       BLKDLST,X:=MAX_DNUM
ADEV_EXIT:    RTS                                                    ; RETURN 
;PAGE
;***************************************************************************************************
;
; SOSLDR1 () 
;
; (PROCESSES KERNEL/INTERPRETER/DRIVER FILES) 
;***************************************************************************************************
SOSLDR1       =          *
              LDX        #$1F                                        ; COPY ROM'S DISK CORE ROUTINE ZPAGE VARS TO SOS ZPAGE 
LDR010:       LDA        $380,X
              STA        SZPAGE,X
              DEX
              BPL        LDR010
;***************************************************************************************************
; PROCESS KERNEL FILE
;***************************************************************************************************
;
; MOVE AND INITIALIZE SOS GLOBALS 
;
              LDA        #<LDR_ADR                                   ; WORK_P:=0:LDR_ADR 
              STA        WORK_P
              LDA        #>LDR_ADR
              STA        WORK_P+1
              JSR        ADVANCE                                     ; ADVANCE(WORK_P.IO, SRC_P DST_P CNT.OUT) 
;
              LDA        B_REG                                       ; MOVE(SRC_P DST_P A=BREG CNT.IN) 
              JSR        MOVE
;
              LDA        B_REG                                       ; SYSBANK:=BREG 
              AND        #$0F
              STA        SYSBANK
              ASL        A                                           ; MEMSIZ:=SYSBANK*2+4 "16K CHUNKS" 
              CLC
              ADC        #4
              STA        MEMSIZE                                     ; AND, MEMSIZE (SIZE IN 16K BYTE "CHUNKS")
;
; MOVE KERNAL CODE 
;
              JSR        ADVANCE                                     ; ADVANCE(WORK_P.IO, SRC_P DST_P CNT.OUT) 
;
              LDA        DST_P                                       ; K_BASE:=DST_P 
              STA        K_BASE
              LDA        DST_P+1
              STA        K_BASE+1
              LDA        B_REG                                       ; MOVE(SRC_P DST_P A=BREG CNT.IN) 
              JSR        MOVE
;
; MOVE LOADER TO BANK 0 AND SWITCH FROM SYSTEM BANK TO BANK 0
;
              LDA        #<$2000                                     ; MOVE(SRC_P=0:2000 DST_P=8F:2000 A=BREG CNT=LDR.END-$2000) 
              STA        SRC_P
              STA        DST_P
              LDA        #>$2000
              STA        SRC_P+1
              STA        DST_P+1
              LDA        #$8F
              STA        CXPAGE+DST_P+1
              LDA        #<(LDREND-$2000)
              STA        CNT
              LDA        #>(LDREND-$2000)
              STA        CNT+1
              LDA        B_REG
              JSR        MOVE
              LDA        #0                                          ; BREG:=0 
              STA        B_REG
;
; INITIALIZE SDT TABLE, KERNEL AND PRINT WELCOME MESSAGE 
;
              LDA        K_DRIVES                                    ; LINK_INIT(A=K_DRIVES DIB1..4.IN, SDT_TBL BLKDLST.IO)
                                                    ;original sos sets only one drive active (K_DRIVES) for boot
                                                    ;K_DRIVES is now set to 2 to allow booting of either drive (unit)
              JSR        LINK_INIT                  ;the link_init sets the last DIB link to zero based on the number of drives
              JSR        SET_UNIT                   ;set the unit to load the SOS.INTERP & SOS.DRIVER files
              JSR        INIT_KRNL                                   ; INIT_KRNL() 
              JSR        WELCOME                                     ; WELCOME() 
;
              LDA        E_REG                                       ; ENABLE ROM BANK
              ORA        #$03
              STA        E_REG
              LDA        ROM_ADR                                     ; IF MONITOR ROM <> NEW
              CMP        #ROM_ID                                     ;    THEN
              BEQ        LDR020
              LDX        #ERR7X                                      ;       ERROR("ROM ERROR:  PLEASE NOTIFY YOUR DEALER")
              LDY        #ERR7L
              JSR        ERROR
LDR020:       LDA        E_REG                                       ; DISABLE ROM BANK
              AND        #$F6
              STA        E_REG
;***************************************************************************************************
; PROCESS INTERPRETER FILE 
;***************************************************************************************************
;
; OPEN SOS INTERPRETER FILE (DEFAULT='SOS.INTERP')
;
              LDY        I_PATH                                      ; OPEN(PATHNAME:=I_PATH 
LDR030:       LDA        I_PATH,Y                                    ;      REFNUM=OPEN_REF 
              STA        PATH,Y                                      ;      SYSBUF_P:=80:LDREND-2000 ) 
              DEY
              BPL        LDR030
;
              LDA        #<(LDREND-$2000)
              STA        SYSBUF_P
              LDA        #>(LDREND-$2000)
              STA        SYSBUF_P+1
              LDA        #$80
              STA        CXPAGE+SYSBUF_P+1
;
;
              BRK
              .BYTE      OPEN
              .WORD      OPEN_PARMS
              BEQ        LDR040
              LDX        #ERR1X                                      ; ERROR("INTERPRETER FILE NOT FOUND") 
              LDY        #ERR1L
              JSR        ERROR
LDR040:       LDA        OPEN_REF
              STA        READ_REF
              STA        CLOSE_REF
;
; READ IN ENTIRE INTERPRETER FILE
;
              LDA        #$80                                        ; READ(REFNUM=READ_REF  
              STA        CXPAGE+RDBUF_P+1                            ;      RDBUF_P:=80:FILE   
              LDA        #<FILE                                      ;      BYTES=$FFFF-FILE+1 
              STA        RDBUF_P                                     ;      BYTESRD=I.BYTESRD ) 
              LDA        #>FILE
              STA        RDBUF_P+1
;
              BRK
              .BYTE      READ
              .WORD      READ_PARMS
              BEQ        LDR050
              LDX        #ERR0X                                      ; ERROR("I/O ERROR") 
              LDY        #ERR0L
              JSR        ERROR
;                                                                                  +---------------+ 
; CLOSE INTERPRETER FILE AND CHECK LABEL                                           ! SEE FIGURE 2. ! 
;                                                                                  +---------------+ 
LDR050:       BRK                                                    ; CLOSE(REFNUM=CLOSE_REF) 
              .BYTE      CLOSE
              .WORD      CLOSE_PARMS
              LDY        #7                                          ; CHECK LABEL
LDR051:       LDA        (RDBUF_P),Y
              CMP        I_LABEL,Y
              BNE        LDR052
              DEY
              BPL        LDR051
              BMI        LDR053
LDR052:       LDX        #ERR2X                                      ; ERROR("INVALID INTERPRETER FILE")
              LDY        #ERR2L
              JSR        ERROR
;
; MOVE INTERPRETER CODE 
;
LDR053:       LDA        #<(I_HDR_CNT-2)                             ; WORK_P:=80:I_HDR_CNT-2 
              STA        WORK_P
              LDA        #>(I_HDR_CNT-2)
              STA        WORK_P+1
              LDA        #$80
              STA        CXPAGE+WORK_P+1
;
              JSR        ADVANCE                                     ; ADVANCE(WORK_P.IO, SRC_P DST_P CNT.OUT) 
;
              LDA        DST_P                                       ; I_BASE_P:=0:DST_P 
              STA        I_BASE_P
              LDA        DST_P+1
              STA        I_BASE_P+1
              LDA        #0
              STA        CXPAGE+I_BASE_P+1
;
              CLC                                                    ; IF DST_P+CNT > K_BASE THEN ERROR 
              LDA        CNT
              ADC        DST_P
              TAX
              LDA        CNT+1
              ADC        DST_P+1
              CPX        K_BASE
              SBC        K_BASE+1
              BEQ        LDR070
              BCC        LDR070
              LDX        #ERR3X                                      ; ERROR("INCOMPATIBLE INTERPRETER") 
              LDY        #ERR3L
              JSR        ERROR
;
LDR070:       LDA        SYSBANK                                     ; MOVE(SRC_P=RDBUF_P DST_P A=SYSBANK CNT.IN) 
              JSR        MOVE
;*************************************************************************************************** 
; PROCESS DRIVER FILE
;*************************************************************************************************** 
;
; OPEN SOS DRIVER FILE (DEFAULT='SOS.DRIVER')
;
              LDY        D_PATH                                      ; OPEN(PATHNAME:=D_PATH 
LDR080:       LDA        D_PATH,Y                                    ;      REFNUM=OPEN_REF 
              STA        PATH,Y                                      ;      SYSBUF_P:=80:LDREND-2000 ) 
              DEY
              BPL        LDR080
;
              BRK
              .BYTE      OPEN
              .WORD      OPEN_PARMS
              BEQ        LDR090
              LDX        #ERR4X                                      ; ERROR("DRIVER FILE NOT FOUND") 
              LDY        #ERR4L
              JSR        ERROR
LDR090:       LDA        OPEN_REF
              STA        READ_REF
              STA        CLOSE_REF
;
; READ IN ENTIRE DRIVER FILE INTO BANK 0 
;
              BRK                                                    ; READ(REFNUM=READ_REF 
              .BYTE      READ                                        ;      RDBUF_P:=80:FILE 
              .WORD      READ_PARMS                                  ;      BYTES=$FFFF-FILE+1 
;                                       ;      BYTESRD=D.BYTESRD )      
              BEQ        LDR100
              LDX        #ERR0X                                      ; ERROR("I/O ERROR") 
              LDY        #ERR0L
              JSR        ERROR
;                                                                                  +---------------+ 
; CLOSE THE DRIVER FILE AND CHECK LABEL                                            ! SEE FIGURE 3. ! 
;                                                                                  +---------------+ 
LDR100:       BRK                                                    ; CLOSE(REFNUM=CLOSE_REF) 
              .BYTE      CLOSE
              .WORD      CLOSE_PARMS
              LDY        #$7                                         ; CHECK LABEL 
LDR101:       LDA        (RDBUF_P),Y
              CMP        D_LABEL,Y
              BNE        LDR102
              DEY
              BPL        LDR101
              BMI        LDR103
LDR102:       LDX        #ERR5X                                      ; ERROR("INVALID DRIVER FILE")
              LDY        #ERR5L
              JSR        ERROR
;
; MOVE CHARACTER SET TABLE 
;
LDR103:       LDA        #<D_CHRSET                                  ; MOVE(SRC_P=D_CHRSET DST_P=$C00 A=0 CNT=$400) 
              STA        SRC_P
              LDA        #>D_CHRSET
              STA        SRC_P+1
              LDA        #<$C00
              STA        DST_P
              LDA        #>$C00
              STA        DST_P+1
              LDA        #<$400
              STA        CNT
              LDA        #>$400
              STA        CNT+1
              LDA        #0
              JSR        MOVE
;
; MOVE KEYBOARD TABLE 
;
              LDA        #<D_KYBD                                    ; MOVE(SRC_P=D_KYBD DST_P=$1700 A=0 CNT=$100.IN) 
              STA        SRC_P
              LDA        #>D_KYBD
              STA        SRC_P+1
              LDA        #<$1700
              STA        DST_P
              LDA        #>$1700
              STA        DST_P+1
              LDA        #<$100
              STA        CNT
              LDA        #>$100
              STA        CNT+1
              LDA        #0
              JSR        MOVE
;   
; RE-INITIALIZE SDT TABLE 
;
              LDY        #<(D_DRIVES-D_FILE)                         ; LINK_INIT(A=D_DRIVES DIB1..4.IN, SDT_TBL BLKDLST.IO) 
;              LDA        (RDBUF_P),Y 
              LDA        K_DRIVES                          ;hardcode it to K_DRIVES (two)
              JSR        LINK_INIT
;
              LDA        #0                                          ; DST_P:=0:I_BASE_P/256*256 
              STA        CXPAGE+DST_P+1
              STA        DST_P
              LDA        I_BASE_P+1
              STA        DST_P+1
              CMP        #$A0                                        ; IF DST_P>=$A000 THEN DST_P:=$A000   
              BCC        LDR105
              LDA        #$A0
              STA        DST_P+1
LDR105:       LDA        SYSBANK                                     ; DSTBANK:=SYSBANK
              STA        DSTBANK
              JSR        REVERSE                                     ; REVERSE(D_HDR_CNT.IN, WORK_P.OUT)
;
; RELOCATE AND MOVE DRIVERS
;
NEXTDRIVER:   JSR        DADVANCE                                    ; "NO DRIVERS LEFT":=DADVANCE(WORK_P.IO SRC_P CNT REL_P.OUT) 
              BCS        LDR140
              JSR        FLAGS                                       ; "INACTIVE":=FLAGS(SRC_P.IN, PG_ALIGN FIRST_ADIB.OUT) 
              BVS        NEXTDRIVER
              JSR        GETMEM                                      ; GETMEM(PG_ALIGN CNT.IN, DST_P DSTBANK DSEGLIST.IO, PREVBANK.OUT)
              JSR        RELOC                                       ; RELOC(SRC_P REL_P DST_P.IN) 
;
              LDA        DSTBANK                                     ; IF DSTBANK < 0 OR DST_P < SRC_P THEN ERROR 
              BMI        LDR120
              LDA        CXPAGE+SRC_P+1                              ;    (CONVERT SRC_P TO BANK SWITCHED ADDRESS) 
              AND        #$7F
              STA        TEMP_BANK
              LDA        SRC_P+1
              BPL        LDR110
              INC        TEMP_BANK
LDR110:       AND        #$7F
              CLC
              ADC        #>$2000
              STA        TEMP_ADRH
              LDA        DST_P                                       ;    (NOW COMPARE)  
              CMP        SRC_P
              LDA        DST_P+1
              SBC        TEMP_ADRH
              LDA        DSTBANK
              SBC        TEMP_BANK
              BCS        LDR130
LDR120:       LDX        #ERR6X                                      ;    ERROR("DRIVER FILE TOO LARGE") 
              LDY        #ERR6L
              JSR        ERROR
;
LDR130:       LDA        DSTBANK                                     ; MOVE(SRC_P DST_P A=DSTBANK CNT.IN) 
              JSR        MOVE
              JSR        LINK                                        ; LINK(DST_P DSTBANK PREVBANK FIRST_ADIB.IN, SDT_TBL BLKDLST.IO) 
              JMP        NEXTDRIVER
;***************************************************************************************************
; SETUP USER ENVIRONMENT 
;***************************************************************************************************
;
; RE-INITIALIZE KERNEL/DRIVERS, ALLOCATE SYSTEM SEGMENTS
;
LDR140:       JSR        INIT_KRNL                                   ; INIT_KRNL() 
              JSR        ALLOC_SEG                                   ; ALLOC_SEG(K_BASE I_BASE_P SYSBANK.IN) 
              JSR        ALLOC_DSEG                                  ; ALLOC_DSEG(DSEGLIST.IN) 
;
; SET PREFIX TO THE BOOT VOLUME
;
              LDA        #0                                          ; TURN VIDEO OFF - PREVENTS CHAR "GROWTH" DURING DOWNLOAD 
              STA        SCRNMODE
              BRK                                                    ; SET.PREFIX(PREFIXPATH=".D1") 
              .BYTE      SETPREFIX
              .WORD      PREFX_PARMS
;     
; LAUNCH CHARACTER SET DOWNLOAD (CONSOLE) AND CLEAR SCREEN 
;
              CLI                                                    ; BEGIN CHARACTER SET DOWNLOAD (CONSOLE) 
;
              LDA        #0                                          ; CLEAR TEXT SCREENS 
              STA        CXPAGE+SRC_P+1
              STA        CXPAGE+DST_P+1
              LDA        #$04
              STA        SRC_P+1
              STA        DST_P+1
              LDA        #$00
              STA        SRC_P
              LDA        #$80
              STA        DST_P
              LDA        #$A0
              LDX        #8
CLEAR0:       LDY        #$77
CLEAR1:       STA        (SRC_P),Y
              STA        (DST_P),Y
              DEY
              BPL        CLEAR1
              INC        SRC_P+1                                     ; NEXT PAGE
              INC        DST_P+1                                     ; NEXT PAGE
              DEX
              BNE        CLEAR0
;
WAIT:         INC        SRC_P                                       ; WAIT FOR DOWNLOAD TO COMPLETE 
              BNE        WAIT
              INX
              BNE        WAIT
;
              LDA        #$80                                        ; TURN VIDEO ON 
              STA        SCRNMODE
              RTS
;***************************************************************************************************
;PAGE
;***************************************************************************************************
;
; SET_DRIVES ( IN:   A=# DRIVES 
;              IN:   DIB1..4    )
; (INITIALIZES DIB LINKS IN KERNEL'S FLOPPY DRIVER) 
;***************************************************************************************************
;
SET_DRIVES    =          *
              TAY                                                    ; SAVE # OF DRIVES
              LDA        #<DIB2                                      ; DIB1:=ADR(DIB2) 
              STA        DIB1
              LDA        #>DIB2
              STA        DIB1+1
              LDA        #<DIB3                                      ; DIB2:=ADR(DIB3) 
              STA        DIB2
              LDA        #>DIB3
              STA        DIB2+1
              LDA        #<DIB4                                      ; DIB3:=ADR(DIB4) 
              STA        DIB3
              LDA        #>DIB4
              STA        DIB3+1
;
              LDA        #0                                          ; CASE (Y=# OF DRIVES) 
              CPY        #2
              BCC        STDR010
              BEQ        STDR020
              CPY        #4
              BCC        STDR030
              BCS        STDR040
;
STDR010:      STA        DIB1                                        ;    1:  DIB1:=0 
              STA        DIB1+1
              RTS
;
STDR020:      STA        DIB2                                        ;    2:  DIB2:=0 
              STA        DIB2+1
              RTS
;
STDR030:      STA        DIB3                                        ;    3:  DIB3:=0 
              STA        DIB3+1
              RTS
;
STDR040:      STA        DIB4                                        ;    4:  DIB4:=0 
              STA        DIB4+1
              RTS                                                    ; RETURN 
;PAGE
;***************************************************************************************************
;
; INIT_KRNL () 
; 
; (CALLS KERNEL INITIALIZATION MODULES) 
;***************************************************************************************************
;
INIT_KRNL     =          *
              LDA        E_REG                                       ; SWITCH IN I/O BANK AND SELECT PRIMARY STACK 
              ORA        #$44                                        ; E:=( 0.1.1.X:0.1.0.0 ) 
              STA        E_REG                                       ;    ( 1.I.S.R:W_P.R.R ) 
;
              LDA        #>SZPAGE                                    ; SWITCH TO SOS ZPAGE 
              STA        Z_REG
;
              JSR        INT_INIT                                    ; CALL KERNEL INITIALIZATION ROUTINES 
              JSR        EVQ_INIT
;              JSR        BFM_INIT2            ;remove calling the copy protection check, we'll disable it in this version of SOS
;              BCS        INITK_ERR            ;will speed things up a little bit. 
              JSR        DMGR_INIT
              JSR        CFMGR_INIT
              JSR        MMGR_INIT
              JSR        BMGR_INIT
              JSR        BFM_INIT
              JSR        CLK_INIT
;
              LDA        E_REG                                       ; SWITCH OUT I/O BANK AND RETURN TO ALTERNATE STACK 
              AND        #$BB                                        ; E:=( 0.0.1.X:0.0.0.0 ) 
              STA        E_REG                                       ;    ( 1.I.S.R:W_P.R.R ) 
;
              LDA        #>CZPAGE                                    ; SWITCH BACK TO USER ZPAGE 
              STA        Z_REG
;
              RTS                                                    ; RETURN 
;
;
INITK_ERR:    LDX        #ERR0X                                      ; ERROR("I/O ERROR") 
              LDY        #ERR0L
              JMP        ERROR
;PAGE
;***************************************************************************************************
;
; ADVANCE ( I/O:  WORK_P 
;           OUT:  SRC_P
;           OUT:  DST_P
;           OUT:  CNT     )
; (ADVANCES WORK_P TO NEXT INTERP.KERNEL MODULE.  INITS SRC_P, DST_P, CNT FOR MOVE) 
;***************************************************************************************************
;
ADVANCE       =          *
              CLC
              LDY        #2                                          ; Y:=0 
              LDA        WORK_P                                      ; WORK_P:=WORK_P+(WORK_P),Y + 4 
              ADC        (WORK_P),Y
              TAX
              INY
              LDA        WORK_P+1
              ADC        (WORK_P),Y
              PHA
              TXA
              ADC        #4
              STA        WORK_P
              PLA
              ADC        #0
              STA        WORK_P+1
              CLC                                                    ; SRC_P:=X:WORK_P+4 
              LDA        WORK_P
              ADC        #<$0004
              STA        SRC_P
              LDA        WORK_P+1
              ADC        #>$0004
              STA        SRC_P+1
              LDA        CXPAGE+WORK_P+1
              STA        CXPAGE+SRC_P+1
              LDY        #0                                          ; DST_P:=0:(WORK_P) 
              STY        CXPAGE+DST_P+1
              LDA        (WORK_P),Y
              STA        DST_P
              INY
              LDA        (WORK_P),Y
              STA        DST_P+1
              INY                                                    ; Y:=2 
              LDA        (WORK_P),Y                                  ; CNT:=(WORK_P),Y 
              STA        CNT
              INY
              LDA        (WORK_P),Y
              STA        CNT+1
              RTS                                                    ; RETURN 
;PAGE
;***************************************************************************************************
;
; REVERSE ( IN:   D_HDR_CNT
;           IN:   SDT_SIZE = CONSTANT
;           I/O:  DRIVER FILE,
;           OUT:  WORK_P      )      )
;
;            LOCAL:  REV_SAVE, REV.TEMP
; (REVERSES TITLE/CODE/RELOC COUNTS TO ALLOW DRIVER FILE TO BE PROCESSED FROM BACK TO FRONT) 
;***************************************************************************************************
REVERSE       =          *
              LDA        #<D_HDR_CNT                                 ; WORK_P:=80:D_HDR_CNT 
              STA        WORK_P
              LDA        #>D_HDR_CNT
              STA        WORK_P+1
              LDA        #$80
              STA        CXPAGE+WORK_P+1
              CLC                                                    ; WORK_P:=WORK_P+(WORK_P)+2 
              LDY        #0
              LDA        WORK_P
              ADC        (WORK_P),Y
              TAX
              INY
              LDA        WORK_P+1
              ADC        (WORK_P),Y
              PHA
              TXA
              ADC        #2
              STA        WORK_P
              PLA
              ADC        #0
              STA        WORK_P+1
              LDA        (WORK_P),Y                                  ; IF (WORK_P)=$FFFF 
              DEY
              AND        (WORK_P),Y                                  ;    THEN 
              CMP        #$FF
              BNE        REV010
              LDX        #ERR10X                                     ;       ERROR("EMPTY DRIVER FILE")
              LDY        #ERR10L
              JSR        ERROR
REV010:       LDA        #$FF
              STA        REV_SAVE
              STA        REV_SAVE+1
;
REV020:       LDA        REV_SAVE                                    ;R1: STACK:=REV_SAVE 
              PHA
              LDA        REV_SAVE+1
              PHA
              LDY        #0                                          ;    REV_SAVE:=(WORK_P)
              LDA        (WORK_P),Y
              STA        REV_SAVE
              INY
              LDA        (WORK_P),Y
              STA        REV_SAVE+1
              PLA                                                    ;    (WORK_P):=STACK 
              STA        (WORK_P),Y
              DEY
              PLA
              STA        (WORK_P),Y
              LDA        REV_SAVE                                    ;    IF REV_SAVE = $FFFF THEN EXIT 
              AND        REV_SAVE+1
              CMP        #$FF
              BEQ        REV_EXIT
REV030:       BIT        REV_SAVE+1                                  ;    IF REV_SAVE >= $8000 THEN ERROR
              BMI        REV040
              CLC                                                    ;    WORK_P:=WORK_P+REV_SAVE+2
              LDA        WORK_P
              ADC        REV_SAVE
              TAX
              LDA        WORK_P+1
              ADC        REV_SAVE+1
              PHA
              BCS        REV040
              TXA
              ADC        #2
              STA        WORK_P
              PLA
              ADC        #0
              STA        WORK_P+1
              BCC        REV020                                      ;    IF C=FALSE THEN R1 
REV040:       LDX        #ERR5X                                      ;                ELSE ERROR("INVALID DRIVER FILE")
              LDY        #ERR5L
              JSR        ERROR
;
REV_EXIT:     RTS                                                    ; RETURN 
;PAGE
;***************************************************************************************************
;
; DADVANCE ( I/O:  WORK_P 
;            OUT:  C="NO DRIVERS LEFT"
;            OUT:  SRC_P
;            OUT:  CNT 
;            OUT:  REL_P )
; (ADVANCES WORK_P TO NEXT DRIVER MODULE.  INITS SRC_P, CNT, REL_P FOR RELOCATION AND MOVE) 
;***************************************************************************************************
DADVANCE      =          *
              LDY        #0                                          ; IF (WORK_P)=$FFFF THEN EXIT "NO DRIVERS LEFT IN FILE" 
              LDA        (WORK_P),Y
              INY
              AND        (WORK_P),Y
              CMP        #$FF
              BNE        DADV010
              SEC                                                    ; C:="NO DRIVERS LEFT" 
              RTS                                                    ; RETURN 
;
;
DADV010:      LDA        WORK_P                                      ; REL_P:=X:WORK_P
              STA        REL_P
              LDA        WORK_P+1
              STA        REL_P+1
              LDA        CXPAGE+WORK_P+1
              STA        CXPAGE+REL_P+1
;
              JSR        DADD                                        ; ADVANCE TO CODE COUNT FIELD 
;
              LDY        #0                                          ; CNT:=(WORK_P) 
              LDA        (WORK_P),Y
              STA        CNT
              INY
              LDA        (WORK_P),Y
              STA        CNT+1
;
              JSR        DADD                                        ; ADVANCE TO TITLE CNT FIELD 
;
              CLC                                                    ; SRC_P:=X:WORK_P+2 
              LDA        WORK_P
              ADC        #2
              STA        SRC_P
              LDA        WORK_P+1
              ADC        #0
              STA        SRC_P+1
              LDA        CXPAGE+WORK_P+1
              STA        CXPAGE+SRC_P+1
;
              JSR        DADD                                        ; ADVANCE TO RELOC FIELD OF NEXT DRIVER 
              CLC                                                    ; C:="DRIVERS LEFT" 
              RTS                                                    ; RETURN 
;PAGE 
;***************************************************************************************************
;
; DADD ( I/O:   WORK_P ) 
;
; (ADVANCES WORK_P TO NEXT FIELD IN DRIVER MODULE) 
;***************************************************************************************************
DADD          =          *
              SEC                                                    ; WORK_P:=WORK_P-(WORK_P)-2 
              LDY        #0
              LDA        WORK_P
              SBC        (WORK_P),Y
              TAX
              INY
              LDA        WORK_P+1
              SBC        (WORK_P),Y
              PHA
              TXA
              SBC        #2
              STA        WORK_P
              PLA
              SBC        #0
              STA        WORK_P+1
              RTS                                                    ; RETURN 
;PAGE
;***************************************************************************************************
;
; FLAGS ( IN:   SRC_P 
;         OUT:  PG_ALIGN
;         OUT:  FIRST_ADIB
;         OUT:  OV="ALL DIBS INACTIVE" )
;
;         LOCAL:  PREV_ADIB_P, DIB_P
; (PROCESSES "INACTIVE" & "PAGE ALIGN" FLAGS IN DRIVER MODULE'S DIBS" 
;***************************************************************************************************
FLAGS         =          *
              SEC                                                    ; C="FIRST DIB" 
FLAG010:      JSR        NEXT_DIB                                    ; NEXT_DIB(SRC_P.IN, DIB_P PG_ALIGN C OV.OUT) 
              BVC        FLAG015                                     ; IF OV <> "INACTIVE" THEN ACTIVE DIB FOUND 
              BCC        FLAG010                                     ; IF C <> "LAST DIB" THEN CHECK NEXT DIB 
              RTS                                                    ; RETURN  (OV:="ALL DIBS INACTIVE") 
;
FLAG015:      PHP                                                    ; PUSH STATUS 
              SEC                                                    ; FIRST_ADIB:=DIB_P-SRC_P 
              LDA        DIB_P
              SBC        SRC_P
              STA        FIRST_ADIB
              LDA        DIB_P+1
              SBC        SRC_P+1
              STA        FIRST_ADIB+1
              LDA        DIB_P                                       ; PREV_ADIB_P:=X:DIB_P
              STA        PREV_ADIB_P
              LDA        DIB_P+1
              STA        PREV_ADIB_P+1
              LDA        CXPAGE+DIB_P+1
              STA        CXPAGE+PREV_ADIB_P+1
              PLP                                                    ; PULL STATUS 
              BCS        FLAG100                                     ; IF C="LAST DIB" THEN EXIT 
;
FLAG020:      JSR        NEXT_DIB                                    ; NEXT_DIB(SRC_P.IN, DIB_P PG_ALIGN C OV.OUT) 
              PHP                                                    ; PUSH STATUS
              LDY        #0                                          ; IF OV="INACTIVE DIB" 
              BVC        FLAG025
              SEC                                                    ;    THEN  
              LDA        PREV_ADIB_P                                 ;       (PREV_ADIB_P):=PREV_ADIB_P-SRC_P 
              SBC        SRC_P
              STA        (PREV_ADIB_P),Y
              INY
              LDA        PREV_ADIB_P+1
              SBC        SRC_P+1
              STA        (PREV_ADIB_P),Y
              JMP        FLAG050
;
FLAG025:      SEC                                                    ;    ELSE 
              LDA        DIB_P                                       ;       (PREV_ADIB_P):=DIB_P-SRC_P
              SBC        SRC_P
              STA        (PREV_ADIB_P),Y
              INY
              LDA        DIB_P+1
              TAX
              SBC        SRC_P+1
              STA        (PREV_ADIB_P),Y
              STX        PREV_ADIB_P+1                               ;       PREV_ADIB_P:=DIB_P
              LDA        DIB_P
              STA        PREV_ADIB_P
FLAG050:      PLP                                                    ; PULL STATUS 
              BCC        FLAG020                                     ; IF C <> "LAST DIB" THEN PROCESS NEXT DIB 
;
FLAG100:      CLV                                                    ; OV:="ACTIVE DIBS" 
              RTS                                                    ; RETURN 
;PAGE
;***************************************************************************************************
;
; NEXT_DIB ( IN:   C="FIRST DIB" 
;            IN:   SRC_P
;            OUT:  DIB_P
;            OUT:  PG_ALIGN
;            OUT:  C="LAST DIB"
;            OUT:  OV="INACTIVE DIB" )
;
;            LOCAL:  DIB_FLAGS, DIB_DCB = CONSTANT
; (ADVANCES TO NEXT DIB IN DRIVER MODULE)  
;***************************************************************************************************
NEXT_DIB      =          *
              LDY        #0
              BCC        NXTD010                                     ; IF C = "FIRST DIB"  
              STY        PG_ALIGN                                    ;    THEN 
              STY        PG_ALIGN+1                                  ;       PG_ALIGN:=0
              LDA        SRC_P                                       ;       DIB_P:=X:SRC_P
              STA        DIB_P
              LDA        SRC_P+1
              STA        DIB_P+1
              LDA        CXPAGE+SRC_P+1
              STA        CXPAGE+DIB_P+1
              JMP        NXTD020
NXTD010:      LDA        SRC_P                                       ;    ELSE
              ADC        (DIB_P),Y                                   ;       DIB_P:=SRC_P+(DIB_P) 
              TAX
              INY
              LDA        SRC_P+1
              ADC        (DIB_P),Y
              STA        DIB_P+1
              STX        DIB_P
;
NXTD020:      LDY        #DIB_FLAGS                                  ; IF (DIB_P),DIB_FLAGS.BIT7 = "INACTIVE" 
              LDA        (DIB_P),Y
              BMI        NXTD030
              BIT        NXTD999                                     ;    THEN 
              BVS        NXTD040                                     ;       OV:="INACTIVE" 
;                                            ELSE 
NXTD030:      AND        #$40                                        ;       IF (DIB_P),DIB_FLAGS.BIT6 = "PAGE ALIGN" 
              BEQ        NXTD040
              CLC                                                    ;          THEN 
              LDA        #DIB_DCB+2                                  ;             PAGE.ALIGN:=DIB_DCB+2+(SRC_P),DIB_DCB
              TAY
              DEY
              DEY
              ADC        (SRC_P),Y
              STA        PG_ALIGN
              INY
              LDA        #0
              ADC        (SRC_P),Y
              STA        PG_ALIGN+1
              CLV                                                    ;       OV:="ACTIVE" 
;
NXTD040:      LDY        #0                                          ; IF (DIB_P) = 0 
              LDA        (DIB_P),Y
              INY
              ORA        (DIB_P),Y
              BNE        NXTD998
              SEC                                                    ;    THEN  C:="LAST DIB" 
              BCS        NXTD999
NXTD998:      CLC                                                    ;    ELSE  C:=NOT "LAST DIB" 
NXTD999:      RTS                                                    ; RETURN 
;***************************************************************************************************
;PAGE
;***************************************************************************************************
;
; GETMEM ( IN:   PG_ALIGN
;          IN:   CNT
;          I/O:  DST_P
;          I/O:  DSTBANK
;          I/O:  DSEGLIST
;          OUT:  PREVBANK )
;
;          LOCAL:  PREVDST
; (COMPUTES # OF PAGES TO ADD TO DRIVER SEGMENT AND WHETHER TO BEGIN A NEW SEGMENT) 
;***************************************************************************************************
GETMEM        =          *
              LDA        DSTBANK                                     ; PREVBANK:=DSTBANK 
              STA        PREVBANK
              LDA        DST_P                                       ; PREVDST:=DST_P
              STA        PREVDST
              LDA        DST_P+1
              STA        PREVDST+1
              JSR        NEWDST                                      ; NEWDST(PG_ALIGN.IN, PREVDST.IN, CNT.IN, DST_P.OUT)
;
              LDA        DST_P+1                                     ; IF DST_P >= $2000
              CMP        #$20
              BCC        GETM010
              SEC                                                    ;    THEN 
              LDA        PREVDST+1                                   ;       A=PAGES:=PREVDST-DST_P 
              SBC        DST_P+1
              CLC
              JSR        BUILD_DSEG                                  ;       BUILD_DSEG(C="NEXT BANK".IN, A=PAGES.IN, DSEGLIST.IO)
              JMP        GETM_EXIT
;                                             ELSE 
GETM010:      DEC        DSTBANK                                     ;       DSTBANK:=DSTBANK-1
              LDA        #<$A000                                     ;       PREVDST:=$A000
              STA        PREVDST
              LDA        #>$A000
              STA        PREVDST+1
              JSR        NEWDST                                      ;       NEWDST(PG_ALIGN.IN, PREVDST.IN, CNT.IN, DST_P.OUT)
              SEC                                                    ;       A="PAGES":=PREVDST-DST_P
              LDA        PREVDST+1
              SBC        DST_P+1
              SEC
              JSR        BUILD_DSEG                                  ;       BUILD_DSEG(C="NEXTBANK".IN, A="PAGES".IN, DSEGLIST.IO)
;
GETM_EXIT:    RTS                                                    ; RETURN 
;PAGE
;***************************************************************************************************
;
; NEWDST ( IN:   PG_ALIGN
;          IN:   PREVDST
;          IN:   CNT
;          I/O:  DST_P      )
; (COMPUTES DESTINATION BASE ADDRESS, ALIGNING ON PAGE BOUNDARY IF REQUESTED) 
;***************************************************************************************************
NEWDST        =          *
              SEC                                                    ; IF (PREVDST-$2000) < CNT
              LDA        PREVDST
              SBC        #<$2000
              TAX
              LDA        PREVDST+1
              SBC        #>$2000
              CPX        CNT
              SBC        CNT+1
              BCS        NEWD010
              LDA        #0                                          ;    THEN
              STA        DST_P                                       ;      DST_P:=0
              STA        DST_P+1
              BEQ        NEWD_EXIT
NEWD010:      SEC                                                    ;    ELSE
              LDA        PREVDST                                     ;       DST_P:=PREVDST-CNT
              SBC        CNT
              STA        DST_P
              LDA        PREVDST+1
              SBC        CNT+1
              STA        DST_P+1
              LDA        PG_ALIGN                                    ;       IF PG_ALIGN <> 0
              ORA        PG_ALIGN+1                                  ;          THEN
              BEQ        NEWD_EXIT
              SEC                                                    ;              DST_P:=(DST_P/256*256)-PG_ALIGN 
              LDA        #0
              SBC        PG_ALIGN
              STA        DST_P
              LDA        DST_P+1
              SBC        PG_ALIGN+1
              STA        DST_P+1
NEWD_EXIT:    RTS                                                    ; RETURN 
;PAGE
;***************************************************************************************************
;
; BUILD_DSEG ( IN:   C="NEXTBANK"
;              IN:   A="PAGES"
;              I/O:  DSEGLIST     )
; (COMPUTES # OF PAGES TO ADD TO DRIVER SEGMENT AND WHETHER TO BEGIN A NEW SEGMENT) 
;***************************************************************************************************
BUILD_DSEG    =          *
              PHA
              BCS        BLDS010                                     ; IF ("NEXTBANK"=TRUE OR DSEGX=$FF) 
              LDA        DSEGX                                       ;    THEN 
              BPL        BLDS020
BLDS010:      INC        DSEGX                                       ;       DSEGX:=DSEGX+1
BLDS020:      LDX        DSEGX
              CLC                                                    ; DSEGLIST(DSEGX):=DSEGLIST(DSEGX)+"PAGES" 
              PLA
              ADC        DSEGLIST,X
              STA        DSEGLIST,X
              RTS                                                    ; RETURN 
;
;
;
DSEGX:        .BYTE      $FF                                         ; DRIVER SEGMENT LIST TABLE 
DSEGLIST:     .BYTE      $0                                          ; # PAGES FOR 1ST DRIVER SEGMENT   (BANK N  ) 
              .BYTE      $0                                          ;        "    2ND       "          (BANK N-1) 
              .BYTE      $0                                          ;        "    3RD       "          (BANK N-2) 
              .BYTE      $0                                          ;        "    4TH       "          (BANK N-3) 
;PAGE
;***************************************************************************************************
;
; RELOC ( IN:   SRC_P 
;         IN:   REL_P
;         IN:   DST_P
;         OUT:  RELOCATED DRIVER MODULE )
;
;         LOCAL:  REL_END, CODE_P
; (RELOCATES DRIVER MODULE'S CODE FIELD USING RELOCATION FIELD) 
;***************************************************************************************************
RELOC         =          *
              SEC                                                    ; REL_END:=REL_P-(REL_P) 
              LDY        #0
              LDA        REL_P
              SBC        (REL_P),Y
              STA        REL_END
              INY
              LDA        REL_P+1
              SBC        (REL_P),Y
              STA        REL_END+1
REL_LOOP:     SEC                                                    ; REL_P:=REL_P-2 
              LDA        REL_P
              SBC        #2
              STA        REL_P
              LDA        REL_P+1
              SBC        #0
              STA        REL_P+1
              LDA        REL_P                                       ; IF REL_P < REL_END THEN EXIT 
              CMP        REL_END
              LDA        REL_P+1
              SBC        REL_END+1
              BCC        REL_EXIT
              LDY        #0                                          ; CODE_P:=X:SRC_P+(REL_P) 
              CLC
              LDA        SRC_P
              ADC        (REL_P),Y
              STA        CODE_P
              INY
              LDA        SRC_P+1
              ADC        (REL_P),Y
              STA        CODE_P+1
              LDA        CXPAGE+SRC_P+1
              STA        CXPAGE+CODE_P+1
              LDY        #0                                          ; (CODE_P):=(CODE_P)+DST_P 
              CLC
              LDA        (CODE_P),Y
              ADC        DST_P
              STA        (CODE_P),Y
              INY
              LDA        (CODE_P),Y
              ADC        DST_P+1
              STA        (CODE_P),Y
              JMP        REL_LOOP                                    ; GOTO REL_LOOP 
;
REL_EXIT:     RTS                                                    ; RETURN 
;PAGE
;***************************************************************************************************
;
; ALLOC_SEG ( IN:   K_BASE 
;             IN:   I_BASE_P 
;             IN:   SYSBANK ) 
;         I_BASE_P
;         D.BASE.PG
; (ALLOCATES SEGMENTS FOR KERNEL, INTERPRETER AND SYSTEM WORK AREA) 
;***************************************************************************************************
ALLOC_SEG     =          *
              BRK                                                    ; REQ.SEG(BASE=(F,0), LIMIT=(F,1D), SEGID=0, SEGNUM) 
              .BYTE      REQSEG
              .WORD      SEGMENT
;
              LDA        #$10                                        ; SET BASE/LIMIT BANKS
              STA        SEGBASE
              STA        SEGLIM
              LDA        #0                                          ; AND INIT BASE PAGE
              STA        SEGBASE+1
;
              LDX        K_BASE+1                                    ; KERNEL SEGMENT, ID=1
              JSR        RSEG
;
              LDX        I_BASE_P+1                                  ; INTERPRETER SEGMENT, ID=2
              JSR        RSEG
              RTS
;PAGE
;***************************************************************************************************
;
; RSEG ( IN:  X=BASE.PAGE OF SEGMENT ) 
;
;*************************************************************************************************** 
RSEG          =          *
              INC        SEGID                                       ; SEGID:=SEGID+1 
              LDY        SEGBASE+1                                   ; LIMIT.PAGE:=BASE.PAGE-1 
              DEY
              STY        SEGLIM+1
              STX        SEGBASE+1                                   ; BASE.PAGE:=X 
;
              CPX        #$A0                                        ; IF BASE>=$A0 OR LIMIT<$A0 THEN  
              BCS        RSEG010                                     ;    THEN 
              LDA        SEGLIM+1                                    ;       REQUEST ONLY ONE SEGMENT 
              CMP        #$A0
              BCC        RSEG010
;
              TXA                                                    ;    ELSE 
              PHA                                                    ;       REQUEST TWO SEGMENTS 
              LDX        #$A0
              STX        SEGBASE+1
;
              BRK                                                    ;       REQ.SEG(BASE, LIMIT, SEGID, SEGNUM) 
              .BYTE      REQSEG
              .WORD      SEGMENT
;
              PLA
              STA        SEGBASE+1
              LDA        #$9F
              STA        SEGLIM+1
              LDA        SYSBANK
              STA        SEGBASE
              STA        SEGLIM
;
;
RSEG010:      BRK                                                    ; REQ.SEG(BASE, LIMIT, SEGID, SEGNUM) 
              .BYTE      REQSEG
              .WORD      SEGMENT
;
              RTS                                                    ; RETURN 
;PAGE
;***************************************************************************************************
;
; ALLOC_DSEG ( IN:   DSEGLIST )
;
; (ALLOCATES SEGMENTS FOR DRIVER MODULES" 
;***************************************************************************************************
ALLOC_DSEG    =          *
              INC        DSEGX                                       ; DSEGX:=DSEGX+1 
              BNE        ALDS010                                     ; IF DSEGX=0 
              LDX        #ERR5X                                      ;    THEN ERROR("INVALID DRIVER FILE")
              LDY        #ERR5L
              JSR        ERROR
;
ALDS010:      LDY        #$FF                                        ; Y:=-1 
ALDS020:      INY                                                    ; WHILE (Y:=Y+1) < DSEGX 
              CPY        DSEGX                                       ;    DO 
              BCS        ALDS_EXIT
              LDA        DSEGLIST,Y                                  ;       PAGECT:=DSEGLIST(Y) 
              STA        SEGPGCNT
              BRK                                                    ;       FINDSEG (SRCHMODE=0.IN, SEGID=3.IN
              .BYTE      FINDSEG                                     ;               PAGECT=DSEGLIST(Y)
              .WORD      SEGMENT1                                    ;               BASE.OUT, LIMIT.OUT)
              JMP        ALDS020
;
ALDS_EXIT:    RTS                                                    ; RETURN 
;PAGE
;***************************************************************************************************
;
; ERROR (IN:  X=MESSAGE INDEX 
;        IN:  Y=MESSAGE LENGTH
; (DISPLAYS ERROR MESSAGE, SOUNDS BELL AND LOOPS UNTIL CONTROL/RESET PRESSED) 
;***************************************************************************************************
ERROR         =          *
              STY        ETEMP                                       ; CENTER MSG (Y:=LEN/2+LEN)      
              SEC
              LDA        #40
              SBC        ETEMP
              LSR        A
              CLC
              ADC        ETEMP
              TAY
;
PRNT010:      LDA        ERR,X                                       ; MOVE MESSAGE TO SCREEN MEMORY 
              STA        EMSGADR-1,Y
              DEX
              DEY
              DEC        ETEMP
              BNE        PRNT010
;                                   
              LDA        #$73                                        ; E:=( 0.1.1.1:0.0.1.1 ) 
              STA        E_REG                                       ;    ( 1.I.S.R:W_P.R.S ) 
              LDA        $C040                                       ; SOUND BELL
              JMP        *                                           ; LOOP UNTIL REBOOT (CTRL/RESET) 
;PAGE
;***************************************************************************************************
; 
; ERROR MESSAGES
;
;***************************************************************************************************
EMSGADR       =          $7A8
;
ERR           =          *
ERR0:         .BYTE      "I/O ERROR"
ERR0L         =          *-ERR0
ERR0X         =          *-ERR-1
ERR1:         .BYTE      "INTERPRETER FILE NOT FOUND"
ERR1L         =          *-ERR1
ERR1X         =          *-ERR-1
ERR2:         .BYTE      "INVALID INTERPRETER FILE"
ERR2L         =          *-ERR2
ERR2X         =          *-ERR-1
ERR3:         .BYTE      "INCOMPATIBLE INTERPRETER"
ERR3L         =          *-ERR3
ERR3X         =          *-ERR-1
ERR4:         .BYTE      "DRIVER FILE NOT FOUND"
ERR4L         =          *-ERR4
ERR4X         =          *-ERR-1
ERR5:         .BYTE      "INVALID DRIVER FILE"
ERR5L         =          *-ERR5
ERR5X         =          *-ERR-1
ERR6:         .BYTE      "DRIVER FILE TOO LARGE"
ERR6L         =          *-ERR6
ERR6X         =          *-ERR-1
ERR7:         .BYTE      "ROM ERROR:  PLEASE NOTIFY YOUR DEALER"
ERR7L         =          *-ERR7
ERR7X         =          *-ERR-1
ERR8:         .BYTE      "TOO MANY DEVICES"
ERR8L         =          *-ERR8
ERR8X         =          *-ERR-1
ERR9:         .BYTE      "TOO MANY BLOCK DEVICES"
ERR9L         =          *-ERR9
ERR9X         =          *-ERR-1
ERR10:        .BYTE      "EMPTY DRIVER FILE"
ERR10L        =          *-ERR10
ERR10X        =          *-ERR-1
;PAGE 
;***************************************************************************************************
;
; WELCOME () 
;
; (PRINTS WELCOME MESSAGE - "APPLE ///", VERSION, DATE/TIME, COPYRIGHT) 
;***************************************************************************************************
WELCOME       =          *
;
;  PRINT "APPLE III" MESSAGE
;
              LDY        #AMSGL
WAM010:       LDA        AMSG-1,Y
              STA        AMSGADR-1,Y
              DEY
              BNE        WAM010
;
;  PRINT SOS VERSION MESSAGE
;
              CLC
              LDA        #40
              ADC        #SOSVERL
              LSR        A
              TAX
              LDY        #SOSVERL
WSM010:       LDA        SOSVER-1,Y
              ORA        #$80
              STA        SMSGADR-1,X
              DEX
              DEY
              BNE        WSM010
;
;  PRINT DATE AND TIME MESSAGE
;
              BRK                                                    ; GET.TIME(TIME.OUT) 
              .BYTE      GETTIME
              .WORD      DTPARMS
;
              LDA        DATETIME+8                                  ;SET UP WEEKDAY
              AND        #$0F
              BEQ        WDM040                                      ;NO CLOCK
              STA        WTEMP
              ASL        A
              ADC        WTEMP
              TAX
              LDY        #3
WDM010:       LDA        DAYNAME-1,X
              STA        DMSG-1,Y
              DEX
              DEY
              BNE        WDM010
;
              LDA        DATETIME+7                                  ;SET UP DATE 
              LDX        DATETIME+6
              STA        DMSG+6
              STX        DMSG+5
;
              LDA        DATETIME+5                                  ;SET UP MONTH 
              AND        #$0F
              LDX        DATETIME+4
              CPX        #$31
              BCC        WDM020
              ADC        #9
WDM020:       STA        WTEMP
              ASL        A
              ADC        WTEMP
              TAX
              LDY        #3
WDM030:       LDA        MONNAME-1,X
              STA        DMSG+7,Y
              DEX
              DEY
              BNE        WDM030
;
              LDA        DATETIME+3                                  ;SET UP YEAR 
              LDX        DATETIME+2
              STA        DMSG+13
              STX        DMSG+12
;
              LDA        DATETIME+10                                 ;SET UP HOUR 
              LDX        DATETIME+09
              STA        DMSG+17
              STX        DMSG+16
;
              LDA        DATETIME+12                                 ;SET UP MINUTE 
              LDX        DATETIME+11
              STA        DMSG+20
              STX        DMSG+19
;
              LDY        #DMSGL                                      ;PRINT DATE & TIME 
WDM050:       LDA        DMSG-1,Y
              ORA        #$80
              STA        DMSGADR-1,Y
              DEY
              BNE        WDM050
;
;  PRINT COPYRIGHT MESSAGE
;
WDM040:       LDY        #CMSGL
WCM010:       LDA        CMSG-1,Y
              STA        CMSGADR-1,Y
              DEY
              BNE        WCM010
              RTS
;PAGE
;***************************************************************************************************
;
; WELCOME () - DATA DECLARATIONS
;
;***************************************************************************************************
;MSB ON
AMSG:         ASCMSBON   "APPLE ///"
AMSGL         =          *-AMSG
AMSGADR       =          (40-AMSGL)/2+$4A8
;MSB OFF
SMSGADR       =          $5A8
DMSG:         .BYTE      "DAY, DD-MON-YY  HH:MM"
DMSGL         =          *-DMSG
DMSGADR       =          (40-DMSGL)/2+$6A8
DAYNAME:      .BYTE      "SUNMONTUEWEDTHUFRISAT"
MONNAME:      .BYTE      "JANFEBMARAPRMAYJUN"
              .BYTE      "JULAUGSEPOCTNOVDEC"
;MSB ON
;;CMSG:         ASCMSBON   "(C)1980,1981,1982 BY APPLE COMPUTER INC."
CMSG:         ASCMSBON   "(C)1980,1981-1984 BY APPLE COMPUTER INC."    ;(S)
CMSGL         =          *-CMSG
CMSGADR       =          (40-CMSGL)/2+$7D0
;MSB OFF
;PAGE
;***************************************************************************************************
;
; SOS SYSTEM CALLS (1) 
;
;***************************************************************************************************
; OPEN (PATHNAME.IN, REFNUM.OUT, OPENLIST.IN, OPENCNT.IN)  ** (ACCESS.IN, PAGES.IN, SYSBUF.IN) 
;*************************************************************************************************** 
OPEN          =          $C8
;
OPEN_PARMS:   .BYTE      $4
              .WORD      PATH
OPEN_REF:     .BYTE      $0
              .WORD      OPEN_LIST
              .BYTE      $4
OPEN_LIST:    .BYTE      $0,$4                                       ; PAGES:=4 
              .WORD      SYSBUF_P
PATH:         .RES       $40                                         ; PATHNAME BUFFER
I_LABEL:      .BYTE      "SOS NTRP"                                  ; FILE LABELS 
D_LABEL:      .BYTE      "SOS DRVR"
;***************************************************************************************************
; READ (REFNUM.IN, BUFFER.IN, BYTES.IN, BYTESREAD.OUT) 
;***************************************************************************************************
READ          =          $CA
;
READ_PARMS:   .BYTE      $4
READ_REF:     .BYTE      $0
READ_BUF:     .WORD      RDBUF_P
READ_BYT:     .WORD      $FFFF-FILE+1
READ_BYTRD:   .WORD      $0
;***************************************************************************************************
; CLOSE (REFNUM.IN) 
;***************************************************************************************************
CLOSE         =          $CC
;
CLOSE_PARMS:  .BYTE      $1
CLOSE_REF:    .BYTE      $0
;*************************************************************************************************** 
; FIND.SEG (SRCHMODE.IN, PAGES.IN, SEGID.IN, BASE.OUT, LIMIT.OUT, SEGNUM.OUT) 
;*************************************************************************************************** 
FINDSEG       =          $41
;
SEGMENT1:     .BYTE      $6                                          ; FIND.SEG(SRCHMODE, SEGID, PAGECT, BASE, LIMIT, SEGNUM) 
SEGSRCH:      .BYTE      $0,$3
SEGPGCNT:     .WORD      $0000
              .WORD      $0
              .WORD      $0
              .BYTE      $0
;PAGE
;***************************************************************************************************
;
; SOS SYSTEM CALLS (2)
;
;***************************************************************************************************
;***************************************************************************************************
; REQUEST.SEG (BASE.IN, LIMIT.IN, SEGID.IN, SEGNUM.OUT) 
;***************************************************************************************************
REQSEG        =          $40
;
SEGMENT:      .BYTE      $4                                          ; REQUEST SEG PARM LIST
SEGBASE:      .BYTE      $F,$0
SEGLIM:       .BYTE      $F,$1D
SEGID:        .BYTE      $0,$0
;*************************************************************************************************** 
; SET.PREFIX (PREFIXPATH.IN) 
;*************************************************************************************************** 
SETPREFIX     =          $C6
PREFX_PARMS:  .BYTE      $1
              .WORD      PREFX_PATH
PREFX_PATH:   .BYTE      $8
              .BYTE      ".PROFILE"
;***************************************************************************************************
; GETTIME (TIME.OUT)
;***************************************************************************************************
GETTIME       =          $63
;
DTPARMS:      .BYTE      1
              .WORD      DATETIME
DATETIME:     .BYTE      "YYYYMMDDWHHMMSSMMM"
;
;
;  SET UNIT TO BOOT FROM BASED ON THE PRODOS UNIT NUMBER THAT LOADED THE KERNEL
;
SET_UNIT:     LDA        P_UNIT
              AND        #$80                 ;Mask of the unit bit
              BEQ        UNIT0                ;if zero, leave as is and boot from '.PROFILE' (unit0)

              LDY        #$17                 ;else copy over the '.PB2' (unit1) paths for the
SD_LOOP:      LDA        I_PATH2,Y            ;sos.kernel and sos.driver files
              STA        I_PATH,Y
              LDA        D_PATH2,Y
              STA        D_PATH,Y
              DEY
              BPL        SD_LOOP
              
              LDA        #4                   ;copy in the device2 default prefix
              STA        PREFX_PATH
              LDY        #$3                  ;just the '.PB2' part
PR_LOOP:      LDA        D_PATH2+1,Y
              STA        PREFX_PATH+1,Y
              DEY
              BPL        PR_LOOP
              
UNIT0:        RTS

SU_END        =          *
SU_LEN        =          SU_END-SET_UNIT

;PAGE
;***************************************************************************************************
;
; END OF SOSLDR CODE
;
;***************************************************************************************************

codeend       = *
SLOP          = $28f8 - codeend
;SLOP          =          $50 - SU_LEN + 4  ;need to fix this up a bit better
              .RES       SLOP                                        ; +-----------------------------------+ 
INITMODULE:                                                          ;.RES $200 ; ! KERNEL'S INIT MODULE RESIDES HERE ! 
LDREND        =          *+$200                                      ; +-----------------------------------+ 

FILE          =          *+$200-$2000+$400
;***************************************************************************************************
; SOS INTERPRETER FILE
;***************************************************************************************************
I_FILE        =          FILE
I_HDR_CNT     =          I_FILE+$8
;***************************************************************************************************
; SOS DRIVER FILE
;***************************************************************************************************
D_FILE        =          FILE
D_HDR_CNT     =          D_FILE+$8
D_DRIVES      =          D_HDR_CNT+$2
D_CHRSET      =          D_DRIVES+$2+$10
D_KYBD        =          D_CHRSET+$10+$400
;***************************************************************************************************
;LST ON
ZZEND         =          *+$200
ZZLEN         =          ZZEND-ZZORG-$200
              ;.IF        ZZLEN-LENLODR
              ;.FATAL     "SOSORG FILE IS INCORRECT FOR SOS LOADER"
              ;.ENDIF

