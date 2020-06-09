; macro to support setting the most significant bit on for ascii strings with EDASM MSB ON
            .macro     ASCMSBON s
            .repeat    .strlen(s), i
            .byte      .strat(s,i) | $80
            .endrepeat
            .endmacro

;SBTL "SOS 1.1  UTILITY MANAGER"
            .SEGMENT   "CODE"
;.RELOC
            .INCLUDE   "SOSORG"
            .ORG       ORGUMGR
ZZORG       =          *
;MSB OFF
;***************************************************************************************************
;          COPYRIGHT (C) APPLE COMPUTER INC. 1980
;                    ALL RIGHTS RESERVED
;***************************************************************************************************
;  UTILITY MANAGER
;
;  THIS MODULE HANDLES THE FOLLOWING SOS CALLS:
;    SET.FENCE,   GET.FENCE
;    SET.TIME,    GET.TIME
;    JOYSTICK,    COLDSTRT
;
;  IN ADDITION, IT CONTAINS THE ROUITNE DATETIME WHICH
;  PROVIDES THE DATE AND TIME FOR THE BLOCK FILE MANAGER.
;
;***************************************************************************************************
;
            .EXPORT    UMGR
            .EXPORT    DATETIME
            .EXPORT    BCDBIN
            .EXPORT    COLDSTRT
;
            .EXPORT    PCLOCK
;
            .IMPORT    SYSBANK
            .IMPORT    CEVPRI
            .IMPORT    SYSERR
            .IMPORTZP  BADSCNUM
            .IMPORTZP  BADJMODE
            .IMPORTZP  XNORESRC
            .IMPORT    ALLOCSIR
            .IMPORT    DEALCSIR
;
            .IMPORT    SELCLDST          ;(S)
;
U_TPARMX    =          $C0
U_REQCODE   =          U_TPARMX
PRIORITY    =          U_TPARMX+1
J_MODE      =          U_TPARMX+1
J_VALUE     =          U_TPARMX+2
TIME        =          U_TPARMX+1
MEMORY      =          U_TPARMX+1
;
BITON2      =          $04
BITON5      =          $20
BITON6      =          $40
BITON7      =          $80
BITOFF5     =          $DF
;
Z_REG       =          $FFD0
E_REG       =          $FFDF
B_REG       =          $FFEF
;PAGE
;***************************************************************************************************
;
; UTILITY SWITCH
;
;***************************************************************************************************
;
;
UMGR:
            LDA        E_REG                                ;SELECT $C000 I/O SPACE
            ORA        #BITON6
            STA        E_REG
;
            LDA        U_REQCODE
            CMP        #USWCNT
            BCS        UMGRERR
            ASL        A
            TAX
            LDA        USWTBL+1,X
            PHA
            LDA        USWTBL,X
            PHA
            RTS
;
UMGRERR:    LDA        #BADSCNUM
            JSR        SYSERR
;
;  UTILITY SWITCH TABLE
;
USWTBL:
            .WORD      SET_FENCE-1
            .WORD      GET_FENCE-1
            .WORD      SET_TIME-1
            .WORD      GET_TIME-1
            .WORD      JOYSTICK-1
            .WORD      SELCLDST-1          ;(S) WAS ->COLDSTRT-1
USWCNT      =          (*-USWTBL)/2
;PAGE
;***************************************************************************************************
;
; SET_FENCE(IN.PRIORITY) SYSTEM CALL
;
; GET_FENCE(OUT.PRIORITY) SYSTEM CALL
;
; THESE TWO CALLS ALLOW THE CALLER TO EITHER RETRIEVE OR SET
; THE CURRENT SYSTEM EVENT PRIORITY THRESHOLD_  BY RAISING
; THE FENCE, A USER MAY INHIBIT THE EXECUTION OF EVENTS WHOSE
; PRIORITY IS EQUAL TO OR LESS THAN THE VALUE OF THE SYSTEM
; FENCE_
;
;***************************************************************************************************
;
;
SET_FENCE   =          *
            LDA        PRIORITY
            STA        CEVPRI
            RTS                                             ; NORMAL EXIT
;
;
GET_FENCE   =          *
            LDA        CEVPRI
            LDY        #0
            STA        (PRIORITY),Y
            RTS                                             ; NORMAL EXIT
;PAGE
;***************************************************************************************************
;
;  SET_TIME(IN.TIME)
;  GET_TIME(OUT.TIME)
;
;  THESE SYSTEM CALLS ALLOW THE USER TO SET AND READ THE
;  SYSTEM'S CLOCK.  THE TIME IS EXPRESSED AS AN EIGHTEEN
;  DIGIT ASCII STRING IN THE FORM "YYYYMMDDWHHMMSSMMM".
;
;    YYYY  YEAR       [1900-1999]
;      MM  MONTH        [01-12]
;      DD  DAY          [01-31]
;       W  WEEKDAY       [1-7]  1 => SUNDAY
;      HH  HOUR         [00-23]
;      MM  MINUTE       [00-59]
;      SS  SECOND       [00-59]
;     MMM  MILLISECOND [000-999]
;
;  THE CLOCK CHIP AUTOMATICALLY MAINTAINS THE TIME AND
;  DATE FROM MILLISECONDS TO MONTHS.  IT DOES NOT MAINTAIN
;  THE YEAR, HOWEVER, NOR DOES IT RECOGNIZE 29 FEBRUARY
;  IN LEAP YEARS.  THE SOFTWARE SETS THE DAY AND MONTH
;  LATCHES TO THE DON'T CARE STATE AND USES THE REMAINING
;  EIGHT BITS TO HOLD A TWO DIGIT BCD YEAR.  THE CLOCK
;  MUST BE RESET AT THE BEGINNING OF EACH YEAR AND ON
;  29 FEBRUARY IN LEAP YEARS.
;
;  SET.TIME ASSUMES THAT THE DATE IS VALID AND CORRECT.
;  THE CENTURY IS IGNORED AND MILLISECONDS ARE ALWAYS SET
;  TO ZERO.  GET.TIME ALWAYS SETS THE CENTURY TO 19.
;
;***************************************************************************************************
;
;
;  TEMPORARY ZERO PAGE
;
PCLK        =          $D0                                  ;POINTER TO SAVED PCLOCK
WKDAY       =          $D2
CKSUM       =          $D3
CLKTEMP     =          $18D4                                ;THROUGH $18DD - ABSOLUTE
;
;  CLOCK LOCAL DATA
;
PCLOCK:     .RES       $0A                                  ;PSEUDO CLOCK REGISTERS
RETRY:      .RES       $01
;
;  CLOCK HARDWARE ADDRESSES
;
CLOCK       =          $C070
CSEC        =          $02
CMIN        =          $03
CMON        =          $07
LDAY        =          $0E
CRESET      =          $12
STATUS      =          $14
;
WKMON:      .BYTE      8,11,11,7,9,12
            .BYTE      7,10,13,8,11,13
;
;
SET_TIME    =          *
            LDX        #$00
            LDY        #$12
            LDA        #'0'
            BNE        STIM011
;
STIM010:
            INX
            LDA        (TIME),Y                             ;CONVERT TIME FROM
STIM011:    AND        #$0F                                 ;  ASCII TO BCD AND
            STA        PCLOCK,X                             ;  TRANSFER TO PCLOCK
            DEY
            CPY        #$07
            BEQ        STIM010
            LDA        (TIME),Y
            ASL        A
            ASL        A
            ASL        A
            ASL        A
            ORA        PCLOCK,X
            STA        PCLOCK,X
            DEY
            BPL        STIM010
;
            LDA        PCLOCK+7                             ;CALCULATE WEEKDAY
            JSR        BCDBIN
            TAX
            LDA        PCLOCK+8
            JSR        BCDBIN
            TAY
            LSR        A
            LSR        A
            STA        WKDAY
            TYA
            AND        #$03
            BNE        STIM015
            CPX        #3
            BCS        STIM015                              ; <SRS 82.162>
            DEY
STIM015:
            CLC
            TYA
            ADC        WKDAY
            ADC        WKMON-1,X
            STA        WKDAY
            LDA        PCLOCK+6
            JSR        BCDBIN
            CLC
            ADC        WKDAY
            SEC
STIM016:    SBC        #7
            CMP        #8
            BCS        STIM016
            STA        PCLOCK+5
;
            LDA        #$D0
            STA        PCLK                                 ;POINT (PCLK) TO 8F:FFD0
            LDA        #$FF
            STA        PCLK+1
            LDA        #$8F
            STA        $1401+PCLK
            LDA        #$A5
            STA        CKSUM                                ;INITIALIZE CHECKSUM
            LDY        #$00
;
STIM020:    LDA        PCLOCK,Y                             ;SAVE PCLOCK
            STA        (PCLK),Y                             ;  BEHIND 6522
            EOR        CKSUM
            STA        CKSUM
            INY
            CPY        #$0A
            BCC        STIM020
            STA        (PCLK),Y                             ;SAVE CHECKSUM
;
            LDA        Z_REG
            PHA                                             ;SAVE ZERO PAGE
            LDA        E_REG
            PHA                                             ;SAVE ENVIRONMENT
            ORA        #BITON7                              ;  AND SET 1 MHZ
            STA        E_REG
;
            LDY        #STATUS
            STY        Z_REG
            LDA        CLOCK                                ;DOES CLOCK EXIST?
            BMI        STIM050                              ;  NO
;
            LDX        #CRESET
            STX        Z_REG
            LDA        #$FF                                 ;RESET ALL COUNTERS
            STA        CLOCK
            STA        CLOCK
;
            LDX        #CSEC-1
STIM030:
            INX
            PHP
            SEI                                             ;DISABLE INTERRUPTS
STIM040:    STX        Z_REG
            LDA        CLOCK                                ;(DUMMY READ FOR STATUS)
            LDA        PCLOCK,X
            STA        CLOCK                                ;SET CLOCK COUNTER
            LDA        CLOCK                                ;(DUMMY READ FOR STATUS)
            STY        Z_REG
            LDA        CLOCK                                ;CHECK STATUS BIT
            BNE        STIM040
            PLP                                             ;RESTORE INTERRUPTS
            CPX        #CMON
            BCC        STIM030
;
            LDX        #LDAY
            STX        Z_REG
            LDA        PCLOCK+8
            ORA        #$CC                                 ;STUFF YEAR INTO DAY
            STA        CLOCK                                ;  AND MONTH LATCHES
            INC        Z_REG
            LDA        PCLOCK+8
            LSR        A
            LSR        A
            ORA        #$CC
            STA        CLOCK
;
STIM050:
            PLA
            STA        E_REG                                ;RESTORE ENVIRONMENT
            PLA
            STA        Z_REG                                ;  AND ZERO PAGE
            RTS
;PAGE
GET_TIME    =          *
            LDA        Z_REG                                ;SAVE ZERO PAGE
            PHA
            LDA        E_REG                                ;SAVE ENVIRONMENT
            PHA
            ORA        #BITON7
            STA        E_REG                                ;SET 1 MHZ
;
            LDY        #STATUS
            STY        Z_REG
            LDA        CLOCK                                ;DOES CLOCK EXIST?
            BMI        GTIM050                              ;  NO
;
            LDA        #$10                                 ;ALLOW $10 RETRYS
            STA        RETRY
GTIM010:    LDX        #CMON+1
            PHP
            SEI                                             ;DISABLE INTERRUPTS
;
GTIM020:
            DEX
            BMI        GTIM030                              ;ALL DONE
            STX        Z_REG
            LDA        CLOCK                                ;COPY CLOCK COUNTERS
            STA        CLKTEMP,X                            ;  TO TEMP REGISTERS
            STY        Z_REG
            LDA        CLOCK                                ;CHECK STATUS BIT
            BEQ        GTIM020
;
            PLP                                             ;CLOCK READ ERROR
            DEC        RETRY
            BPL        GTIM010                              ;TRY AGAIN
            BMI        GTIM050
;
GTIM030:    PLP                                             ;RESTORE INTERRUPTS
            LDX        #LDAY+1
            STX        Z_REG
            LDA        CLOCK                                ;READ YEAR FROM DAY
            SEC                                             ;  AND MONTH LATCHES
            ROL        A
            ROL        A
            DEC        Z_REG
            AND        CLOCK
            STA        CLKTEMP+8
;
            LDX        #$09
GTIM040:    LDA        CLKTEMP,X                            ;COPY CLOCK DATA
            STA        PCLOCK,X                             ;  TO PSEUDO CLOCK
            DEX
            BPL        GTIM040
;
GTIM050:    LDA        #$19
            STA        PCLOCK+9
;
            PLA
            STA        E_REG                                ;RESTORE ENVIRONMENT
            PLA
            STA        Z_REG                                ;  AND ZERO PAGE
;
            LDY        #$11
            LDX        #$00
GTIM060:    LDA        PCLOCK,X                             ;GET MOST SIGNIFICANT
            LSR        A                                    ;  BCD DIGIT
            LSR        A
            LSR        A
            LSR        A
            ORA        #$30                                 ;CONVERT TO ASCII
            STA        (TIME),Y
            INX
            DEY
            BMI        GTIM080
GTIM070:    LDA        PCLOCK,X                             ;GET LEAST SIGNIFICANT
            AND        #$0F                                 ;  BCD DIGIT
            ORA        #$30                                 ;CONVERT TO ASCII
            STA        (TIME),Y
            DEY
            CPY        #$07
            BNE        GTIM060
            INX
            BNE        GTIM070
GTIM080:
            RTS
;PAGE
;***************************************************************************************************
;
;  SUBROUTINE DATETIME
;
;  THIS SUBROUTINE READS THE CLOCK AND WRITES A DATE/TIME
;  STAMP TO A FOUR BYTE BUFFER ON THE CALLER'S ZERO PAGE;
;  THE DATA FORMAT IS SHOWN BELOW.  ON ENTRY, X MUST POINT
;  TO THE BUFFER.  ON EXIT, ALL REGISTERS ARE CLOBBERED_
;  IF AN ERROR OCCURS, CARRY IS SET AND THE BUFFER IS
;  SET TO ZERO; OTHERWISE, CARRY IS CLEARED_
;
;   BITS: 7 6 5 4 3 2 1 0
;    X+0  M M M D D D D D
;    X+1  Y Y Y Y Y Y Y M
;    X+2   -  MINUTE   -
;    X+3   - - HOUR  - -
;
;***************************************************************************************************
;
;  TEMPORARY STORAGE
;
OFFSET:     .BYTE      0
ERRCNT:     .BYTE      0
CLKREGS:    .RES       5
MIN         =          CLKREGS+0
HOUR        =          CLKREGS+1
DAY         =          CLKREGS+3
MON         =          CLKREGS+4
YEAR        =          CLKREGS+2
;
;
DATETIME    =          *
            STX        OFFSET
            LDA        Z_REG
            PHA                                             ;SAVE ZERO PAGE
            LDA        E_REG
            PHA                                             ;  AND ENVIRONMENT
            ORA        #BITON7+BITON6                       ;SET 1 MHZ AND
            STA        E_REG                                ;  ENABLE I/O SPACE
;
            LDY        #STATUS
            STY        Z_REG
            LDA        CLOCK                                ;DOES CLOCK EXIST?
            BMI        DT030                                ;  NO
;
            LDA        #8
            STA        ERRCNT                               ;ALLOW 8 RETRYS
DT010:      LDX        #CMON+1
            PHP
            SEI                                             ;DISABLE INTERRUPTS
;
DT020:
            DEX
            CPX        #CMIN
            BCC        DT050
            STX        Z_REG
            LDA        CLOCK                                ;READ THE CLOCK
            STA        CLKREGS-CMIN,X
            STY        Z_REG
            LDA        CLOCK                                ;CHECK STATUS
            BEQ        DT020
;
            PLP                                             ;CLOCK READ ERROR
            DEC        ERRCNT
            BPL        DT010
DT030:
            PLA
            STA        E_REG                                ;RESTORE ENVIRONMENT
            PLA
            STA        Z_REG                                ;  AND ZERO PAGE
            LDX        #CMON-CMIN
DT040:      LDA        PCLOCK+CMIN,X
            STA        CLKREGS,X
            DEX
            BPL        DT040
            LDX        PCLOCK+8
            JMP        DT060
;
DT050:      PLP                                             ;READ YEAR FROM LATCHES
            LDA        #LDAY+1
            STA        Z_REG
            LDA        CLOCK
            SEC
            ROL        A
            ROL        A
            DEC        Z_REG
            AND        CLOCK
            TAX
;
            PLA
            STA        E_REG                                ;RESTORE ENVIRONMENT
            PLA
            STA        Z_REG                                ;  AND ZERO PAGE
;
DT060:
            TXA
            JSR        BCDBIN                               ;CONVERT YEAR TO BINARY
            STA        YEAR
            LDA        MON                                  ;CONVERT MONTH AND DAY
            JSR        BCDBIN                               ;  TO BINARY THEN
            ASL        A                                    ;  COMBINE WITH YEAR
            ASL        A                                    ;  TO FORM DATE STAMP
            ASL        A
            ASL        A
            ASL        A
            STA        MON
            ROL        YEAR
            LDA        DAY
            JSR        BCDBIN
            ORA        MON
            LDX        OFFSET
            STA        0,X
            LDA        YEAR
            STA        1,X
            LDA        MIN                                  ;CONVERT MINUTE
            JSR        BCDBIN
            STA        2,X
            LDA        HOUR                                 ;CONVERT HOUR
            JSR        BCDBIN
            STA        3,X
            CLC
            RTS
;PAGE
;***************************************************************************************************
;
;  SUBROUTINE BCDBIN
;
;  THIS SUBROUTINE CONVERTS A BYTE FROM BCD TO BINARY.
;  THE BYTE IS PASSED AND RETURNED IN A.  THERE IS NO
;  ERROR CHECKING.  Y IS DESTROYED AND X IS UNCHANGED_
;
;***************************************************************************************************
;
BCDBIN      =          *
            PHA
            LSR        A                                    ;ISOLATE TENS DIGIT FOR
            LSR        A                                    ;  INDEXING THE TABLE
            LSR        A
            LSR        A
            TAY
            PLA
            AND        #$0F                                 ;GET UNITS
            CLC
            ADC        TENS,Y                               ;ADD IN TENS
            RTS
;
TENS:       .BYTE      00,10,20,30,40,50,60,70,80,90
;PAGE
;***************************************************************************************************
;
;  SOS CALL $64 -- JOYSTICK INPUT
;    JOYSTICK(IN.J_MODE; OUT.J_VALUE)
;
;***************************************************************************************************
;
;
AD_INPUT    =          $D0
AD_TEMP     =          $D1
;
PA_SW0      =          $C061                                ;PORT A, SWITCH 0
PA_SW1      =          $C063                                ;PORT A, SWITCH 1
PB_SW0      =          $C062                                ;PORT B, SWITCH 0
PB_SW1      =          $C060                                ;PORT B, SWITCH 1
;
AD_SEL0     =          $C058                                ;A/D SELECT CONTROLS
AD_SEL1     =          $C05E
AD_SEL2     =          $C05A
AD_CHRG     =          $C05C                                ;A/D RAMP CHARGE /
AD_STRT     =          $C05D                                ;    START TIMEOUT
AD_FLAG     =          $C066                                ;A/D TIMEOUT FLAG
;
TCHARGE     =          500                                  ;CHARGE TIME FOR A/D
TOFFSET     =          360                                  ;OFFSET TIME TO A/D WINDOW
;
ANALOG      =          $F4A8                                ;ROM ENTRY FOR ANALOG INPUT
ANLOG1      =          $F4AB                                ;  INTERRUPT REENTRY
D_T2        =          $FFD8                                ;TIMER
D_ACR       =          $FFDB                                ;AUXILIARY CONTROL REGISTER
D_IFR       =          $FFDD                                ;INTERRUPT FLAG REGISTER
;
ENSEL       =          $C0DC
ENSIO       =          $C0DE
;
;
JOYSTICK    =          *
            LDA        J_MODE                               ;VALIDATE J_MODE
            CMP        #$08
            BCC        JS010
            LDA        #BADJMODE
JS_ERR:     JSR        SYSERR
;
JS010:      JSR        AD_SETUP                             ;SET UP RESOURCES
            BCS        JS_ERR
            LDA        J_MODE                               ;READ PORT B OR PORT A?
            AND        #BITON2
            BNE        JS020
            LDA        PB_SW0                               ;PORT B
            LDX        PB_SW1
            LDY        #$01
            BNE        JS030
JS020:      LDA        PA_SW0                               ;PORT A
            LDX        PA_SW1
            LDY        #$03
JS030:      STY        AD_INPUT                             ;SAVE INPUT SELECT
            AND        #BITON7
            BEQ        JS040
            LDA        #$FF
JS040:      LDY        #$00
            STA        (J_VALUE),Y                          ;RETURN SWITCH 0
            TXA
            AND        #BITON7
            BEQ        JS050
            LDA        #$FF
JS050:
            INY
            STA        (J_VALUE),Y                          ;RETURN SWITCH 1
;
            LSR        J_MODE
            BCC        JS060
            LDA        AD_INPUT
            JSR        AD_READ                              ;READ A/D
            LDY        #$02
            STA        (J_VALUE),Y                          ;RETURN X AXIS
JS060:      INC        AD_INPUT
            LSR        J_MODE
            BCC        JS070
            LDA        AD_INPUT
            JSR        AD_READ                              ;READ A/D
            LDY        #$03
            STA        (J_VALUE),Y                          ;RETURN Y AXIS
;
JS070:      JSR        AD_CLNUP                             ;CLEAN UP
            RTS                                             ;  AND EXIT
;PAGE
;***************************************************************************************************
;
;  SUBROUTINE AD_SETUP
;  THIS SUBROUTINE SETS UP THE ENVIRONMENT AND RESOURCES
;  FOR READING THE JOYSTICKS.  IF AN ERROR OCCURS, CARRY
;  IS SET AND AN ERROR NUMBER IS RETURNED IN A.
;  OTHERWISE, CARRY IS CLEARED_
;
;***************************************************************************************************
AD_SETUP    =          *
            LDA        #JOYSIRSIZ
            LDX        #<JOYSIRTBL
            LDY        #>JOYSIRTBL
            JSR        ALLOCSIR                             ;ALLOCATE RESOURCES
            BCC        ADS010
            LDA        #XNORESRC
            RTS
ADS010:     LDA        E_REG
            AND        #$7F                                 ;SET 2 MHZ,
            ORA        #$43                                 ;  ENABLE ROM, & I/O SPACE
            STA        E_REG
            PHP
            SEI
            LDA        D_ACR
            AND        #BITOFF5                             ;SET UP TIMER
            STA        D_ACR
            PLP
            BIT        ENSEL                                ;DISABLE ENSEL
            BIT        ENSIO                                ;SET ENSIO FOR INPUT
            RTS
;
JOYSIRTBL   =          *
            .BYTE      $0C,0,0,0,0                          ;ENSIO
            .BYTE      $0D,0,0,0,0                          ;ENSEL
            .BYTE      $0E,0,0,0,0                          ;6522 D_T2
JOYSIRSIZ   =          *-JOYSIRTBL
;***************************************************************************************************
;
;  SUBROUTINE AD_CLNUP
;  THIS SUBROUTINE RESTORES THE ENVIRONMENT AND RELEASES
;  THE RESOURCES AFTER READING THE JOYSTICKS.
;
;***************************************************************************************************
AD_CLNUP    =          *
            LDA        E_REG
            AND        #$3C                                 ;RESTORE RAM AT $C000 & $F000
            STA        E_REG
            LDA        #JOYSIRSIZ
            LDX        #<JOYSIRTBL
            LDY        #>JOYSIRTBL
            JSR        DEALCSIR                             ;DEALLOCATE RESOURCES
            RTS
;PAGE
;***************************************************************************************************
;
;  SUBROUTINE AD_READ
;  THIS SUBROUTINE READS A SPECIFIED A/D INPUT AND RETURNS
;  AN 8 BIT RESULT.  IT ASSUMES THAT THE A/D RESOURCES HAVE
;  BEEN ALLOCATED, THE I/O SPACE AND $F000 ROM HAVE BEEN
;  SELECTED, AND THE SYSTEM IS RUNNING IN 2 MHZ MODE_
;
;  PARAMETERS:
;    A:  A/D INPUT PORT (0-7)
;
;  RETURN VALUE:
;    A:  RESULT (0 - 255)
;    X, Y:  UNDEFINED
;
;***************************************************************************************************
;
AD_READ     =          *
            LSR        A                                    ;SELECT THE APPROPRIATE
            BIT        AD_SEL0                              ;  A/D INPUT
            BCC        ADR010
            BIT        AD_SEL0+1
ADR010:     LSR        A
            BIT        AD_SEL1
            BCC        ADR020
            BIT        AD_SEL1+1
ADR020:     LSR        A
            BIT        AD_SEL2
            BCC        ADR030
            BIT        AD_SEL2+1
ADR030:
            PHP
;
ADR040:
            CLI
            BIT        AD_CHRG                              ;CHARGE A/D CAPACITOR
            LDA        #<TCHARGE
            STA        D_T2
            LDA        #>TCHARGE
            STA        D_T2+1
            LDA        #BITON5
ADR050:     BIT        D_IFR
            BEQ        ADR050
;
            SEI
            SEC
            LDA        #<TOFFSET
            STA        D_T2                                 ;SET UP TIMER
            LDA        #>TOFFSET
            BIT        AD_STRT                              ;START A/D TIMEOUT
            JSR        ANALOG                               ;MEASURE CONVERSION TIME
            BCC        ADR070
;
ADR060:     CLI                                             ;PROCESS AN INTERRUPT
            SEI
            BIT        AD_FLAG                              ;STILL TIMING?
            BPL        ADR040                               ;  NO -- START OVER
            JSR        ANLOG1                               ;  YES -- CONTINUE
            BCS        ADR060
;
ADR070:
            PLP
            EOR        #$FF                                 ;NORMALIZE RESULT
            BMI        ADR080                               ;RESULT < 0
            STA        AD_TEMP
            TYA
            EOR        #$FF
            LSR        AD_TEMP
            ROR        A
            LSR        AD_TEMP
            ROR        A
            LSR        AD_TEMP
            BNE        ADR090                               ;RESULT > 255
            ROR        A
            ADC        #0
            RTS
ADR080:     LDA        #0
            RTS
ADR090:     LDA        #$FF
            RTS
;PAGE
;***************************************************************************************************
;
;  SYSTEM COLD START
;
;  THIS ROUTINE IS CALLED TO TELL THE USER TO REBOOT THE
;  SYSTEM.  IT CLEARS THE SCREEN, DISPLAYS A MESSAGE,
;  OVERWRITES BANKED MEMORY, AND HANGS UNTIL THE USER
;  PERFORMS A HARD RESET.
;
;***************************************************************************************************
;
;
COLDSTRT    =          *
            SEI                                             ;SHUT DOWN INTERRUPTS
            LDA        #$40                                 ;  AND IGNORE NMI
            STA        $FFCA
            ;LDA        #$67        ;(S) let the use be able
            ;STA        E_REG       ;(S) to press reset       ;DISABLE RESET
                                    ;(S) had to power on/off
                                    ;(S) before
                                    ;(S) 5 bytes shorter
            LDA        #$00
            STA        Z_REG                                ;USE PAGE ZERO
;
            LDX        SYSBANK
            LDA        #$BF
            LDY        #$00
            STY        MEMORY
CS010:      STA        MEMORY+1
            STX        B_REG
            LDA        #$A0
CS020:      STA        (MEMORY),Y                           ;SET MEMORY TO BLANKS
            DEY
            BNE        CS020
            DEC        MEMORY+1
            BNE        CS020
            DEX
            BPL        CS010
;
            LDY        #6
CS030:      STA        $C050,Y                              ;SELECT 40 COLUMN
            DEY                                             ;  BLACK & WHITE TEXT
            BPL        CS030
;
            LDY        #BOOTLEN
CS040:      LDA        BOOTMSG-1,Y                          ;PRINT BOOT MESSAGE
            STA        BOOTADR-1,Y
            DEY
            BNE        CS040
;
            LDA        #$77
            STA        E_REG                                ;ENABLE RESET
            BNE        *              ;(S) save a byte      ;HANG UNTIL RESET
                                      ;(S) needed so we have six
                                      ;(S) to fit the new message
;PAGE
;MSB ON
BOOTMSG:    ASCMSBON   "Insert startup disk; press CTRL-RESET" ;(S)  old ->"INSERT SYSTEM DISKETTE & REBOOT"
                                                               ;(S) the new message is 6 chars longer
BOOTLEN     =          *-BOOTMSG
BOOTADR     =          (40-BOOTLEN)/2+$628
;MSB OFF
;LST ON
ZZEND       =          *
ZZLEN       =          ZZEND-ZZORG
            .IF        ZZLEN-LENUMGR
            .FATAL     "SOSORG FILE IS INCORRECT FOR UMBR"
            .ENDIF

