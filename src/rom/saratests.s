; Update rom diagnostics for hard disk booting
; - Removed User entry RAM test to free up some space
; - Add test for Alpha lock key down to choose boot:
;    down = do normal floppy boot
;    up   = do hard diskboot first, search for prodos block mode card
;           and load block 0 from that, fallback to floppy if no
;           card found
; - Bypass rom check
; - Add test for shift key pressed:
;    not pressed = boot unit0
;    pressed     = boot unit1
;
; Updates by Robert Justice


;******************************************************************        
;* APPLE /// ROM - DIAGNOSTIC ROUTINES        
;* COPYRIGHT 1979 BY APPLE COMPUTER, INC.        
;******************************************************************        

           .setcpu "6502"
		   .segment "CODE"
		   
;           .ABSOLUTE
;           .PROC   SARATESTS

;******************************************************************
;
; SARA DIAGNOSTIC TEST ROUTINES
;
; DECEMBER 18,1979
;  BY
; W. BROEDNER & R. LASHLEY 0000
;
; COPYRIGHT 1979 BY APPLE COMPUTER, INC.
;
;******************************************************************

ROM        =    $01
ZRPG       =    $00
ZRPG1      =    $10
PTRLO      =    ZRPG1+$08
PTRHI      =    ZRPG1+$09
BNK        =    ZRPG1+$0A
IBCMD      =    $87
IBBUFP     =    $85
PREVTRK    =    $91
BLOCKIO    =    $F479
CV         =    $5D
STK0       =    $FF
IBNK       =    $1400+PTRHI
PHPR       =    $1800+ZRPG1
KYBD       =    $C000
KEYBD      =    $C008
KBDSTRB    =    $C010
PDLEN      =    $C058
ADRS       =    $C047
GRMD       =    $C050
TXTMD      =    $C051
ADTO       =    $C066
DISKOFF    =    $C0D0
ACIAST     =    $C0F1
ACIACM     =    $C0F2
ACIACN     =    $C0F3
SLT1       =    $C100
SLT2       =    $C200
SLT3       =    $C300
SLT4       =    $C400
EXPROM     =    $CFFF
ZPREG      =    $FFD0
SYSD1      =    $FFDF
SYSD2      =    $FFD2
SYSD3      =    $FFD3
SYSEO      =    $FFE0
BNKSW      =    $FFEF
SYSE2      =    $FFE2
SYSE3      =    $FFE3
COUT       =    $FC39
CROUT1     =    $FD07
KEYIN      =    $FD0F
SETCVH     =    $FBDB
CLDSTRT    =    $FD98
SETUP      =    $FD9D
MONITOR    =    $F901
;
; prodos card block driver addreses
p_ibcmd    =    $42             ; disk command (=1 for read)
p_unit     =    $43             ; (16*slot)+(128*(drive-1))
p_ibbufp   =    $44             ; prodos block dev buffer pointer
p_blknum   =    $46
p_dent     =    $48             ; device call entry address.

ScanStart  =    $C4             ; Slot number to start scan from
;
           .ORG    $F4C5
RAMTBL:    .BYTE   $00,$B1,$B2,$BA,$B9,$10,$00,$13
              
CHPG       =    *     
           .BYTE  "RA"
           .BYTE   $CD          ; M
           .BYTE  "RO"
           .BYTE   $CD          ; M
           .BYTE  "VI"
           .BYTE   $C1          ; A
           .BYTE  "ACI"
           .BYTE   $C1          ; A
           .BYTE  "A/"
           .BYTE   $C4          ; D
           .BYTE  "DIAGNOSTI"
                   
           .BYTE   $C3          ; C
           .BYTE  "Z"
           .BYTE   $D0          ; P
           .BYTE  "RETR"
           .BYTE   $D9          ; Y
;
; SETUP SYSTEM
; Entry point from reset
;
           LDA     #$52+ROM     ; TURN OFF SCREEN, SET 2MHZ SPEED
           STA     SYSD1        ; AND RUN OFF ROM
           LDX     #00          ; SET BANK SWITCH TO ZERO
           STX     SYSEO        
           STX     BNKSW        
           STX     ZPREG        ; AND SET ZERO PAGE SAME
           DEX             
           STX     SYSD2        ; PROGRAM DDR'S
           STX     SYSD3        
           TXS             
           INX             
           LDA     #$0F        
           STA     SYSE3        
           LDA     #$3F        
           STA     SYSE2        
           LDY     #$0E        
DISK1:     LDA     DISKOFF,Y        
           DEY             
           DEY             
           BPL     DISK1        
           LDA     KEYBD        ; test for ctrl key pressed
           AND     #04          
           BNE     NXBYT        ; if no, do diagnostic tests 
           JMP     RECON        ; else skip them and go boot
;                    
; VERIFY ZERO PAGE                
;                     
NXBYT:     LDA     #01          ; ROTATE A 1 THROUGH
NXBIT:     STA     ZRPG,X       ; EACH BIT IN THE 0 PG
           CMP     ZRPG,X       ; TO COMPLETELY TEST
NOGOOD:    BNE     NOGOOD       ; THE PAGE. HANG IF NOGOOD.
           ASL     A            ; TRY NEXT BIT OF BYTE
           BNE     NXBIT        ; UNTIL BYTE IS ZERO.
           INX                  ; CONTINUE UNTIL PAGE
           BNE     NXBYT        ; IS DONE.
CNTWR:     TXA                  ; PUSH A DIFFERENT
           PHA                  ; BYTE ONTO THE
           INX                  ; STACK UNTIL ALL
           BNE     CNTWR        ; STCK BYTES ARE FULL.
           DEX                  ; THEN PULL THEM
           STX     PTRLO        ; OFF AND COMPARE TO
PULBT:     PLA                  ; THE COUNTER GOING
           CMP     PTRLO        ; BACKWARDS. HANG IF
           BNE     NOGOOD       ; THEY DON'T AGREE.
           DEC     PTRLO        ; GET NEXT COUNTER BYTE
           BNE     PULBT        ; CONTINUE UNTIL STACK
           PLA                  ; IS DONE. TEST LAST BYTE
           BNE     NOGOOD       ; AGAINST ZERO.
;                        
; SIZE IN MEMORY                
;                        
           LDX     #08          ; ZERO THE BYTES USED TO DISPLAY
NOMEM:     STA     ZRPG1,X      ; THE BAD RAM LOCATIONS
           DEX                  ; EACH BYTE= A CAS LINE
           BPL     NOMEM        ; ON THE SARA BOARD.
           LDX     #02          ; STARTING AT PAGE 2
NMEM1:     STX     PTRHI        ; TEST THE LAST BYTE
           LDA     #00          ; IN EACH MEM PAGE TO
           LDY     #$FF         ; SEE IF THE CHIPS ARE
           STA     (PTRLO),Y    ; THERE..(AVOID 0 & STK PAGES)
           CMP     (PTRLO),Y    ; CAN THE BYTE BE O'D?
           BEQ     NMEM2        
           JSR     RAM          ; NO, FIND WHICH CAS IT IS.
           STY     ZRPG1,X      ; SET CORRES. BYTE TO $FF
           LDX     PTRHI        ; RESTORE X REGISTER
NMEM2:     INX                  ; AND INCREMENT TO NEXT
           CPX     #$C0         ; PAGE UNTIL I/O IS REACHED.
           BNE     NMEM1        
           LDX     #$20         ; THEN RESET TO PAGE 20
           INC     BNKSW        ; AND GOTO NEXT BANK TO
           LDA     BNKSW        ; CONTINUE.(MASK INPUTS
           AND     #$0F         ; FROM BANKSWITCH TO SEE
           CMP     #03          ; WHAT SWITCH IS SET TO)
           BNE     NMEM1        ; CONTINUE UNTIL BANK '3'
;                        
; SETUP SCREEN                
;                        
ERRLP:     JSR     SETUP        ; CALL SCRN SETUP ROUTINE
           LDX     #00          ; SETUP I/O AGAIN
           STX     SYSEO        ; FOR VIA TEST
           DEX                  ; PROGRAM DATA DIR
           STX     SYSD2        ; REGISTERS
           STX     SYSD3        
           LDA     #$3F        
           STA     SYSE2        
           LDA     #$0F        
           STA     SYSE3        
           LDX     #$10         ; HEADING OF 'DIAGNOSTICS' WITH
           JSR     STRWT        ; THIS SUBROUTINE
ERRLP1:    LDX     #00          ; PRINT 'RAM'
           STX     CV           ; SET CURSOR TO 2ND LINE
           LDA     #04          ; SPACE CURSOR OUT 3
           JSR     SETCVH       ; (X STILL=0 ON RETURN)
           JSR     STRWT        ; THE SAME SUBROUTINE
           LDX     #07          ; FOR BYTES 7 - 0 IN
RAMWT1     =    *                 
           LDA     ZRPG1,X      ; OUT EACH BIT AS A
           LDY     #08          ; ' ' OR '1' FOR INDICATE BAD OR MISSING RAM
RAMWT2:    ASL     A            ; CHIPS SUBROUTINE 'RAM'        RAM
           PHA                  ; SETS UP THESE BYTES
           LDA     #$AE         ; LOAD A '.' TO ACC.
           BCC     RAMWT4         
           LDA     #$31         ; LOAD A '1' TO ACC.
RAMWT4:    JSR     COUT         ; AND PRINT IT
           PLA                  ; RESTORE BYTE
           DEY                  ; AND ROTATE ALL 8
           BNE     RAMWT2       ; TIMES
           JSR     CROUT1       ; CLEAR TO END OF LINE.
           DEX                     
           BPL     RAMWT1                
;                                
; ZPG & STK TEST                        
;                                
           TXS                     
           STY     BNKSW                
ZP1:       TYA                     
           STA     ZPREG                
           STA     STK0                
           INY                     
           TYA                     
           PHA                     
           PLA                     
           INY                     
           CPY     #$20                
           BNE     ZP1                
           LDY     #00                
           STY     ZPREG                
           STX     PTRLO                
ZP2:       INX                     
           STX     PTRHI                
           TXA                     
           CMP     (PTRLO),Y                
           BNE     ZP3                
           CPX     #$1F                
           BNE     ZP2                
           BEQ     ROMTST                
ZP3        =       *            ; CHIP IS THERE, BAD ZERO AND STACK
           LDX     #$1A         ; SO PRINT 'ZP' MESSAGE
           JSR     MESSERR      ; & SET FLAG (2MHZ MODE)
;                               
; ROM TEST ROUTINE                        
;                               
ROMTST:    LDA     #00          ; SET POINTERS TO
           TAY                  ; $F000
           LDX     #$F0           
           STA     PTRLO          
           STX     PTRHI        ; SET X TO $FF
           LDX     #$FF         ; FOR WINDOWING I/O
ROMTST1:   EOR     (PTRLO),Y    ; COMPUTE CHKSUM ON
           CPX     PTRHI
           BNE     ROMTST2      ; EACH ROM BYTE,
           CPY     #$BF         ; RANGES FFC0-FFEF
           BNE     ROMTST2         
           LDY     #$EF           
ROMTST2:   INY                    
           BNE     ROMTST1         
           INC     PTRHI          
           BNE     ROMTST1         
           TAY                  ; TEST ACC. FOR 0
           BNE     VIATST       ; branch on no, as rom has changed
                                ; was BEQ - YES, NEXT TEST
           LDX     #03          ; PRINT 'ROM' AND
           JSR     MESSERR      ; SET ERROR
;                                 
; VIA TEST ROUTINE                        
;
VIATST:    CLC                  ; SET UP FOR ADDING BYTES
           CLD                    
           LDA     SYSEO        ; MASK OFF INPUT BITS
           AND     #$3F         ; AND STORE BYTE IN
           STA     PTRLO        ; TEMPOR. LOCATION
           LDA     BNKSW        ; MASK OFF INPUT BITS
           AND     #$4F         ; AND ADD TO STORED
           ADC     PTRLO        ; BYTE IN TEMP. LOC.
           ADC     ZPREG        ; ADD REMAINING
           STA     PTRLO        ; REGISTERS OF THE
           LDA     SYSD1        ; VIA'S
           AND     #$5F         ; (MASK THIS ONE)
           ADC     PTRLO        ; AND TEST
           ADC     SYSD2        ; TO SEE
           ADC     SYSD3        ; IF THEY AGREE
           ADC     SYSE2        ; WITH THE RESET
           ADC     SYSE3        ; CONDITION.
           CMP     #$E0+ROM     ;  =E1?
           BEQ     ACIA         ; YES, NEXT TEST
           LDX     #06          ; NO, PRINT 'VIA' MESS
           JSR     MESSERR      ; AND SET ERROR FLAG
;                               
; ACIA TEST                        
;                               
ACIA:      CLC                  ; SET UP FOR ADDITION
           LDA     #$9F         ; MASK INPUT BITS
           AND     ACIAST       ; FROM STATUS REG
           ADC     ACIACM       ; AND ADD DEFAULT STATES
           ADC     ACIACN       ; OIF CONTROL AND COMMAND
           CMP     #$10         ; REGS.        =10?
           BEQ     ATD          ; YES, NEXT TEST
           LDX     #09          ; NO,        'ACIA' MESSAGE AND
           JSR     MESSERR      ; THEN SET ERROR FLAG
;                                
; A/D TEST ROUTINE                        
;                                
ATD:       LDA     #$C0                
           STA     $FFDC                
           LDA     PDLEN+2                
           LDA     PDLEN+6                
           LDA     PDLEN+4                
           LDY     #$20                
ADCTST1:   DEY                  ; WAIT FOR 40 USEC
           BNE     ADCTST1         
           LDA     PDLEN+5      ; SET A/D RAMP
ADCTST3:   INY                  ; COUNT FOR CONVERSION
           BEQ     ADCERR         
           LDA     ADTO         ; IF BIT 7=1?
           BMI     ADCTST3      ; YES, CONTINUE
           TYA                  ; NO, MOVE COUNT TO ACC
           AND     #$E0         ; ACC<32
           BEQ     KEYPLUG         
ADCERR     =       *            ; NO
           LDX     #$0D         ; PRINT 'A/D' MESS
           JSR     MESSERR      ; AND SET ERROR FLAG
;                                
; KEYBOARD PLUGIN TEST                
;                                
KEYPLUG:   LDA     KEYBD        ; IS KYBD PLUGGED IN?
           ASL     A            ; (IS LIGHT CURRENT
           BPL     SEX          ; PRESENT?) NO, BRANCH
           LDA     SYSD1        ; IS ERROR FLAG SET?
           BMI     SEX          ; ERROR HANG
;
; RECONFIGURE THE SYSTEM
;
RECON:     LDA     #$77         ; TURN ON SCREEN
           STA     SYSD1
           JSR     CLDSTRT      ; INITIALIZE MONITOR AND DEFAULT CHARACTER SET
           BIT     KBDSTRB      ; CLEAR KEYBOARD
           LDA     EXPROM       ; DISABLE ALL SLOTS
RETRY:     LDA     $C020
           LDA     #$08         ; TEST FOR "ALPHA LOCK"
           AND     KEYBD
           BEQ     BOOT         ; YES, DO FLOPPY BOOT
           LDA     #$10         ; TEST FOR "APPLE 1"
           AND     KEYBD
           BNE     HDBOOT       ; NO, DO HARD DISK BOOT FIRST
           JSR     MONITOR      ; AND NEVER COME BACK
;
; system exerciser stub (error)
; 
SEX:       JMP     SEX


Signature: .byte $FF, $20, $FF, $00    ; Disk card signature for disk controller
           .byte $FF, $03
;
; Hard disk boot
;
HDBOOT:    lda     SYSD1               ; set 1Mhz for better compatibility
           ora     #$80                ; with the prodos cards.
           sta     SYSD1
           lda     #ScanStart          ; load starting scan slot (Cs)
           sta     PTRHI
           lda     #$00
           sta     PTRLO

CheckNext: ldy     #$05                ; We check all 3 sig bytes, starting from last
@1:        lda     (PTRLO),y
           cmp     Signature,Y
           bne     NoMatch             ; No device if bytes don't match
           dey
           dey
           bpl     @1
            
           ldy     #$ff                ; $CxFF - check last byte
           lda     (PTRLO),y
           beq     NoMatch             ; if $00, is a Disk II 16 sector device, error
           cmp     #$ff
           bne     Match               ; if its not $ff (Disk II 13 sector device)
                                       ; Then we found an intelligent disk controller :-)

NoMatch:   dec     PTRHI               ; Else try next slot
           lda     PTRHI
           and     #$07
           bne     CheckNext           ; Check next slot
                                       ; else, not found then try floppy boot
;
; floppy boot
;
BOOT:      LDX     #01          ; READ BLOCK 0
           STX     IBCMD         
           DEX                   
           STX     IBBUFP       ; INTO RAM AT $A000
           LDA     #$A0            
           STA     IBBUFP+1        
           LSR     A            ; FOR TRACK 80
           STA     PREVTRK      ; MAKE IT RECALIBRATE TOO!
           TXA                  
           JSR     BLOCKIO                
BOOTCHK:   BCC     GOBOOT       ; IF WE'VE SUCCEEDED. DO IT UP
           LDX     #$1C            
           JSR     STRWT        ; 'RETRY'
           JSR     KEYIN          
           BCS     RETRY           
GOBOOT:    JMP     $A000        ; GO TO IT FOOL...


Match:     sta     p_dent              ; Set card driver entry low byte
           lda     PTRHI
           sta     p_dent+1            ; Set card driver entry high byte
           asl
           asl
           asl
           asl
           sta     p_unit
           lda     KEYBD
           and     #$01                ;test for any key pressed
           beq     unit0               ;no, boot unit 0
           lda     p_unit
           ora     #$80                ;yes, boot unit 1
           sta     p_unit
unit0:     ldx     #0                  ;block 0
           stx     p_blknum
           stx     p_blknum+1
           stx     p_ibbufp            ;load into $A000
           inx
           stx     p_ibcmd             ;read
           lda     #$a0
           sta     p_ibbufp+1
           jsr     pblockio            ;prodos card blockio
           jmp     BOOTCHK
            
pblockio:  jmp     (p_dent)            ;device block entry 

; pad out so sara test subroutines start from the original address
bootend    = *
           .res    $f738 - bootend, $00               
;
;F738
;

;              
;******************************                
; SARA TEST SUBROUTINES                
;******************************                
;                
STRWT:     LDA     CHPG,X                
           PHA             
           ORA     #$80         ; NORMAL VIDEO        
           JSR     COUT         ; & PRINT        
           INX                  ; NXT        
           PLA                  ; CHR        
           BPL     STRWT                
           JMP     CROUT1       ; CLR TO END OF LINE        
;                
; SUBROUTINE RAM                
;                
RAM:       PHA                  ; SV ACC        
           TXA                  ; CONVRT        
           LSR     A            ; ADD TO        
           LSR     A            ; USE FOR        
           LSR     A            ; 8 ENTRY        
           LSR     A                
           PHP             
           LSR     A                
           PLP             
           TAX                  ; LOOKUP        
           LDA     RAMTBL,X     ; IF VAL        
           BPL     RAMO         ; <0, GET        
           PHA                  ; WHICH        
           LDA     BNKSW                
           AND     #$0F                
           TAX             
           PLA             
           CPX     #00                
           BEQ     RAM1         ; BANK?        
           LSR     A            ; SET
           LSR     A            ; PROPER        
           LSR     A            ; RAM        
           DEX                  ; VAL        
           BNE     RAM1                
           AND     #05          ; CONVERT        
RAMO:      BNE     RAM1         ; TO VAL        
           TXA             
           BEQ     RAM00                
           LDA     #03                
RAM00:     BCC     RAM1                
           EOR     #03                
RAM1:      AND     #07          ; BANKSW        
           TAX             
           PLA             
           RTS             
;               
; SUBROUTINE ERROR                
;                
MESSERR:   JSR     STRWT        ; PRINT MESSAGE FIRST        
ERROR:     LDA     #$F2+ROM     ; SET 1        
           STA     SYSD1        ; MHZ MO        
           RTS                
;                
; SUBROUTINE RAMSET                
;                
RAMSET:    LDX     #01                
           STX     BNK                
           LDY     #00                
           LDA     #$AA                
           SEC             
RAMSET1:   PHA             
           PHP             
           LDA     BNK                
           ORA     #$80                
           STA     IBNK                
           LDA     #02                
           STA     PTRHI                
           LDX     #00                
           STX     PTRLO                
           PLP             
           PLA             
           RTS             
;               
; SUBROUTINE PTRINC                
;               
PTRINC:    PHA             
           INC     PTRLO                
           BNE     RETS                
           LDA     BNK                
           BPL     PINC1                
           LDA     PTRHI                
           CMP     #$13                
           BEQ     PINC2                
           CMP     #$17                
           BNE     PINC1                
           INC     PTRHI                
PINC2:     INC     PTRHI                
PINC1:     INC     PTRHI                
           BNE     RETS                
           DEC     BNK                
           DEC     BNK                
           JSR     RAMSET1                
RETS:      PLA             
           LDX     BNK                
           CPX     #$FD                
           RTS             
;                
; SUBROUTINE RAMERR                
;                
RAMERR:    PHA             
           LDX     PTRHI                
           LDY     BNK                
           BMI     RAMERR4                
           TXA             
           BMI     RAMERR5                
           CLC             
           ADC     #$20                
RAMERR2:   STY     BNKSW                
           TAX             
RAMERR3:   JSR     RAM                
           PLA             
           PHA             
           LDY     #00                
           EOR     (PTRLO),Y                
           ORA     ZRPG1,X                
           STA     ZRPG1,X                
           PLA             
           RTS             
RAMERR4:   LDA     #00                
           STA     BNKSW
           BEQ     RAMERR3
RAMERR5:   SEC     
           SBC     #$60
           INY     
           BNE     RAMERR2
;                
; SUBROUTINE RAMWT        
;                
RAMWT:     EOR     #$FF
           STA     (PTRLO),Y
RAMRD:     CMP     (PTRLO),Y
           BNE     RAMERR
                
;           .END        

;F7FE