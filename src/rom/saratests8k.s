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
SET1MEG    =    $F34C
MSWAIT     =    $f456
;
PSUEDO_DMA      =            $F800                     ;psuedo DMA code in ROM.
MON_ZPAGE       =            $03
addrdma         =            $C2
Z_REG           =            $FFD0                     ;Zero page register for psuedo DMA
e_reg           =            $ffdf
b_reg           =            $ffef

xmsg3           = $ffff
msg3l           = $ff
prnt_msg        = $ffff ;dummy
bootp           = $ffff
INIT            = $ffff
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
           .BYTE  "RETR"
           .BYTE   $D9          ; Y
CHPGEND    =    *		   
		   .res    $f4ee - CHPGEND,0 ;pad out 
;
; SETUP SYSTEM
; Entry point from reset
;
           .org $f4EE           ;keep same as orig rom entry point
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
;           LDA     KEYBD        ; test for ctrl key pressed
;           AND     #04          
;           BNE     NXBYT        ; if no, do diagnostic tests 
;           JMP     RECON        ; else skip them and go boot
;
; Removed all Diagnostic routines to make room
;                    
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


Signature: .byte $FF, $20, $FF, $00    ; Disk card signature for disk controller
           .byte $FF, $03
;
; Hard disk boot
;
HDBOOT:    jsr     PROFILE             ; try to boot profile in slot4
                                       ;  if we come back from this, assume either profile not found, or not ready
                                       ;  now try prodos cards.
           jsr     SET1MEG             ; set 1Mhz for better compatibility
                                       ; with the prodos cards.
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
           LDX     #0           ;#$1C            
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
           bcc     GOBOOT              ;noerror, go run the bootblock
                                       ;else lets try the floppy
           ldy     #0                  ;wait some more for drive to turn off
wait1:     jsr     MSWAIT              ;helps Fujinet catch when the drive does enable
           dey
           bne     wait1
           jmp     BOOT                ;go boot floppy

pblockio:  jmp     (p_dent)            ;device block entry 


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
           
; pad out so sara test subroutines start from the original address
codeend    = *

;tail = tail_end - tail_start
;fill = $f7fe - tail
;size = fill - codeend
           .res    $1d9, $00         


;----------------------------------------------------------------------
; Common between primary and secondary rom pages
;----------------------------------------------------------------------
tail_start = *

;----------------------------------------------------------------------

PROFILE:   LDA          e_reg
           AND          #$FD     ;bit2->0
           STA          e_reg    ;-- switch to secondary rom bank
           JSR          INIT
           BCS          PRERROR

           ldx          xmsg3                     ;booting profile
           ldy          #msg3l
           jsr          prnt_msg
		   JMP          bootp    ;go boot the profile

PRERROR:   LDA          e_reg
           ORA          #$02     ;bit2->1 
           STA          e_reg    ;-- from secondary rom bank
		   RTS

;----------------------------------------------------------------------
;
;  This routine sets up the Z_REG with the 256 byte page to dma to
;  will dma into the current bank selected
;  It also disables interupts for the duration (up to 256 usec)
;  of the transfer.
;
;----------------------------------------------------------------------

GO_DMA:         LDA          e_reg
				ORA          #$02     ;bit2->1 
				STA          e_reg    ;-- from secondary rom bank

                LDA          e_reg
                ORA          #$80                      ;or in 1 MHz bit
                STA          e_reg
                LDA          addrdma                   ;Set Z_REG to DMA page address
                STA          Z_REG
                LDA          #$FF                      ;Make sure we transfer 256 bytes
                SEC
                JSR          PSUEDO_DMA                ;  do DMA transfer
                LDA          #MON_ZPAGE                ;Restore proper zero page.
                STA          Z_REG

                LDA          e_reg
                AND          #$FD     ;bit2->0
                STA          e_reg    ;-- pass back to secondary rom bank
                RTS
tail_end = *

;F7FE

              .IF        tail_end  <> $F7FE
              .FATAL     "end address is incorrect for saratests.s"
              .ENDIF