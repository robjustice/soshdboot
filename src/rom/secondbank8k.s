;
;  Second bank of Apple /// rom to support Profile booting
;
;  This includes the bootloader code modified to be put in the A3 rom
;  to allow booting directly from the real Profile drive
;  and also slimmed down code from the Profile driver for read only.
;  it using Psuedo DMA for the block reading.
;  
;
;  This bootloader in here loads both the SOS.DRIVER and SOS.KERNEL files
;     SOS.DRIVER is loaded into Bank0 $3000-9fff & Bank1 $2000-9fff
;     SOS.KERNEL is loaded into the highest bank from $1E00 onwards

;  It also reads in the first block of the SOS.INTERP file to 
;  determine the load address and passes this to the sosldr
;
;  by Robert Justice
;
                .feature labels_without_colons
                .setcpu "6502"
                .segment     "CODE"
                .org         $f000 

;*
;* hardware addresses
;*
e_reg           =            $ffdf
b_reg           =            $ffef


;*
;* zero page storage (z reg = $03)
;*

zpage           =            $e0
begin           =            zpage+2                   ; & 3
end             =            zpage+4                   ; & 5
blk_ctr         =            zpage+6
temp            =            zpage+7

sosldr          =            zpage+8                   ; & 9



;* =  ates
;*
dirblk0         =            $a400
entry0          =            dirblk0+4                 ; loc of first file entry in directory
entry_len       =            entry0+$1f                ; loc of entry length in directory
storage         =            0                         ; file's storage type
sapling         =            $20                       ; storage type = tree index file w/one index block
rootdir         =            $f0                       ; storage type = root directory
nextdblk        =            2                         ; loc of next directory block
;*
k_xblk          =            $c00                      ;start loc of sos.kernel's index block
xblk            =            $11                       ; loc of index block address in file entry
k_file          =            $1e00                     ; start loc of sos.kernel file
k_label         =            k_file+0                  ; loc of label in file "sos.kernel"
k_hdr_cnt       =            k_file+8                  ;   "    header       "
k_flags         =            k_file+8+3                ; loc of k_flags in sos.kernel

;----------------------------------------------------------------------
;
;       Hardware I/O Addresses
;
;----------------------------------------------------------------------

E_REG           =            $FFDF                     ;system environment register
Z_REG           =            $FFD0                     ;Zero page register for psuedo DMA
PSUEDO_DMA      =            $F800                     ;psuedo DMA code in ROM.

; hard coded for profile card in slot4
SLOT            =            4
WR_PORT         =            $C080+SLOT*16             ;Write to Z8 RAM (byte at a time)
RD_PORT         =            $C081+SLOT*16             ;Read from Z8 RAM (byte at a time)
BUSY            =            $C082+SLOT*16             ;Z8 not ready
CLR_PARITY      =            $C083+SLOT*16             ;Clear parity error.
;
;----------------------------------------------------------------------
;
;       Constants
;
;----------------------------------------------------------------------

SLOTCN          =            $C000+$100*SLOT
NOTCMD          =            SLOTCN+0                  ;Command line Low
SETCMD          =            SLOTCN+4                  ;Command line High
SETWRT          =            SLOTCN+1                  ;Low
SETRD           =            SLOTCN+5                  ;High
INTDSABL        =            SLOTCN+2
INTENABL        =            SLOTCN+6
RWLO            =            SLOTCN+3
RWHI            =            SLOTCN+7
RST             =            SLOTCN+$0C
CLRRST          =            SLOTCN+8

MON_ZPAGE       =            $03

;----------------------------------------------------------------------
;
;       Command codes
;
;----------------------------------------------------------------------

WDGTRD          =            0
WDGTWRTVER      =            2
WDGTSTAT        =            3
WDGTWRT         =            1

;----------------------------------------------------------------------
;
;       Local variables
;
;----------------------------------------------------------------------

addrdma         =            $C2

Z8CMD           =            $C6
COUNTR          =            $C7
BSYLO           =            $C8                         ;FLAG set when driver has seen busy lo
LONGWAIT        =            $C9
WAITTIME        =            $CA
PIPPIN_RESET    =            $CB
STATUS1         =            $D6
STATUS2         =            $D7
STATUS3         =            $D8
STATUS4         =            $D9
PARITY_ERR      =            $DA
RESET_FLAG      =            $DB
BAD_RESPONSE    =            $DC



;----------------------------------------------------------------------
;
;       More Zero page equates
;
;----------------------------------------------------------------------

name_offset     =            $CC
TIMOUT          =            $CE
RTRYCNT         =            $CF
RTRYTHRESH      =            $D0
CMD_RTRYCNT     =            $D1
RSPNS           =            $D2
BLK_RTRYCNT     =            $D3
blok            =            $D4



;*******************************************************************
;*
;* local data storage
;*
;*******************************************************************

namlen          =       10
name_k:         .byte        "SOS.KERNEL"
name_d:         .byte        "SOS.DRIVER"
name_i:         .byte        "SOS.INTERP"

;
; messages
;
msg             =            *                         ; message table

msg0:           .byte        "    I/O ERR    "
msg0l           =            *-msg0
xmsg0:          .word        *-msg-1

msg1:           .byte        "FILE NOT FOUND"
msg1l           =            *-msg1
xmsg1:          .word        *-msg-1

msg2:           .byte        " INVALID FILE  "
msg2l           =            *-msg2
xmsg2:          .word        *-msg-1

msg3:           .byte        "BOOTING PROFILE"
msg3l           =            *-msg3
xmsg3:          .word        *-msg-1

msg4:           .byte        "LOADING SOS.KERNEL"
msg4l           =            *-msg4
xmsg4:          .word        *-msg-1


;*****************************************************************
;*
;* sos system boot - main code body
;*
;*****************************************************************
;
; turn off interrupts & decimal mode
;
bootp:          sei
                cld
;
; find highest memory bank in system and set bank reg to it
; - max memsize = 512k. (support OnThree 512k memory card)
;
                lda          #$0e                      ; load highest bank for 512k
                sta          b_reg
                sta          $2000
                lda          #$06                      ; highest bank for 256k
                sta          b_reg
                sta          $2000                     ; will overwrite bank e value if not 512k
                lda          #$0e
                sta          b_reg
                cmp          $2000
                beq          boot006                   ; yes, its 512k
                lsr a
                sta          b_reg
                
                ldx          #0
boot005:        dec          b_reg
                stx          $2000
                lda          $2000
                bne          boot005
;
; read in blocks 1 thru n (rest of boot and all of root dir.)
;
boot006:        lda          #1
                sta          blok
                lda          #0
                sta          blok+1
                lda          #$a2
                sta          addrdma

rd_dir:         jsr          read_blk                  ; rest of boot (block 1)
                inc          addrdma
                inc          blok
                lda          blok                      ; have all directory blocks been read?
                cmp          #6
                bcc          rd_dir                    ; loop if not.    

;
; read in SOS.DRIVER file
;
                lda          b_reg                     ; save high bank
                pha
                lda          #0                        ; load SOS.DRIVER into bank0
                sta          b_reg
                lda          #10                       ; set offset to point to SOS.DRIVER name
                sta          name_offset
                jsr          searchdir                 ; search directory for file 'SOS.DRIVER'
                jsr          rdidxblk                  ; read SOS.DRIVER index block
                lda          #$30                      ; read SOS.DRIVER into bank0, $3000 on
                jsr          rddatablks

                pla                                    ; restore high bank
                sta          b_reg

;
; read in SOS.KERNEL file
;
                lda          #0                        ; reset offset to point to SOS.KERNEL name
                sta          name_offset
                jsr          searchdir                 ; search directory for file 'SOS.KERNEL'
                jsr          rdidxblk                  ; read SOS.KERNEL index block
                lda          #$16                      ; read SOS.KERNEL into highest bank, $1e00 on
                jsr          rddatablks                ; rddatablks has a hack to skip pages 18-1f
				                                       ;  we correct the load below

;
; move the $16-$17 pages to $1e,1f
;  cannot psuedo dma to pages $18-$1f, so we load the first block into $16-$17
;  then move it to the correct place
;
                ldx  #0
:               lda  $1600,x
                sta  $1e00,x
                lda  $1700,x
                sta  $1f00,x
                inx
                bne  :-

;
; have a peak in the first block of the SOS.INTERP file to get the load address
;  and update into the sos loader
;
                lda          #20                       ;set offset to point to SOS.INTERP name
                sta          name_offset
                jsr          searchdir                 ;search directory for file 'SOS.INTERP'
                jsr          rdidxblk                  ;read SOS.INTERP index block
				lda          #0
				sta          k_xblk+1                  ;we only want the first block, set the 2nd block index to 0
				sta          k_xblk+256+1
                lda          #$16                      ;read SOS.INTERP first block into $1600
                jsr          rddatablks
                lda          $1600+8                   ;check option header length, only checking the low byte for now
                tax                                    ; either zero, or the length if its there
                lda          $160b,x                   ;grab the load address and save
				sta          k_flags                   ;and use this to set the driver top (start) page
                                                       ; in the sos loader
;
; build sos loader entry point address
;
entry_a3:       clc                                    ; sosldr:=k.hdr.cnt+(k.hdr.cnt)
                lda          #<(k_hdr_cnt+6)
                adc          k_hdr_cnt
                sta          sosldr
                lda          #>(k_hdr_cnt+6)
                adc          k_hdr_cnt+1
                sta          sosldr+1
;
; now jump to sos loader (secondary bootstrap)
;
                jmp          (sosldr)


;*******************************************************************
;*
;* search directory for file
;*
;* input: 'begin' ptr points to directory entry
;*        name_offset set 0 or 10 for SOS.KERNEL or SOS.DRIVER
;*        
;*******************************************************************

searchdir:      lda          #<entry0                  ;get lo byte of address
                sta          begin
                lda          #>entry0
                sta          begin+1

search:         clc                                    ; end:=begin+512-entry.len
                lda          begin+1
                adc          #2
                sta          end+1
                sec
                lda          begin
                sbc          entry_len
                sta          end
                lda          end+1
                sbc          #0
                sta          end+1

srch020:        ldy          #0                        ; does count match?
                lda          (begin),y
                and          #$f
                cmp          #namlen
                bne          srch040                   ; no match

                tay
                clc
                adc          name_offset               ; offset based on file name to compare
                tax
srch030:        lda          (begin),y                 ; do chars match?
                cmp          name_k-1,x
                bne          srch040                   ; no match
                dex
                dey
                bne          srch030

                ldy          #storage                  ;test storage type
                lda          (begin),y                 ;must be sapling
                and          #$f0
                cmp          #sapling
                beq          match
                cmp          #rootdir                  ;skip if stg type=rootdir
                beq          srch040

                ldx          xmsg2                     ;err,invalid file
                ldy          #msg2l
                jsr          prnt_msg
                jmp          hang

srch040:        clc
                lda          begin
                adc          entry_len
                sta          begin
                lda          begin+1
                adc          #0
                sta          begin+1
                lda          end
                cmp          begin                     ;is begin <=end?
                lda          end+1
                sbc          begin+1
                bcs          srch020                   ;yes,search next field in current block

                clc                                    ;begin :=end+entry.len
                lda          end
                adc          entry_len
                sta          begin
                lda          end+1
                adc          #0
                sta          begin+1

                dec          blk_ctr
                bne          search                    ;search the next dir block

                ldx          xmsg1                     ;err, can't find file
                ldy          #msg1l
                jsr          prnt_msg
                jmp          hang

match:          rts


;*******************************************************************
;*
;* read file index block into $0c00
;*
;* input: 'begin' ptr points to directory entry
;*        
;*******************************************************************

rdidxblk:       ldy          #xblk
                lda          (begin),y
                sta          blok
                iny
                lda          (begin),y
                sta          blok+1
                lda          #>k_xblk                   ;get hi byte of address
                sta          addrdma
                jsr          read_blk                   ; index block
                rts

;*******************************************************************
;*
;* read in file data blocks
;*
;* input: a = buffer high byte (assumes buffer low byte always = 0)
;*        file index block loaded into $0c00
;*
;*******************************************************************

rddatablks:     sta          addrdma
                lda          #0
                sta          temp

data010:        ldx          temp                      ; get block address of next data block
                lda          k_xblk,x
                sta          blok
                lda          k_xblk+$100,x
                sta          blok+1

                lda          blok                      ; is next block address = 0 ?
                ora          blok+1 
                beq          rd_done                   ; yes, stop reading

data020:        jsr          read_blk                  ; read data block
                inc          temp                      ; bump for next time
                inc          addrdma

                lda          addrdma                   ; if its loading the driver file wrap
                cmp          #$a0                      ; to bank1 if we are past the end of bank0
                bne          data030                   ; go check for kernel file 1st block hack
                lda          #$20
                sta          addrdma
                inc          b_reg
                bne          data010                   ; bra always

data030:        cmp          #$18                      ; jump around the non dma'able $18xx-1fxx pages
                bne          data010
                lda          #$20
                sta          addrdma
                bne          data010                   ; bra always

rd_done:        rts

;*******************************************************************
;*
;* print message
;*
;* input: msg index  (x)
;*  msg length (y)
;*******************************************************************
msgline         =            $5a8                      ; prnt.msg routine

prnt_msg        =            *
                sty          temp                      ; center msg (y:=40-len/2+len)
                sec
                lda          #40
                sbc          temp
                lsr          a
                clc
                adc          temp
                tay

prnt010:        lda          msg,x
                sta          msgline-1,y
                dex
                dey
                dec          temp
                bne          prnt010
				rts

hang:           lda          $c040                     ; sound bell
                jmp          *                         ; hang until reboot (ctrl/reset)


;----------------------------------------------------------------------
;
;       These Profile Read Block routines are cut from the Profile
;       SOS driver and simplified for the booting case
;
; 
;----------------------------------------------------------------------
;
;  Profile Init
;
;----------------------------------------------------------------------

INIT            =            *
                LDA          #$FF
                STA          PIPPIN_RESET
                STA          COUNTR
                LDA          #0
                STA          PARITY_ERR
                STA          BAD_RESPONSE
                STA          RESET_FLAG
                JSR          S1M
                JSR          DINIT
                STA          CLR_PARITY                ;Clear any previous parity errors.
                LDY          BUSY
                JSR          S2M                       ;set 2MHz mode
                TYA
                AND          #1                        ;check if profile drive is connected (bit0)
                BNE          NODRV
                TYA
                BPL          @40                       ;check if profile drive is busy (bit7)
                LDA          #$FF
                STA          BSYLO
                BNE          DINIT
@40             LDA          BSYLO                     ;has BSY been low?
                BNE          OK
                JMP          ERROR                     ;otherwise error exit

OK              CLC
                RTS

NODRV
ERROR           SEC
                RTS     


S2M             LDA          E_REG
                AND          #$7F                       ;and out 1MHz bit
                STA          E_REG
                RTS
				
S1M             LDA          E_REG
                ORA          #$80                      ;or in 1 MHz bit
                STA          E_REG
                RTS

;
;----------------------------------------------------------------------
;
;  Profile driver -- initialization request.
;
;----------------------------------------------------------------------

DINIT           =            *
                JSR          SETUPREAD                 ;init profile?
                LDA          INTDSABL
                JSR          SETUPWRITE
                JSR          SETUPREAD
                CLC
                RTS                                    ;normal exit


;
;----------------------------------------------------------------------
;
;  Profile driver -- read request.
;
;  Transfers one 512 byte block from disk to buffer
;
;  - assume always page aligned, using DMA
;
;  - input:
;    blok    - block number lsb
;    blok+1  - block number msb
;    addrdma  - 256byte page number to dma to
;
;----------------------------------------------------------------------

read_blk        =            *

RDBLOCK         LDA          #2                        ;Allow 2 retries
                STA          BLK_RTRYCNT
RPAR_RETRY      =            *                         ;Parity error retries enter here
                STA          CMD_RTRYCNT               ;clear # communication tries
                LDA          #$0A                      ;set retry count to 10
                STA          RTRYCNT
                LDA          #3
                STA          RTRYTHRESH                ;set reseek/rewrite threshold

RCOM_RETRY      =            *                         ;CMD-BSY bad response retries enter here
                LDA          #0
                STA          LONGWAIT
                LDA          #WDGTRD
                STA          Z8CMD
                LDA          #1
                STA          RSPNS                     ;set up expected Z8 response
                JSR          SNDCMD                    ;do a CMD-BSY handshake
                BCC          @70
                JMP          DR_ERR1
@70             BNE          RCOM_RETRY                ;bad response - try again.

;               Send command bytes

                JSR          SND_CMDBYTES
                BCS          RDRETRY                   ;try again if parity error
                LDA          #2
                STA          RSPNS                     ;set up for execution
                LDA          #$FF
                STA          LONGWAIT                  ;adjust timeout wait
                JSR          SNDCMD                    ;2nd CMD-BSY for read opn
                BCS          DR_ERR1                   ;timeout on cmd-bsy
                BNE          RCOM_RETRY                ;bad response - try again.

;               read should be complete at this point

                JSR          GETSTAT                   ;get status bytes first
                BCS          RDRETRY                   ;try again if parity error
                BPL          @10
                INC          COUNTR                    ;time to reset PIPPIN?
                BEQ          RCOM_RETRY                ;branch if not
                BNE          DR_ERR1                   ;reset PIPPIN
@10             JSR          DATRANS
                BCS          RDRETRY
                LDA          STATUS1                   ;Now check status for good read
                AND          #1
                BNE          LDAXIO
                LDA          STATUS2
                AND          #$58
                BNE          LDAXIO

                JMP          GOODEXIT

RDRETRY         DEC          BLK_RTRYCNT               ;can we retry?
                BMI          DR_ERR1
                JMP          RPAR_RETRY

DR_ERR1         INC          PIPPIN_RESET
                BNE          @10
                JSR          RESET_PIPPIN
                LDA          #0
                STA          CMD_RTRYCNT
                JMP          RCOM_RETRY
@10             JSR          NOTCMDLN
LDAXIO          LDA          #$ff
                BNE          BADEXIT

GOODEXIT        LDA          #01
                STA          RSPNS                     ;DUMMY HANDSHAKE FOR PROFILE TO UPDATE
                JSR          SNDCMD                    ; ANY PENDING INFO TO DISK.
                LDA          #$FF
                STA          Z8CMD                     ;we're going to send an invalid command
                JSR          SND_CMDBYTES
                JSR          SETCMDLN                  ;raise the CMD line
                JSR          SETCMDLN                  ;give Profile a little time
                JSR          NTCMDLN1                  ;lower the CMD line (so the ready light
;                                                        doesn't go out)
				RTS

BADEXIT         JSR          GOODEXIT
                ldx          xmsg0                     ;err, i/o error
                ldy          #msg0l
                jsr          prnt_msg
                jmp          hang

;----------------------------------------------------------------------
;
;  Profile Block I/O transfer routine.  This routine will transfer 512
;  bytes to users buffer from RAM buffer of Profile's Z8.
;  - assume page aligned and 512 bytes, psuedo DMA is used.
;
;----------------------------------------------------------------------

DATRANS         JSR          SETUPREAD
                LDA          E_REG
                AND          #$7F                      ;and out 1MHz bit
                STA          E_REG
                JSR          GO_DMA                    ;transfer first 256 bytes
                INC          addrdma                   ;next page
                JSR          GO_DMA                    ;transfer second 256 bytes
                JSR          S1M
                JSR          CHKPARITY                 ;Test for parity error in transfer.
                PHP
                JSR          SETUPREAD                 ;restore read state
                JSR          S2M                       ;back to 2 MHz
                PLP
                RTS

;----------------------------------------------------------------------
;
;  The following are routines for handling the communications protocol
;  of sending commands, and receiving result codes.
;
;----------------------------------------------------------------------
;
;  SNDCMD performs a CMD-BSY handshake with the Z8 and checks for a
;  correct response.  If the Z8 responds with an incorrect code, a
;  'no go' code is sent by the Apple and the handshake is retried
;  up to 2 times.  On return, Carry=1 means a handshake timeout or
;  three retries attempted.  A non-zero return means an incorrect
;  response from the Z8 that may be retried.
;
;----------------------------------------------------------------------

SNDCMD          JSR          S1M
                JSR          WAITBSYLO
                BCS          SENDERR                   ;error exit if BSY isn't low
                JSR          SETCMDLN                  ;raise cmd
                JSR          WAITBSYHI                 ;wait for bsy to go hi
                BCS          SENDERR                   ;timeout
                LDY          RD_PORT                   ;read response byte from Z8
                CPY          RSPNS                     ;correct?
                BEQ          CONT                      ;yes if taken
                LDA          #2
                STA          BAD_RESPONSE
                ORA          STATUS3
                STA          STATUS3
                LDA          #$AA                      ;tell Z8 that response not OK
                JSR          BSYACK                    ;drop cmd, wait for bsy to go lo
                BCS          SENDERR                   ;timeout on bsy going lo
                INC          CMD_RTRYCNT               ;bump retry count for bad response
                LDY          CMD_RTRYCNT
                CPY          #2                        ;2 retries yet?
                BCS          SENDERR                   ;yes if taken
                RTS

CONT            LDA          #$55                      ;indicate good response
                JSR          BSYACK
                BCS          SENDERR                   ;bsy timeout
                LDA          #0                        ;indicate good return to caller
SENDERR         RTS

WAITBSYHI       LDY          #0                        ;set .5sec timeout
                STY          TIMOUT
                CLC
ALOOP           LDA          BUSY
                BPL          BSYHIRET                  ;done if taken
                DEY
                BNE          ALOOP
                DEC          TIMOUT
                BNE          ALOOP
                SEC                                    ;timeout
BSYHIRET        RTS

WAITBSYLO       LDY          #1
                LDA          LONGWAIT
                BEQ          @10
                LDY          #$10                      ;set up for 8 second wait max.
@10             STY          WAITTIME
                LDY          #0                        ;set .5sec timeout
                STY          TIMOUT
                CLC
BLOOP           LDA          BUSY
                BMI          BSYLORET                  ;done if taken
                DEY
                BNE          BLOOP
                DEC          TIMOUT
                BNE          BLOOP
                DEC          WAITTIME
                BNE          BLOOP
                SEC                                    ;timeout
BSYLORET        RTS

;----------------------------------------------------------------------
;
;  SND_CMDBYTES sends the command string to widget.
;  Enter with cmd=bsy=lo.  Error return if get parity error - Carry = 1
;
;----------------------------------------------------------------------

SND_CMDBYTES    JSR          SETUPWRITE                ;get in proper state
                LDA          Z8CMD                     ;send command string - cmd, blok+1
                STA          WR_PORT                   ;blok, retries, retry threshold
                LDA          #0             ;MSBLOCK
                STA          WR_PORT
                LDA          blok+1
                STA          WR_PORT
                LDA          blok
                STA          WR_PORT
                LDA          RTRYCNT
                STA          WR_PORT
                LDA          RTRYTHRESH
                STA          WR_PORT
                JSR          SETUPREAD                 ;finish writing last byte and
                JSR          CHKPARITY                 ;check for parity error
                RTS
;
;----------------------------------------------------------------------
;
;GETSTAT retrieves the status bytes from widget.  The one-byte
;result code is returned in Y.
;
;----------------------------------------------------------------------

GETSTAT         JSR          S1M
                LDA          RD_PORT
                STA          STATUS1
                LDA          RD_PORT
                STA          STATUS2
                LDA          RD_PORT
                PHA
                LDA          RD_PORT
                STA          STATUS4
                JSR          CHKPARITY
                PLA
                ORA          PARITY_ERR
                ORA          BAD_RESPONSE
                ORA          RESET_FLAG
                STA          STATUS3
                LDA          STATUS1
                RTS
;
;----------------------------------------------------------------------
;
;  CHKPARITY checks the parity error line and shifts it into Carry, so
;  Carry = 1 is a parity error on return to caller.
;
;----------------------------------------------------------------------

CHKPARITY       LDA          BUSY                      ;get parity error - on bit 6
                STA          CLR_PARITY                ;clear it for next transfer
                ASL          A
                ASL          A                         ;shift it into carry
                BCC          @10
                LDA          #1
                STA          PARITY_ERR
                ORA          STATUS3
                STA          STATUS3
@10             JMP          S2M                       ;exit via setting 2 MHz mode
;
;----------------------------------------------------------------------
;
;  SETUPWRITE sets CRW and DATRW lo on the Apple /// interface board
;  to prepare for a write operation to widget
;
;----------------------------------------------------------------------

SETUPWRITE      JSR          S1M
                LDA          SETWRT                    ;set crw lo
SET_WRITEDIR    LDA          RWLO                      ;set datarw lo
                RTS


SETUPREAD       JSR          S1M
                LDA          SETRD                     ;set crw hi
                LDA          RWHI                      ;set datarw hi
                RTS
;
;----------------------------------------------------------------------
;
;  BSYACK completes the cmd-bsy handshake by outputting the response
;  byte to widget, dropping cmd, and waiting for bsy to go lo.
;  Enter with the widget response ($55 or $AA) in A.
;
;----------------------------------------------------------------------

BSYACK          STA          WR_PORT                   ;store response byte
                JSR          SET_WRITEDIR              ;enable bus out to widget
                JSR          NTCMDLN1                  ;drop cmd
                JSR          WAITBSYLO
                JSR          SETUPREAD                 ;restore read state
                JMP          S2M                       ;exit via setting 2 MHz mode


NOTCMDLN        JSR          SETUPREAD
NTCMDLN1        LDA          NOTCMD
                RTS

SETCMDLN        JSR          SETUPREAD
                LDA          SETCMD
                RTS

RESET_PIPPIN    LDA          #4
                STA          RESET_FLAG
                ORA          STATUS3
                STA          STATUS3
                JSR          S1M
                LDA          RST
                LDY          #$25
@10             DEY
                BNE          @10
                LDA          CLRRST                    ;clear reset
				JMP          S2M                       ;exit via setting 2 MHz mode


bootcodeend         = *
               .res    $f7b4 - bootcodeend, 0

;----------------------------------------------------------------------
; Common between primary and secondary rom pages
;----------------------------------------------------------------------
                .org         $F7B4

;----------------------------------------------------------------------

PROFILE:        LDA          E_REG
                AND          #$FD     ;bit2->0
                STA          E_REG    ;-- from primary rom bank
                JSR          INIT
                BCS          PRERROR

                ldx          xmsg3                     ;booting profile
                ldy          #msg3l
                jsr          prnt_msg
		        JMP          bootp    ;go boot the profile

PRERROR:        LDA          E_REG
                ORA          #$02     ;bit2->1 
                STA          E_REG    ;-- pass back to primary rom bank
		        RTS

;----------------------------------------------------------------------
;
;  This routine sets up the Z_REG with the 256 byte page to dma to
;  will dma into the current bank selected
;  It also disables interupts for the duration (up to 256 usec)
;  of the transfer.
;
;----------------------------------------------------------------------

GO_DMA:         LDA          E_REG
				ORA          #$02     ;bit2->1 
				STA          E_REG    ;-- switch to primary rom bank

                LDA          E_REG
                ORA          #$80                      ;or in 1 MHz bit
                STA          E_REG
                LDA          addrdma                   ;Set Z_REG to DMA page address
                STA          Z_REG
                LDA          #$FF                      ;Make sure we transfer 256 bytes
                SEC
                JSR          PSUEDO_DMA                ;  do DMA transfer
                LDA          #MON_ZPAGE                ;Restore proper zero page.
                STA          Z_REG

                LDA          E_REG
				AND          #$FD     ;bit2->0
				STA          E_REG    ;-- from primary rom bank
                RTS

F7FE;

;****************************************************************
;*
;* padding to end of Secondary 4k ROM page.
;*
;****************************************************************

codeend    = *
           .res    $ffff - codeend + 1, $00         

