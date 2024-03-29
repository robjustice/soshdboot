;  Modified sos boot loader for booting off prodos block mode
;  device. 
;  This version fits in one block for use as a 'boot' floppy
;  allowing the soshdboot to work without the rom
;
;  - Add test for any key pressed:
;      not pressed = boot unit0
;      pressed     = boot unit1
;
;  By Robert Justice
;  
;
;                msb          off
;                sbtl         "soshdboot 1blk.1"
;*******************************************************************
;*
;* sos system boot
;*
;* the code resides on blocks 0 and 1 of every sos diskette.
;* its job is to locate the file named 'sos.kernel' on the
;* boot diskette (drive 1), load the entire file into memory
;* and then transfer control to the second stage boot,
;* (sos loader).
;*
;* this first stage boot is designed to have minimal knowledge
;* of both the rom code and the operating system including
;* its associated drivers.
;*
;* assumptions:
;*
;*   1.  screen is cleared and 40 column b&w mode is selected.
;*
;*   2.  blockio routine is in rom with the entry pt at $f479.
;*
;*   3.  hardware:  see =  ates
;*
;*   4.  sos directory format
;*
;*   5.  file 'sos.kernel' format
;*
;* potential problems:
;*
;*   1.  this code disregards the address/count information
;*       affixed to the front of the sos loader module.
;*
;*   2.  if code grows beyond current size, the padding at end of the code
;*       needs to be modified.  (the code currently resides in less than a
;*       single block; thus two padding statements necessary to have total
;*       be two blocks on diskette.)
;*
;*
;******************************************************************

                .segment     "DATA"
                .org         $a000


;*
;* hardware addresses
;*
e_reg           =            $ffdf
b_reg           =            $ffef
keyd            =            $c000
keybd           =            $c008
kybdstrb        =            $c010


;*
;* zero page storage (z reg = $03)
;*

; prodos card block driver addreses
dcmd            =            $42                       ;disk command (=1 for read)
unit            =            $43                       ;(16*slot)+(128*(drive-1))
buff            =            $44                       ;prodos block dev buffer pointer
blok            =            $46
dent            =            $48                       ;device call entry address.

scanstart       =            $C4                       ; Slot number to start scan from

zpage           =            $e0
begin           =            zpage+2                   ; & 3
end             =            zpage+4                   ; & 5
blk_ctr         =            zpage+6
temp            =            zpage+7

sosldr          =            zpage+8                   ; & 9
ptr             =            zpage+10                  ; & 11


;* equates
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
;*****************************************************************
;*
;* sos system boot - entry point
;*
;*****************************************************************

bootinfo        =            *
asmbase         =            *                         ;assembly base address
runbase         =            $a000                     ;execution base address
                jmp          boot
                .byte        "SOSHDBOOT"        ; sos boot identification "stamp"

;*******************************************************************
;*
;* local data storage
;*
;*******************************************************************

namlen:         .byte        10
name:           .byte        "SOS.KERNEL"
name2:          .byte        "SOS KRNL"
name2_len       =            *-name2
;
; messages - trimmed to save space
;
msg0:           .byte        "BOOT ERROR  "
msg0l           =            *-msg0-1

signature:      .byte        $FF, $20, $FF, $00    ; Disk card signature for disk controller
                .byte        $FF, $03


;*****************************************************************
;*
;* sos system boot - main code body
;*
;*****************************************************************
;
; turn off interrupts & decimal mode
;
boot:           sei
                cld

;
; set up environment register and init stack
;
                lda          #$f7                      ;1mhz enbl
;                                                       i/o enbl
;                                                       primary stack enbl
;                                                       reset/nmi enbl
;                                                       write prot. dsbl
;                                                       primary stack enbl
;                                                       rom1 enbl
;                                                       rom enbl
                sta          e_reg
                ldx          #$fb
                txs
                ;bit          kybdstrb                  ; turns off kybd
                ;lda          #$40                      ; "rti" instruction
                ;sta          $ffca                     ; prevents reboot w/keyboard nmi
;
; find highest memory bank in system and set bank reg to it
; - max memsize = 512k. (support OnThree 512k memory card)
;
                lda          #$0e                ; load highest bank for 512k
                sta          b_reg
                sta          $2000
                lda          #$06                ; highest bank for 256k
                sta          b_reg
                sta          $2000               ; will overwrite bank e value if not 512k
                lda          #$0e
                sta          b_reg
                cmp          $2000
                beq          boot006             ; yes, its 512k
                lsr a
                sta          b_reg
                
                ldx          #0
boot005:        dec          b_reg
                stx          $2000
                lda          $2000
                bne          boot005
;
; find card slot
;
boot006:        lda          #scanstart          ; load starting scan slot (cs)
                sta          ptr+1
                lda          #$00                
                sta          ptr

checknext:      ldy          #$05                ; we check all 3 sig bytes, starting from last
chk2:           lda          (ptr),y
                cmp          signature,y
                bne          nomatch             ; no device if bytes don't match
                dey
                dey
                bpl          chk2
            
                ldy          #$ff                ; $cxff - check last byte
                lda          (ptr),y
                beq          nomatch             ; if $00, is a disk ii 16 sector device, error
                cmp          #$ff
                bne          sigmatch            ; if its not $ff (disk ii 13 sector device)
                                          ; then we found an intelligent disk controller :-)

nomatch:        dec          ptr+1               ; else try next slot
                lda          ptr+1
                and          #$07
                bne          checknext           ; check next slot

                lda          #'C'                ; else, error, card not found
                jmp          prnt_msg

sigmatch:       sta          dent                ; Set card driver entry low byte
                lda          ptr+1
                sta          dent+1              ; Set card driver entry high byte
                asl
                asl
                asl
                asl
                sta          unit

                tax
                bit           keyd               ;test for keyboard data
                bpl           unit0              ;none, boot unit0

                txa
                ora          #$80                ;yes, boot unit1
                sta          unit

unit0:          lda          #1
                sta          blok
                lda          #0
                sta          blok+1
                sta          buff
                lda          #$a2
                sta          buff+1

                jsr          read_blk  ; rest of boot (block 1)

                inc          blok                    ; first root directory block (block 2)
                lda          #0
                sta          blk_ctr
rd_dir:         inc          buff+1
                inc          buff+1
                inc          blk_ctr

                jsr          read_blk  ; root directory

                ldy          #nextdblk                 ; if nextdir field = 0 then done
                lda          (buff),y
                sta          blok
                iny
                lda          (buff),y
                sta          blok+1
                bne          rd_dir
                lda          blok
                bne          rd_dir



;block 1 - part

;
; search directory for file 'sos.kernel'
;
                lda          #<entry0                   ;get lo byte of address
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
                cmp          namlen
                bne          srch040                   ; no match

                tay
srch030:        lda          (begin),y                 ; do chars match?
                cmp          name-1              ,y
                bne          srch040                   ; no match
                dey
                bne          srch030

                ldy          #storage                  ;test storage type
                lda          (begin),y                 ;must be sapling
                and          #$f0
                cmp          #sapling
                beq          match
                cmp          #rootdir                  ;skip if stg type=rootdir
                beq          srch040

                lda          #'D'                ; else, error, dir error?
                jmp          prnt_msg

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

                lda          #'S'                ; else, error, sos.kernel not found
                jmp          prnt_msg

;
; file entry 'sos.kernel' found
; read in its index block ($c00) and first data block ($1e00)
;
match:          ldy          #xblk
                lda          (begin),y
                sta          blok
                iny
                lda          (begin),y
                sta          blok+1
                lda          #<k_xblk                  ;get lo byte of address
                sta          buff
                lda          #>k_xblk                  ;get hi byte of address
                sta          buff+1
                jsr          read_blk                  ; index block

                lda          #<k_file                  ;get lo byte of address
                sta          buff
                lda          #>k_file
                sta          buff+1
                lda          k_xblk
                sta          blok
                lda          k_xblk+$100
                sta          blok+1
                jsr          read_blk                  ; first data block
;
; check the label, should be 'sos krnl'
;
                ldx          #name2_len-1
chk010:         lda          k_label,x
                cmp          name2,x
                beq          chk020
                lda          #'B'                ; else, error, bad sos.kernel
                jmp          prnt_msg
chk020:         dex
                bpl          chk010
;
; read in the rest of the data blocks in file "sos.kernel"
;
                lda          #0
                sta          temp

data010:        inc          temp
                inc          buff+1
                inc          buff+1

                ldx          temp                      ; get block address of next data block
                lda          k_xblk,x
                sta          blok
                lda          k_xblk+$100,x
                sta          blok+1

                lda          blok                      ; is next block address = 0 ?
                bne          data020
                lda          blok+1
                beq          entry_a3                  ; yes, stop reading

data020:        jsr          read_blk                  ; read data block
                jmp          data010                   ;  and repeat
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

;*********************************************************************
;*
;* finished !!
;*
;* state of registers:
;*
;* b reg = highest 32k bank
;* e reg = $77
;* z reg = $03
;*
;* file "sos.kernel":
;*
;* index block is at $c00...$fff
;* data block 0 is at $2200..$23ff
;* data block 1 is at $2400..$25ff
;*  " "
;* data block n "
;*
;*******************************************************************

;*******************************************************************
;*
;* read block routine
;*
;* input: blok & buff
;*
;*******************************************************************

read_blk        =            *
                lda          #1
                sta          dcmd                      ;read
                jsr          blockio
                bcs          rd_err
                rts                                    ; normal exit

rd_err:         lda          #'R'                      ; read error
                jmp          prnt_msg
                
blockio:        jmp          (dent)                    ;device block entry   

;*******************************************************************
;*
;* print error message
;*
;* input: error letter (a)
;*
;*******************************************************************
msgline         =            $5a8                      ; prnt.msg routine

prnt_msg:       sta          msg0+msg0l                ; store the error letter
                ldy          #msg0l

prnt010:        lda          msg0,y                    ; copy the message to screen
                sta          msgline-1+5,y
                dey
                bpl          prnt010

                lda          $c040                     ; sound bell
                jmp          *                         ; hang until reboot (ctrl/reset)

;****************************************************************
;*
;* padding to end of two blocks. modify if code length increases
;* beyond one block.
;*
;****************************************************************

pad             =            *-asmbase
                .res         512-pad,0                 ;pad to end of block
zzend           =            *
