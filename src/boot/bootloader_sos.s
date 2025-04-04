;  Modified sos boot loader for booting off prodos block mode
;  device. 
;  This version fits in one block for use as a 'boot' floppy
;  allowing the soshdboot to work without the rom
;
;  - Add test for any key pressed:
;      not pressed = boot unit0
;      pressed     = boot unit1
;
;  if TDM is defined, builds desktopmanager version, this loads sos
;  one bank lower than the highest available
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
k_flags         =            k_file+8+3                ; loc of k_flags in sos.kernel

; highest available bank for 512k ram board
.ifdef TDM
highbank        =            13                        ;one lower for desktop manager bootloader 
.else
highbank        =            14
.endif

;*****************************************************************
;*
;* sos system boot - entry point
;*
;*****************************************************************

bootinfo        =            *
asmbase         =            *                         ;assembly base address
runbase         =            $a000                     ;execution base address


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
                lda          #highbank           ; load highest bank for 512k
                sta          b_reg
                sta          $2000
                lda          #highbank-8         ; highest bank for 256k
                sta          b_reg
                sta          $2000               ; will overwrite bank e value if not 512k
                lda          #highbank
                sta          b_reg
                cmp          $2000
                beq          boot006             ; yes, its 512k
                lsr a
                sta          b_reg
                
                ldx          #0
				stx          ptr           ;save some bytes
				stx          blok+1        ;save some bytes
                stx          buff          ;save some bytes
boot005:        dec          b_reg
                stx          $2000
                lda          $2000
                bne          boot005
;
; find card slot
;
boot006:        lda          #scanstart          ; load starting scan slot (cs)
                sta          ptr+1
                ;lda          #$00                
                ;sta          ptr            ;do above to shave some bytes

checknext:      ldy          #$05                ; we check all 3 sig bytes, starting from last
chk2:           lda          (ptr),y
                cmp          signature-1,y
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
                ;lda          #0
                ;sta          blok+1       ;do above to shave some bytes
                ;sta          buff         ;do above to shave some bytes
                lda          #$a2
                sta          buff+1

rd_dir:         jsr          read_blk            ; rest of boot (block 1)
                inc          buff+1
                inc          buff+1
                inc          blok
				lda          blok                  ; have all directory blocks been read?
				cmp          #6
				bcc          rd_dir

;
; read in SOS.DRIVER file
;
                lda          b_reg                     ; save high bank
                pha
                lda          #0                        ; load SOS.DRIVER into bank0
                sta          b_reg


                jsr          searchdir                 ; search directory for file 'SOS.DRIVER'
                                                       ;  and read index block
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
                                                       ;  and read index block
                lda          #$1e                      ; read SOS.KERNEL into highest bank, $1e00 on
                jsr          rddatablks

;
;  have a peak in the first block of the SOS.INTERP file to get the load address
;  and update into the sos loader
;
                lda          #20                       ; reset offset to point to SOS.INTERP name
                sta          name_offset
                jsr          searchdir                 ; search directory for file 'SOS.INTERP'
                                                       ; and read index block
				lda          #0
				sta          k_xblk+1                  ;we only want the first block, set the 2nd block index to 0
				sta          k_xblk+256+1
                lda          #$16                      ; read SOS.INTERP into $1600 
                jsr          rddatablks
                lda          $1600+8                   ;check option header length, only checking the low byte for now
                tax                                    ; either zero, or the length if its there
                lda          $1600+$0b,x               ;grab the load address
				sta          k_flags                   ;and use this to set the driver top (start) page
                                                       ; in the sos loader
;
; build sos loader entry point address
;
entry_a3:       clc                                    ; sosldr:=k.hdr.cnt+(k.hdr.cnt)
                lda          #<(k_hdr_cnt+6+runbase-asmbase)
                adc          k_hdr_cnt
                sta          sosldr
                lda          #>(k_hdr_cnt+6+runbase-asmbase)
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
;* file "sos.driver":
;*
;* data block 0 is at bank0:$3000..$31ff
;* data block 1 is at bank0:$3200..$33ff
;*  " "
;* data block n "
;*
;*******************************************************************

;*******************************************************************
;*
;* search directory for file
;*
;* input: 'begin' ptr points to directory entry
;*        name_offset set 0 or 10 for SOS.KERNEL or SOS.DRIVER
;*        
;*******************************************************************

searchdir:      lda          #<(entry0+runbase-asmbase)   ;get lo byte of address
                sta          begin
                lda          #>(entry0+runbase-asmbase)
                sta          begin+1
				lda          #5
				sta          blk_ctr

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
                cmp          namlen+runbase-asmbase
                bne          srch040                   ; no match

                tay
                clc
                adc          name_offset               ; offset based on file name to compare
                tax
srch030:        lda          (begin),y                 ; do chars match?
                cmp          name_k-1+runbase-asmbase,x
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

match:
; fall through to read index block

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
                lda          #<(k_xblk+runbase-asmbase) ;get lo byte of address
                sta          buff
                lda          #>(k_xblk+runbase-asmbase) ;get hi byte of address
                sta          buff+1
                jsr          read_blk+runbase-asmbase  ; index block
                rts

;*******************************************************************
;*
;* read in file data blocks
;*
;* input: a = buffer high byte (assumes buffer low byte always = 0)
;*        file index block loaded into $0c00
;*
;*******************************************************************

rddatablks:     sta          buff+1
                lda          #0
                sta          buff
                sta          temp

data010:        ldx          temp                      ; get block address of next data block
                lda          k_xblk,x
                sta          blok
                lda          k_xblk+$100,x
                sta          blok+1

                lda          blok                      ; is next block address = 0 ?
                ora          blok+1 
                beq          rd_done                   ; yes, stop reading

data020:        jsr          read_blk+runbase-asmbase  ; read data block
                inc          temp                      ; bump for next time
                inc          buff+1
                inc          buff+1

                lda          buff+1                    ; if its loading the driver file wrap
                cmp          #$a0                      ; to bank1 if we are past the end of bank0
                bne          data010
                lda          #$20
                sta          buff+1
                inc          b_reg
                bne          data010                   ; bra always

rd_done:        rts

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

prnt_msg:       sta          a:msg0+msg0l                ; store the error letter
                ldy          #msg0l

prnt010:        lda          msg0,y                    ; copy the message to screen
                sta          msgline-1+5,y
                dey
                bpl          prnt010

                lda          $c040                     ; sound bell
                jmp          *                         ; hang until reboot (ctrl/reset)

;*******************************************************************
;*
;* local data storage
;*
;*******************************************************************

namlen:         .byte        10
name_k:         .byte        "SOS.KERNEL"
name_d:         .byte        "SOS.DRIVER"
name_i:         .byte        "SOS.INTERP"
name_offset:    .byte        10                        ;initially set to SOS.DRIVER

;
; messages - trimmed to save space
;
msg0:           .byte        " ERROR  "
msg0l           =            *-msg0-1

signature:      .byte        $20, $FF, $00    ; Disk card signature for disk controller
                .byte        $FF, $03


;****************************************************************
;*
;* padding to end of two blocks. modify if code length increases
;* beyond one block.
;*
;****************************************************************

pad             =            *-asmbase
                .res         512-pad,0                 ;pad to end of block
zzend           =            *
