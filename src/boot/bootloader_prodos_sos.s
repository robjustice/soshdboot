;  modified prodos/sos boot loader for booting off prodos block mode
;  device. For use with modified A3 boot rom.
;  
;  The prodos part is unchanged. If the block 0 is loaded in by an
;  Apple II or Apple //e rom, boot prodos
;  
;  If block 0 is loaded by the supporting modified Apple /// rom
;  then it calls the block mode interface and loads block 1
;  into A000 and executes that.
;  
;  The block1/sos bootloader then uses the blockmode interface to
;  boot sos from the block mode card 
;
;  This version is updated for the Apple/// part to load both the
;  SOS.DRIVER and SOS.KERNEL files
;
;     SOS.DRIVER is loaded into Bank0 $3000-9fff & Bank1 $2000-9fff
;     SOS.KERNEL is loaded into the highest bank from $1E00 onwards

;     
;  This is an attempt to allow any driver to be used for booting
;  To allow this fit, the error messages have been slimmed down,
;   and file validation has been removed 
;  
;  By Robert Justice
;
;
;                sbtl         'universal boot loader - stage 2'
;
; prodos universal boot loader.  this is the second stage boot
;   for all apple manufactured apple ii disk drives.
; it is located at block zero (0) of a prodos or sos formatted
;   disk(ette).  if booted in apple /// native mode, the regular
;   sos boot will be attempted.
;
;
; macro to support the high bit ascii in ca65
                .macro     ascmsbon s
                .repeat    .strlen(s), i
                .byte      .strat(s,i) | $80
                .endrepeat
                .endmacro

                .segment     "DATA"
                .org         $800
dcmd            =            $42                       ;disk command (=1 for read)
unit            =            $43                       ;(16*slot)+(128*(drive-1))
buff            =            $44                       ;ram address
blok            =            $46                       ;disk address

dent            =            $48                       ;device call entry address.
idxl            =            $4a                       ;pointer to low page of index block
idxh            =            $4c                       ;pointer to high page of index block
idxp            =            $4e                       ;index byte pointer.
iobuff          =            $60

; the following are for disk ii only:

dbuf            =            $26
slotz           =            $2b
oddbits         =            $3c
sector          =            $3d
trktmp          =            $40
track           =            $41
prior           =            $50
trkn            =            $51
rtrycnt         =            $52
curtrk          =            $53
trkcnt          =            $54
;
q6l             =            $c08c
motoron         =            $c089
motoroff        =            $c088
phaseoff        =            $c080
nbuf1           =            $300
dnib            =            $2d6

; directory dependent stuff...
clrscrn         =            $fc58
scrn            =            $5ae
dostyp          =            $ff
sosid           =            $c00
entlen          =            sosid+$23
kernel          =            $2000

xboot:          .byte        $01                       ;(prodos boot id)
entry:          sec                                    ;(apple iii enters xboot 'ora $38')
                bcs          entry1                    ;branch if not apple iii native mode.
                jmp          goapl3                    ;go do apple iii boot!

entry1:         stx          unit                      ;save unit number.
                cmp          #$03                      ;for disk ii.
                php                                    ;save result, it may be irrelevent.
                txa                                    ;find out if disk ii.
                and          #$70                      ;strip drive # if any.
                lsr          a
                lsr          a                         ;get slot address.
                lsr          a
                lsr          a
                ora          #$c0
                sta          dent+1
                ldy          #$ff                      ;look at last byte.
                sty          dent
                plp                                    ;restore carry (if disk ii & sect 0&2 read carry set).
                iny                                    ;make y=0
                lda          (dent),y                  ;get device entry addr.
                bne          ndsk2                     ;branch if not disk ii (16 sector).
                bcs          isdsk2                    ;branch if it is disk ii, but block 0 read.
                lda          #3                        ;make rom read only sector 2
                sta          xboot                     ;to complete block 0
                inc          sector                    ;(was = 1)
                lda          dent+1                    ;do rts to re-enter rom.
                pha
                lda          #$5b
                pha
                rts                                    ;go read sector 2 into $900.

isdsk2:         sta          trktmp                    ;make sure previous track =0
                sta          dent                      ;and dent points at beginning of slot
                ldy          #$63                      ;move code from card to ram
mvboot:         lda          (dent),y
                sta          zzstart-$5e,y
                iny
                cpy          #$eb                      ;have we moved enough?
                bne          mvboot
                ldx          #6                        ;now modify code to handle errors.
modboot:        ldy          mods,x
                lda          chgs,x
                sta          zzstart,y
                lda          endcode,x
                sta          zzzend,x
                dex
                bpl          modboot
                lda          #>d2io                    ;reset device entry
                sta          dent+1                    ; to point at disk ii routines.
                lda          #<d2io                    ;get low addr (must be <$80)
ndsk2:          ldy          #0                        ;make sure y=0 again.
                cmp          #$f9
                bcs          bterr1                    ;branch if not bootable device.
                sta          dent                      ;save low adr of device call entry.
                sty          iobuff
                sty          idxl
                sty          idxh                      ;(y=0)
                sty          idxp
                sty          blok+1
                iny
                sty          dcmd                      ;set read command.
                iny
                sty          blok                      ; to read directory blocks
                lda          #$c                       ; 2-5 at $c00
                sta          iobuff+1
                sta          idxl+1
;
rddir:          jsr          goread                    ;call read block routine.
                bcs          bterr2                    ;give up on error.
                inc          iobuff+1
                inc          iobuff+1
                inc          blok
                lda          blok                      ;have all directory blocks been read?
                cmp          #6
                bcc          rddir                     ;loop if not.

                lda          sosid                     ;is it a prodos (sos) directory?
                ora          sosid+1
bterr1:         bne          booterr                   ;branch if not.
                lda          #4                        ;begin look-up with first entry past header.
                bne          nxdent1                   ;branch always
nxdent:         lda          idxl
nxdent1:        clc
                adc          entlen                    ;bump to next directory entry.
                tay                                    ;save in y for now.
                bcc          nxdent2                   ;branch if not a page cross.
                inc          idxl+1
                lda          idxl+1                    ;check for new block.
                lsr          a                         ;if even then new block.
                bcs          nxdent2
                cmp          #$a                       ;have all file names been compared?
                beq          nopro                     ;branch if no pro.kernel.
                ldy          #4                        ;else, begin at block beginning.
nxdent2:        sty          idxl                      ;note: this method treats garbage at
                lda          sysname                   ; the end of the dir block as an entry.
                and          #$f                       ;get target name length.
                tay
lookpro:        lda          (idxl),y                  ;look for matching name.
                cmp          sysname,y                 ;last to first method.
                bne          nxdent                    ;branch if no match.
                dey                                    ;else check all characters.
                bpl          lookpro                   ;including length/storage type.
                and          #$f0                      ;make sure storage type is a tree!
                cmp          #$20
                bne          booterr                   ;branch if not.
                ldy          #$10                      ;get file type & index block addr.
                lda          (idxl),y
                cmp          #dostyp                   ;is it a system file?
                bne          booterr
                iny
                lda          (idxl),y
                sta          blok
                iny
                lda          (idxl),y
                sta          blok+1
                lda          #0                        ;now set up to read kernel.
                sta          idxl
                ldy          #$1e                      ;read index block at $1e00 and
                sty          idxl+1                    ; kernel at $2000
                sty          iobuff+1
                iny
                sty          idxh+1
rdkernl:        jsr          goread                    ;read index block.
bterr2:         bcs          booterr
                inc          iobuff+1
                inc          iobuff+1
                ldy          idxp                      ;get index pointer
                inc          idxp                      ;bump for next time.
                lda          (idxl),y
                sta          blok
                lda          (idxh),y                  ;high disk addr.
                sta          blok+1
                ora          (idxl),y                  ;if both=0 then done.
                bne          rdkernl                   ;branch if more to read.

                jmp          kernel                    ;go execute kernel code.

nopro           =            *
booterr         =            *
                jmp          quitmes

sysname:        .byte        $26
                .byte        "PRODOS         "

goread:         lda          iobuff
                sta          buff
                lda          iobuff+1
                sta          buff+1
                jmp          (dent)

mods:           .byte        mod1,mod2,mod3,mod4
                .byte        mod5,mod6,mod7

chgs:           .byte        chg1,chg2,chg3,chg4
                .byte        chg5,chg6,chg7

endcode         =            *
                ldx          slotz
                clc
                rts
                jmp          seek

goapl3          =            *+$9800
                lda          #$9f                      ;make apple iii boot using block 1.
                pha                                    ;(the return address is $a000)
                lda          #$ff
                pha
                inc          blok                      ;read block 1.
                nop
                jmp          (dent)

;
quitmes:        jsr          clrscrn                   ;clear video.
                ldy          #meslen                   ;print message centered on screen.
prmess:         lda          errmess,y
                sta          scrn,y
                dey
                bpl          prmess
hang:           jmp          hang
;
meslen          =            28
errmess:        ascmsbon     "*** UNABLE TO LOAD PRODOS  ***"

setphase:       lda          curtrk                    ;get current track
clrphase:       and          #3                        ;mask for 1 of 4 phases
                rol          a                         ;double for phaseon/off index
                ora          slotz
                tax
                lda          phaseoff,x                ;turn on/off one phase
                lda          #$2c
;************************
;                        *
;   mswait subroutine    *
;                        *
;************************
mswait:         ldx          #$11
msw1:           dex                                    ;delay 86 usec.
                bne          msw1
                sbc          #$1                       ;done 'n' intervals?
                bne          mswait                    ;(a-reg counts)
                ldx          slotz                     ;restore x-reg
                rts


d2io:           lda          blok                      ;figure out track & sector.
                and          #7                        ;strip track for now.
                cmp          #4
                and          #3
                php
                asl          a
                plp
                rol          a                         ;now we have the first sector of block.
                sta          sector
                lda          blok+1                    ;get high block #
                lsr          a                         ;shift hi addr to carry.
                lda          blok                      ;now figure track #
                ror          a
                lsr          a
                lsr          a
                sta          track
                asl          a
                sta          trkn
                lda          buff+1
                sta          dbuf+1
                ldx          slotz
                lda          motoron,x
                jsr          rdsector                  ;go read sector.
                inc          dbuf+1                    ;bump address
                inc          sector
                inc          sector                    ;and sector #
                bcs          quitrd                    ;branch if error.
                jsr          rdsector
quitrd:         ldy          motoroff,x
erretrn:        rts                                    ;return error status in carry.

rdsector        =            *                         ;do seek then read sector.

seek:           lda          trktmp                    ;get track we're on.
                asl          a
                sta          curtrk
                lda          #$0
                sta          trkcnt                    ;halftrack count.
seek2:          lda          curtrk                    ;save curtrk for
                sta          prior                     ;delayed turnoff.
                sec
                sbc          trkn                      ;delta-tracks.
                beq          seekend                   ;br if curtrk=destination
                bcs          out                       ;(move out, not in)
                inc          curtrk                    ;incr current track (in).
                bcc          skin                      ;(always taken)
out:            dec          curtrk                    ;decr current track (out).
skin:           sec
step2:          jsr          setphase
                lda          prior
                clc                                    ;for phaseoff
                jsr          clrphase                  ;de-energize previous phase.
                bne          seek2                     ;(always taken)
seekend         =            *

rdsect1:        ldy          #$7f                      ;allow 127 mistakes.
                sty          rtrycnt
                php

tryread:        plp                                    ;fix stack.

rdhead:         sec                                    ;anticipate error.
                dec          rtrycnt                   ;if = 0 then give up!
                beq          erretrn                   ;branch if can't fine/read sector.
                clc                                    ;indicate reading header.
rddata:         php                                    ;carry set if reading sector.
rd0:            dey                                    ;every time y=0 decrement find count.
                beq          tryread

zzstart         =            *
; from zzstart to zzend code is moved from
; rom and modified to match this code...

rd1:            lda          q6l,x                     ;read a byet from the state machine.
                bpl          rd1                       ;loop until ready.

;                dsect
                .segment "DATA2"
                .org         zzstart+5                 ;equivalent to org *

rd1a:           eor          #$d5                      ;mark 1?
mod1            =            <(*-zzstart+1)
                bne          rd0                       ;branch if not.
chg1            =            <(rd0-*)
rd2:            lda          q6l,x
                bpl          rd2
                cmp          #$aa                      ;mark 2?
                bne          rd1a
                nop                                    ;waste a little time.
rd3:            lda          q6l,x
                bpl          rd3
                cmp          #$96                      ;header mark 3?
                beq          rdhd1                     ;branch if it is.
                plp                                    ;were we looking for data mark 3?
mod2            =            <(*-zzstart+1)
                bcc          rdhead                    ;branch if not.
chg2            =            <(rdhead-*)
                eor          #$ad                      ;data mark 3?
                beq          rddt1                     ;go read data field if true...
mod3            =            <(*-zzstart+1)
rdhd0:          bne          rdhead                    ;otherwise, start over.
chg3            =            <(rdhead-*)
rdhd1:          ldy          #3                        ;read in trk,sect,&volume #.
rdhd2:          sta          trktmp                    ;save last result in temp
rdhd3:          lda          q6l,x
                bpl          rdhd3
                rol          a
                sta          oddbits                   ;save odd bits (7,5,3,1)
rdhd4:          lda          q6l,x
                bpl          rdhd4
                and          oddbits                   ;combine even and odd to form value.
                dey
                bne          rdhd2                     ;read in next pair.
                plp
                cmp          sector                    ;last byte formed is sector#
mod4            =            <(*-zzstart+1)
                bne          rdhead                    ;branch if target sector not found.
chg4            =            <(rdhead-*)

                lda          trktmp                    ;previous result is track #
                cmp          track                     ;is desired track found?
mod5            =            <(*-zzstart+1)
                bne          goseek                    ;re-seek if mismatch.
chg5a           =            *
mod6            =            <(*-zzstart+1)
                bcs          rddata                    ;branch if proper track always.
chg6            =            <(rddata-*)

rddt1:          ldy          #$56                      ;read 2 bit groupings first.
rddt1a:         sty          oddbits
rddt2:          ldy          q6l,x
                bpl          rddt2
                eor          dnib,y                    ;denibblize using table left from boot rom.
                ldy          oddbits                   ;save in nbuf1
                dey
                sta          nbuf1,y
                bne          rddt1a                    ;loop until all 86 groups are read.

rddt3:          sty          oddbits                   ;now count up for 6-bit groups.
rddt4:          ldy          q6l,x
                bpl          rddt4
                eor          dnib,y
                ldy          oddbits                   ;save result to specified buffer.
                sta          (dbuf),y
                iny
                bne          rddt3                     ;loop for 256 bytes.
rdchk:          ldy          q6l,x                     ;now verify checksum...
                bpl          rdchk
                eor          dnib,y                    ;must be =  al...
mod7            =            <(*-zzstart+1)
                bne          rdhd0                     ;branch if error.
chg7            =            <(rdhd0-*)
                ldy          #0                        ;make y=0
nxttwo:         ldx          #$56                      ;now combine 2-bit group with 6 bit group
twobit:         dex                                    ;all done with this group?
                bmi          nxttwo                    ;branch if so.
                lda          (dbuf),y
                lsr          nbuf1,x
                rol          a
                lsr          nbuf1,x
                rol          a
                sta          (dbuf),y
                iny
                bne          twobit

zzzend          =            *

                ldx          slotz
                clc                                    ;indicate good read.
                rts
chg5            =            <(*-chg5a)
goseek:         jmp          seek

;                dend
                .segment     "DATA"
                .res         $a00-zzstart-5,0


; the following is the apple /// sos boot loader.
;                msb          off
;                sbtl         "sos system boot 2blk.2"
;*******************************************************************
;*
;* sos system boot
;*
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
;*   2.  blok routine is in rom with the entry pt at $f479.
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
kybdstrb        =            $c010


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
;*****************************************************************
;*
;* sos system boot - entry point
;*
;*****************************************************************

bootinfo        =            *
asmbase         =            *                         ;assembly base address
runbase         =            $a000                     ;execution base address
                jmp          boot+runbase-asmbase
                .byte        "SOSBOOT 1.5A"            ; sos boot identification "stamp"

;*******************************************************************
;*
;* local data storage
;*
;*******************************************************************

namlen:         .byte        10
name_k:         .byte        "SOS.KERNEL"
name_d:         .byte        "SOS.DRIVER"
name_offset:    .byte        10                        ;initially set to SOS.DRIVER

;
; messages
;
msg             =            *                         ; message table

msg0:           .byte        "I/O ERR"
msg0l           =            *-msg0
xmsg0:          .word        *-msg-1

msg1:           .byte        "FILE NOT FOUND"
msg1l           =            *-msg1
xmsg1:          .word        *-msg-1

msg2:           .byte        "INVALID FILE"
msg2l           =            *-msg2
xmsg2:          .word        *-msg-1

;*****************************************************************
;*
;* sos system boot - main code body
;*
;*****************************************************************
;
; turn off interrupts & decimal mode
;  Assume with new boot rom that the device entry point and unit are set
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
                bit          kybdstrb                  ; turns off kybd
                lda          #$40                      ; "rti" instruction
                sta          $ffca                     ; prevents reboot w/keyboard nmi
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
                sta          buff
                lda          #$a2
                sta          buff+1

rd_dir:         jsr          read_blk+runbase-asmbase  ; rest of boot (block 1)
                inc          buff+1
                inc          buff+1
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
                lda          #$1e                      ; read SOS.KERNEL into highest bank, $1e00 on
                jsr          rddatablks

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

;********************************************************************
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

                ldx          xmsg2+runbase-asmbase     ;err,invalid file
                ldy          #msg2l
                jmp          prnt_msg+runbase-asmbase

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

                ldx          xmsg1+runbase-asmbase     ;err, can't find file
                ldy          #msg1l
                jmp          prnt_msg+runbase-asmbase

match:         rts


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
                sta          dcmd                      ; read
                jsr          blkio
                bcs          rd_err
                rts                                    ; normal exit

rd_err:         ldx          xmsg0+runbase-asmbase     ;err, i/o error
                ldy          #msg0l
                jmp          prnt_msg+runbase-asmbase
                
blkio:          jmp          (dent)                    ;device block entry   

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

prnt010:        lda          msg+runbase-asmbase,x
                sta          msgline-1,y
                dex
                dey
                dec          temp
                bne          prnt010

                lda          $c040                     ; sound bell
                jmp          *+runbase-asmbase         ; hang until reboot (ctrl/reset)

;****************************************************************
;*
;* padding to end of two blocks. modify if code length increases
;* beyond one block.
;*
;****************************************************************

pad             =            *-asmbase
                .res         512-pad,0                 ; pad to end of block
zzend           =            *
