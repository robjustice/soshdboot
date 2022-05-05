; This version of the Problock3 driver is modifed for including in the SOS Kernel
; This replaces the normal DISK3 driver, hence '.ORG ORGDISK3'
; It has 4 x DIBs, 2 are just place holder to match SOS and are set inactive
; The number of disk drives set in the driver file is still read by the SOS loader
; 
; As the driver is now always in non bank switched memory, we can remove the part
;  that was copied to 18f0 and run there to work around being bank switched out
;  
;
; SOS Block driver for Prodos compatible block mode cards
; code based on the focus3 driver source code
; Thanks David Schmidt!!
; 
; If the Driver Slot is configured with ff, then the driver scans all slots
; from 4 to 1 looking for the prodos card signature and uses the first
; one it finds.
; If the Driver Slot is configured with 1 - 4, then it uses that configured slot
; The default setting is to autoscan
;
; before calling the card firmware the driver
; - saves the screenholes for the current slot and slot0
; - saves zeropage locations 20-4f (this seemed to remove any inconsistencies)
; - disable interrupts
; - tries to detect if there is any font loading underway and waits if there is
;   so as not to corrupt the font
;
; - this version converts the extended indirect buffer pointer to a bank and 
;   offset within the bank. It moves a section of code to a low mem address
;   that never gets bank switched, and then calls the card firmware from there.
;   on return from the firmware, it then restores the driver bank and returns
;   to the driver code
;   This is modeled off the Profile driver
;   This allows cards that use absolute indexed addressing to work (CFFA3000 & Liron)
;
; By Robert Justice
;
;            .TITLE "Apple /// Prodos Block Mode Driver"

            .setcpu "6502"
Debug = 0

            .SEGMENT   "CODE"
            .INCLUDE   "SOSORG"            
            .ORG       ORGDISK3
ZZORG:            
            .EXPORT    DIB1                                  ;DIB1
            .EXPORT    DIB2                                  ;DIB2
            .EXPORT    DIB3                                  ;DIB3
            .EXPORT    DIB4                                  ;DIB4

            .EXPORT    TmpScrH                               ;Used to copy in card init'd screen holes
            
            .IMPORT    SYSERR
            .IMPORT    SELC800
            .IMPORT    ALLOCSIR
            .IMPORT    DEALCSIR

DriverVersion   = $005B      ; Version number
DriverMfgr      = $524A      ; Driver Manufacturer - RJ
DriverType      = $E1        ; No formatter present for the time being
DriverSubtype   = $02        ;
ScanStart       = $04        ; Slot number to start scan from
AutoScan        = $FF        ; Auto scan slots

;
; SOS Equates
;
ExtPG       = $1401          ; Driver extended bank address offset
EReg        = $FFDF          ; Environment register
E_IFR       = $FFED          ; VIA E Interrupt Flag Register
E_IER       = $FFEE          ; VIA E Interrupt Enable Register
Bank_Reg    = $FFEF          ; Bank register
CWrtOff     = $C0DA          ; Character loading off
CWrtOn      = $C0DB          ; Character loading on
E1908       = $1908          ;GLOBAL FLAG FOR MOUSE DRIVER
                             ;TO SAY WE CANNOT BE INTERRUPTED


;
; SOS Zero page parameters
;
ReqCode     = $C0            ; Request code
SOS_Unit    = $C1            ; Unit number
SosBuf      = $C2            ; SOS buffer pointer
ReqCnt      = $C4            ; Requested byte count
CtlStat     = $C2            ; Control/status code
CSList      = $C3            ; Control/status list pointer
SosBlk      = $C6            ; Starting block number
QtyRead     = $C8            ; Bytes read return by D_READ


;
; Parameter block specific to current SOS request
;
Num_Blks    = $E2            ; Number of blocks requested (we'll never ever have > 128 blocks)
Count       = $E3            ; 2 bytes lb,hb

;
; Extra zero page variables
;
ScreenBase  = $E5            ; 2 bytes lb,hb for save/restore screenholes
Pointer     = $E7            ; 2 byte pointer for signature check
CurrBank    = $E9            ; current bank (needs to be out of bank switching memory)

;
; SOS Error Codes
;
XDNFERR     = $10            ; Device not found
XBADDNUM    = $11            ; Invalid device number
XREQCODE    = $20            ; Invalid request code
XCTLCODE    = $21            ; Invalid control/status code
XCTLPARAM   = $22            ; Invalid control/status parameter
XNORESRC    = $25            ; Resource not available
XBADOP      = $26            ; Invalid operation
XIOERROR    = $27            ; I/O error
XNODRIVE    = $28            ; Drive not connected
XBYTECNT    = $2C            ; Byte count not a multiple of 512
XBLKNUM     = $2D            ; Block number to large
XDISKSW     = $2E            ; Disk switched
XDCMDERR    = $31            ; device command ABORTED error occurred
XCKDEVER    = $32            ; Check device readiness routine failed
XNORESET    = $33            ; Device reset failed
XNODEVIC    = $38            ; Device not connected

;
; Switch Macro
;
.MACRO        SWITCH index,bounds,adrs_table,noexec    ; See SOS Reference
.IFNBLANK index                           ; If PARM1 is present,
            lda        index              ; load A with switch index
.ENDIF
.IFNBLANK   bounds                        ; If PARM2 is present,
            cmp        #bounds+1          ; perform bounds checking
            bcs        @110               ; on switch index
.ENDIF
            asl        A                  ; Multiply by 2 for table index
            tay
            lda        adrs_table+1,y     ; Get switch address from table
            pha                           ; and push onto Stack
            lda        adrs_table,y
            pha
.IFBLANK    noexec
            rts                           ; Exit to code
.ENDIF
@110:
.ENDMACRO


;------------------------------------
;
; Device identification Block (DIB) - Volume #1
;
;------------------------------------

DIB1:       .word   DIB2             ; Link pointer
            .word   Entry            ; Entry pointer
            .byte   $08              ; Name length byte
            .byte   ".PROFILE       "; Device name
            .byte   $80              ; Active, no page alignment
DIB1_Slot:  .byte   AutoScan         ; Slot number
            .byte   $00              ; Unit number
            .byte   DriverType       ; Type
            .byte   DriverSubtype    ; Subtype
            .byte   $00              ; Filler
DIB1_Blks:  .word   $0000            ; # Blocks in device
            .word   DriverMfgr       ; Manufacturer
            .word   DriverVersion    ; Driver version
            .word   $0000            ; DCB length followed by DCB
;
; Device identification Block (DIB) - Volume #2
; Page alignment begins here
;
DIB2:       .word   DIB3             ; Link pointer
            .word   Entry            ; Entry pointer
            .byte   $04              ; Name length byte
            .byte   ".PB2           "; Device name
            .byte   $80              ; Active
DIB2_Slot:  .byte   AutoScan         ; Slot number
            .byte   $01              ; Unit number
            .byte   DriverType       ; Type
            .byte   DriverSubtype    ; Subtype
            .byte   $00              ; Filler
DIB2_Blks:  .word   $0000            ; # Blocks in device
            .word   DriverMfgr       ; Driver manufacturer
            .word   DriverVersion    ; Driver version
            .word   $0000            ; DCB length followed by DCB

;
; dummy dib3 & dib4, maybe for future use
; set them inactive so they are not used
;
; Device identification Block (DIB) - Volume #3
;
DIB3:       .word   DIB4             ; Link pointer
            .word   Entry            ; Entry pointer
            .byte   $04              ; Name length byte
            .byte   ".PB3           "; Device name
            .byte   $00              ; InActive
DIB3_Slot:  .byte   AutoScan         ; Slot number
            .byte   $02              ; Unit number
            .byte   DriverType       ; Type
            .byte   DriverSubtype    ; Subtype
            .byte   $00              ; Filler
DIB3_Blks:  .word   $0000            ; # Blocks in device
            .word   DriverMfgr       ; Driver manufacturer
            .word   DriverVersion    ; Driver version
            .word   $0000            ; DCB length followed by DCB
;
; Device identification Block (DIB) - Volume #4
;
DIB4:       .word   0000             ; Link pointer
            .word   Entry            ; Entry pointer
            .byte   $04              ; Name length byte
            .byte   ".PB4           "; Device name
            .byte   $00              ; InActive
DIB4_Slot:  .byte   AutoScan         ; Slot number
            .byte   $03              ; Unit number
            .byte   DriverType       ; Type
            .byte   DriverSubtype    ; Subtype
            .byte   $00              ; Filler
DIB4_Blks:  .word   $0000            ; # Blocks in device
            .word   DriverMfgr       ; Driver manufacturer
            .word   DriverVersion    ; Driver version
            .word   $0000            ; DCB length followed by DCB

;------------------------------------
;
; Local storage locations
;
;------------------------------------

LastOP:     .res    $02, $FF            ; Last operation for D_REPEAT calls
SIR_Addr:   .word   SIR_Tbl
SIR_Tbl:    .res    $05, $00
SIR_Len     =       *-SIR_Tbl
MaxUnits:   .byte   $02                    ; The maximum number of units

DCB_Idx:    .byte   $00                    ; DCB 0's blocks
            .byte   DIB2_Blks-DIB1_Blks    ; DCB 1's blocks
			.byte	DIB3_Blks-DIB1_Blks	   ; DCB 2's blocks
			.byte	DIB4_Blks-DIB1_Blks	   ; DCB 3's blocks

CardIsOK:   .byte   $00                    ; Have we found an intelligent disk controller yet?

TmpScrH:    .res    $10, $00            ; Storage for screenholes, slot0 & current slot
TmpZero:    .res    $30, $00            ; Storage to save zero page locations
                                        ; we set the prodos call data in here and then swap
                                        ; it in before we call the interface
ProCommand  = TmpZero + $22             ; Command        - $42
ProUnit     = TmpZero + $23             ; Unit number    - $43
ProBuf      = TmpZero + $24             ; buffer pointer - $44
ProBlock    = TmpZero + $26             ; Block number   - $46

ProBufOff   = $44                       ; buffer pointer - $44

Signature:  .byte $FF, $20, $FF, $00    ; Disk card signature for disk controller
            .byte $FF, $03

ProBank:    .byte   $00                 ; destination bank for block data
Offset:     .byte   $00

xbytetmp:   .byte   $00                 ; save xbyte


;------------------------------------
;
; Driver request handlers
;
;------------------------------------

Entry:
            jsr     Dispatch            ; Call the dispatcher
            ldx     SOS_Unit            ; Get drive number for this unit
            lda     ReqCode             ; Keep request around for D_REPEAT
            sta     LastOP,x            ; Keep track of last operation
            rts

;
; The Dispatcher.  Note that if we came in on a D_INIT call,
; we do a branch to Dispatch normally.  
; Dispatch is called as a subroutine!
;
DoTable:    .word    DRead-1            ; 0 Read request
            .word    DWrite-1           ; 1 Write request
            .word    DStatus-1          ; 2 Status request
            .word    DControl-1         ; 3 Control request
            .word    BadReq-1           ; 4 Unused
            .word    BadReq-1           ; 5 Unused
            .word    BadOp-1            ; 6 Open - valid for character devices
            .word    BadOp-1            ; 7 Close - valid for character devices
            .word    DInit-1            ; 8 Init request
            .word    DRepeat-1          ; 9 Repeat last read or write request
Dispatch:    SWITCH  ReqCode,9,DoTable  ; Serve the request

;
; Dispatch errors
;
BadReq:     lda     #XREQCODE           ; Bad request code!
            jsr     SYSERR              ; Return to SOS with error in A

BadOp:      lda     #XBADOP             ; Invalid operation!
            jsr     SYSERR              ; Return to SOS with error in A

;
; D_REPEAT - repeat the last D_READ or D_WRITE call
;
DRepeat:    ldx     SOS_Unit
            lda     LastOP,x            ; Recall the last thing we did
            cmp     #$02                ; Looking for operation < 2
            bcs     BadOp               ; Can only repeat a read or write
            sta     ReqCode
            jmp     Dispatch

NoDevice:   lda     #XDNFERR            ; Device not found
            jsr     SYSERR              ; Return to SOS with error in A

;
; D_INIT call processing - called once each for all volumes.
; on first entry we search the slots from 4 to 1 for a block devices 
; and use the first one we find
;
DInit:      
            lda     CardIsOK            ; Check if we have checked for a card already
            bne     FoundCard           ; Yes, skip signature check

CheckSig:
            lda     DIB1_Slot           ; Check configured slot for autoscan
            bpl     FixedSlot           ; No, use configured DIB1 slot
            lda     #ScanStart          ; else load starting scan slot
FixedSlot:  ora     #$C0                ; Form a $Cs00 address, where s = slot #
            sta     Pointer+1
            lda     #$00                
            sta     Pointer
            sta     Pointer+ExtPG       ; zero out the xbyte

CheckNext:  ldy     #$05                ; We check all 3 sig bytes, starting from last
@1:         lda     (Pointer),y
            cmp     Signature,Y
            bne     NoMatch             ; No device if bytes don't match
            dey
            dey
            bpl     @1
            
            ldy     #$ff                ; $CxFF - check last byte
            lda     (Pointer),y
            cmp     #$00
            beq     NoMatch             ; if $00, is a Disk II 16 sector device, error
            cmp     #$ff
            bne     Match               ; if its not $ff (Disk II 13 sector device)
                                        ; Then we found an intelligent disk controller :-)
            
NoMatch:    lda     DIB1_Slot           ; No match, check if we are autoscanning
            cmp     #$ff
            bne     NoDevice            ; No we are not, error
            
            dec     Pointer+1           ; Else try next slot
            lda     Pointer+1
            and     #$07
            bne     CheckNext           ; Check next slot
            beq     NoDevice            ; Otherwise we did not find one :-(     
            
Match:      sta     ProDrvAdr+1         ; Set card driver entry low byte
            lda     Pointer+1
            sta     ProDrvAdr+2         ; Set card driver entry high byte
            and     #$07
            sta     DIB1_Slot           ; Set found slot for both DIBs
            sta     DIB2_Slot
            ora     #$10                ; SIR = $10+slot#
            sta     SIR_Tbl
            sta     CardIsOK            ; Remember that we found a card
            lda     #SIR_Len
            ldx     SIR_Addr
            ldy     SIR_Addr+1
            jsr     ALLOCSIR            ; Claim SIR
            bcs     NoDevice   

FoundCard:  
                                        ; Lets get volume size            
            lda     #$00                ; 0=status
            sta     ProCommand          ; Command for blockdriver
            jsr     SetProUnit          ; set unitnumber
            lda     Bank_Reg
            sta     ProBank
            jsr     ProDriver           ; call driver
            bcs     NoDevice2
            lda     ProUnit             ; determine which unit
            bmi     Unit1            
            stx     DIB1_Blks
            sty     DIB1_Blks+1
            clc
            rts

Unit1:      stx     DIB2_Blks
            sty     DIB2_Blks+1
            clc
            rts

NoDevice2:  lda     #XDNFERR            ; Device not found
            jsr     SYSERR              ; Return to SOS with error in A

;
; D_READ call processing
;
DRead:
            lda     CardIsOK            ; Did we previously find a card?
            bne     DReadGo
            jmp     NoDevice            ; If not... then bail
DReadGo:
            jsr     CkCnt               ; Checks for validity, aborts if not
            jsr     CkUnit              ; Checks for unit below unit max
            jsr     SetProUnit          ; Set Prodos unit
            lda     #$00                ; Zero # bytes read
            sta     Count               ; Local count of bytes read
            sta     Count+1
            tay
            sta     (QtyRead),Y         ; Userland count of bytes read
            iny
            sta     (QtyRead),Y         ; Msb of userland bytes read
            lda     Num_Blks            ; Check for block count greater than zero
            beq     ReadExit
            jsr     FixUp               ; Correct for addressing anomalies
            lda     #$01                ; 1=read
            sta     ProCommand          ; Prepare to read a block
            lda     SosBlk
            sta     ProBlock
            lda     SosBlk+1
            sta     ProBlock+1
            lda     ProBufOff+ExtPG     ; save xbyte
            sta     xbytetmp


            
ReadOne:    jsr     SetProBuf           ; set Prodos buffer pointer
                                        ; This saves the overlap if needed
            jsr     ProDriver           ; call card prodos firmware interface
            bcs     IO_Error
            jsr     CheckSwap           ; Check for the overlap case and swap if needed

            inc     Count+1             ; increment our byte count by 512
            inc     Count+1
            inc     ProBlock
            bne     SkipReadMSBIncrement
            inc     ProBlock+1
SkipReadMSBIncrement:
            inc     SosBuf+1            ; Go get the next block in the buffer
            jsr     BumpSosBuf          ;   ...512 bytes in, and check the pointer
            dec     Num_Blks
            bne     ReadOne
            ldy     #0
            lda     Count               ; Local count of bytes read
            sta     (QtyRead),Y         ; Update # of bytes actually read
            lda     Count+1
            iny
            sta     (QtyRead),Y
            clc
ReadExit:
            lda     xbytetmp            ; restore xbyte
            sta     ProBufOff+ExtPG
            rts                         ; Exit read routines

IO_Error:   lda     #XIOERROR           ; I/O error
            jsr     SYSERR              ; Return to SOS with error in A

;
; D_WRITE call processing
;
DWrite:
            lda     CardIsOK            ; Did we previously find a card?
            bne     DWriteGo
            jmp     NoDevice            ; If not... then bail

DWriteGo:
            jsr     CkCnt               ; Checks for validity, aborts if not
            jsr     CkUnit              ; Checks for unit below unit max
            jsr     SetProUnit          ; Set Prodos unit
            lda     Num_Blks            ; Check for block count greater than zero
            beq     WriteExit
            jsr     FixUp               ; Correct for addressing anomalies
            lda     #$02                ; 2=write
            sta     ProCommand
            lda     SosBlk
            sta     ProBlock
            lda     SosBlk+1
            sta     ProBlock+1
            lda     ProBufOff+ExtPG     ; save xbyte
            sta     xbytetmp


WriteOne:   jsr     SetProBuf           ; set Prodos buffer pointer
            
            jsr     ProDriver           ; call card prodos firmware interface
            bcs     IO_Error
            jsr     CheckSwap           ; Check for the overlap case and swap if needed

            inc     ProBlock            ; Bump the block number
            bne     SkipWriteMSBIncrement
            inc     ProBlock+1
SkipWriteMSBIncrement:
            inc     SosBuf+1            ; Go get the next block in the buffer
            jsr     BumpSosBuf          ;   ...512 bytes in, and check the pointer
            dec     Num_Blks
            bne     WriteOne
            clc
WriteExit:
            lda     xbytetmp            ; restore xbyte
            sta     ProBufOff+ExtPG
            rts

;
; D_STATUS call processing
;  $00 = Driver Status
;  $FE = Return preferred bitmap location ($FFFF)
;
DStatus:
            lda     CardIsOK            ; Did we previously find a card?
            bne     DStatusGo

            jmp     NoDevice            ; If not... then bail

DStatusGo:
            lda     CtlStat             ; Which status code to run?
            bne     DS0
                                        ; get status from card
            lda     #$00                ; 0=status
            sta     ProCommand          ; Prepare to get status
            jsr     SetProUnit          ; set unitnumber
            jsr     ProDriver           ; call prodos block driver
            bcc     @ok
            jmp     NoDevice
            
@ok:        rts
            
DS0:        cmp     #$FE
            bne     DSWhat

            ldy     #$00                ; Return preferred bit map locations.
            lda     #$FF                ; We return FFFF, don't care
            sta     (CSList),Y
            iny
            sta     (CSList),Y       
            clc
            rts

DSWhat:     lda     #XCTLCODE           ; Control/status code no good
            jsr     SYSERR              ; Return to SOS with error in A

;
; D_CONTROL call processing
;  $00 = Reset device
;
DControl:
            lda     CardIsOK            ; Did we previously find a card?
            bne     DContGo
            jmp     NoDevice            ; If not... then bail
            
DContGo:    lda     CtlStat             ; Control command
            beq     CReset
            jmp     DCWhat              ; Control code no good!
CReset:     clc                         ; No-op
DCDone:     rts
          
DCWhat:     lda     #XCTLCODE           ; Control/status code no good
            jsr     SYSERR              ; Return to SOS with error in A

;------------------------------------
;
; Utility routines
;
;------------------------------------

;
; Check ReqCnt to ensure it is a multiple of 512.
;
CkCnt:      lda     ReqCnt              ; Look at the lsb of bytes requested
            bne     @1                  ; No good!  lsb should be 00
            lda     ReqCnt+1            ; Look at the msb
            lsr     A                   ; Put bottom bit into carry, 0 into top
            sta     Num_Blks            ; Convert bytes to number of blks to xfer
            bcc     CvtBlk              ; Carry is set from LSR to mark error.
@1:         lda     #XBYTECNT
            jsr     SYSERR              ; Return to SOS with error in A

;
; Test for valid block number; abort on error
;
CvtBlk:
            ldx     SOS_Unit
            ldy     DCB_Idx,x
            sec
            lda     DIB1_Blks+1,y       ; Blocks on unit msb
            sbc     SosBlk+1            ; User requested block number msb
            bvs     BlkErr              ; Not enough blocks on device for request
            beq     @1                  ; Equal msb; check lsb.
            rts                         ; Greater msb; we're ok.
@1:         lda     DIB1_Blks,y         ; Blocks on unit lsb
            sbc     SosBlk              ; User requested block number lsb
            bvs     BlkErr              ; Not enough blocks on device for request
            rts                         ; Equal or greater msb; we're ok.

BlkErr:     lda     #XBLKNUM
            jsr     SYSERR              ; Return to SOS with error in A

BumpSosBuf: inc     SosBuf+1            ; Increment SosBuf MSB
            ; fallthrough to FixUp

;
; Fix up the buffer pointer to correct for addressing
; anomalies.  We just need to do the initial checking
; for two cases:
; 00xx bank N -> 80xx bank N-1
; 20xx bank 8F if N was 0
; FDxx bank N -> 7Dxx bank N+1
; If pointer is adjusted, return with carry set
;
FixUp:      lda     SosBuf+1            ; Look at msb
            beq     @1                  ; That's one!
            cmp     #$FD                ; Is it the other one?
            bcs     @2                  ; Yep. fix it!
            rts                         ; Pointer unchanged, return carry clear.
@1:         lda     #$80                ; 00xx -> 80xx
            sta     SosBuf+1
            dec     SosBuf+ExtPG        ; Bank N -> band N-1
            lda     SosBuf+ExtPG        ; See if it was bank 0
            cmp     #$7F                ; (80) before the DEC.
            bne     @3                  ; Nope! all fixed.
            lda     #$20                ; If it was, change both
            sta     SosBuf+1            ; Msb of address and
            lda     #$8F
            sta     SosBuf+ExtPG        ; Bank number for bank 8F
            rts                         ; Return carry set
@2:         and     #$7F                ; Strip off high bit
            sta     SosBuf+1            ; FDxx ->7Dxx
            inc     SosBuf+ExtPG        ; Bank N -> bank N+1
@3:         rts                         ; Return carry set

CkUnit:     lda     SOS_Unit            ; Checks for unit below unit max
            cmp     MaxUnits
            bmi     UnitOk
NoUnit:     lda     XBADDNUM            ; Report no unit to SOS
            jsr     SYSERR
UnitOk:     clc
            rts

;
; Throttle back to 1 MHz
;
GoSlow:     pha
            php
            lda     EReg
            ora     #$80
            sta     EReg
            plp
            pla
            rts

;
; Throttle up to 2 MHz
;
GoFast:     pha
            php
            lda     EReg
            and     #$7F
            sta     EReg
            plp
            pla
            rts

;
; Set Prodos buffer pointer
;
SetProBuf:
            ldx     Bank_Reg            ; get current bank for later
            bit     SosBuf+ExtPG        ; Check if extended addressing is enabled (xbyte bit7=1)
            bpl     NoExtAddr           ; No, just copy the buffer pointer in and return
            lda     SosBuf+ExtPG        ; get the bank pair
            and     #$0f
            cmp     #$0f                ; check for special case xbyte=8F
            bne     nextchk            
            ldx     #0                  ; yes, xbyte=8F, then set bank 0
            beq     NoExtAddr           ; and keep pointers as is

nextchk:    bit     SosBuf+1            ; Check if high bit is set to determine required bank
            bpl     lowbank
            clc
            adc     #1                  ; increment bank
lowbank:    tax                         ; keep bank in x
            lda     SosBuf+1
            cmp     #$7e                ; Check for the overlap case
            bcc     BufOk               ; <7e = ok
            beq     Chklsb1             ; =7e then check lsb
            cmp     #$7f
            beq     OlapCase            ; =7f then its an overlap case
            bne     BufOk               ; then must be ok
            
Chklsb1:    lda     SosBuf              ; get SosBuf lsb
            bne     OlapCase            ; !=0 means an overlap case
            lda     SosBuf+1

BufOk:      and     #$7f                ; mask of high bit
            clc
            adc     #$20                ; and add offset for start of bank $20 to high byte
            bne     SetBuf              ; always branch (skip the lda SosBuf+1)
            
NoExtAddr:  lda     SosBuf+1            ; setup ProBuf
SetBuf:     sta     ProBuf+1
            lda     SosBuf            
            sta     ProBuf
            lda     #0                  ; turn off extended addressing
            sta     ProBufOff+ExtPG
            txa
            ora     #$f0
            tax
            stx     ProBank             ; store required bank
            rts
;
; No overlap into System Bank
; 
;  7E00 -----      9E00 -----  
;       |   |           |   |
;       |   |           |   |
;       |   |           |   |
;       |   |           |   |
;       |   |           |   |
;  7F00 |---|      9F00 |---|
;       |   |           |   |
;       |   |           |   |
;       |   |           |   |
;       |   |           |   |
;       |   |           |   |
;  7FFF -----      9FFF -----       
;
; Overlap into System Bank, 256 bytes or less
;
;  7E80 -----      9E80 -----                  7F00 -----      9F00 -----  
;       |   |           |   |                       |   |           |   |
;       |   |           |   |                       |   |           |   |
;       |   |           |   |                       |   |           |   |
;       |   |           |   |                       |   |           |   |
;       |   |           |   |                       |   |           |   |
;  7F80 |---|      9F80 |---|                  8000 |---|      A000 |---| Bank Overlap
;       |   |           |   |                       |   |           |///|
;       |   |           |   |                       |   |           |///|
;       |   |      A000 |---| Bank Overlap          |   |           |///|
;       |   |           |///|                       |   |           |///|
;       |   |           |///|                       |   |           |///|
;  807F -----      A07F -----                  80FF -----      A0FF -----       
;
; Overlap into System Bank, >256 bytes
;
;  7F80 -----      9F80 -----  
;       |   |           |   |
;       |   |           |   |
;       |   |      A000 |---| Bank Overlap
;       |   |           |///|
;       |   |           |///|
;  8080 |---|      A080 |---|
;       |   |           |///|
;       |   |           |///|
;       |   |           |///|
;       |   |           |///|
;       |   |           |///|
;  817F -----      A17F -----       
;
;
; Enter here for SosBuf = 7E01 to 7FFF
;
OlapCase:	
            lda     SosBuf              ; init pointer
            sta     Pointer
            lda     #$9f
            sta     Pointer+1
            lda     #$00                
            sta     Pointer+ExtPG       ; turn off extended addressing for pointer
            sec                         ; find starting offset
            sbc     SosBuf
            sta     Offset              ; offset to start of overlap
            tay                         ; Y contains offset

            lda     ProCommand          ; lets check if its a write
            cmp     #$02
            beq     SetWrite            ; yes it is
                                        ; else its a read
            lda     SosBuf+1            ; check which case it is
            cmp     #$7e
            beq     case7e              ; case 7e01-7eff, need to copy the second 256 overlap
            lda     SosBuf              ; must be 7f, now check lsb
            beq     case7f00            ; if lsb=0 (7f00), do full second 256 overlap
            jsr     CopyData            ; case 7f01-7fff, 256-offset + 256 bytes to save

case7f00:   inc     Pointer+1           ; Set pointer msb to A0
case7e:     inc     SosBuf+1            ; set SosBuf to second 256
            jsr     CopyData           
            dec     SosBuf+1            ; Restore SosBuf pointer
            lda     SosBuf+1
            jmp     BufOk               ; Now setup the rest of the pointers and return

SetWrite:   jsr     SwapOvrlap
            lda     SosBuf+1
            jmp     BufOk               ; Now setup the rest of the pointers and return

CopyData:   lda     (Pointer),Y
            sta     (SosBuf),Y
            iny
            bne     CopyData
            rts

;
; Check is its our overlap case and if it is then
; Swap A000-A0xx or A000-A1xx with the previously saved data
;

CheckSwap:  bit     SosBuf+ExtPG        ; Check if extended addressing is enabled (xbyte bit7=1)
            bpl     ItsOk               ; No, just return
            lda     SosBuf+ExtPG        ; Check for special xbyte 8F
            cmp     #$8f
            beq     ItsOk               ; yes, just return
            
            lda     #$9f                ; Restore Pointer
            sta     Pointer+1
            ldy     Offset              ; get offset

SwapOvrlap: lda     SosBuf+1            ; check for overlap case SosBuf=7E01-7FFF
            cmp     #$7e
            bcc     ItsOk               ; <7E, its ok, just return
            beq     chklsb              ; is 7E, then check lsb
            cmp     #$7f
            bne     ItsOk               ; its ok, keep going
            lda     SosBuf              ; else check lsb
            beq     c7f00
            bne     c7f

chklsb:     lda     SosBuf
            beq     ItsOk               ; 7E00 is ok, return
            bne     c7e                 ; else notok, 7E01- swap data 

c7f:        jsr     SwapData            ; 7f01-7fff -> swap a000-a0xx & a100-a1ff
c7f00:      inc     Pointer+1           ; 7f00 -> swap a000-a0ff
c7e:        inc     SosBuf+1            ; 7e01-7eff -> swap a000-a0xx
            jsr     SwapData
            dec     SosBuf+1            ; Restore SosBuf pointer
ItsOk:      rts

SwapData:   lda     (Pointer),Y
            pha
            lda     (SosBuf),Y 
            sta     (Pointer),Y
            pla
            sta     (SosBuf),Y
            iny
            bne     SwapData
            rts


;
; jsr to card firmware driver
; We update the address based on the slot and firmware CxFF byte
;
ProDriver:  sei                         ; disable interrupts while changing things
            lda     #$FF
            sta     E1908               ; E1908 = NON-ZERO LOCKOUT MOUSE
            jsr     SaveMem             ; save and swap in card screen hole & zeropage
            jsr     GoSlow
            lda     Bank_Reg            ; remember current bank
            sta     CurrBank
            ldx     ProBank             ; get destination bank
            stx     Bank_Reg            ; switch to new bank 
ProDrvAdr:  jsr     $0000               ; call device entry
            lda     CurrBank            ; restore driver bank
            sta     Bank_Reg		
            sei                         ; Keep interrupts off incase card firmware reenabled
            jsr     GoFast
            jsr     RestMem             ; save and swap out card screen hole & zeropage
            lda     #$18                ; Clear CB1 & CB2 flags - VBL
            sta     E_IFR               ; this seems more for mame, its a little different
            lda     #$00
            sta     E1908               ; SAY OK TO MOUSE
            cli                         ; enable interrupts again
            rts

;
; Save current screenholes and restore peripheral card values (current slot + slot0)
; and save zeropage $42-$47
;
SaveMem:    lda     E_IER
            and     #$18                ; See if either CB1 or CB2 interrupts were enabled
            beq     SkipVbl             ; No, they were not, skip the wait for vbl
            
                                        ; now wait for 2xVBL before we muck with the screenholes
            lda     #$18                ; Clear CB2 flag - VBL
            sta     E_IFR
VWait:      bit     E_IFR               ; Wait for vertical retrace
            beq     VWait
                                        ; wait for another one to ensure font data is loaded
            lda     #$18                ; Clear CB2 flag - VBL
            sta     E_IFR
VWait2:     bit     E_IFR               ; Wait for vertical retrace
            beq     VWait2

SkipVbl:    bit     CWrtOff             ; disable font loading

            jsr     SwapScrH            ; Swap in the screenhole data with ours
            jsr     SwapZero            ; swap zeropage
            rts

;
; Save Card screen holes and restore original values
; and restore zeropage $42-$47
;
RestMem:    pha
            php                         ; keep carry error indication
            jsr     SwapZero            ; swap zeropage
            jsr     SwapScrH            ; Swap back the original screenhole data            
            plp
            pla
            rts

;            
; Swap screenholes with driver values 
; we do Slot 0 and current slot values
; This nice code is from Peter Ferrie, thanks 
;
SwapScrH:   lda     #$07                ; Init ZP Screenbase, X and A
            sta     ScreenBase+1
            lda     #$00
            sta     ScreenBase
            sta     ScreenBase+ExtPG
            ldx     #$0F
            lda     #$F8
@loop:
            tay                         ; A holds screen page index at loop entry
            lda     (ScreenBase),Y   
            pha                         ; Save current screen hole byte on stack.
            lda     TmpScrH,X
            sta     (ScreenBase),Y      ; Restore screen hole byte from array 
            pla
            sta     TmpScrH,X           ; Copy saved screen hole byte into array.
            dex
            bmi     done                ; Exit when array is full (all 16 bytes are copied, X<0).
            txa                         ; TXA/LSR tests whether array index is odd or even
            lsr                         ; and sets carry accordingly (1 = odd).
            tya                         ; Bring screen index into A for manipulation
            eor     DIB1_Slot           ; Cycle page index between $x8 and $x8+n as long as N in 1..7
            bcc     @loop               ; Take branch every other loop, using array index odd/even 
                                        ;  (carry still valid from TXA/LSR)
            eor     #$80                ; Cycle page index between $F8 and $78
            bpl     @loop               ; If flipping from $78->$F8 (now negative), continue
            dec     ScreenBase+1        ; Go to next page counting down ($07->$06, etc.)
            bne     @loop               ; Always -- equiv. to BRA or JMP.  Never reaches 0.
done:       rts

;
; Swap zeropage with driver values
;
SwapZero:   ldx     #$2f
@loop2:     lda     $20,X   
            pha                         ; Save current zeropage byte on stack.
            lda     TmpZero,X
            sta     $20,X               ; Restore zeropage byte from temp storage 
            pla
            sta     TmpZero,X           ; Copy saved zeropage byte into temp storage.
            dex
            bpl     @loop2
            rts

;
; Set Prodos unit number
;
SetProUnit: lda     DIB1_Slot           ; create the slot/unit byte
            asl                         ; 7 6 5 4 3 2 1 0
            asl                         ; D S S S 0 0 0 0
            asl
            asl
            ldx     SOS_Unit            ; check unit
            beq     @unit0 
            ora     #$80                ; is unit 1
@unit0:     sta     ProUnit
            rts


CurrEnd     =       *
Disk3End    =       $EE04

; add padding to make the same length as the disk3 driver
PADDING      =         Disk3End - CurrEnd
             .RES      PADDING

ZZEND       =          *
ZZLEN       =          ZZEND-ZZORG

            .IF        ZZLEN-LENDISK3
            .FATAL     "SOSORG FILE IS INCORRECT FOR DISK3"
            .ENDIF
