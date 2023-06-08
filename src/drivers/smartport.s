; SmartPort driver for prodos compatible SmartPort cards
; This is based on the problock3 driver with some learnings aquired along the journey of
; developing that.
; 
; The driver dynamically sets the slot when it receives an init request.
; Slots are searched from 4 to 1 for the rom signature indicating the card
; supports the Smartport interface. The first one it finds, it updates
; the DIB slot numbers to the found slot.
;
; The driver swaps in the screen holes and changes the ZP to $03($0300) to disable the A3 enhanced
; indirect addressing. This is incase the card firmware uses indirect addressing to fill the
; requested buffer. It then swaps them back and restores the ZP to $18($1800) before returning to SOS.
; It also disables VBL interrupts that can be used for font loading by the 
; console before any screenhole changes to avoid font loading corruption.
;
; This version uses a 512 byte buffer for the firmware to store the block data, and then copies
; this to the requested address pointer after returning from the firmware 
;

;			.TITLE "Apple /// Smartport Driver"
			.PROC  Smartport

			.setcpu "6502"
			.reloc

DriverVersion	= $003B		; Version number
DriverMfgr		= $524A		; Driver Manufacturer - RJ
DriverType		= $E1		; No formatter present for the time being
DriverSubtype	= $02		;
ScanStart		= $04		; Slot number to start scan from
AutoScan        = $FF       ; Auto scan slots

;
; SOS Equates
;
ExtPG		= $1401			; Driver extended bank address offset
AllocSIR	= $1913			; Allocate system internal resource
SELC800		= $1922			; Enable Expansion Rom Space
DeAlcSIR	= $1916			; De-allocate system internal resource
SysErr		= $1928			; Report error to system
ZPReg       = $FFD0         ; Zero Page register
EReg		= $FFDF			; Environment register
E_IFR       = $FFED         ; VIA E Interrupt Flag Register
E_IER       = $FFEE         ; VIA E Interrupt Enable Register
CWrtOff     = $C0DA         ; Character(font) loading off




;
; SOS Zero page parameters
;
ReqCode		= $C0			; Request code
SOS_Unit	= $C1			; Unit number
SosBuf		= $C2			; SOS buffer pointer
ReqCnt		= $C4			; Requested byte count
CtlStat		= $C2			; Control/status code
CSList		= $C3			; Control/status list pointer
SosBlk		= $C6			; Starting block number
QtyRead		= $C8			; Bytes read return by D_READ


;
; Parameter block specific to current SOS request
;
Num_Blks	= $D2			; Number of blocks requested (we'll never ever have > 128 blocks)
Count		= $D3			; 2 bytes lb,hb

;
; Extra zero page variables
;
ScreenBase  = $D5           ; 2 bytes lb,hb for save/restore screenholes
Pointer     = $D7           ; 2 byte pointer for signature check
SosBuf2     = $D9           ; 2 byte temp pointer for buffer transfers

;
; SOS Error Codes
;
XDNFERR		= $10			; Device not found
XBADDNUM	= $11			; Invalid device number
XREQCODE	= $20			; Invalid request code
XCTLCODE	= $21			; Invalid control/status code
XCTLPARAM	= $22			; Invalid control/status parameter
XNORESRC	= $25			; Resource not available
XBADOP  	= $26			; Invalid operation
XIOERROR	= $27			; I/O error
XNODRIVE	= $28			; Drive not connected
XBYTECNT	= $2C			; Byte count not a multiple of 512
XBLKNUM 	= $2D			; Block number to large
XDISKSW		= $2E			; Disk switched
XDCMDERR	= $31			; device command ABORTED error occurred
XCKDEVER	= $32			; Check device readiness routine failed
XNORESET	= $33			; Device reset failed
XNODEVIC	= $38			; Device not connected

;
; Smartport command codes
;
SP_Status     = $00
SP_ReadBlock  = $01
SP_WriteBlock = $02
SP_Format     = $03
SP_Control    = $04
SP_Init       = $05
SP_Open       = $06
SP_Close      = $07
SP_Read       = $08
SP_Write      = $09


;
; Switch Macro
;
.MACRO		SWITCH index,bounds,adrs_table,noexec	; See SOS Reference
.IFNBLANK index							; If PARM1 is present,
			lda		index				; load A with switch index
.ENDIF
.IFNBLANK	bounds						; If PARM2 is present,
			cmp		#bounds+1			; perform bounds checking
			bcs		@110				; on switch index
.ENDIF
			asl		A					; Multiply by 2 for table index
			tay
			lda		adrs_table+1,y		; Get switch address from table
			pha							; and push onto Stack
			lda		adrs_table,y
			pha
.IFBLANK	noexec
			rts							; Exit to code
.ENDIF
@110:
.ENDMACRO

			.SEGMENT "TEXT"

;
; Comment Field of driver
;
			.word	$FFFF ; Signal that we have a comment
			.word	COMMENT_END - COMMENT
COMMENT:	.byte	"Apple /// Smartport Driver"
COMMENT_END:

			.SEGMENT	"DATA"

;------------------------------------
;
; Device identification Block (DIB) - Volume #0
;
;------------------------------------

DIB_0:		.word	DIB_1			; Link pointer
			.word	Entry			; Entry pointer
			.byte	$08				; Name length byte
			.byte	".PROFILE       "; Device name
			.byte	$80				; Active, no page alignment
DIB0_Slot:	.byte	AutoScan		; Slot number
			.byte	$00				; Unit number
			.byte	DriverType		; Type
			.byte	DriverSubtype	; Subtype
			.byte	$00				; Filler
DIB0_Blks:	.word	$0000			; # Blocks in device
			.word	DriverMfgr		; Manufacturer
			.word	DriverVersion	; Driver version
			.word	$0000			; DCB length followed by DCB
;
; Device identification Block (DIB) - Volume #1
; Page alignment begins here
;
alignstart  = *

;Buffer:     .res    512             ; buffer for smartport interface in this bank

DIB_1:		.word	DIB_2			; Link pointer
			.word	Entry			; Entry pointer
			.byte	$03				; Name length byte
			.byte	".S2            "; Device name
			.byte	$80				; Active
DIB1_Slot:	.byte	AutoScan		; Slot number
			.byte	$01				; Unit number
			.byte	DriverType		; Type
			.byte	DriverSubtype	; Subtype
			.byte	$00				; Filler
DIB1_Blks:	.word	$0000			; # Blocks in device
			.word	DriverMfgr		; Driver manufacturer
			.word	DriverVersion	; Driver version
			.word	$0000			; DCB length followed by DCB
;
; Device identification Block (DIB) - Volume #2
;
DIB_2:		.word	DIB_3			; Link pointer
			.word	Entry			; Entry pointer
			.byte	$03				; Name length byte
			.byte	".S3            "; Device name
			.byte	$80				; Active
DIB2_Slot:	.byte	AutoScan		; Slot number
			.byte	$02				; Unit number
			.byte	DriverType		; Type
			.byte	DriverSubtype	; Subtype
			.byte	$00				; Filler
DIB2_Blks:	.word	$0000			; # Blocks in device
			.word	DriverMfgr		; Driver manufacturer
			.word	DriverVersion	; Driver version
			.word	$0000			; DCB length followed by DCB
;
; Device identification Block (DIB) - Volume #3
;
DIB_3:		.word	DIB_4			; Link pointer
			.word	Entry			; Entry pointer
			.byte	$03				; Name length byte
			.byte	".S4            "; Device name
			.byte	$80				; Active
DIB3_Slot:	.byte	AutoScan		; Slot number
			.byte	$03				; Unit number
			.byte	DriverType		; Type
			.byte	DriverSubtype	; Subtype
			.byte	$00				; Filler
DIB3_Blks:	.word	$0000			; # Blocks in device
			.word	DriverMfgr		; Driver manufacturer
			.word	DriverVersion	; Driver version
			.word	$0000			; DCB length followed by DCB
;
; Device identification Block (DIB) - Volume #4
;
DIB_4:		.word	DIB_5			; Link pointer
			.word	Entry			; Entry pointer
			.byte	$03				; Name length byte
			.byte	".S5            "; Device name
			.byte	$80				; Active
DIB4_Slot:	.byte	AutoScan		; Slot number
			.byte	$04				; Unit number
			.byte	DriverType		; Type
			.byte	DriverSubtype	; Subtype
			.byte	$00				; Filler
DIB4_Blks:	.word	$0000			; # Blocks in device
			.word	DriverMfgr		; Driver manufacturer
			.word	DriverVersion	; Driver version
			.word	$0000			; DCB length followed by DCB
;
; Device identification Block (DIB) - Volume #5
;
DIB_5:		.word	DIB_6			; Link pointer
			.word	Entry			; Entry pointer
			.byte	$03				; Name length byte
			.byte	".S6            "; Device name
			.byte	$80				; Active
DIB5_Slot:	.byte	AutoScan		; Slot number
			.byte	$05				; Unit number
			.byte	DriverType		; Type
			.byte	DriverSubtype	; Subtype
			.byte	$00				; Filler
DIB5_Blks:	.word	$0000			; # Blocks in device
			.word	DriverMfgr		; Driver manufacturer
			.word	DriverVersion	; Driver version
			.word	$0000			; DCB length followed by DCB
;
; Device identification Block (DIB) - Volume #6
;
DIB_6:		.word	DIB_7			; Link pointer
			.word	Entry			; Entry pointer
			.byte	$03				; Name length byte
			.byte	".S7            "; Device name
			.byte	$80				; Active
DIB6_Slot:	.byte	AutoScan		; Slot number
			.byte	$06				; Unit number
			.byte	DriverType		; Type
			.byte	DriverSubtype	; Subtype
			.byte	$00				; Filler
DIB6_Blks:	.word	$0000			; # Blocks in device
			.word	DriverMfgr		; Driver manufacturer
			.word	DriverVersion	; Driver version
			.word	$0000			; DCB length followed by DCB
;
; Device identification Block (DIB) - Volume #7
;
DIB_7:		.word	$0000			; Link pointer
			.word	Entry			; Entry pointer
			.byte	$03				; Name length byte
			.byte	".S8            "; Device name
			.byte	$80				; Active
DIB7_Slot:	.byte	AutoScan		; Slot number
			.byte	$07				; Unit number
			.byte	DriverType		; Type
			.byte	DriverSubtype	; Subtype
			.byte	$00				; Filler
DIB7_Blks:	.word	$0000			; # Blocks in device
			.word	DriverMfgr		; Driver manufacturer
			.word	DriverVersion	; Driver version
			.word	$0000			; DCB length followed by DCB

;------------------------------------
;
; Local storage locations
;
;------------------------------------

LastOP:		.res	$08, $FF			; Last operation for D_REPEAT calls
SIR_Addr:	.word	SIR_Tbl
SIR_Tbl:	.res	$05, $00
SIR_Len		=		*-SIR_Tbl
MaxUnits:	.byte	$08					; The maximum number of units

DCB_Idx:	.byte	$00					; DCB 0's blocks
			.byte	DIB1_Blks-DIB0_Blks	; DCB 1's blocks
			.byte	DIB2_Blks-DIB0_Blks	; DCB 2's blocks
			.byte	DIB3_Blks-DIB0_Blks	; DCB 3's blocks
			.byte	DIB4_Blks-DIB0_Blks	; DCB 4's blocks
			.byte	DIB5_Blks-DIB0_Blks	; DCB 5's blocks
			.byte	DIB6_Blks-DIB0_Blks	; DCB 6's blocks
			.byte	DIB7_Blks-DIB0_Blks	; DCB 7's blocks

CardIsOK:	.byte	$00					; Have we found an intelligent disk controller yet?

TmpScrH:    .res    $10, $00            ; Storage for screenholes, slot0 & current slot
TmpZero:    .res    $30, $00            ; Storage to save $20-$4F zero page locations

Signature:  .byte   $FF, $20, $FF, $00  ; Disk card signature for SP
            .byte   $FF, $03, $FF, $00
            
; Smartport Status Call Parameter List
StatParam:  .byte   $03               ; #params
            .byte   $00               ; Unit
            .word   Buffer            ; Status list pointer for Result
StatCode:   .byte   $00               ; Status Code

; Smartport Control Call Parameter List
CtrlParam:  .byte   $03               ; #params
            .byte   $00               ; Unit
            .word   Buffer            ; Ctrl list pointer
CtrlCode:   .byte   $00               ; Control Code

; Smartport Read/Write Call Parameter List
RWParam:    .byte   $03               ; #params
            .byte   $00               ; Unit
            .word   Buffer            ; Data buffer pointer
SPBlk:      .byte   $00,$00,$00       ; Block Number (low byte,mid byte,high byte)

; Hack for relocatable code supporting only 16bit values
StatParAdr: .word StatParam
CtrlParAdr: .word CtrlParam
RWParamAdr: .word RWParam

; .res 151
;alignend    =  *
;padding     =  alignend - alignstart
;            .res    padding
;            

Buffer:     .res    512

;------------------------------------
;
; Driver request handlers
;
;------------------------------------

Entry:  	
			jsr		Dispatch			; Call the dispatcher
			ldx		SOS_Unit			; Get drive number for this unit
			lda		ReqCode				; Keep request around for D_REPEAT
			sta		LastOP,x			; Keep track of last operation
			rts

;
; The Dispatcher.  Note that if we came in on a D_INIT call,
; we do a branch to Dispatch normally.  
; Dispatch is called as a subroutine!
;
DoTable:	.word	DRead-1				; 0 Read request
			.word	DWrite-1			; 1 Write request
			.word	DStatus-1			; 2 Status request
			.word	DControl-1			; 3 Control request
			.word	BadReq-1			; 4 Unused
			.word	BadReq-1			; 5 Unused
			.word	DOpen-1				; 6 Open - valid for character devices
			.word	DClose-1			; 7 Close - valid for character devices
			.word	DInit-1				; 8 Init request
			.word	DRepeat-1			; 9 Repeat last read or write request
Dispatch:	SWITCH	ReqCode,9,DoTable	; Serve the request

;
; Dispatch errors
;
BadReq:		lda		#XREQCODE			; Bad request code!
			jsr		SysErr				; Return to SOS with error in A
BadOp:		lda		#XBADOP				; Invalid operation!
			jsr		SysErr				; Return to SOS with error in A

;
; D_REPEAT - repeat the last D_READ or D_WRITE call
;
DRepeat:	ldx		SOS_Unit
			lda		LastOP,x			; Recall the last thing we did
			cmp		#$02				; Looking for operation < 2
			bcs		BadOp				; Can only repeat a read or write
			sta		ReqCode
			jmp		Dispatch

;
; D_INIT call processing - called once each for all volumes.
; on first entry we search the slots from 4 to 1 for a smartport device 
; and use the first one we find
;
DInit:
			lda		CardIsOK			; Check if we have checked for a card already
			beq		CheckSig            ; No, lets check
            jmp     FoundCard           ; Yes, skip signature check      

CheckSig:	jsr     GoSlow              ; set 1MHz, Grappler plus does not like 2M	
            lda     DIB0_Slot           ; Check configured slot for autoscan
            bpl     FixedSlot           ; No, use configured DIB1 slot
            lda     #ScanStart          ; else load starting scan slot
FixedSlot:  ora		#$C0				; Form a $Cs00 address, where s = slot #
			sta		Pointer+1
			lda		#$00				
			sta		Pointer
            sta     Pointer+ExtPG       ; zero out the xbyte
                                        
                                        ; We check all 4 sp sig bytes, starting from last
CheckNext:	ldy		#$05                ; <-- hack, just check the 3 sig bytes, then CFFA will work in slot1-4
@1:			lda		(Pointer),y         ;     this assumes the card detected can do smartport even though it
			cmp		Signature,Y         ;     does not indicate it
			bne		NoMatch 			; No device if bytes don't match
            dey
            dey
            bpl     @1

                                        ; We found a SmartPort disk controller
            ldy     #$ff                ; $CxFF - check last byte for entry point
			lda		(Pointer),y
            sta     PDEntry+1           ; Set card prodos driver entry low byte
            clc
            adc     #3                  ; add 3 to find Smartport entry point
            sta     SPEntry+1           ; Set card smartport driver entry low byte
            lda     Pointer+1
            sta     PDEntry+2           ; Set card prodos driver entry high byte
            sta     SPEntry+2           ; Set card smartport driver entry high byte
            and     #$07
            sta     DIB0_Slot           ; Set found slot for all DIBs
            sta     DIB1_Slot
            sta     DIB2_Slot
            sta     DIB3_Slot
            sta     DIB4_Slot
            sta     DIB5_Slot
            sta     DIB6_Slot
            sta     DIB7_Slot
			ora		#$10				; SIR = $10+slot#
			sta		SIR_Tbl
			sta		CardIsOK			; Remember that we found a card
			lda		#SIR_Len
			ldx		SIR_Addr
			ldy		SIR_Addr+1
			jsr		AllocSIR			; Claim SIR
			bcs		NoDevice
                                        ; Lets check number of devices connected
                                        ; From P8 1.7 source:
                                        ;    the Smartportinterface does not set up
                                        ;    its device list until it receives a
                                        ;    Prodos Status call ; therefore, we have
                                        ;    made two calls : first a prodos status call
                                        ;    then a smartport status call
            lda     DIB0_Slot           ; From Prodos style drive/slot
            asl                         ; 7 6 5 4 3 2 1 0
            asl                         ; D S S S 0 0 0 0
            asl
            asl
            sta     TmpZero+$23         ; Set slot for block driver call        
            jsr     ProDriver           ; Do block driver status call
                                        ; Now setup for smartport status call
            lda		#SP_Status			; Status cmd=0
			sta		CmdNum      	    ; Command for SP call
            sta		StatParam+1         ; Set unit 00 in Param list
            Sta     StatParam+4         ; Set status call type - 00
            lda     StatParAdr          ; Set Status parameter list pointer
            sta     CmdList
            lda     StatParAdr+1
            Sta     CmdList+1
            jsr     SmartPort           ; call smartport
            bcs		NoDevice

            lda     Buffer              ; First byte returned is the num devices
            beq     NoDevice            ; if zero, no devices attached
            sta     MaxUnits            ; otherwise update maxunits
            bne     FoundCard           ; always branch

NoMatch:    dec     Pointer+1           ; Try next slot
            lda     Pointer+1
            and     #$07
            beq     NoDevice            ;  we did not find one :-(
            jmp     CheckNext           ; Check next slot            

FoundCard:                              
            jsr     CkUnit              ; Checks for unit below unit max
            jsr     UpdVolSize          ; Its ok, lets Get volume size and update DIB
			clc
			rts
            
NoDevice:	lda		#XDNFERR			; Device not found
			jsr		SysErr				; Return to SOS with error in A

;
; D_READ call processing
;
DRead:
			lda		CardIsOK			; Did we previously find a card?
			bne		DReadGo
			jmp		NoDevice			; If not... then bail
DReadGo:
            jsr		CkCnt				; Checks for validity, aborts if not
			jsr		CkUnit				; Checks for unit below unit max
			lda		#$00				; Zero # bytes read
			sta		Count				; Local count of bytes read
			sta		Count+1
            tay
            sta     (QtyRead),Y         ; Userland count of bytes read
            iny
            sta     (QtyRead),Y         ; Msb of userland bytes read
			lda		Num_Blks			; Check for block count greater than zero
			beq		ReadExit
			jsr		FixUp				; Correct for addressing anomalies

			lda		#SP_ReadBlock       ; Setup SP command code
			sta		CmdNum
            lda     RWParamAdr          ; Set Read/Write parameter list pointer
            sta     CmdList
            lda     RWParamAdr+1
            Sta     CmdList+1

            ldx     SOS_Unit            ; Set unit in Param list
            inx                         ; smartport units start from 1
            stx		RWParam+1
            lda		SosBlk              ; copy over blocknum
			sta		SPBlk
			lda		SosBlk+1
			sta		SPBlk+1            
            
ReadOne:	jsr		SmartPort           ; card smartport firmware interface
			bcs		IO_Error

            ldy     SosBuf              ;setup pointer for second 256 bytes
			sty     SosBuf2             ;  quicker to do in one loop
			ldy     SosBuf+1
			iny
			sty     SosBuf2+1
            ldy     SosBuf+ExtPG
            sty     SosBuf2+ExtPG
            ldy     #0

@10:        lda     Buffer,Y            ; copy first 256 bytes from Buffer to SosBuf
            sta     (SosBuf),Y          ;
            lda     Buffer+256,Y        ; copy second 256 bytes from Buffer to SosBuf
            sta     (SosBuf2),Y         ;
            dey
            bne     @10

			inc		SPBlk
			bne		SkipReadMSBIncrement
			inc		SPBlk+1
SkipReadMSBIncrement:
			jsr		BumpSosBuf			;   ...512 bytes in, and check the pointer
			dec		Num_Blks
			bne		ReadOne
            ldy     #0
            lda     Count               ; Local count of bytes read
            sta     (QtyRead),Y         ; Update # of bytes actually read
            lda     Count+1
            iny
            sta     (QtyRead),Y
			clc
ReadExit:   rts							; Exit read routines

IO_Error:	lda		#XIOERROR			; I/O error
			jsr		SysErr				; Return to SOS with error in A


;
; D_WRITE call processing
;
DWrite:
			lda		CardIsOK			; Did we previously find a card?
			bne		DWriteGo
			jmp		NoDevice			; If not... then bail

DWriteGo:
			jsr		CkCnt				; Checks for validity, aborts if not
			jsr		CkUnit				; Checks for unit below unit max
			lda		Num_Blks			; Check for block count greater than zero
			beq		WriteExit
			jsr		FixUp				; Correct for addressing anomalies

			lda		#SP_WriteBlock       ; Setup SP command code
			sta		CmdNum
            lda     RWParamAdr          ; Set Read/Write parameter list pointer
            sta     CmdList
            lda     RWParamAdr+1
            Sta     CmdList+1

            ldx     SOS_Unit            ; Set unit in Param list
            inx                         ; smartport units start from 1
            stx		RWParam+1
			lda		SosBlk
			sta		SPBlk
			lda		SosBlk+1
			sta		SPBlk+1

WriteOne:   
            ldy     SosBuf              ;setup pointer for second 256 bytes
			sty     SosBuf2             ;  quicker to do in one loop
         	ldy     SosBuf+1
			iny
			sty     SosBuf2+1
            ldy     SosBuf+ExtPG
            sty     SosBuf2+ExtPG
            ldy     #0

@10:        lda     (SosBuf),Y          ; copy first 256 bytes from SosBuf to Buffer
            sta     Buffer,Y            ;
            lda     (SosBuf2),Y         ; copy second 256 bytes from SosBuf to Buffer
            sta     Buffer+256,Y        ;
            dey
            bne     @10

			jsr		SmartPort
			bcs		IO_Error
			inc		SPBlk		    	; Bump the block number
			bne		SkipWriteMSBIncrement
			inc		SPBlk+1
SkipWriteMSBIncrement:
			jsr		BumpSosBuf			;   ...512 bytes in, and check the pointer
			dec		Num_Blks
			bne		WriteOne
			clc
WriteExit:
			rts

;
; D_STATUS call processing
;  $00 = Driver Status
;  $FE = Return preferred bitmap location ($FFFF)
;
; Smartport status bute
;  7 0=character device 1=block device
;  6 1=write allowed
;  5 1=read allowed
;  4 1=device online or disk in drive
;  3 0=format allowed
;  2 1=medium write protected
;  1 1=device currently interupting
;  0 1=device currently open(char devs only)
;

; SOS status byte
;  7 0=device not busy 1=device busy
;  6 not used
;  5 not used
;  4 not used
;  3 not used
;  2 not used
;  1 0=device not write-protected 1=device write-protected
;  0 not used




DStatus:
			lda		CardIsOK			; Did we previously find a card?
			bne		DStatusGo
			jmp		NoDevice			; If not... then bail

DStatusGo:
        	lda		CtlStat				; Which status code to run?
			bne		DS0
                                        ; get status from card
            lda		#SP_Status          ; Setup SP command code
			sta		CmdNum
            lda     StatParAdr          ; Set Status parameter list pointer
            sta     CmdList
            lda     StatParAdr+1
            Sta     CmdList+1
            
            ldx     SOS_Unit            ; Set unit in Param list
            inx
            stx		StatParam+1         ; smartport units start from 1

            lda     #$00                ; Set status call type - 00
            Sta     StatParam+4

            jsr     SmartPort           ; call prodos block driver
            bcc     @ok
            jmp     NoDevice
            
@ok:		lda     Buffer              ; get returned status byte
            and     #$02                ; mask and leave just write protect bit
                                        ; + always device not busy
            asl                         ; move one bit left to align with SOS
            ldy		#$00
			sta		(CSList),Y            
            rts
            
DS0:		cmp		#$FE
			bne		DSWhat

			ldy		#$00				; Return preferred bit map locations.
			lda		#$FF				; We return FFFF, don't care
			sta		(CSList),Y
			iny
			sta		(CSList),Y       
			clc
			rts

DSWhat:		lda		#XCTLCODE			; Control/status code no good
			jsr		SysErr				; Return to SOS with error in A

;
; D_CONTROL call processing
; Just pass these through to smartport interface

DControl:
            lda		CardIsOK			; Did we previously find a card?
			bne		DControlGo
			jmp		NoDevice			; If not... then bail

DControlGo: 
            lda		#SP_Control         ; Setup SP command code
			sta		CmdNum
            lda     CtrlParAdr          ; Set Control parameter list pointer
            sta     CmdList
            lda     CtrlParAdr+1
            sta     CmdList+1

            ldx     SOS_Unit            ; Set unit in Param list
            inx                         ; smartport units start from 1
            stx		CtrlParam+1

            lda     CtlStat				; copy control command code
            sta     CtrlCode            ; into param list

                                        ; copy the control list
            ldy     #0                  ; get the length of the control list
            lda     (CSList),y          ; we'll code this for smartport not sos
            sta     Buffer              ; so first two bytes for length
            iny
            lda     (CSList),y
            sta     Buffer+1
            pha                         ; save the length msb for later

            ldx     Buffer              ; get low byte of length
            beq     @Zero1
            iny
@L1:        lda     (CSList),y
            sta     Buffer,y
            iny
            bne     @skip
            inc     CSList+1            
@skip:      dex
            bne     @L1

@Zero1:     pla                         ; get the length msb
            beq     @Done               ; its zero, we are done
            sec                         ; dec a
            sbc     #1
            pha                         ; save msb
            jmp     @L1                 

@Done:      jsr		SmartPort
			bcs		Ctrl_Error
            rts            

Ctrl_Error:	lda		#XIOERROR			; I/O error
			jsr		SysErr				; Return to SOS with error in A            



;DCWhat:	lda		#XCTLCODE			; Control/status code no good
;			jsr		SysErr				; Return to SOS with error in A

;			lda		CardIsOK			; Did we previously find a card?
;			BNE		DContGo
;			JMP		NoDevice			; If not... then bail
;			
;DContGo:	LDA		CtlStat				; Control command
;			BEQ		CReset
;			JMP		DCWhat				; Control code no good!
;CReset:		clc							; No-op
;DCDone:		rts          
;DCWhat:	lda		#XCTLCODE			; Control/status code no good
;			jsr		SysErr				; Return to SOS with error in A


;
; D_OPEN call processing
;
DOpen:      
            rts

;
; D_CLOSE call processing
;
DClose:      
            rts


;------------------------------------
;
; Utility routines
;
;------------------------------------

;
; Check ReqCnt to ensure it is a multiple of 512.
;
CkCnt:		lda		ReqCnt				; Look at the lsb of bytes requested
			bne		@1					; No good!  lsb should be 00
			lda		ReqCnt+1			; Look at the msb
			lsr		A					; Put bottom bit into carry, 0 into top
			sta		Num_Blks			; Convert bytes to number of blks to xfer
			bcc		CvtBlk				; Carry is set from LSR to mark error.
@1:			lda		#XBYTECNT
			jsr		SysErr				; Return to SOS with error in A

;
; Test for valid block number; abort on error
;
CvtBlk:
			ldx		SOS_Unit
			ldy		DCB_Idx,x
			sec
			lda		DIB0_Blks+1,y		; Blocks on unit msb
			sbc		SosBlk+1			; User requested block number msb
			bvs		BlkErr				; Not enough blocks on device for request
			beq		@1					; Equal msb; check lsb.
			rts							; Greater msb; we're ok.
@1:			lda		DIB0_Blks,y			; Blocks on unit lsb
			sbc		SosBlk				; User requested block number lsb
			bvs		BlkErr				; Not enough blocks on device for request
			rts							; Equal or greater msb; we're ok.
BlkErr:		lda		#XBLKNUM
			jsr		SysErr				; Return to SOS with error in A

BumpSosBuf:	inc		SosBuf+1			; Increment SosBuf MSB
            inc     SosBuf+1
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
FixUp:		lda		SosBuf+1			; Look at msb
			beq		@1					; That's one!
			cmp		#$FD				; Is it the other one?
			bcs		@2					; Yep. fix it!
			rts							; Pointer unchanged, return carry clear.
@1:			lda		#$80				; 00xx -> 80xx
			sta		SosBuf+1
			dec		SosBuf+ExtPG		; Bank N -> band N-1
			lda		SosBuf+ExtPG		; See if it was bank 0
			cmp		#$7F				; (80) before the DEC.
			bne		@3					; Nope! all fixed.
			lda		#$20				; If it was, change both
			sta		SosBuf+1			; Msb of address and
			lda		#$8F
			sta		SosBuf+ExtPG		; Bank number for bank 8F
			rts							; Return carry set
@2:			and		#$7F				; Strip off high bit
			sta		SosBuf+1			; FDxx ->7Dxx
			INC		SosBuf+ExtPG		; Bank N -> bank N+1
@3:			rts							; Return carry set

CkUnit:		lda		SOS_Unit			; Checks for unit below unit max
			cmp		MaxUnits
			bmi		UnitOk
NoUnit:		lda		XBADDNUM			; Report no unit to SOS
			jsr		SysErr
UnitOk:		clc
			rts

;
; Throttle back to 1 MHz
;
GoSlow:		pha
			php
			lda		EReg
			ora		#$80
			sta		EReg
			plp
			pla
			rts

;
; Throttle up to 2 MHz
;
GoFast:		pha
			php
			lda		EReg
			and		#$7F
			sta		EReg
			plp
			pla
			rts

;
; Get Volume size for Unit and update DIB
;
UpdVolSize: lda		#SP_Status   		; set command for SP call
			sta		CmdNum
            lda     StatParAdr          ; Set Status parameter list pointer
            sta     CmdList
            lda     StatParAdr+1
            Sta     CmdList+1
            
            ldx     SOS_Unit            ; Set unit in Param list
            inx
            stx		StatParam+1         ; smartport units start from 1
            lda     #$00                ; Set status call type - 00
            Sta     StatParam+4
            
            jsr     SmartPort           ; call smartport
            bcs		@error

            lda     Buffer              ; First byte returned is the unit status
            bpl     @error              ; bit7=0 - is a char device, error
                                        ; only check that for now
            lda     Buffer+1            ; Next is the low byte of size in blocks
            ldx		SOS_Unit
			ldy		DCB_Idx,x
			sta		DIB0_Blks,y
            lda     Buffer+2            ; and then the mid byte of size 
			sta		DIB0_Blks+1,y
            rts
            
@error:     jmp     NoDevice

;
; jsr to card prodos driver
; We update the address based on the slot and firmware CxFF byte
;
ProDriver:  
			sei                         ; disable interrupts while we have the zeropage changed
			lda     #$03                ; Set Zeropage to 3 to disable A3 Enhanced Indirect addressing
            sta     ZPReg     
            jsr     SaveMem             ; save and swap in card screen hole & zeropage
            jsr		GoSlow              ; down to 1MHz to ensure compatibility
PDEntry:    jsr     $0000               ; Update this address
			jsr		GoFast              ; back to 2MHz again
            jsr     RestMem             ; save and swap out card screen hole & zeropage
            ldx     #$18                ; Restore SOS Zero page (keep error in A)
            stx     ZPReg
            rts

; jsr to card SmartPort driver
; We update the address based on the slot and firmware CxFF byte + 3
SmartPort:  
			sei                         ; disable interrupts while we have the zeropage changed
            lda     #$03                ; Set Zeropage to 3 to disable A3 Enhanced Indirect addressing
            sta     ZPReg     
            jsr     SaveMem             ; save and swap in card screen hole & zeropage
            jsr		GoSlow              ; down to 1MHz to ensure compatibility
SPEntry:    jsr     $0000               ; Update this address
CmdNum:     .byte   $00
CmdList:    .word   $0000
			jsr		GoFast              ; back to 2MHz again
            jsr     RestMem             ; save and swap out card screen hole & zeropage
            ldx     #$18                ; Restore SOS Zero page (keep error in A)
            stx     ZPReg
            rts
;
; Save current screenholes and restore peripheral card values (current slot + slot0)
; Save zeropage $20-$4F & xbyte for pointer
;
SaveMem:    lda     E_IER             
            and     #$18                ; See if either CB1 or CB2 interrupts are enabled
            beq     SkipVbl             ; No, they are not, skip the wait for vbl
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

SkipVbl:    bit     CWrtOff          ; Then disable font loading

            jsr     SwapScrH         ; Swap in the screenhole data with ours
            jsr     SwapZero         ; Swap in the driver zeropage values
            rts

;
; Save Card screen holes and restore original values
; Restore zeropage $20-$4F & xbyte
;
RestMem:
            pha
            php                         ; save error indication in carry
            jsr     SwapZero            ; Swap back the original zeropage values
            jsr     SwapScrH            ; Swap back the original screenhole data
            plp                         ; restore error indication in carry
            pla
            rts
;            
; Swap screenholes with driver values 
; we do Slot 0 and current slot values
; This nice code is from Peter Ferrie 
;
SwapScrH:
            lda     #$07             ; Init ZP $D0-$D1, X and A
            sta     ScreenBase+1
            lda     #$00
            sta     ScreenBase
            ldx     #$0F
            lda     #$F8
@loop:
            tay                      ; A holds screen page index at loop entry
            lda     (ScreenBase),Y   
            pha                      ; Save current screen hole byte on stack.
            lda     TmpScrH,X
            sta     (ScreenBase),Y   ; Restore screen hole byte from array 
            pla
            sta     TmpScrH,X       ; Copy saved screen hole byte into array.
            dex
            bmi     done             ; Exit when array is full (all 16 bytes are copied, X<0).
            txa                      ; TXA/LSR tests whether array index is odd or even
            lsr                      ; and sets carry accordingly (1 = odd).
            tya                      ; Bring screen index into A for manipulation
            eor     DIB0_Slot        ; Cycle page index between $x8 and $x8+n as long as N in 1..7
            bcc     @loop            ; Take branch every other loop, using array index odd/even 
                                     ;  (carry still valid from TXA/LSR)
            eor     #$80             ; Cycle page index between $F8 and $78
            bpl     @loop            ; If flipping from $78->$F8 (now negative), continue
            dec     ScreenBase+1     ; Go to next page counting down ($07->$06, etc.)
            bne     @loop            ; Always -- equiv. to BRA or JMP.  Never reaches 0.
done:       rts

;
; Swap zeropage with driver values
; save all of $20 - $4F
;
SwapZero:   ldx     #$2f
@loop2:
            lda     $20,X   
            pha                      ; Save current zeropage byte on stack.
            lda     TmpZero,X
            sta     $20,X            ; Restore zeropage byte from temp storage 
            pla
            sta     TmpZero,X        ; Copy saved zeropage byte into temp storage.
            dex
            bpl     @loop2           ; Exit when temp storage is full
            rts


			.ENDPROC
			.END