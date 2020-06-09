; Update bfm.init2.s
; - this is not called any more, we just include padding to keep the kernel the
;   same size.
;
; Updates by Robert Justice
; 

;SBTL "SOS 1.1  BFM.INIT2"
;.RELOC
             .SEGMENT  "CODE"
             .INCLUDE  "SOSORG"
             .ORG      ORGBFMI
ZZORG:
;MSB OFF
;***************************************************************************************************
;          COPYRIGHT (C) APPLE COMPUTER INC.  1980
;                    ALL RIGHTS RESERVED
;***************************************************************************************************
;
; BLOCK FILE MANAGER INIT2
;
;  SECONDARY INITIALIZATION ROUTINE FOR BLOCK FILE MANAGER
;
; MODIFIED: 03/25/81 TO UTILIZE NEW
;   DISK DRIVER'S SEEKDSK3 ROUTINE.
;  CHANGES MARKED BY 'D3RRA81084'
;
; MODIFIED: 08/19/81 TO WORK WITH NEW               
;   SOSLDR MODULE.
;***************************************************************************************************
;

ZZEND:
ZZLEN        =         ZZEND-ZZORG
; add padding to make $400 bytes long
PADDING      =         $400-ZZLEN
             .RES      PADDING
             .IF       $400-LENBFMI
             .FATAL    "SOSORG FILE IS INCORRECT FOR BFM.INIT2"
             .ENDIF

