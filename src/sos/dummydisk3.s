;
; placeholder for the internal driver, originally the disk3 driver.
; free space is now free for future use
; 
            .setcpu "6502"
            .SEGMENT   "CODE"
            .INCLUDE   "SOSORG"            
            .ORG       ORGDISK3
ZZORG:            
;
;
Disk3start  =       *
Disk3End    =       $EE04

; add padding to make the same length as the disk3 driver
PADDING      =         Disk3End - Disk3start
             .RES      PADDING
;
;
ZZEND       =          *
ZZLEN       =          ZZEND-ZZORG

            .IF        ZZLEN-LENDISK3
            .FATAL     "SOSORG FILE IS INCORRECT FOR DISK3"
            .ENDIF
