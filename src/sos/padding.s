; padding to make kernel the same size as original
; original 1.3 kernel has 65 '00's at the end
;
 .org $F000     ;the linker is just adding this at then end, so value not important
 .res $41,$00   ;65 zeros
 