rem test selector_hd.po image
rem
rem adjust to your mame path
set MAMEPATH=C:\Storage\_emu\mame

rem copy in the new apple3 rom, assumes the apple3 folder to be there
copy /b ..\rom\apple3hdboot.rom %MAMEPATH%\roms\apple3\soshdboot.bin

rem run mame ( note, -nothrottle in command for speedy testing)
%MAMEPATH%\mame apple3 -rompath %MAMEPATH%\roms -bios 1 -skip_gameinfo -resolution 640x480 -window -nothrottle -sl1 cffa2 -hard1 ..\disks\sos_selector_hd.po -sl2 softcard3
