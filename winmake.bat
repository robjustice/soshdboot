@echo off
rem
rem soshdboot Windows makefile
rem
rem clunky, but does the job. 
rem borrowed some better ideas from 4cade make, thanks qkumba
rem
rem Variables
setlocal enabledelayedexpansion

rem Build stuff
SET AC=build\ac.jar
SET CA65=build\ca65.exe
SET LD65=build\ld65.exe
SET A3DUTIL=build\A3Driverutil.py
SET BOOTLOADER=build\bootloader.py

SET PYTHON=C:\python27\python.exe

SET BOOTDISK=disks\soshdboot.dsk
SET BOOTTDMDISK=disks\soshdboot_tdm.dsk
SET DISK=disks\sos_selector_hd.po
SET TDMDISK=disks\sos_selector_tdm_hd.po
SET PLDISK=disks\plasma_hd.po


if "%1" equ "sos" (
call :md
call :sos
goto :EOF
)

if "%1" equ "rom" (
call :md
call :rom
goto :EOF
)

if "%1" equ "disk3" (
call :md
call :disk3
goto :EOF
)

if "%1" equ "boot" (
call :md
call :boot
goto :EOF
)

if "%1" equ "all" (
:all
echo off
call :md
call :sos
call :rom
call :boot
call :disk3
goto :EOF
)

if "%1" equ "clean" (
:clean
echo y|1>nul 2>nul rd lst /s
echo y|1>nul 2>nul rd obj /s
echo y|1>nul 2>nul rd out /s
goto :EOF
)

echo usage: %0 clean / sos / boot / rom / disk3 / all
goto :EOF

:md
2>nul md lst\sos
2>nul md lst\rom
2>nul md lst\disk3
2>nul md lst\boot
2>nul md obj\sos
2>nul md obj\rom
2>nul md obj\disk3
2>nul md obj\boot
2>nul md out
goto :EOF

rem Assemble SOS and add SOS.KERNEL to the disk images
:sos
%CA65% src/sos/sosldr.s -l lst/sos/sosldr.lst -o obj/sos/sosldr.o
%CA65% src/sos/init.s -l lst/sos/init.lst -o obj/sos/init.o
%CA65% src/sos/sysglob.s -l lst/sos/sysglob.lst -o obj/sos/sysglob.o
%CA65% src/sos/bfm.init2.s -l lst/sos/bfm.init2.lst -o obj/sos/bfm.init2.o
%CA65% src/sos/print.s -l lst/sos/bfm.lst -o obj/sos/bfm.o
%CA65% src/sos/oprmsg.s -l lst/sos/oprmsg.lst -o obj/sos/oprmsg.o
%CA65% src/sos/ipl.s -l lst/sos/ipl.lst -o obj/sos/ipl.o
%CA65% src/sos/umgr.s -l lst/sos/umgr.lst -o obj/sos/umgr.o
%CA65% src/sos/problock3kernel.s -l lst/sos/problock3kernel.lst -o obj/sos/problock3kernel.o
%CA65% src/sos/syserr.s -l lst/sos/syserr.lst -o obj/sos/syserr.o
%CA65% src/sos/scmgr.s -l lst/sos/scmgr.lst -o obj/sos/scmgr.o
%CA65% src/sos/fmgr.s -l lst/sos/fmgr.lst -o obj/sos/fmgr.o
%CA65% src/sos/cfmgr.s -l lst/sos/cfmgr.lst -o obj/sos/cfmgr.o
%CA65% src/sos/devmgr.s -l lst/sos/devmgr.lst -o obj/sos/devmgr.o
%CA65% src/sos/bufmgr.s -l lst/sos/bufmgr.lst -o obj/sos/bufmgr.o
%CA65% src/sos/memmgr.s -l lst/sos/memmgr.lst -o obj/sos/memmgr.o
%CA65% src/sos/padding.s -l lst/sos/padding.lst -o obj/sos/padding.o
%LD65% obj/sos/sosldr.o obj/sos/init.o obj/sos/sysglob.o obj/sos/bfm.init2.o obj/sos/bfm.o obj/sos/oprmsg.o obj/sos/ipl.o obj/sos/umgr.o obj/sos/Problock3Kernel.o obj/sos/syserr.o obj/sos/devmgr.o obj/sos/scmgr.o obj/sos/fmgr.o obj/sos/cfmgr.o obj/sos/bufmgr.o obj/sos/memmgr.o obj/sos/padding.o -o out/SOS.KERNEL#0C0000 -C build/apple3.cfg
java -jar %AC% -d %DISK% SOS.KERNEL
java -jar %AC% -p %DISK% SOS.KERNEL SOS $0000 < out/SOS.KERNEL#0C0000
java -jar %AC% -d %TDMDISK% SOS.KERNEL
java -jar %AC% -p %TDMDISK% SOS.KERNEL SOS $0000 < out/SOS.KERNEL#0C0000
java -jar %AC% -d %PLDISK% SOS.KERNEL
java -jar %AC% -p %PLDISK% SOS.KERNEL SOS $0000 < out/SOS.KERNEL#0C0000
rem echo off
goto :EOF

rem Assemble Monitor ROM
:rom
%CA65% src/rom/diskio.s -l lst/rom/diskio.lst -o obj/rom/diskio.o
%CA65% src/rom/saratests.s -l lst/rom/saratests.lst -o obj/rom/saratests.o
%CA65% src/rom/monitor.s -l lst/rom/monitor.lst -o obj/rom/monitor.o
%LD65% obj/rom/diskio.o obj/rom/saratests.o obj/rom/monitor.o -o out/apple3.rom -C build/apple3.cfg
goto :EOF

rem Assemble disk3 driver and update SOS.DRIVER file in disk images
:disk3
%CA65% src/disk3/disk3.s -l lst/disk3/disk3.lst -o obj/disk3/disk3.o
%LD65% obj/disk3/disk3.o -o obj/disk3/disk3.o65 -C build/Apple3_o65.cfg
java -jar %AC% -g %DISK% SOS.DRIVER > out/SOS.DRIVER#0c0000
1>nul %PYTHON% %A3DUTIL% update obj/disk3/disk3.o65 out/SOS.DRIVER#0c0000
java -jar %AC% -d %DISK% SOS.DRIVER
java -jar %AC% -p %DISK% SOS.DRIVER SOS $0000 < out/SOS.DRIVER#0c0000
java -jar %AC% -g %TDMDISK% SOS.DRIVER > out/SOS.DRIVER_TDM#0c0000
1>nul %PYTHON% %A3DUTIL% update obj/disk3/disk3.o65 out/SOS.DRIVER_TDM#0C0000
java -jar %AC% -d %TDMDISK% SOS.DRIVER
java -jar %AC% -p %TDMDISK% SOS.DRIVER SOS $0000 < out/SOS.DRIVER_TDM#0C0000
java -jar %AC% -g %PLDISK% SOS.DRIVER > out/SOS.DRIVER_PL#0c0000
1>nul %PYTHON% %A3DUTIL% update obj/disk3/disk3.o65 out/SOS.DRIVER_PL#0C0000
java -jar %AC% -d %PLDISK% SOS.DRIVER
java -jar %AC% -p %PLDISK% SOS.DRIVER SOS $0000 < out/SOS.DRIVER_PL#0C0000
goto :EOF

PLASMADISK

rem Assemble Boot Loaders
:boot
%CA65% src/boot/bootloader_sos.s -l lst/boot/bootloader_sos.lst -o obj/boot/bootloader_sos.o
%LD65% obj/boot/bootloader_sos.o -o out/bootloader_1blk.bin -C build/apple3.cfg
%CA65% src/boot/bootloader_sos_tdm.s -l lst/boot/bootloader_sos_tdm.lst -o obj/boot/bootloader_sos_tdm.o
%LD65% obj/boot/bootloader_sos_tdm.o -o out/bootloader_tdm_1blk.bin -C build/apple3.cfg
%CA65% src/boot/bootloader_prodos_sos.s -l lst/boot/bootloader_prodos_sos.lst -o obj/boot/bootloader_prodos_sos.o
%LD65% obj/boot/bootloader_prodos_sos.o -o out/bootloader_2blk.bin -C build/apple3bs.cfg
%CA65% src/boot/bootloader_prodos_sos_tdm.s -l lst/boot/bootloader_prodos_sos_tdm.lst -o obj/boot/bootloader_prodos_sos_tdm.o
%LD65% obj/boot/bootloader_prodos_sos_tdm.o -o out/bootloader_tdm_2blk.bin -C build/apple3bs.cfg
1>nul %PYTHON% %BOOTLOADER% out\bootloader_1blk.bin %BOOTDISK%
1>nul %PYTHON% %BOOTLOADER% out\bootloader_tdm_1blk.bin %BOOTTDMDISK%
1>nul %PYTHON% %BOOTLOADER% out\bootloader_2blk.bin %DISK%
1>nul %PYTHON% %BOOTLOADER% out\bootloader_tdm_2blk.bin %TDMDISK%
1>nul %PYTHON% %BOOTLOADER% out\bootloader_2blk.bin %PLDISK%
goto :EOF

