# Introduction

The Apple /// has always had the annoying limitation of only being able to boot from the internal floppy drive.
I have had some thoughts previously on how to allow booting of other devices on the three, and the development of my problock3 driver re kindled these.
The result detailed here is an updated ROM and an updated version of SOS that allows booting directly from Prodos Block mode cards. 
Yay, no floppy needed!!

## Recap of the standard Apple /// boot

- The Apple /// includes only 4k of firmware in rom. This has code to look only to the internal floppy drive for booting. After some diagnostic checks are run, it looks to the internal floppy drive and loads the bootloader (block0) into $A000. It then jumps to the code at $A000 to run the bootloader.

- The bootloader then loads in block 1, block 2 through to the last linked block of the volume directory to address $A200 onwards. The volume directory is located on the disk starting from block 2, so this then means the directory blocks are in memory starting from $A400. It searches through these for the 'SOS.KERNEL' filename, and then loads that file into the highest bank at address $1E00 (determined from the bootloaders memory test). Then the sos loader code is run from $1E70.

- The SOS.KERNEL includes the sosldr code that moves the kernel into the correct memory location in the system bank, and then loads the SOS.DRIVER and SOS.INTERP from the internal floppy. The kernel actually includes in itself a driver for the disk3 that has the same structure as any other driver. This has DIB's for each of the four floppy drives. Once the kernel is loaded, it initialises itself with the first DIB of the internal Disk3 driver and then uses normal SOS system calls to loaded in the SOS.DRIVER and SOS.INTERP files. The paths for these files are hard coded into the kernel, .D1/SOS.INTERP and .D1/SOS.DRIVER. The SOS.INTERP is loaded first and moved into the highest memory bank. The SOS.DRIVER file is loaded next, and each driver is relocated to memory starting from below the interpreter. Once the drivers are relocated, then the kernel is reinitialised to link in all of the drivers loaded. The sosldr then jumps to the start address of the interpreter and runs the application.

# The updated Apple /// Hard disk boot 

The approach that I took, was to replace the sos.kernel internal disk3 driver with my problock3 driver. This seemed to allow the smallest amount of change to achieve the desired hd booting. There is a few other parts that also needed updating to support this, I'll run through them all here.

## ROM
This has had the user entry ram test removed to free up some space in the ROM. Code has been added to search the slots for a prodos block mode card, and then load block 0 into $A000. Once this is loaded, the ROM code jumps to the bootloader code.
The default boot order is:
 - 1. Search for prodos cards from slot4 to slot1, and boot if one found
 - 2. Else, if none are found boot of the internal floppy
    
If the Alpha lock key is down when the system is reset, then this will boot directly of the internal floppy.
  
If any key is pressed early/during boot, then the rom will load block0 from unit1 (second drive) of the found prodos card. This gives the opportunity to boot the second drive of a card directly. I struggled with the best key combo for this. Its something I might need to go back to and revisit.

To replace the ROM, I have tested two options so far:
- Use a TMS2532 4k eprom. This is pin compatible and can be placed straight into the A3 motherboard. These can be a bit tricky to program. I made an adapter to swap three pins to make it look like a 2732, and was able to program it that way.
- Use an adapter like this one. It adapts a standard 2764 EPROM to be able to use in the A3 socket. It is physically a bit high, not to sure if it will fit in the case. It works fine functionally, and we actually get the use 8k of firmware space with this option. (the A3 motherboard supports two 4k rombanks, I'm only using the first 4k bank)
  http://store.go4retro.com/2364-adapter/
  
## Boot loader
  
I ended up providing two different boot loaders, as the dual boot functionality became apparent during the development.
  
### Two block boot loader
This one is a modified version of the ProDOS/SOS two block boot loader. The SOS part is modified to boot SOS off the same card that the ROM loaded the boot loader from. The SOS part also includes support for booting the second unit. The ProDOS part is unmodified, giving us the ability to boot the disk as either ProDOS or SOS. This one is included in the provided hd po images.
  
The memory test has been updated based on the onthree 512k bootloader, this should support all memory combinations  (128k,256k,512k). ie loading the SOS.INTERP into the highest available bank. I have tested with 256k and 512k ram combos.
  
### One block boot loader
This one is a trimmed down SOS only boot loader designed to be used as a 'bootloader' floppy. It fits into 1 block and can be loaded by the standard Apple /// ROM. This one is included in the soshdboot.dsk floppy image, and can be used to boot the hd sos from the floppy drive. It's quite quick as we only need to load 1 block (2 sectors) off the floppy, then the rest off the prodos card. 

The memory test has been updated based on the onthree 512k bootloader, this should support all memory combinations  (128k,256k,512k). ie loading the SOS.INTERP into the highest available bank. I have tested with 256k and 512k ram combos.
 

For both of these bootloaders, there is also a Desktop Manager version. This loads things one bank below the highest available, as the highest bank is reserved for the Desktop Manager software.
  
## SOS.KERNEL
Modified SOS.KERNEL:
- The internal disk3 driver is replaced with an updated Problock3 driver. Some changes to the code to align with being loaded internally in the kernal. Also some optimisation is possible as the driver is now always in the non bank switched system bank, so no need to copy that code to the 18fx page to avoid the bank switching.
- The path names for the SOS.INTERP and SOS.DRIVER have been updated with the .PROFILE name for the problock driver. Also a second set of names is included to allow booting of the second drive.
- The default prefix is updated as required, depending on the unit that is selected for booting.
- The SOS inbuilt copy protection is removed. This speeds things up a little, and there is not much point having this if we are not booting from a floppy that may have had copy protection.
- The number of problock drives is hard coded to two. So the settings of drives via SCP is ignored.
  
## Disk3 driver
The original internal floppy driver modified to look like a standard sos driver. Set to use all four drives. Does not look to the setting in SCP. 

This is included in the SOS.DRIVER files on the hd po images.

Note: I have not tested this extensively, so be a little careful with the disks you use.
  
# Disk images ready to go
The following disk images are provided prebuilt and ready for use in the disks folder:

- selector_hd.po

    The updated bootloader, SOS.KERNEL, SOS.DRIVER, and ProDOS 2.4.2 installed
    
    Selector /// SOS.INTERP loaded on startup
    
    This image is based on the apple3rtr bos image, it needed some adjustments to suit Selector /// 
    - Advanced Visicalc - hand decoded interp added, runs now (bos was able to load protected interpreters)
    - added Apple Writer 4. from wap disks, now this runs (bos was able to load protected interpreters)
    - added rs232 driver and correct fonts, Access /// and Access 3270 run now
    - extracted correct font from Lazarus /// disk and added, now displays correctly when run
    - updated Quickfile///, now this runs
    - added some games into a games menu :)
    
    A desktop manager (tdm) version of this is also provided. This has a different boot_loader and SOS.DRIVER installed.
    

- plasma_hd.po

   This is a copy of the Alpha1 release 800k disk put onto a 16mb image with the updated bootloader, SOS.KERNEL, SOS.DRIVER installed
   
   Plasma SOS.INTERP loaded on startup
   
   Get into that fantastic plasma command line fast!
  
- soshdboot.dsk
   
   This is a blank prodos disk with a special 1 block boot loader to load sos from the harddisk.
   this can be used if the rom has not been changed. Still very quick as only one block is read from 
   floppy, the rest is from the harddisk. 
   
   A desktop manager (tdm) version of this is also provided. This has a different boot_loader installed.

# Compatibility

I have tested the following cards in combination with real Apple /// hardware

| Card | Apple3 | Titan_3plus2 | Titan_3plus2e | Comments |
| --- | --- | --- | --- | --- |
| Booti | Works great | Works great | My Booti seems to struggle with this combo. Will boot the floppy ok | Tested with the card set to block mode |
| CFFA3000 | Works great | Works great | Works great | works great with all combo's |
| CFFA v1.3 | Works great | Works great | My CFFA seems to struggle with this combo | The CF Card needs to be formatted for Apple2 mode. This means 32mb partitions which are problematic with some SOS apps. Also seems a bit intermittent on boot in slot1, rest seem ok. I think some sort of init problem, maybe zp|


# Build

The following tools are used:

- ca65 assembler    (needs to be in your path)
- ld65 linker       (needs to be in your path)
- a3driverutil.py   (included in build folder)
- Applecommander    (included in build folder)
- bootloader.py     (included in build folder)

I have also included my clunky windows make file, does the job for me. Tidied it up a bit with some ideas from qkumba's one in 4cade, thanks. You might need to update the paths in winmake to suit (sorry I coded in the python path as I was having issues with the different versions, python 2.7 required at the moment).

```
git clone https://github.com/robjustice/soshdboot
cd soshdboot
winmake all
```
This will make the SOS.KERNEL, SOS.DRIVER, Bootloaders and ROM and then update them on all of the disk images. The built ROM image,  SOS.KERNEL, SOS.DRIVER and bootloaders will be placed in the 'out' folder.

I also included my test bat files to launch MAME to test each image, these are in the test folder. You will need to update the MAME path, and create the apple3 folder in MAME's roms folder (and delete/move your apple3.zip file). MAME will throw a warning saying the ROM is incorrect due to the modifed rom, just need to hit a key. 

## One more thing..

I have also added some additional support to the Selector /// image for booting the Titan card emulations without the floppy. This was not supported with the standard Selector software. Apparently Titan refused to share the technical details with OnThree (thanks for finding that Jorma).

These are available in the Languages menu on the Selector_hd.po image. 
  
- added Titan 3plus2 emulation start. (this is a hand patched version of the Selector Apple2 emulation launcher).
  I have also patched the loaded F8 rom so it can boot of block mode cards. (only check for three sig bytes)

  Two menu options added to Selector, standard slot search (7to1) and reversed (1to6) to suit the A3.

- added Titan 3plus2e emulation start. This is a new interpreter based on disassembing the 3plus2e boot disk.
  no save setting supported yet. Its a quick put together that just loads everything into the interpreter bank and then moves everything in memory to match the original 3plus2e boot disk.
  
  Two menu options added to Selector, standard slot search (7to1) and reversed (1to6) to suit the A3
  
  Note: for this 3plus2e setup, I have had mixed results when using the Booti card and this combo. When using a CFFA3000, it works fine.
      
With either of these we can now boot Prodos from the same harddisk image without using the floppy drive :-)

I'll update here with a link to the details for the patching and source code for the 3plus2e interpreter once I tidy it up.

