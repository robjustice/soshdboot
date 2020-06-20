# Introduction

The Apple /// has always had the annoying limitation of only being able to boot from the internal floppy drive.
I have had some thoughts previously on how to allow booting of other devices on the three, and the development of my problock3 driver re kindled these.
The result detailed here is an updated ROM and version of SOS that allows booting directly from Prodos Block mode cards. Yay, no floppy needed!!

## Recap of the standard Apple /// boot

- The Apple /// includes only 4k of firmware in rom. This has code to look only to the internal floppy drive for booting. After some diagnostic checks are run, it looks to internal floppy drive and loads the bootloader (block0) into $A000. It then jumps to the code at $A000 to run the bootloader.

- The bootloader then loads in block 1, block 2 through to the last linked block of the directory to address $A200 onwards. The volume directory is located on the disk starting from block 2, so this then means the directory blocks are in memory from $A400. It searches through these for the 'SOS.KERNEL' filename, and then loads that file into the highest bank determined from its memory test starting at address $1E00. Then the sos loader code is run from $1E70.

- The SOS.KERNEL includes the sosldr code that moves the kernel into the correct memory location in the system bank, and then loads the SOS.DRIVER and SOS.INTERP from the internal floppy. The kernel actually includes in itself a driver for the disk3 that has the same structure as any other driver. This has DIB's for each of the four floppy drives. Once the kernel is loaded, it initialises itself with the first DIB of the internal Disk3 driver and then uses normal SOS system calls to loaded in the SOS.DRIVER and SOS.INTERP files. The paths for these files are hard coded into the kernel, .D1/SOS.INTERP and .D1/SOS.DRIVER. The SOS.INTERP is loaded first and moved into the highest memory bank. The SOS.DRIVER file is loaded next, and each driver is relocated to memory starting from below the interpreter. Once the drivers are relocated, then the kernel is reinitialised to link in all of the drivers loaded. The sosldr then jumps to the start address of the interpreter and runs the application.

# The updated Apple /// boot 

The approach that I took, was to replace the sos.kernel internal disk3 driver with my problock3 driver. This seemed to allow the smallest amount of change to achieve the desired hd booting. There is a few other parts that also needed updating, I'll run through that here.

  ## ROM
  This has had the user entry ram test removed to free up some space. Code has been added to search the slots for a prodos block mode card, and then load block 0 into $A000. Once loaded, the ROM code jumps to the bootloader code.
  The default boot order is:
    1. Search for prodos cards from slot4 to slot1, and boot if one found
    2. Else, if none are found boot of the internal floppy
    
  If the Alpha lock key is down when the system is reset, then this will boot directly of the internal floppy.
  
  If any key is pressed during boot, then the rom will load block0 from unit1 (second drive) of the found prodos card. This gives the opportunity to boot the second drive of a card directly.
  
  ## Boot loader
  
  I ended up providing two different boot loaders, as this would allow the dual boot of ProDOS or SOS.
  
  ### Two block boot loader
  This one is a modified version of the ProDOS/SOS two block boot loader. 
  The SOS part is modified to boot SOS off the same card that the ROM loaded the boot loader from. The SOS part also icludes support for booting the second unit.
  The ProDOS part is unmodified, giving us the ability to boot the disk as either ProDOS or SOS. This one is included in the provided hd po images.
  The memory test part has been updated based on the onthree 512k bootloader, this will support all memory combinations. ie loading the SOS.INTERP into the highest available bank.
  
  ### One block boot loader
  This one is a trimmed down SOS only boot loader designed to be used as a 'bootloader' floppy. It fits in 1 block and can be loaded by the standard Apple /// ROM. This one is included in the soshdboot.dsk floppy image, and can be used to boot the hd sos. It's quite quick as we only need to loaded 1 block (2 sectors) off the floppy, then the rest off the prodos card. 
  The memory test part has been updated based on the onthree 512k bootloader, this will support all memory combinations. ie loading the SOS.INTERP into the highest available bank.
 
  For both of these bootloaders, there is also a Desktop Manager version. This loads things one bank below the highest available, as this is reserved for the Desktop Manager software.
  
  ## SOS.KERNEL
  Modified SOS.KERNEL file.
  The internal disk3 driver is replaced with an updated Problock3 driver. Some changes to the code to align with being loaded internally in the kernal. Also some optimisation is possible as the driver is now always in the non bank switched system bank, so need to copy that code to the 18fx page to avoid the bank switching.
  The path names for the SOS.INTERP and SOS.DRIVER have been updated with the .PROFILE name for the problock driver. Also a second set of names is included to allow booting of the second drive.
  The default prefix is updated as required.
  The SOS inbuilt copy protection is removed. This speeds things up a little, and there is not mmuch point having this if we are not booting of floppy.
  The number of drives is hard coded to two. So the settings of drives via SCP is ignored.
  
  ## Disk3 driver
  The original internal floppy driver modified to look like a standard sos driver. This is included in the SOS.DRIVER files on the hd po images. Built using ca65 and the a3driverutil.
  
# Disks
The following disk images are provided prebuilt and ready for use:

- selector_hd.po

    The updated bootloader, SOS.KERNEL, SOS.DRIVER, ProDOS 2.4.2 installed
    
    Selector /// SOS.INTERP loaded on startup
    
    This image is based on the apple3rtr bos image, it needed some adjustments to suit Selector /// 
    - Advanced Visicalc - hand decoded interp added, runs now (bos was able to load protected interpreters)
    - added Apple Writer 4. from wap disks, now this runs (bos was able to load protected interpreters)
    - added rs232 driver and correct fonts, Access /// and Access 3270 run now
    - extracted correct font from Lazarus /// disk and added, now displays correctly when run
    - updated Quickfile///, now this runs
    - added some games into a games menu :)
    

- plasma_hd.po

   The updated bootloader, SOS.KERNEL, SOS.DRIVER, and ProDOS 2.4.2 installed
   
   Plasma SOS.INTERP loaded on startup
   
   Get into that fantastic plasma command line fast!
  
- soshdboot.dsk
   
   This is a blank prodos disk with a special 1 block boot loader to load sos from the harddisk.
   this can be used if the rom has not been changed. Still very quick as only one block is read from 
   floppy, the rest is from the harddisk. 

# Build

I have included my clunky windows make file, does the job for me. Tidied it up a bit with some ideas from qkumba's one in 4cade, thanks.

The following tools are used and included in the build folder:
ca65 assembler 
ld65 linker
a3driverutil.py
Applecommander
bootloader.py


## One more thing..

I have also added some additional support to the Selector /// image for booting the Titan card emulations. This was not supported with the standard Selector software. Apparently Titan would not share the technical details with OnThree.
These are available in the Languages menu on the Selector_hd.po image. 
  
- added Titan 3plus2 emulation start. (this is a hand patched version of the Selector Apple2 emulation launcher). 

This has also had the 2+ rom changed to search the boot slots from S1 to S6

- added Titan 3plus2e emulation start. This is a new interpreter based on disassembing the 3plus2e boot disk.
  no save setting supported yet. 
  
  Two menu options added to Selector, standard slot search (7to1) and reversed (1to6) to suit the A3
  
  Note: for this 3plus2e setup, I have had mixed results when using the Booti card and this combo. When using a CFFA3000, it works fine.
      
With either of these we can now boot Prodos from the same harddisk image :-)

I'll update here with a link to the details for the patching and source code for the 3plus2e interpreter once I tidy it up.
