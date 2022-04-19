# cfi_obj_mod
A tool to integrate my developed CFI-check into ELF object files for ARMv7-M

## What exactly for?

In my bachelor thesis I developed and implemented a coarse-grained CFI check for ARMv7-M based embedded systems. The implementation provided two ways for integrating the changes into an application by instrumentalizing the application code. This is the actual tool beside cfi_asm_mod implementing one of these ways. 

## How it works

The build system should compile all object files, but do not link them yet. The call of cfi_obj_mod needs to include a configuration file that specifies which object files will be analysed and modified. The tool then applies all modifications and resulting adjustments in each object file and writes it back to the same file. Afterwards, the build system can continue to link all object files into the final application.

More detailed explanation will be here soon! 
If you're good at reading and understanding code, you can also figure out the exact process by looking into the source code ;)
