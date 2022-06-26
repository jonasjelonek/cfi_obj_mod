# cfi_obj_mod
A tool to integrate my developed CFI-check into ELF object files for ARMv7-M.
This work is part of my bachelor thesis, begun in November 2021, finished in April 2022.

## What exactly for?

In my bachelor thesis I developed and implemented a coarse-grained CFI check for ARMv7-M based embedded systems. The implementation provided two ways for integrating the changes into an application by instrumentalizing the application code. This is the actual tool beside cfi_asm_mod implementing one of these ways. 

## How it works

The build system should compile all object files, but do not link them yet. The call of cfi_obj_mod needs to include a configuration file that specifies which object files will be analysed and modified. The tool then applies all modifications and resulting adjustments in each object file and writes it back to the same file. Afterwards, the build system can continue to link all object files into the final application.

More detailed explanation will be here soon! 
If you're good at reading and understanding code, you can also figure out the exact process by looking into the source code ;)

## Questions, hints, general interest

Of course, you can always contact me if you have any questions or hints regarding this tool or in general my bachelor thesis. You can find my contact information in my profile.
All hints and questions regarding the purpose of this tool and its implementation, or regarding my bachelor thesis are welcome. The same applies to the Rust code, as I am still learning heavily writing code in Rust at the time of writing this.
