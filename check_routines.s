.thumb
.syntax unified

.section .text

.extern	Default_Handler					@ Default_Handler should be present in binaries for SiLabs EFR32MG12 Cortex-M4

.global _cfi_check_ra
.type   _cfi_check_ra, %function

@ What needs to be added to beginning of function prolog
@	push	{r4}					@ Save reg
@	ldr		r4, <addr>				@ Load secret
@	eor		lr, lr, r4				@ Obfuscate return address
@	pop		{r4}					@ Restore reg

@ Routine to verify the address in lr is valid
.thumb_func
_cfi_check_ra:
	push	{r3, r4, r5}						@ Save registers
@	ldr		r3, <addr>							@ Load secret
@	eor		lr, lr, r3							@ Deobfuscate return address

	mov		r3, #0xFFFF							@ Upper code space address
	movt	r3, #0x3FFF
	cmp		lr, r3
	it		hs									@ Unsigned higher or same
	bhs		exception_handler

	mov		r3, lr								@ Copy LR address to reg

#	ldr		r3, [r3, #-5]						@ Load instruction from modified LR address
												@ Load operation above sometimes has strange behaviour (wrong data loaded)
	sub		r4, r3, #5							@ Decrease address (-4 because we want previous instruction, -1 due to execution state bit)
	ldr		r3, [r4]							@ Load opcode into r3

	@ Building the AND mask
	mov		r4, #0xF800							@ Put low hword 0xF800 to r4
	movt	r4, #0xD000							@ Put top hword 0xD000 to r4

	and		r5, r3, r4							@ Apply mask on opcode 

	@ Compare with BL signature
	mov		r4, #0xF000							@ Put 0xF000 to register
	movt	r4, #0xD000							@ Put 0xD000 to top halfword of register
	cmp		r5, r4								@ Compare masked value with signature
	itt		eq									@ ARM conditional block
	popeq	{r3, r4, r5}						@ Restore registers
	bxeq	lr									@ Leave

	@ Check for BLX
	mov		r4, #0x0000							@ Clear r4
	movt	r4, #0x4787							@ AND mask
	and		r5, r3, r4							@ Apply mask on opcode
	movt	r4, #0x4780							@ Signature of BLX
	cmp		r5, r4								@ Compare masked value with signature
	itt		eq
	popeq	{r3, r4, r5}
	bxeq	lr

	@b		Default_Handler						@ Jump to Default Exception Handler

	.size	_cfi_check_ra, .-_cfi_check_ra	@ Calculate and set symbol size

exception_handler:
	b		Default_Handler