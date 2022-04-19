use std::ops::Not;

use crate::common::*;
use crate::elf::{
	Endianness
};

/* tuple: (mask, signature) */
pub const BX_LR: (u16, u16) = (0xffff, 0x4770);
pub const POP_T1: (u16, u16) = (0xff00, 0xbd00);
pub const POP_T2: (u32, u32) = (0xa000_ffff, 0x8000_e8bd);
pub const POP_T3: (u32, u32) = (0xffff_ffff, 0xfb04_f85d);

#[derive(Debug)]
pub enum Instruction {
	LdrT1,
	LdrT2,
	BT1,
	BT2,
	BT3,
	BT4,
	Bl,
	CbzCbnz,
}

#[inline] pub fn sign_extend_16(mut value: u16, mut bits: u8) -> i16 {
	bits -= 1;
	if value & (1 << bits) == (1 << bits) {
		value = ((2_u16.pow((16 - bits) as u32) - 1) << bits) | (value & (2_u16.pow(bits as u32) - 1))
	} 
	value as i16
}
#[inline] pub fn sign_extend_32(mut value: u32, mut bits: u8) -> i32 {
	bits -= 1;
	if value & (1 << bits) == (1 << bits) {
		value = ((2_u32.pow((32 - bits) as u32) - 1) << bits) | (value & (2_u32.pow(bits as u32) - 1))
	} 
	value as i32
}

#[inline] fn cond_from_branch_t3(byte0: u8, byte1: u8) -> u8 { byte0 >> 6 + (byte1 & 0x03) }

#[inline] pub fn decode_branch_t1(opcode: [u8; 2]) -> (i16, u8) { 
	(sign_extend_16((opcode[0] as u16) << 1, 9), opcode[1] & 0xf) 
}
#[inline] pub fn decode_branch_t2(opcode: [u8; 2]) -> i16 { 
	sign_extend_16(((opcode[0] as u16) + (((opcode[1] as u16) & 0x07) << 8)) << 1, 12)
}
pub fn decode_branch_t3(opcode: [u8; 4]) -> (i32, u8) {
	let imm11: u32 = (opcode[2] as u32) + (((opcode[3] as u32) & 0x07) << 8);
	let imm6: u32 = (opcode[0] & 0x3f) as u32;
	let j1: u32 = ((opcode[3] & 0x20) >> 5) as u32;
	let j2: u32 = ((opcode[3] & 0x08) >> 3) as u32;
	let s: u32 = ((opcode[1] & 0x04) >> 2) as u32;
	let cond = ((opcode[0] & 0xc0) >> 6) + (opcode[1] & 0x03);
	let imm21 = (imm11 + (imm6 << 11) + (j1 << 17) + (j2 << 18) + (s << 19)) << 1;
	(sign_extend_32(imm21, 21), cond)
}
pub fn decode_branch_t4(opcode: [u8; 4]) -> i32 {
	let imm11: u32 = (opcode[2] as u32) + (((opcode[3] as u32) & 0x07) << 8);
	let imm10: u32 = (opcode[0] as u32) + (((opcode[1] as u32) & 0x03) << 8);
	let s: u32 = ((opcode[1] & 0x04) >> 2) as u32;
	let j1: u32 = ((opcode[3] & 0x20) >> 5) as u32;
	let j2: u32 = ((opcode[3] & 0x08) >> 3) as u32;
	let (i1, i2): (u32, u32) = ( !(j1 ^ s) & 0x01, !(j2 ^ s) & 0x01 );
	let imm25 = (imm11 + (imm10 << 11) + (i2 << 21) + (i1 << 22) + (s << 23)) << 1;
	sign_extend_32(imm25, 25)
}
pub fn decode_cbz_cbnz(opcode: [u8; 2]) -> (u8, u8, u8) {
	// Tuple components: (1) encoded offset, (2) operation bit (zero or nonzero), (3) compare register
	( (opcode[0] >> 3 | ((opcode[1] & 0x02) << 4)) << 1, (opcode[1] & 0x08) >> 3, opcode[0] & 0x07)
}

#[inline] pub fn encode_branch_t1(mut offset: i16, cond: u8) -> [u8; 2] {
	offset >>= 1;
	[ ((offset) & 0xff) as u8, (cond & 0xf) + 0xd0 ]
}
#[inline] pub fn encode_branch_t2(mut offset: i16) -> [u8; 2] {
	offset >>= 1;
	[ ((offset) & 0xff) as u8, 0xe0 + ((offset >> 8) & 0x07) as u8 ]
}
#[inline] pub fn encode_ldr_lit_t1(reg: u8, mut offset: u16) -> [u8; 2] {
	offset >>= 2;
	[ offset as u8, 0b01001000 + (reg & 0x07) ]
}
#[inline] pub fn encode_ldr_lit_t2(reg: u8, mut offset: u16) -> [u8; 4] {
	let add = if offset >= 0 { 1 } else { 0 };
	[
		(add << 7) + 0b01011111,
		0b11111000,
		(offset & 0xff) as u8,
		((offset >> 8) as u8) + (reg << 4)
	]
}
pub fn encode_branch_t3(mut offset: i32, cond: u8) -> [u8; 4] {
	let mut insn: [u8; 4] = [0; 4];
	let sign: u8 = match offset < 0 {
		true => 0x1, false => 0x0
	};
	offset >>= 1;

	let imm11: u16 = (offset & 0b11111111111) as u16;
	let imm6: u8 = ((offset >> 11) & 0b111111) as u8;
	let j2: u8 = ((offset >> 21) & 0x01) as u8;
	let j1: u8 = ((offset >> 22) & 0x01) as u8;
	
	insn[0] = imm6 + ((cond & 0x03) << 6);
	insn[1] = 0xf0 | (sign << 2) | (cond & 0xc);
	insn[2] = (imm11 & 0xff) as u8;
	insn[3] = 0x80 | (j1 << 5) | (j2 << 3) | (((imm11 & 0x700) >> 8) as u8);

	insn
}

fn encode_branch_wide_nocond(mut offset: i32, bl: bool) -> [u8; 4] {
	let mut insn: [u8; 4] = [0; 4];
	let sign: u8 = match offset < 0 {
		true => 0x1, false => 0x0
	};
	offset >>= 1;

	let imm11: u16 = (offset & 0b0111_1111_1111) as u16;
	let imm10: u16 = ((offset & 0b0001_1111_1111_1000_0000_0000) >> 11) as u16;
	let i2: u8 = ((offset & 0b0010_0000_0000_0000_0000_0000) >> 21) as u8;
	let i1: u8 = ((offset & 0b0100_0000_0000_0000_0000_0000) >> 22) as u8;
	
	let j1: u8 = ((!i1) ^ sign) & 0x1;
	let j2: u8 = ((!i2) ^ sign) & 0x1;
	insn[0] = (imm10 & 0xff) as u8;
	insn[1] = 0xf0 | (sign << 2) | (((imm10 & 0x300) >> 8) as u8);
	insn[2] = (imm11 & 0xff) as u8;
	insn[3] = if bl { 0xd0 } else { 0x90 } | (j1 << 5) | (j2 << 3) | (((imm11 & 0x700) >> 8) as u8);

	insn
}
#[inline] pub fn encode_branch_t4(offset: i32) -> [u8; 4] { encode_branch_wide_nocond(offset, false) }
#[inline] pub fn encode_bl(offset: i32) -> [u8; 4] { encode_branch_wide_nocond(offset, true) }
#[inline] pub fn encode_cbz_cbnz(mut offset: u8, operation: u8, reg: u8) -> [u8; 2] {
	offset >>= 1;
	[ ((offset & 0x1f) << 3) | (reg & 0x07), 0xb1 | ((operation & 0x01) << 3) | ((offset & 0x20) >> 5) ]
}

pub fn modify_popt1(data: &mut [u8]) {
	if data.len() != 4 {
		print_err_panic!("Exactly 4 bytes need to be passed to modify_popt1!");
	}

	const POPT2_HW1: [u8; 2] = [ 0xbd, 0xe8 ];
	// Byte 0 of T1 can be copied to Byte 2 of T2
	data[2] = data[0];
	data[0] = POPT2_HW1[0];
	data[1] = POPT2_HW1[1];
	data[3] = 0x40;
}
pub fn modify_popt2(data: &mut [u8]) {
	if data.len() != 4 {
		print_err_panic!("Exactly 4 bytes need to be passed to modify_popt2!");
	}

	clear_bit!(data[3], 7);
	set_bit!(data[3], 6);
}
pub fn modify_popt3(data: &mut [u8]) {
	if data.len() != 4 {
		print_err_panic!("Exactly 4 bytes need to be passed to modify_popt3!");
	}

	data[3] &= 0b11101111;
}

pub fn is_return(data: &[u8], endian: Endianness) -> Option<ReturnType> {
	match data.len() {
		2 => {
			let value: u16 = match endian {
				Endianness::Big => u16::from_be_bytes(data.try_into().expect("")),
				Endianness::Little => u16::from_le_bytes(data.try_into().expect("")),
			};
			match true {
				_x if (value & BX_LR.0 == BX_LR.1) => Some(ReturnType::Bxlr),
				_y if (value & POP_T1.0 == POP_T1.1) => Some(ReturnType::PopT1),
				_ => None,
			}
		},
		4 => {
			let value: u32 = match endian {
				Endianness::Big => u32::from_be_bytes(data.try_into().expect("")),
				Endianness::Little => u32::from_le_bytes(data.try_into().expect("")),
			};
			match true {
				_x if ((value as u16) & BX_LR.0 == BX_LR.1) => Some(ReturnType::Bxlr),
				_y if ((value as u16) & POP_T1.0 == POP_T1.1) => Some(ReturnType::PopT1),
				_z if (value & POP_T2.0 == POP_T2.1) => Some(ReturnType::PopT2),
				_w if (value & POP_T3.0 == POP_T3.1) => Some(ReturnType::PopT3),
				_ => None,
			}
		},
		_ => print_err_panic!("Byte slice has invalid length for return operation."),
	}
}

pub fn adjust_instruction(opcode_data: &[u8], pos: usize, fsize: u32, shift_map: &Vec<(usize, usize)>) -> Option<([u8; 4], bool)> {
	let mut opcode = match opcode_data.len() {
		2 => [ opcode_data[0], opcode_data[1], 0, 0 ],
		4 => opcode_data.try_into().unwrap(),
		_ => panic!("Invalid instruction length!"),
	};

	let instr_type = match true {
		_ if opcode[1] & 0xf8 == 0x48 => Instruction::LdrT1,
		_ if opcode[0] & 0x7f == 0x5f && opcode[1] == 0xf8 => Instruction::LdrT2,
		_ if opcode[1] & 0xf0 == 0xd0 => Instruction::BT1,
		_ if opcode[1] & 0xf8 == 0xe0 => Instruction::BT2,
		_ if (opcode[1] & 0xf8 == 0xf0) && (opcode[3] & 0xd0 == 0x80) => Instruction::BT3,
		_ if (opcode[1] & 0xf8 == 0xf0) && (opcode[3] & 0xd0 == 0x90) => Instruction::BT4,
		_ if (opcode[1] & 0xf8 == 0xf0) && (opcode[3] & 0xd0 == 0xd0) => Instruction::Bl,
		_ if (opcode[1] & 0xf5 == 0xb1) => Instruction::CbzCbnz,
		_ => return None,
	};

	let mut offset: i32 = match instr_type {
		Instruction::LdrT1 => (opcode[0] << 2)  as i32,
		Instruction::LdrT2 => ((opcode[2] as i32) + (( (opcode[3] as i32) & 0xf) << 8)) * (if (opcode[0] >> 7) == 1 { 1 } else { -1 }),
		Instruction::BT1 => decode_branch_t1([ opcode[0], opcode[1] ]).0 as i32,
		Instruction::BT2 => decode_branch_t2([ opcode[0], opcode[1] ]) as i32,
		Instruction::BT3 => decode_branch_t3(opcode).0,
		Instruction::BT4 => decode_branch_t4(opcode),
		Instruction::Bl => decode_branch_t4(opcode),	/* BL is identical to B.W T4 except for one bit */
		Instruction::CbzCbnz => decode_cbz_cbnz([ opcode[0], opcode[1] ]).0 as i32,
	};
	if offset == 0 || offset == -4 || (offset.abs() as u32) > fsize { return None; }

	let wide: bool = match instr_type {
		Instruction::LdrT2 | Instruction::BT3 | Instruction::BT4 | Instruction::Bl => true,
		_ => false
	};
	let dest = (pos as isize) + (offset as isize) + 4; //(if wide { 4 } else { 2 });

	// Exclude some cases that do require no adjustments.
	if pos < shift_map[0].0 && dest < (shift_map[0].0 as isize) {
		// Instruction and its destination are before first shift, no adjustments required.
		//println!("Instruction at {} and its destination {} are before any shift, therefore not affected!", pos, dest);
		return None;
	} else if let Some(last) = shift_map.last() && pos >= last.0 && dest > ((last.0 + last.1) as isize) {
		// Instruction and its destination are behind last shift, no adjustments required.
		//println!("Instruction at {} and its destination {} are behind the last shift, therefore not affected!", pos, dest);
		return None;
	}

	for shift in shift_map.iter() {
		// Skip if instruction is not affected by this shift.
		if pos > shift.0 {
			if dest > ((shift.0 + shift.1) as isize) {
				//println!("Skipping shift, instruction is behind shift and refers behind shift range");
				continue; 
			} else {
				// Destination of instruction is within shift range.
				//println!("Instruction at {} is behind shift origin but its destination {} is within shift range!", pos, dest);
				offset += match true {
					_ if offset < 0 => (shift.1 as i32) * -1,
					_ => shift.1 as i32,
				};
			}
			
		} else if pos > shift.0 && dest <= (shift.0 as isize) { 
			// Negative offset is increased
			//println!("Instruction behind shift but destination before shift. Pos {}, Offset {}, subtracting {}", pos, offset, shift.1);
			offset -= shift.1 as i32;
		} else { 
			// Positive offset is increased
			//println!("Instruction before shift and destination behind shift. Pos {}, Offset {}, adding {}", pos, offset, shift.1);
			offset += shift.1 as i32;
		}
	}

	match instr_type {
		Instruction::LdrT1 => { let oc = encode_ldr_lit_t1(opcode[1] & 0x07, offset as u16); opcode[0] = oc[0]; opcode[1] = oc[1]; },
		Instruction::LdrT2 => opcode = encode_ldr_lit_t2(opcode[3] >> 4, offset as u16),
		Instruction::BT1 => {
			let oc = encode_branch_t1(offset as i16, decode_branch_t1([ opcode[0], opcode[1] ]).1);
			opcode[0] = oc[0]; opcode[1] = oc[1]; 
		},
		Instruction::BT2 => { let oc = encode_branch_t2(offset as i16); opcode[0] = oc[0]; opcode[1] = oc[1]; },
		Instruction::BT3 => opcode = encode_branch_t3(offset, cond_from_branch_t3(opcode[0], opcode[1])),
		Instruction::BT4 => opcode = encode_branch_t4(offset),
		Instruction::Bl => opcode = encode_bl(offset),
		Instruction::CbzCbnz => {
			if offset < 0 { print_err_panic!("Negative offset not possible with CBZ/CBNZ!"); }
			let params = decode_cbz_cbnz([ opcode[0], opcode[1] ]);
			let oc = encode_cbz_cbnz(offset as u8, params.0, params.1);
			opcode[0] = oc[0]; opcode[1] = oc[1];
		}
	}

	Some( (opcode, wide) )
}