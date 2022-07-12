#![feature(seek_stream_len)]
#![feature(let_chains)]

#[macro_use]
pub mod common;
pub mod config;
pub mod elf;
pub mod instruction;

use std::env;
use std::fs::{File};
use std::io::{BufReader};
use std::path::{PathBuf};
use std::time::Instant;

use elf::{SymbolVisibility, SymbolIndex, SymbolBinding};

use crate::common::*;
use crate::elf::{
	ElfFile32, 
	ElfKind,
	Endianness,
	Relocation32,
	SectionIndex,
	Section32, 
	SymbolTable32, 
	Symbol32, 
	SymbolKind, RelocationTable32, SectionHeader32, SectionKind
};

const CFI_CHECKCODE_NAME: &'static str = "_cfi_check_ra";
const NOPN: [u8; 2] = [ 0x00, 0xbf ];
const BW_PLACEHOLDER: [u8; 4] = [ 0xff, 0xf7, 0xfe, 0xbf ];

fn main() {
	let args: Vec<String> = env::args().collect();
	let cfg: config::WorkflowConfig = config::parse_cfg_file(&args[1]);
	let start = Instant::now();

	/* Check whether CFIF is already available, if not then build it */
	let mut cfi_framework_source: PathBuf = PathBuf::new();
	if !cfg.cfi_framework_precompiled {
		print_hint!("CFI framework needs to be compiled");
		let operation =  match &cfg.cfi_compile_operation {
			Some(x) => x,
			None => print_err_panic!("Compile operation description is mandatory when pre-compiled is not set!")
		};

		match operation.execute() {
			Ok(()) => { cfi_framework_source = PathBuf::from(cfg.cfi_compile_operation.unwrap().output) },
			Err(e) => print_err_panic!("Failed to compile CFI framework: {}", e),
		}
	}
	print_ok!("Compiled cfi framework to '{}' in {} msecs", cfi_framework_source.to_str().unwrap(), start.elapsed().as_secs_f64() * 1000f64);

	for target in cfg.process_targets.iter() {
		if !target.exists() {
			match cfg.skip_invalid_targets {
				true => {
					print_skip!("Skipping target '{}' cause file does not exist!", target.to_str().unwrap());
					continue;
				},
				false => print_err_panic!("Process aborted, target '{}' does not exist!", target.to_str().unwrap())
			}
		}
		print_ok!("Processing target '{}'", target.to_str().unwrap());
		process_target(target);
	}
}

fn process_target(path: &PathBuf) -> () {
	let mut file_read: BufReader<File> = BufReader::new(File::open(path).unwrap());
	let mut start = Instant::now();

	let obj_file_hdr = match ElfFile32::parse_header(&mut file_read) {
		Ok(h) => h,
		Err(e) => print_err_panic!("Cannot parse object file header of '{}': {}", path.to_str().unwrap(), e)
	};
	if obj_file_hdr.file_kind() != ElfKind::Relocatable {
		print_err!("Target is not a relocatable object file, it is {:?} ! ('{}')", obj_file_hdr.file_kind(), path.to_str().unwrap_or(""));
		print_hint!("Parsing took {} msecs!", start.elapsed().as_secs_f64() * 1000f64);
		return;
	}

	let mut obj_file: ElfFile32 = match ElfFile32::parse(obj_file_hdr, &mut file_read) {
		Ok(x) => x,
		Err(e) => print_err_panic!("Cannot parse object file '{}': {}", path.to_str().unwrap(), e)
	};
	print_hint!("Parsing took {} msecs!", start.elapsed().as_secs_f64() * 1000f64);

	let endian = obj_file.endian().unwrap();
	if let Some(symtab) = &mut obj_file.symbol_table {
		for section in obj_file.sections.iter_mut() {
			if section.name.starts_with(".text") {
				if section.header.sh_size < 2 {
					print_skip!("Skipping section '{}' because it is too small!", section.name);
					continue;
				}

				print_ok!("Processing section '{}' with idx {}", section.name, section.idx.0);
				find_and_handle_returns_in_section(section, symtab, endian, path.to_str().unwrap());
			}
		}
	}

	print_ok!("Writing to file: {}", &path.to_str().unwrap());

	start = Instant::now();
	obj_file.write_to_file(path).unwrap();
	print_ok!("Wrote file in {} msecs", start.elapsed().as_secs_f64() * 1000f64);
}

fn add_or_get_checkroutine_symbol(symtab: &mut SymbolTable32) -> SymbolIndex {
	let symbol_name = CFI_CHECKCODE_NAME.to_string();
	if let Some(s) = symtab.symbols.iter().find(|sym| sym.name == symbol_name) {
		s.idx
	} else {
		symtab.add_symbol(Symbol32 { 
			st_name: 0, 
			st_value: 0, 
			st_size: 0, 
			st_info: SymbolKind::None + SymbolBinding::Global, 
			st_other: SymbolVisibility::Default.into(), 
			st_shndx: SectionIndex(elf::constants::SHN_UNDEF as u32), 
			name: symbol_name, 
			idx: SymbolIndex(0),		/* Can be 0, will be determined dynamically by add_symbol */
		})
	}	
}

fn find_and_handle_returns_in_section(section: &mut Section32, symtab: &mut SymbolTable32, endian: Endianness, file_path: &str) -> () {
	let checkroutine_symbol: SymbolIndex = add_or_get_checkroutine_symbol(symtab);
	let mut funcs: Vec<&mut Symbol32> = symtab.symbols.iter_mut()
		/* consider type 'None' also to have them in the list for modifications */
		.filter(|s| { s.st_shndx == section.idx && (s.kind() == SymbolKind::Function || s.kind() == SymbolKind::None) })
		.collect::<Vec<&mut Symbol32>>();
	let mut num_of_returns: u32 = 0;

	let mut objdump_cmd = std::process::Command::new("arm-none-eabi-objdump");
	objdump_cmd.args([ "-j", section.name.as_str(), "-d", file_path]);
	let disassembly = String::from_utf8(objdump_cmd.output().unwrap().stdout).unwrap();
	let disassembly_lines = disassembly.lines().collect::<Vec<&str>>();

	funcs.sort_by(|a, b| a.partial_cmp(&b).unwrap());
	for f_idx in 0..funcs.len() {
		if funcs[f_idx].kind() != SymbolKind::Function { continue; }

		let mut shift_map: Vec<(usize, usize)> = Vec::new();
		let func_start: u32 = funcs[f_idx].st_value - 1;
		let mut pos: u32 = 0;

		while pos < (func_start + funcs[f_idx].st_size) {
			let pos_cast: usize = pos as usize;
			//println!("pos is {}, section len is {}, func_start is {}, func_end is {}", pos, section.data.len(), func_start, func_start + funcs[f_idx].st_size);
			let bytes: &[u8] = match true {
				_ if (section.data.len() - pos_cast) >= 4 => &section.data[pos_cast..=(pos_cast + 3)],
				_ if (section.data.len() - pos_cast) >= 2 => &section.data[pos_cast..=(pos_cast + 1)],
				_ => { pos += 2; continue }
			};

			match instruction::is_return(bytes, endian) {
				Some(x) => {
					/*if !instruction::verify_is_return_and_not_data(bytes, section.header.sh_addr + pos, 
							x, &disassembly_lines, endian) 
					{
						print_hint!("Found return which is not a real return at address {}", section.header.sh_addr + pos);
						pos += 2;
						continue;
					}*/

					num_of_returns += 1;
					print_ok!("Found return in '{}' at offset {} of type {:?}", section.name, pos, x);

					let cfi_call_pos = pos;
					let old_sect_data_len: usize = section.data.len();
					let mut addend: u32 = 0;
					let mut forward: u32 = 2;

					match x {
						ReturnType::Bxlr => {
							section.data[pos_cast] = BW_PLACEHOLDER[0];
							section.data[pos_cast + 1] = BW_PLACEHOLDER[1];
							if (pos_cast + 3) != section.data.len() && [ section.data[pos_cast + 2], section.data[pos_cast + 3] ] == NOPN {
								section.data[pos_cast + 2] = BW_PLACEHOLDER[2];
								section.data[pos_cast + 3] = BW_PLACEHOLDER[3];
								forward = 4;
							} else {
								if funcs[f_idx].st_size > 2 {
									/* Add NOPN to ensure function_size % 4 == 0, thus 4-byte aligned functions */
									section.data.splice((pos_cast + 2)..(pos_cast + 2), [ BW_PLACEHOLDER[2], BW_PLACEHOLDER[3], NOPN[0], NOPN[1] ] );
									addend = 4;
								} else { /* Allow 2-byte long functions */
									section.data.splice((pos_cast + 2)..(pos_cast + 2), [ BW_PLACEHOLDER[2], BW_PLACEHOLDER[3] ] );
									addend = 2;
								}
								forward = 2 + addend;
							}
						},
						ReturnType::PopT1 => {
							// make T2 out of T1 to be able to specify LR.
							if (pos_cast + 3) < section.data.len() && [ section.data[pos_cast + 2], section.data[pos_cast + 3] ] == NOPN {
								instruction::modify_popt1(&mut section.data[pos_cast..=(pos_cast + 3)]);
								section.data.splice((pos_cast + 4)..(pos_cast + 4), BW_PLACEHOLDER);
								addend = 4;
							} else {
								section.data.splice((pos_cast + 2)..(pos_cast + 2), [ 0x00, 0x00 ]);
								instruction::modify_popt1(&mut section.data[pos_cast..=(pos_cast + 3)]);
								section.data.splice((pos_cast + 4)..(pos_cast + 4), [ &BW_PLACEHOLDER[..], &NOPN[..] ].concat());
								addend = 8;
							}
							forward = 8;
						}, 
						ReturnType::PopT2 => {
							instruction::modify_popt2(&mut section.data[pos_cast..(pos_cast + 4)]);
							section.data.splice((pos_cast + 4)..(pos_cast + 4), BW_PLACEHOLDER);

							addend = 4;
							forward = 8;
						},
						ReturnType::PopT3 => {
							instruction::modify_popt3(&mut section.data[pos_cast..(pos_cast + 4)]);
							section.data.splice((pos_cast + 4)..(pos_cast + 4), BW_PLACEHOLDER);

							addend = 4;
							forward = 8;
						}
					}
					if addend != 0 {
						shift_map.push( (pos_cast + 2, addend as usize) );
						funcs[f_idx].st_size += addend;
						((f_idx + 1)..funcs.len()).for_each(|i| funcs[i].st_value += addend);	/* subsequent symbols */
					}
					pos += forward;

					if let Some(relo_table) = &mut section.relocations {
						relo_table.add_relocation(Relocation32 {
							r_offset: if x == ReturnType::Bxlr { cfi_call_pos } else { cfi_call_pos + 4 },
							r_info: ((elf::constants::R_ARM_THM_JUMP24 as u32) | ((checkroutine_symbol.0 as u32) << 8)) as u32,
						});
					} else {
						let mut relo_table = RelocationTable32::new(symtab.idx, section.idx);
						relo_table.add_relocation(Relocation32 {
							r_offset: if x == ReturnType::Bxlr { cfi_call_pos } else { cfi_call_pos + 4 },
							r_info: ((elf::constants::R_ARM_THM_JUMP24 as u32) | ((checkroutine_symbol.0 as u32) << 8)) as u32,
						});
						section.relocations = Some(relo_table);
					}
				},
				None => { pos += 2; continue }
			}
		}
		if shift_map.len() == 0 { continue; }

		// Adjust all instructions with relative adressing
		let mut pos = func_start as usize;
		while pos < ((func_start + funcs[f_idx].st_size) as usize) {
			let end = if (pos + 4) >= section.data.len() { 2 } else { 4 };
			if let Some(modified) = instruction::adjust_instruction(&section.data[pos..(pos + end)], pos, funcs[f_idx].st_size, &shift_map) {
				let (opcode, wide): ([u8; 4], bool) = modified;
				section.data[pos] = opcode[0];
				section.data[pos + 1] = opcode[1];
				if wide {
					section.data[pos + 2] = opcode[2];
					section.data[pos + 3] = opcode[3];
					pos += 2;
				}
			}
			pos += 2;
		}

		// Adjust offsets of consecutive symbols and data symbols within the function
		/*let size_growth = shift_map.iter().fold(0, |acc, elem| acc + elem.1);
		for j in 0..funcs.len() {
			if funcs[j].kind() == SymbolKind::None && funcs[j].st_value > 0 && funcs[j].st_value < (func_start + funcs[f_idx].st_size) {
				let old_st_value = funcs[j].st_value;
				for shift in shift_map.iter() {
					if old_st_value >= (shift.0 as u32) {
						funcs[j].st_value += shift.1 as u32;
					} else {
						break;
					}
				}
			}
		}*/

		// Adjust all relocations pointing into the current function
		if let Some(relotbl) = &mut section.relocations {
			for reloc in relotbl.relocations.iter_mut() {
				if reloc.get_symbol_idx() == checkroutine_symbol { continue; }
				for shift in shift_map.iter() {
					if reloc.r_offset >= (shift.0 as u32) {
						reloc.r_offset += shift.1 as u32;
					} else {
						break;
					}
				}
			}
		}
	}

	println!("Identified and modified {} returns!", num_of_returns);
}