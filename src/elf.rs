use std::fmt::{Debug, Display, Formatter};
use std::io::{Error as IOError, ErrorKind, Read, SeekFrom, Seek, BufWriter, Write};
use std::ops::{Add};
use std::path::PathBuf;
use std::fs::File;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Endianness {
	Little,
	Big,
}
impl TryFrom<u8> for Endianness {
	type Error = String;

	fn try_from(val: u8) -> Result<Self, Self::Error> {
		match val {
			1 => Ok(Self::Little),
			2 => Ok(Self::Big),
			x => Err(format!("Value {} is not a valid endian identifier!", x)),
		}
	}
}

pub trait ValueRead: Read + Seek + Sized {

	fn read_unsigned(&mut self, endian: Endianness, bit_width: u8) -> Result<u64, IOError>;

	fn read_u8(&mut self) -> Result<u8, IOError>;
	fn read_u16(&mut self, endian: Endianness) -> Result<u16, IOError>;
	fn read_u24(&mut self, endian: Endianness) -> Result<u32, IOError>;
	fn read_u32(&mut self, endian: Endianness) -> Result<u32, IOError>;
	fn read_u64(&mut self, endian: Endianness) -> Result<u64, IOError>;
	fn read_u128(&mut self, endian: Endianness) -> Result<u128, IOError>;

	fn read_i16(&mut self, endian: Endianness) -> Result<i16, IOError>;
	fn read_i24(&mut self, endian: Endianness) -> Result<i32, IOError>;
	fn read_i32(&mut self, endian: Endianness) -> Result<i32, IOError>;
	fn read_i64(&mut self, endian: Endianness) -> Result<i64, IOError>;

	fn read_f32(&mut self, endian: Endianness) -> Result<f32, IOError>;
	fn read_f64(&mut self, endian: Endianness) -> Result<f64, IOError>;

	fn read_bytes(&mut self, len: usize) -> Result<Vec<u8>, IOError>;

	fn read_string(&mut self, len: usize) -> Result<String, IOError>;
	fn read_nt_string(&mut self) -> Result<String, IOError>;
}
impl<R: Read + Seek> ValueRead for R {

	fn read_unsigned(&mut self, endian: Endianness, bit_width: u8) -> Result<u64, IOError> {
		match bit_width {
			32 => Ok(self.read_u32(endian)? as u64),
			64 => Ok(self.read_u64(endian)?),
			_ => Err(IOError::new(ErrorKind::Unsupported, "Unsupported bit width!"))
		}
	}
	fn read_u8(&mut self) -> Result<u8, IOError> {
		let mut buf: [u8; 1] = [0];
		match self.read_exact(&mut buf) {
			Ok(()) => Ok( buf[0] ),
			Err(e) => Err(e),
		}
	}
	fn read_u16(&mut self, endian: Endianness) -> Result<u16, IOError> {
		let mut buf: [u8; 2] = [0; 2];
		match self.read_exact(&mut buf) {
			Ok(()) => match endian {
				Endianness::Little => Ok(u16::from_le_bytes(buf)),
				Endianness::Big => Ok(u16::from_be_bytes(buf)),
			},
			Err(e) => Err(e),
		}
	}
	fn read_u24(&mut self, endian: Endianness) -> Result<u32, IOError> {
		let mut buf: [u8; 3] = [0; 3];
		match self.read_exact(&mut buf) {
			Ok(()) => match endian {
				Endianness::Little => Ok(u32::from_le_bytes( [ buf[0], buf[1], buf[2], 0 ])),
				Endianness::Big => Ok(u32::from_be_bytes( [ 0, buf[0], buf[1], buf[2] ])),
			},
			Err(e) => Err(e),
		}
	}
	fn read_u32(&mut self, endian: Endianness) -> Result<u32, IOError> {
		let mut buf: [u8; 4] = [0; 4];
		match self.read_exact(&mut buf) {
			Ok(()) => match endian {
				Endianness::Little => Ok(u32::from_le_bytes(buf)),
				Endianness::Big => Ok(u32::from_be_bytes(buf)),
			},
			Err(e) => Err(e),
		}
	}
	fn read_u64(&mut self, endian: Endianness) -> Result<u64, IOError> {
		let mut buf: [u8; 8] = [0; 8];
		match self.read_exact(&mut buf) {
			Ok(()) => match endian {
				Endianness::Little => Ok(u64::from_le_bytes(buf)),
				Endianness::Big => Ok(u64::from_be_bytes(buf)),
			},
			Err(e) => Err(e),
		}
	}
	fn read_u128(&mut self, endian: Endianness) -> Result<u128, IOError> {
		let mut buf: [u8; 16] = [0; 16];
		match self.read_exact(&mut buf) {
			Ok(()) => match endian {
				Endianness::Little => Ok(u128::from_le_bytes(buf)),
				Endianness::Big => Ok(u128::from_be_bytes(buf)),
			},
			Err(e) => Err(e),
		}
	}

	fn read_i16(&mut self, endian: Endianness) -> Result<i16, IOError> {
		let mut buf: [u8; 2] = [0; 2];
		match self.read_exact(&mut buf) {
			Ok(()) => match endian {
				Endianness::Little => Ok(i16::from_le_bytes(buf)),
				Endianness::Big => Ok(i16::from_be_bytes(buf)),
			},
			Err(e) => Err(e),
		}
	}
	fn read_i24(&mut self, endian: Endianness) -> Result<i32, IOError> {
		let mut buf: [u8; 3] = [0; 3];
		match self.read_exact(&mut buf) {
			Ok(()) => match endian {
				Endianness::Little => Ok(i32::from_le_bytes( [ buf[0], buf[1], buf[2], 0 ])),
				Endianness::Big => Ok(i32::from_be_bytes( [ 0, buf[0], buf[1], buf[2] ])),
			},
			Err(e) => Err(e),
		}
	}
	fn read_i32(&mut self, endian: Endianness) -> Result<i32, IOError> {
		let mut buf: [u8; 4] = [0; 4];
		match self.read_exact(&mut buf) {
			Ok(()) => match endian {
				Endianness::Little => Ok(i32::from_le_bytes(buf)),
				Endianness::Big => Ok(i32::from_be_bytes(buf)),
			},
			Err(e) => Err(e),
		}
	}
	fn read_i64(&mut self, endian: Endianness) -> Result<i64, IOError> {
		let mut buf: [u8; 8] = [0; 8];
		match self.read_exact(&mut buf) {
			Ok(()) => match endian {
				Endianness::Little => Ok(i64::from_le_bytes(buf)),
				Endianness::Big => Ok(i64::from_be_bytes(buf)),
			},
			Err(e) => Err(e),
		}
	}
	fn read_f32(&mut self, endian: Endianness) -> Result<f32, IOError> {
		let mut buf: [u8; 4] = [0; 4];
		match self.read_exact(&mut buf) {
			Ok(()) => match endian {
				Endianness::Little => Ok(f32::from_le_bytes(buf)),
				Endianness::Big => Ok(f32::from_be_bytes(buf)),
			},
			Err(e) => Err(e),
		}
	}
	fn read_f64(&mut self, endian: Endianness) -> Result<f64, IOError> {
		let mut buf: [u8; 8] = [0; 8];
		match self.read_exact(&mut buf) {
			Ok(()) => match endian {
				Endianness::Little => Ok(f64::from_le_bytes(buf)),
				Endianness::Big => Ok(f64::from_be_bytes(buf)),
			},
			Err(e) => Err(e),
		}
	}

	fn read_bytes(&mut self, len: usize) -> Result<Vec<u8>, IOError> {
		let mut bytes = vec![0u8; len];
		self.read_exact(&mut bytes)?;
		Ok(bytes)
	}
	fn read_string(&mut self, len: usize) -> Result<String, IOError> {
		let bytes = self.read_bytes(len)?;
		match String::from_utf8(bytes) {
			Ok(s) => Ok(s),
			Err(_e) => Err(IOError::new(ErrorKind::Other, "Byte slice is not a valid utf8 sequence"))
		}
	}

	/**
	 * Reads a null-terminated string from underlying reader.
	 * It reads bytes until a zero-byte is encountered, stops then and returns
	 * the string.
	 */
	fn read_nt_string(&mut self) -> Result<String, IOError> {
		let mut buf: Vec<u8> = Vec::new();
		let mut chr: u8 = self.read_u8()?;
		while chr != 0 {
			buf.push(chr);
			chr = self.read_u8()?;
		}

		match String::from_utf8(buf) {
			Ok(s) => Ok(s),
			Err(_e) => Err(IOError::new(ErrorKind::Other, "Byte slice is not a valid utf8 sequence"))
		}
	}
}

pub trait NullTerminated {
	fn as_nt_bytes(&self) -> Vec<u8>;
}
impl NullTerminated for &str {
	fn as_nt_bytes(&self) -> Vec<u8> {
		let mut bytes = self.as_bytes().to_vec();
		bytes.push(0);
		bytes
	}
}
impl NullTerminated for String {
	fn as_nt_bytes(&self) -> Vec<u8> {
		let mut bytes = self.as_bytes().to_vec();
		bytes.push(0);
		bytes
	}
}

pub fn vec_contains_slice<T>(vec: &Vec<T>, slc: &[T]) -> Option<usize> 
where T: PartialEq
{
'outer:	
	for i in 0..vec.len() {
		if vec[i] != slc[0] { continue; }

'inner:	for j in 1..slc.len() {
			if vec[i + j] != slc[j] {
				continue 'outer;
			}
		}
		return Some(i);
	}
	None
}

#[derive(Debug)]
pub struct ParseError {
	pub message: String,
}
impl Display for ParseError {
	fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
		write!(f, "Error while parsing: {}", self.message)
	}
}
impl std::error::Error for ParseError { }
impl From<std::io::Error> for ParseError {
	fn from(err: std::io::Error) -> Self { 
		Self { message: err.to_string() }
	}
}
impl From<&'static str> for ParseError {
	fn from(err: &'static str) -> Self {
		Self { message: err.to_string() }
	}
}
impl From<String> for ParseError {
	fn from(err: String) -> Self {
		Self { message: err }
	}
}

///
/// This small ELF library is only targeted to be used with 32-bit ARM ELF files.
/// The basic set of common stuff is implemented but no specialities for other architectures.
/// Something like dynsym section are not handled properly and will likely break!
/// 

// TODO: For the following types only one section at a time can be present in a file:
//			SHT_SYMTAB, SHT_HASH, SHT_DYNAMIC, SHT_DYNSYM
//		 Additionally: afaik there is only one relocation section per "regular" section

pub struct ElfFile32 {
	pub header: FileHeader32,
	pub program_headers: Vec<ProgramHeader32>,
	pub symbol_table: Option<SymbolTable32>,
	pub sections: Vec<Section32>,
}

impl ElfFile32 {
	pub fn parse_header<R: ValueRead>(data: &mut R) -> Result<FileHeader32, ParseError> { Ok(FileHeader32::parse(data)?) }

	pub fn parse<R: ValueRead>(header: FileHeader32, data: &mut R) -> Result<Self, ParseError> {
		let mut elf_file: Self = Self { 
			header: header, 
			program_headers: vec![],
			symbol_table: None,
			sections: vec![] 
		};
		let endian: Endianness = Endianness::try_from(elf_file.header.e_ident.ei_data)?;
		if elf_file.header.e_ident.ei_class != 1 {
			return Err(ParseError { message: "invalid value for ei_data".to_string() });
		}

		data.seek(SeekFrom::Start(elf_file.header.e_phoff.into()))?;
		for _ in 0..(elf_file.header.e_phnum) { elf_file.program_headers.push(ProgramHeader32::parse(data, endian)?); }

		data.seek(SeekFrom::Start(elf_file.header.e_shoff.into()))?;
		/* e_shnum is set to SHN_UNDEF when there are more than 2^16 - 1 sections */
		let num_of_sections = match elf_file.header.e_shnum {
			constants::SHN_UNDEF => {
				data.seek(SeekFrom::Current(20))?;
				let num: u32 = data.read_u32(endian)?;
				data.seek(SeekFrom::Current(-24))?;
				num
			},
			x => x as u32
		};
		let mut section_headers: Vec<SectionHeader32> = Vec::new();
		for _ in 0..num_of_sections { section_headers.push(SectionHeader32::parse(data, endian)?) }

		let mut idx: u32 = 0;
		let shstrtab_hdr: &SectionHeader32 = &section_headers[elf_file.header.e_shstrndx as usize];
		for shdr in section_headers.iter() {
			match shdr.section_kind() {
				SectionKind::Symtab => {
					if let None = elf_file.symbol_table {
						let strtab_hdr: &SectionHeader32 = &section_headers[shdr.sh_link as usize];
						let mut symtab: SymbolTable32 = match SymbolTable32::parse(shdr, strtab_hdr, data, endian) {
							Ok(s) => s,
							Err(e) => print_err_panic!("Failed to parse symbol table: {}", e),
						};
						symtab.idx = SectionIndex(idx);
						if shdr.sh_name != 0 {
							symtab.name = shstrtab_hdr.get_string_at_offset(data, shdr.sh_name)?;
						}
						elf_file.symbol_table = Some(symtab);
					} else {
						print_hint!("Additional symbol table will be discarded!");
					}
				},
				SectionKind::Strtab | SectionKind::Rel => {},
				_ => {
					let rel_shdr = section_headers
						.iter()
						.find(|s| { s.section_kind() == SectionKind::Rel && s.sh_info == idx });
					let mut section = Section32::parse(shdr, rel_shdr, data, endian)?;
					section.idx = SectionIndex(idx);
					if shdr.sh_name != 0 {
						section.name = shstrtab_hdr.get_string_at_offset(data, shdr.sh_name)?
					}
					if let Some(reltab) = &mut section.relocations {
						reltab.name = shstrtab_hdr.get_string_at_offset(data, reltab.header.sh_name)?;
						reltab.idx = SectionIndex(idx + 1);
						//idx += 1;
					}

					elf_file.sections.push(section);
				}
			}
			idx += 1;
		}
		Ok(elf_file)
	}

	pub fn section_by_name(&self, name: &str) -> Option<&Section32> {
		for s in self.sections.iter() {
			if &s.name[..] == name {
				return Some(s);
			}
		}

		None
	}
	pub fn section_by_name_mut(&mut self, name: &str) -> Option<&mut Section32> {
		for s in self.sections.iter_mut() {
			if &s.name[..] == name {
				return Some(s);
			}
		}

		None
	}

	/**
	 * Retrieves the real idx (usable as index into sections array) from the original idx.
	 * The original idx is preserved as it's used for references inside an ELF file.
	 */
	pub fn section_seek_real_idx(&self, idx: SectionIndex) -> Option<usize> {
		for i in 0..self.sections.len() {
			if self.sections[i].idx == idx {
				return Some(i);
			}
		}
		return None;
	}
	pub fn section_real_idx_by_name(&self, name: &str) -> Option<usize> {
		for i in 0..self.sections.len() {
			if &self.sections[i].name[..] == name {
				return Some(i);
			}
		}
		return None;
	}
	pub fn section_real_idx_by_type(&self, skind: SectionKind) -> Option<usize> {
		for i in 0..self.sections.len() {
			if self.sections[i].kind() == skind {
				return Some(i);
			}
		}
		None
	}

	#[inline] pub fn kind(&self) -> ElfKind { ElfKind::from(self.header.e_type) }
	#[inline] pub fn endian(&self) -> Result<Endianness, ParseError> { Ok(Endianness::try_from(self.header.e_ident.ei_data)?) }

	fn shdr_fix_indices(shdr: &mut SectionHeader32, mapping: &Vec<(SectionIndex, u32)>) {
		if let Some(entry) = mapping.iter().find(|x| (x.0).0 == shdr.sh_link) {
			shdr.sh_link = entry.1;
		}

		if (shdr.sh_flags & constants::SHF_INFO_LINK) == 0 { return; }
		if let Some(entry) = mapping.iter().find(|x| (x.0).0 == shdr.sh_link) {
			shdr.sh_info = entry.1;
		}
	}
	fn relo_tbl_fix_indices(relocations: &mut Vec<Relocation32>, mapping: &Vec<(SymbolIndex, u32)>) {
		for relo in relocations {
			let relo_sym_idx = relo.get_symbol_idx();
			if let Some(entry) = mapping.iter().find(|x| x.0 == relo_sym_idx) {
				relo.set_symbol_idx(SymbolIndex(entry.1));
			}
		}
	}
	fn refresh_section_indices(&mut self) {
		let mut idx_map: Vec<(SectionIndex, u32)> = Vec::new();
		let mut idx: u32 = 0;
		for section in self.sections.iter_mut() {
			if section.idx.0 != 0 && section.idx.0 != idx { idx_map.push( (section.idx, idx) ); }
			idx += 1;

			if let Some(relo_tbl) = &mut section.relocations { 
				if relo_tbl.idx.0 != 0 && relo_tbl.idx.0 != idx { idx_map.push( (relo_tbl.idx, idx) ); }

				// Just preventively set the sh_info of the relo_tbl header to point to the section it belongs.
				relo_tbl.header.sh_info = idx - 1;
				idx += 1;
			}
		}
		if let Some(sym_tbl) = &mut self.symbol_table && sym_tbl.idx.0 != 0 {
			if sym_tbl.idx.0 != idx { idx_map.push( (sym_tbl.idx, idx) ); }
			sym_tbl.header.sh_link = idx + 1;
			idx += 2;

			// go through symbols and adjust all 'st_shndx' values
			let mut cached_idx: Option<&(SectionIndex, u32)> = None;
			for sym in sym_tbl.symbols.iter_mut() {
				if let Some(cached) = cached_idx && sym.st_shndx == cached.0 {
					sym.st_shndx = SectionIndex(cached.1);
				} else {
					if let Some(entry) = idx_map.iter().find(|x| x.0 == sym.st_shndx) {
						sym.st_shndx = SectionIndex(entry.1);
						cached_idx = Some(entry);
					}
				}
			}
		}
		self.header.e_shstrndx = idx as u16;

		for section in self.sections.iter_mut() {
			if section.kind() == SectionKind::Group {
				// Loop through 32-bit values of section containing section header indexes
				for i in (0..section.data.len()).step_by(4) {
					let sect_idx = u32::from_le_bytes((&section.data[i..(i + 4)]).try_into().unwrap());
					if let Some(mapping) = idx_map.iter().find(|x| (x.0).0 == sect_idx) {
						let new_bytes: [u8; 4] = mapping.1.to_le_bytes();
						section.data[i] = new_bytes[0];
						section.data[i + 1] = new_bytes[1];
						section.data[i + 2] = new_bytes[2];
						section.data[i + 3] = new_bytes[3];
					}
				}
			}

			Self::shdr_fix_indices(&mut section.header, &idx_map);
			if let Some(relo_table) = &mut section.relocations {
				Self::shdr_fix_indices(&mut relo_table.header, &idx_map);
			}
		}
	}
	fn refresh_symbol_indices(&mut self) {
		if let Some(sym_tbl) = &self.symbol_table {
			let mut idx_map: Vec<(SymbolIndex, u32)> = Vec::new();
			let mut idx: u32 = 0;
			sym_tbl.symbols.iter().for_each(|sym| {
				if sym.idx.0 != idx { idx_map.push( (sym.idx, idx) ); }
				idx += 1;
			});

			for section in self.sections.iter_mut() {
				if section.kind() == SectionKind::Group {
					// Adjust sh_info with symbol index
					if let Some(entry) = idx_map.iter().find(|x| (x.0).0 == section.header.sh_info) {
						section.header.sh_info = entry.1;
					}
				}

				if let Some(relo_table) = &mut section.relocations {
					Self::relo_tbl_fix_indices(&mut relo_table.relocations, &idx_map);
				}
			}
		}
	}

	pub fn write_to_file(mut self, path: &PathBuf) -> std::io::Result<()> {
		let dest = File::create(path)?;
		let endian: Endianness = self.endian().unwrap();

		let mut writer = BufWriter::new(dest);
		let mut shdr_bytes: Vec<u8> = Vec::new();
		let mut shstr_data: Vec<u8> = Vec::new();
		let mut shstr_hdr = SectionHeader32::new(SectionKind::Strtab);

		// Skip bytes for header and write program headers at first
		let phdr_pos = writer.seek(SeekFrom::Start(self.header.len() as u64))? as u32;
		for phdr in self.program_headers.iter() {
			writer.write(&phdr.as_bytes(endian))?;
		}
		self.header.e_phnum = self.program_headers.len() as u16;
		self.header.e_phoff = if self.program_headers.len() == 0 { 0 } else { phdr_pos };

		// Updates all section indices and references in case a section has been removed or inserted in between.
		self.refresh_section_indices();
		// Updates all symbol indices and references in case a symbol has been removed or inserted in between.
		self.refresh_symbol_indices();

		// write parts of sections to bytestream(s)
		for sect in self.sections.iter_mut() {
			// Consider alignment
			if sect.header.sh_addralign != 0 && let align_off = (writer.stream_position()? as u32) % sect.header.sh_addralign && align_off != 0 {
				let tmp_vec = vec![0; align_off as usize];
				writer.write(&tmp_vec)?;
			}

			if sect.kind() != SectionKind::Null {
				sect.header.sh_offset = writer.stream_position()? as u32;
			}
			sect.header.sh_size = sect.data.len() as u32;
			writer.write(&sect.data)?;

			if let Some(relo_tbl) = &mut sect.relocations {
				// Consider alignment
				if relo_tbl.header.sh_addralign != 0 && let align_off = (writer.stream_position()? as u32) % relo_tbl.header.sh_addralign && align_off != 0 {
					let tmp_vec = vec![0; align_off as usize];
					writer.write(&tmp_vec)?;
				}

				relo_tbl.header.sh_offset = writer.stream_position()? as u32;
				writer.write(&relo_tbl.as_bytes(endian))?;

				let relo_name_bytes = [".rel", &sect.name[..]].concat().as_nt_bytes();
				if let Some(pos) = vec_contains_slice(&shstr_data, &relo_name_bytes[..]) {
					relo_tbl.header.sh_name = pos as u32;
					sect.header.sh_name = relo_tbl.header.sh_name + 4;
				} else {
					relo_tbl.header.sh_name = shstr_data.len() as u32;
					sect.header.sh_name = relo_tbl.header.sh_name + 4;
					shstr_data.extend(relo_name_bytes);
				}

				shdr_bytes.extend(sect.header.as_bytes(endian));
				shdr_bytes.extend(relo_tbl.header.as_bytes(endian));
			} else {
				let name_bytes = sect.name.as_nt_bytes();
				if let Some(pos) = vec_contains_slice(&shstr_data, &name_bytes[..]) {
					sect.header.sh_name = pos as u32;
				} else {
					sect.header.sh_name = shstr_data.len() as u32;
					shstr_data.extend(name_bytes);
				}

				shdr_bytes.extend(sect.header.as_bytes(endian));
			}
		}
		if let Some(sym_tbl) = &mut self.symbol_table {
			// Derive .strtab from .symtab
			let mut strtab = sym_tbl.derive_string_table();

			sym_tbl.header.sh_offset = writer.stream_position()? as u32;
			writer.write(&sym_tbl.as_bytes(endian))?;

			sym_tbl.header.sh_name = shstr_data.len() as u32;
			shstr_data.extend(".symtab".as_nt_bytes());
			shdr_bytes.extend(sym_tbl.header.as_bytes(endian));

			strtab.header.sh_offset = writer.stream_position()? as u32;
			strtab.header.sh_name = shstr_data.len() as u32;

			shstr_data.extend(".strtab".as_nt_bytes());
			shdr_bytes.extend(strtab.header.as_bytes(endian));
			writer.write(&strtab.data)?;
		}

		shstr_hdr.sh_name = shstr_data.len() as u32;
		shstr_data.extend(".shstrtab".as_nt_bytes());

		shstr_hdr.sh_offset = writer.stream_position()? as u32;
		shstr_hdr.sh_size = shstr_data.len() as u32;
		shdr_bytes.extend(shstr_hdr.as_bytes(endian));

		// Write .shstrtab and then all section headers
		writer.write(&shstr_data).unwrap();
		self.header.e_shoff = writer.stream_position()? as u32;
		writer.write(&shdr_bytes).unwrap();

		self.header.e_shnum = (shdr_bytes.len() / (SectionHeader32::LEN as usize)) as u16;
		self.header.e_shstrndx = self.header.e_shnum - 1;

		writer.seek(SeekFrom::Start(0)).unwrap();
		writer.write(&self.header.as_bytes(endian))?;

		writer.flush()?;

		// File header should be written at last as the information depends on the actual file structure
		Ok(())
	}
}

// ############################################################################
// ELF header types
// ############################################################################

pub enum ElfClass {
	Class32,
	Class64
}

///
/// ELF - e_ident
/// 
#[repr(C, packed)]
#[derive(Debug)]
pub struct Ident {
	pub ei_magic: [u8; 4],
	pub ei_class: u8,
	pub ei_data: u8,
	pub ei_version: u8,
	pub ei_osabi: u8,
	pub ei_abiversion: u8,
}
impl Ident {
	pub fn parse<'a, R: ValueRead>(data: &mut R) -> Result<Self, ParseError> {
		let mut magic: [u8; 4] = [0; 4];
		data.read_exact(&mut magic[..])?;
		if magic != constants::ELF_MAGIC {
			return Err(ParseError { message: "Invalid ELF magic!".to_string() });
		}

		let ident = Ident {
			ei_magic: magic,
			ei_class: data.read_u8()?,
			ei_data: data.read_u8()?,
			ei_version: data.read_u8()?,
			ei_osabi: data.read_u8()?,
			ei_abiversion: data.read_u8()?
		};
		data.seek(SeekFrom::Current(7))?; // ei_pad
		Ok(ident)
	}
	pub fn as_bytes(&self) -> [u8; 16] {
		let data: [u8; 9] = unsafe { std::mem::transmute_copy::<Ident, [u8; 9]>(self) };
		[ data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7], data[8], 0, 0, 0, 0, 0, 0, 0 ]
	}
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum ElfKind {
	None,
	Relocatable,
	Executable,
	Dynamic,
	Core,
	OsSpecific(u16),
	CpuSpecific(u16),
	Unsupported(u16),
}
impl From<u16> for ElfKind {
	fn from(val: u16) -> Self {
		match val {
			constants::ET_NONE => Self::None,
			constants::ET_REL => Self::Relocatable,
			constants::ET_EXEC => Self::Executable,
			constants::ET_DYN => Self::Dynamic,
			constants::ET_CORE => Self::Core,
			x if (constants::ET_LOOS..=constants::ET_HIOS).contains(&x) => Self::OsSpecific(x),
			y if (constants::ET_LOPROC..=constants::ET_HIPROC).contains(&y) => Self::CpuSpecific(y),
			z => Self::Unsupported(z),
		}
	}
}

#[repr(C)]
pub struct FileHeader32 {
	pub e_ident: Ident,
	pub e_type: u16,
	pub e_machine: u16, 
	pub e_version: u32,
	pub e_entry: u32,
	pub e_phoff: u32,
	pub e_shoff: u32,
	pub e_flags: u32,
	pub e_ehsize: u16,
	pub e_phentsize: u16,
	pub e_phnum: u16,
	pub e_shentsize: u16,
	pub e_shnum: u16,
	pub e_shstrndx: u16,
}
impl FileHeader32 {

	pub fn parse<R: ValueRead>(data: &mut R) -> Result<Self, ParseError> {
		let ident = Ident::parse(data)?;
		let endian: Endianness = match ident.ei_data {
			1 => Endianness::Little,
			2 => Endianness::Big,
			_ => return Err(ParseError { message: "invalid value for ei_data".to_string() }),
		};
		Ok(Self {
			e_ident: ident,
			e_type: data.read_u16(endian)?,
			e_machine: data.read_u16(endian)?,
			e_version: data.read_u32(endian)?,
			e_entry: data.read_u32(endian)?,
			e_phoff: data.read_u32(endian)?,
			e_shoff: data.read_u32(endian)?,
			e_flags: data.read_u32(endian)?,
			e_ehsize: data.read_u16(endian)?,
			e_phentsize: data.read_u16(endian)?,
			e_phnum: data.read_u16(endian)?,
			e_shentsize: data.read_u16(endian)?,
			e_shnum: data.read_u16(endian)?,
			e_shstrndx: data.read_u16(endian)?
		})
	}

	pub fn as_bytes(&self, endian: Endianness) -> Vec<u8> {
		let mut bytes: Vec<u8> = Vec::new();
		bytes.extend(self.e_ident.as_bytes());
		match endian {
			Endianness::Little => {
				bytes.extend(self.e_type.to_le_bytes());
				bytes.extend(self.e_machine.to_le_bytes());
				bytes.extend(self.e_version.to_le_bytes());
				bytes.extend(self.e_entry.to_le_bytes());
				bytes.extend(self.e_phoff.to_le_bytes());
				bytes.extend(self.e_shoff.to_le_bytes());
				bytes.extend(self.e_flags.to_le_bytes());
				bytes.extend(self.e_ehsize.to_le_bytes());
				bytes.extend(self.e_phentsize.to_le_bytes());
				bytes.extend(self.e_phnum.to_le_bytes());
				bytes.extend(self.e_shentsize.to_le_bytes());
				bytes.extend(self.e_shnum.to_le_bytes());
				bytes.extend(self.e_shstrndx.to_le_bytes());
			},
			Endianness::Big => {
				bytes.extend(self.e_type.to_be_bytes());
				bytes.extend(self.e_machine.to_be_bytes());
				bytes.extend(self.e_version.to_be_bytes());
				bytes.extend(self.e_entry.to_be_bytes());
				bytes.extend(self.e_phoff.to_be_bytes());
				bytes.extend(self.e_shoff.to_be_bytes());
				bytes.extend(self.e_flags.to_be_bytes());
				bytes.extend(self.e_ehsize.to_be_bytes());
				bytes.extend(self.e_phentsize.to_be_bytes());
				bytes.extend(self.e_phnum.to_be_bytes());
				bytes.extend(self.e_shentsize.to_be_bytes());
				bytes.extend(self.e_shnum.to_be_bytes());
				bytes.extend(self.e_shstrndx.to_be_bytes());
			}
		}
		bytes
	}

	#[inline] pub fn endian(&self) -> Option<Endianness> { 
		match self.e_ident.ei_data {
			1 => Some(Endianness::Little),
			2 => Some(Endianness::Big),
			_ => None
		}
	}
	#[inline] pub fn class(&self) -> Option<ElfClass> {
		match self.e_ident.ei_class {
			1 => Some(ElfClass::Class32),
			2 => Some(ElfClass::Class64),
			_ => None
		}
	}
	#[inline] pub fn file_kind(&self) -> ElfKind { ElfKind::from(self.e_type) }
	#[inline] pub fn len(&self) -> u16 { 
		match self.class() {
			Some(ElfClass::Class32) => 52,
			Some(ElfClass::Class64) => 64,
			None => 0,
		}
	}
}

// ############################################################################
// Program header types
// ############################################################################

#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd)]
pub enum ProgramHeaderType {
	Null,
	Load,
	Dynamic,
	Interp,
	Note,
	Shlib,
	Phdr,
	Tls,

	OsSpecific(u32),
	ProcessorSpecific(u32),
	Unknown(u32)
}
impl From<u32> for ProgramHeaderType {
	fn from(val: u32) -> Self {
		match val {
			constants::PT_NULL => Self::Null,
			constants::PT_LOAD => Self::Load,
			constants::PT_DYNAMIC => Self::Dynamic,
			constants::PT_INTERP => Self::Interp,
			constants::PT_NOTE => Self::Note,
			constants::PT_SHLIB => Self::Shlib,
			constants::PT_PHDR => Self::Phdr,
			constants::PT_TLS => Self::Tls,
			t if (constants::PT_LOOS..=constants::PT_HIOS).contains(&t) => Self::OsSpecific(t),
			t if (constants::PT_LOPROC..=constants::PT_HIPROC).contains(&t) => Self::ProcessorSpecific(t),
			x => Self::Unknown(x),
		}
	}
}
impl From<ProgramHeaderType> for u32 {
	fn from(val: ProgramHeaderType) -> Self {
		match val {
			ProgramHeaderType::Null => constants::PT_NULL,
			ProgramHeaderType::Load => constants::PT_LOAD,
			ProgramHeaderType::Dynamic => constants::PT_DYNAMIC,
			ProgramHeaderType::Interp => constants::PT_INTERP,
			ProgramHeaderType::Note => constants::PT_NOTE,
			ProgramHeaderType::Shlib => constants::PT_SHLIB,
			ProgramHeaderType::Phdr => constants::PT_PHDR,
			ProgramHeaderType::Tls => constants::PT_TLS,
			ProgramHeaderType::OsSpecific(x)
				| ProgramHeaderType::ProcessorSpecific(x)
				| ProgramHeaderType::Unknown(x) => x
		}
	}
}

#[repr(C)]
#[derive(Debug)]
pub struct ProgramHeader32 {
	pub p_type: u32,
	pub p_offset: u32,
	pub p_vaddr: u32,
	pub p_paddr: u32,
	pub p_filesz: u32,
	pub p_memsz: u32,
	pub p_flags: u32,
	pub p_align: u32,
}
impl ProgramHeader32 {
	fn parse<R: ValueRead>(data: &mut R, endian: Endianness) -> Result<Self, ParseError> {
		Ok(Self {
			p_type: data.read_u32(endian)?,
			p_offset: data.read_u32(endian)?,
			p_vaddr: data.read_u32(endian)?,
			p_paddr: data.read_u32(endian)?,
			p_filesz: data.read_u32(endian)?,
			p_memsz: data.read_u32(endian)?,
			p_flags: data.read_u32(endian)?,
			p_align: data.read_u32(endian)?
		})
	}

	pub fn as_bytes(&self, endian: Endianness) -> Vec<u8> {
		let mut bytes: Vec<u8> = Vec::new();
		match endian {
			Endianness::Little => {
				bytes.extend(self.p_type.to_le_bytes());
				bytes.extend(self.p_offset.to_le_bytes());
				bytes.extend(self.p_vaddr.to_le_bytes());
				bytes.extend(self.p_paddr.to_le_bytes());
				bytes.extend(self.p_filesz.to_le_bytes());
				bytes.extend(self.p_memsz.to_le_bytes());
				bytes.extend(self.p_flags.to_le_bytes());
				bytes.extend(self.p_align.to_le_bytes());
			},
			Endianness::Big => {
				bytes.extend(self.p_type.to_be_bytes());
				bytes.extend(self.p_offset.to_be_bytes());
				bytes.extend(self.p_vaddr.to_be_bytes());
				bytes.extend(self.p_paddr.to_be_bytes());
				bytes.extend(self.p_filesz.to_be_bytes());
				bytes.extend(self.p_memsz.to_be_bytes());
				bytes.extend(self.p_flags.to_be_bytes());
				bytes.extend(self.p_align.to_be_bytes());
			}
		}
		bytes
	}

	#[inline] fn kind(&self) -> ProgramHeaderType { ProgramHeaderType::from(self.p_type) }
}

// ############################################################################
// Abstract section types
// ############################################################################

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct SectionIndex(pub u32);

#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd)]
pub enum SectionKind {
	Null,						// 0x00 - Null section
	Progbits,					// 0x01 - Program information
	Symtab,						// 0x02 - Symbol table
	Strtab,						// 0x03 - String table
	Rela,						// 0x04 - Relocation (w/ addend)
	Hash,						// 0x05 - Symbol hash table
	Dynamic,					// 0x06 - Dynamic linking info
	Note,						// 0x07 - Note section
	Nobits,						// 0x08 - Not present in file
	Rel,						// 0x09 - Relocation (no addend)
	Shlib,						// 0x0a - Reserved
	Dynsym,						// 0x0b - Symbols for dynamic linking
	InitArray,					// 0x0e - Pointer to initialization functions / constructors
	FiniArray,					// 0x0f - Pointer to termination functions / destructors
	PreinitArray,				// 0x10 - 
	Group, 						// 0x11 - Group section
	SymtabShndx,				// 0x12 - Table that contains extended indizes for a symtab
	
	ProcessorSpecific(u32),		// Section type in range of processor specific sections
	OsSpecific(u32),			// Section type in range of operating system specific sections
	ApplicationSpecific(u32),	// Section type in range of application specific sections
	Unsupported(u32)
}
impl From<SectionKind> for u32 {
	fn from(val: SectionKind) -> u32 {
		match val {
			SectionKind::Null => constants::SHT_NULL, 
			SectionKind::Progbits => constants::SHT_PROGBITS, 
			SectionKind::Symtab => constants::SHT_SYMTAB,
			SectionKind::SymtabShndx => constants::SHT_SYMTAB_SHNDX,
			SectionKind::Strtab => constants::SHT_STRTAB, 
			SectionKind::Rela => constants::SHT_RELA,
			SectionKind::Hash => constants::SHT_HASH,
			SectionKind::Dynamic => constants::SHT_DYNAMIC,
			SectionKind::Note => constants::SHT_NOTE,
			SectionKind::Nobits => constants::SHT_NOBITS,
			SectionKind::Rel => constants::SHT_REL,
			SectionKind::Shlib => constants::SHT_SHLIB,
			SectionKind::Dynsym => constants::SHT_DYNSYM,
			SectionKind::InitArray => constants::SHT_INIT_ARRAY,
			SectionKind::FiniArray => constants::SHT_FINI_ARRAY,
			SectionKind::PreinitArray => constants::SHT_PREINIT_ARRAY,
			SectionKind::Group => constants::SHT_GROUP,
			SectionKind::SymtabShndx => constants::SHT_SYMTAB_SHNDX,
			SectionKind::ProcessorSpecific(t) => t,
			SectionKind::OsSpecific(t) => t,
			SectionKind::ApplicationSpecific(t) => t,
			SectionKind::Unsupported(t) => t,
		}
	}
}
impl From<u32> for SectionKind {
	fn from(val: u32) -> Self {
		match val {
			constants::SHT_NULL => Self::Null,
			constants::SHT_PROGBITS => Self::Progbits,
			constants::SHT_SYMTAB => Self::Symtab,
			constants::SHT_STRTAB => Self::Strtab,
			constants::SHT_RELA => Self::Rela,
			constants::SHT_NOBITS => Self::Nobits,
			constants::SHT_REL => Self::Rel,
			constants::SHT_SYMTAB_SHNDX => Self::SymtabShndx,
			constants::SHT_GROUP => Self::Group,
			x if (constants::SHT_LOPROC..=constants::SHT_HIPROC).contains(&x) => Self::ProcessorSpecific(x),
			x if (constants::SHT_LOOS..=constants::SHT_HIOS).contains(&x) => Self::OsSpecific(x),
			x if (constants::SHT_LOUSER..=constants::SHT_HIUSER).contains(&x) => Self::ApplicationSpecific(x),
			x => Self::Unsupported(x)
		}
	}
}

#[derive(Debug, Clone, Copy)]
pub struct SectionHeader32 {
	pub sh_name: u32,
	pub sh_type: u32,
	pub sh_flags: u32,
	pub sh_addr: u32,
	pub sh_offset: u32,
	pub sh_size: u32,
	pub sh_link: u32,
	pub sh_info: u32,
	pub sh_addralign: u32,
	pub sh_entsize: u32,
}
impl SectionHeader32 {
	pub const LEN: u16 = 40;

	pub fn new(sect_type: SectionKind) -> Self {
		SectionHeader32 { 
			sh_name: 0, 
			sh_type: sect_type.into(), 
			sh_flags: 0, 
			sh_addr: 0, 
			sh_offset: 0, 
			sh_size: 0, 
			sh_link: 0, 
			sh_info: 0, 
			sh_addralign: 1, 
			sh_entsize: 0 
		}
	}
	pub fn parse<R: ValueRead>(data: &mut R, endian: Endianness) -> Result<Self, ParseError> {
		Ok(Self {
			sh_name: data.read_u32(endian)?,
			sh_type: data.read_u32(endian)?,
			sh_flags: data.read_u32(endian)?,
			sh_addr: data.read_u32(endian)?,
			sh_offset: data.read_u32(endian)?,
			sh_size: data.read_u32(endian)?,
			sh_link: data.read_u32(endian)?,
			sh_info: data.read_u32(endian)?,
			sh_addralign: data.read_u32(endian)?,
			sh_entsize: data.read_u32(endian)?
		})
	}
	pub fn as_bytes(&self, endian: Endianness) -> Vec<u8> {
		let mut bytes: Vec<u8> = Vec::new();

		match endian {
			Endianness::Little => {
				bytes.extend(self.sh_name.to_le_bytes());
				bytes.extend(self.sh_type.to_le_bytes());
				bytes.extend(self.sh_flags.to_le_bytes());
				bytes.extend(self.sh_addr.to_le_bytes());
				bytes.extend(self.sh_offset.to_le_bytes());
				bytes.extend(self.sh_size.to_le_bytes());
				bytes.extend(self.sh_link.to_le_bytes());
				bytes.extend(self.sh_info.to_le_bytes());
				bytes.extend(self.sh_addralign.to_le_bytes());
				bytes.extend(self.sh_entsize.to_le_bytes());
			},
			Endianness::Big => {
				bytes.extend(self.sh_name.to_be_bytes());
				bytes.extend(self.sh_type.to_be_bytes());
				bytes.extend(self.sh_flags.to_be_bytes());
				bytes.extend(self.sh_addr.to_be_bytes());
				bytes.extend(self.sh_offset.to_be_bytes());
				bytes.extend(self.sh_size.to_be_bytes());
				bytes.extend(self.sh_link.to_be_bytes());
				bytes.extend(self.sh_info.to_be_bytes());
				bytes.extend(self.sh_addralign.to_be_bytes());
				bytes.extend(self.sh_entsize.to_be_bytes());
			}
		}

		bytes
	}

	#[inline] fn section_kind(&self) -> SectionKind { SectionKind::from(self.sh_type) }

	pub fn get_string_at_offset<R: ValueRead>(&self, data: &mut R, offset: u32) -> Result<String, ParseError> {
		if self.section_kind() != SectionKind::Strtab {
			return Err(ParseError { message: "Cannot get string from non-STRTAB section!".to_string() })
		}

		let orig_pos = data.stream_position()?;
		data.seek(SeekFrom::Start((self.sh_offset + offset) as u64))?;

		let mut name: String = String::new();
		let mut chr: u8 = data.read_u8()?;
		while chr != 0 {
			name.push(chr.into());
			chr = data.read_u8()?;
		}

		data.seek(SeekFrom::Start(orig_pos))?;
		Ok(name)
	}
}

#[derive(Debug)]
pub enum ElfSection {
	SymbolTable(SymbolTable32),
	RelocationTable(RelocationTable32),
	Other(Section32),
}
impl ElfSection {
	pub fn idx(&self) -> SectionIndex {
		match self {
			Self::SymbolTable(s) => s.idx,
			Self::RelocationTable(s) => s.idx,
			Self::Other(s) => s.idx,
		}
	}
	pub fn name(&self) -> &String {
		match self {
			Self::SymbolTable(s) => &s.name,
			Self::RelocationTable(s) => &s.name,
			Self::Other(s) => &s.name,
		}
	}
	pub fn name_mut(&mut self) -> &mut String {
		match self {
			Self::SymbolTable(s) => &mut s.name,
			Self::RelocationTable(s) => &mut s.name,
			Self::Other(s) => &mut s.name,
		}
	}
	pub fn header(&self) -> &SectionHeader32 {
		match self {
			Self::SymbolTable(s) => &s.header,
			Self::RelocationTable(s) => &s.header,
			Self::Other(s) => &s.header,
		}
	}
	pub fn section_kind(&self) -> SectionKind {
		match self {
			Self::SymbolTable(_) => SectionKind::Symtab,
			Self::RelocationTable(_) => SectionKind::Rel,
			Self::Other(s) => s.header.section_kind(),
		}
	}

	pub fn set_section_idx(&mut self, idx: SectionIndex) -> () {
		match self {
			Self::SymbolTable(s) => s.idx = idx,
			Self::RelocationTable(s) => s.idx = idx,
			Self::Other(s) => s.idx = idx,
		}
	}
}
impl Display for ElfSection {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::SymbolTable(s) => write!(f, "Symbol table '{}' with {} symbols", s.name, s.symbols.len()),
			Self::RelocationTable(s) => write!(f, "Relocation table '{}' with {} relocations", s.name, s.relocations.len()),
			Self::Other(s) => write!(f, "Other section '{}' of len {} bytes", s.name, s.data.len())
		}
	}
}

#[derive(Debug)]
pub struct Section32 {
	pub idx: SectionIndex,
	pub name: String,
	pub header: SectionHeader32,
	pub data: Vec<u8>,
	pub relocations: Option<RelocationTable32>
}
impl Section32 {
	pub fn parse<R: ValueRead>(header: &SectionHeader32, rel_shdr: Option<&SectionHeader32>, data: &mut R, endian: Endianness) -> Result<Self, ParseError> {
		let content: Vec<u8>;
		let curr_pos = data.seek(SeekFrom::Current(0))?;

		data.seek(SeekFrom::Start(header.sh_offset.into()))?;
		content = data.read_bytes(header.sh_size as usize)?;

		data.seek(SeekFrom::Start(curr_pos))?;
		Ok(Self {
			idx: SectionIndex(0),
			name: String::new(),
			header: (*header).clone(),
			data: content,
			relocations: {
				match rel_shdr {
					Some(shdr) => Some(RelocationTable32::parse(shdr, data, endian)?),
					None => None
				}
			}
		})
	}

	#[inline] pub fn kind(&self) -> SectionKind { self.header.section_kind() }
}
impl Display for Section32 {
	fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
		write!(f, "Section {}", self.name)
	}
}

// ############################################################################
// Symbol table types
// ############################################################################

#[derive(Debug)]
pub enum SymbolBinding {
	Local,                  /* 0 */
	Global,                 /* 1 */
	Weak,                   /* 2 */

	Reserved(u8),

	OsSpecific(u8),         /* 10 - 12 */
	CpuSpecific(u8),        /* 13 - 15 */
}
impl From<u8> for SymbolBinding {
	fn from(val: u8) -> Self {
		match val {
			0 => Self::Local,
			1 => Self::Global,
			2 => Self::Weak,
			x if (10..=12).contains(&x) => Self::OsSpecific(x),
			y if (13..=15).contains(&y) => Self::CpuSpecific(y),
			z => Self::Reserved(z),
		}
	}
}
impl From<SymbolBinding> for u8 {
	fn from(val: SymbolBinding) -> Self {
		match val {
			SymbolBinding::Local => 0,
			SymbolBinding::Global => 1,
			SymbolBinding::Weak => 2,
			SymbolBinding::OsSpecific(x) | SymbolBinding::CpuSpecific(x) | SymbolBinding::Reserved(x) => x,
		}
	}
}

#[derive(Debug, PartialEq)]
pub enum SymbolKind {
	None,                   /* 0 */
	Object,                 /* 1 */
	Function,               /* 2 */
	Section,                /* 3 */
	File,                   /* 4 */
	Common,                 /* 5 */
	Tls,                    /* 6 */

	Reserved(u8),
	OsSpecific(u8),         /* 10 - 12 */
	CpuSpecific(u8),		/* 13 - 15 */
}
impl From<u8> for SymbolKind {
	fn from(val: u8) -> Self {
		match val {
			0 => Self::None,
			1 => Self::Object,
			2 => Self::Function,
			3 => Self::Section,
			4 => Self::File,
			5 => Self::Common,
			6 => Self::Tls,
			x if (10..=12).contains(&x) => Self::OsSpecific(x),
			y if (13..=15).contains(&y) => Self::CpuSpecific(y),
			z => Self::Reserved(z),
		}
	}
}
impl From<SymbolKind> for u8 {
	fn from(val: SymbolKind) -> Self {
		match val {
			SymbolKind::None => 0,
			SymbolKind::Object => 1,
			SymbolKind::Function => 2,
			SymbolKind::Section => 3,
			SymbolKind::File => 4,
			SymbolKind::Common => 5,
			SymbolKind::Tls => 6,
			SymbolKind::OsSpecific(x) | SymbolKind::CpuSpecific(x) | SymbolKind::Reserved(x) => x,
		}
	}
}
impl Add<SymbolBinding> for SymbolKind {
	type Output = u8;

	fn add(self, rhs: SymbolBinding) -> Self::Output {
		(Into::<Self::Output>::into(rhs) << 4) 
			+ Into::<Self::Output>::into(self)
	}
}

#[derive(Debug)]
pub enum SymbolVisibility {
	Default,
	Internal,
	Hidden,
	Protected,
	Exported,
	Singleton,
	Eliminate,
	Undefined(u8),
}
impl From<u8> for SymbolVisibility {
	fn from(val: u8) -> Self {
		match val {
			0 => Self::Default,
			1 => Self::Internal,
			2 => Self::Hidden,
			3 => Self::Protected,
			4 => Self::Exported,
			5 => Self::Singleton,
			6 => Self::Eliminate,
			x=> Self::Undefined(x),
		}
	}
}
impl From<SymbolVisibility> for u8 {
	fn from(val: SymbolVisibility) -> Self {
		match val {
			SymbolVisibility::Default => 0,
			SymbolVisibility::Internal => 1,
			SymbolVisibility::Hidden => 2,
			SymbolVisibility::Protected => 3,
			SymbolVisibility::Exported => 4,
			SymbolVisibility::Singleton => 5, 
			SymbolVisibility::Eliminate => 6,
			SymbolVisibility::Undefined(x) => x,
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct SymbolIndex(pub u32);

#[derive(Debug)]
pub struct Symbol32 {
	pub st_name: u32,
	pub st_value: u32,
	pub st_size: u32,
	pub st_info: u8,
	pub st_other: u8,
	pub st_shndx: SectionIndex,

	pub name: String,
	pub idx: SymbolIndex,
}
impl Symbol32 {
	pub fn parse<R: ValueRead>(data: &mut R, endian: Endianness) -> Result<Self, ParseError> {
		Ok(Self {
			st_name: data.read_u32(endian)?,
			st_value: data.read_u32(endian)?,
			st_size: data.read_u32(endian)?,
			st_info: data.read_u8()?,
			st_other: data.read_u8()?,
			st_shndx: SectionIndex(data.read_u16(endian)? as u32),
			name: String::new(),
			idx: SymbolIndex(0),
		})
	}

	pub fn as_bytes(&self, endian: Endianness) -> Vec<u8> {
		let mut bytes: Vec<u8> = Vec::new();
		match endian {
			Endianness::Little => {
				bytes.extend(self.st_name.to_le_bytes());
				bytes.extend(self.st_value.to_le_bytes());
				bytes.extend(self.st_size.to_le_bytes());
				bytes.extend(self.st_info.to_le_bytes());
				bytes.extend(self.st_other.to_le_bytes());
				bytes.extend((self.st_shndx.0 as u16).to_le_bytes());
			},
			Endianness::Big => {
				bytes.extend(self.st_name.to_be_bytes());
				bytes.extend(self.st_value.to_be_bytes());
				bytes.extend(self.st_size.to_be_bytes());
				bytes.extend(self.st_info.to_be_bytes());
				bytes.extend(self.st_other.to_be_bytes());
				bytes.extend((self.st_shndx.0 as u16).to_be_bytes());
			}
		}
		bytes
	}

	#[inline] pub fn kind(&self) -> SymbolKind { SymbolKind::from(self.st_info & 0xf) }
	#[inline] pub fn binding(&self) -> SymbolBinding { SymbolBinding::from(self.st_info >> 4) }
	#[inline] pub fn visibility(&self) -> SymbolVisibility { SymbolVisibility::from(self.st_other) }
}
impl Display for Symbol32 {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "Symbol '{}' (st_name: {}, st_value: {}, st_size: {}, st_info: {}, st_other: {}, st_shndx: {})", 
			self.name, self.st_name, self.st_value, self.st_size, self.st_info, self.st_other, self.st_shndx.0)
	}
}

#[derive(Debug)]
pub struct SymbolTable32 {
	pub idx: SectionIndex,
	pub name: String,
	pub header: SectionHeader32,
	pub symbols: Vec<Symbol32>,
}
impl SymbolTable32 {
	pub fn parse<R: ValueRead>(shdr: &SectionHeader32, strtab_hdr: &SectionHeader32, data: &mut R, endian: Endianness) -> Result<Self, ParseError> {
		data.seek(SeekFrom::Start(shdr.sh_offset as u64))?;

		let mut symtab: Self = Self { idx: SectionIndex(0), name: "".to_string(), header: (*shdr).clone(), symbols: Vec::new() };
		for idx in 0..(shdr.sh_size / shdr.sh_entsize) {
			let mut symbol = match Symbol32::parse(data, endian) {
				Ok(s) => s,
				Err(e) => return Err(ParseError {
					message: format!("Failed to parse symbol with idx {} at pos {} with stream len {}: {}", idx, data.stream_position()?, data.stream_len()?, e) 
				}),
			};
			symbol.idx = SymbolIndex(idx);
			symbol.name = strtab_hdr.get_string_at_offset(data, symbol.st_name).unwrap();
			symtab.symbols.push(symbol);
		}
		Ok(symtab)
	}

	pub fn as_bytes(&mut self, endian: Endianness) -> Vec<u8> {
		let mut bytes: Vec<u8> = Vec::new();
		for sym in self.symbols.iter() {
			bytes.extend(sym.as_bytes(endian));
		}
		self.header.sh_size = bytes.len() as u32;

		bytes
	}
	pub fn add_symbol(&mut self, mut symbol: Symbol32) -> SymbolIndex {
		self.symbols.sort_by(|a, b| a.idx.partial_cmp(&b.idx).unwrap());

		let idx = SymbolIndex(self.symbols[self.symbols.len() - 1].idx.0 + 1);
		symbol.idx = idx;
		self.symbols.push(symbol);
		idx
	}
	pub fn derive_string_table(&mut self) -> Section32 {
		let mut string_bytes: Vec<u8> = vec![0];
		for symbol in self.symbols.iter_mut() {
			if symbol.name.len() == 0 { continue; }

			symbol.st_name = string_bytes.len() as u32;

			// TODO possible optimization: check if the string already exists, also as part of other strings. ELF allows this!
			string_bytes.extend(symbol.name.as_nt_bytes());
		}
		let mut hdr = SectionHeader32::new(SectionKind::Strtab);
		hdr.sh_size = string_bytes.len() as u32;
		Section32 {
			idx: SectionIndex(0),
			name: ".strtab".to_string(),
			header: hdr,
			data: string_bytes,
			relocations: None,
		}
	}
}

// ############################################################################
// Relocation types
// ############################################################################

#[derive(Debug)]
pub struct Relocation32 {
	pub r_offset: u32,
	pub r_info: u32,
}
impl Relocation32 {
	pub fn parse<R: ValueRead>(data: &mut R, endian: Endianness) -> Result<Self, ParseError> {
		Ok(Self {
			r_offset: data.read_u32(endian)?,
			r_info: data.read_u32(endian)?,
		})
	}

	#[inline] pub fn get_symbol_idx(&self) -> SymbolIndex { SymbolIndex(self.r_info >> 8) }
	#[inline] pub fn set_symbol_idx(&mut self, idx: SymbolIndex) { self.r_info = (idx.0 << 8) + (self.r_info & 0xf) }
}

#[derive(Debug)]
pub struct RelocationTable32 {
	pub idx: SectionIndex,
	pub name: String,
	pub header: SectionHeader32,
	pub relocations: Vec<Relocation32>,
}
impl RelocationTable32 {
	pub fn parse<R: ValueRead>(shdr: &SectionHeader32, data: &mut R, endian: Endianness) -> Result<Self, ParseError> {
		data.seek(SeekFrom::Start(shdr.sh_offset as u64))?;

		let mut reltab: Self = Self { idx: SectionIndex(0), name: "".to_string(), header: (*shdr).clone(), relocations: Vec::new() };
		for idx in 0..(shdr.sh_size / shdr.sh_entsize) {
			let relocation = match Relocation32::parse(data, endian) {
				Ok(s) => s,
				Err(e) => return Err(ParseError {
					message: format!("Failed to parse relocation with idx {} at pos {} with stream len {}: {}", idx, data.stream_position()?, data.stream_len()?, e) 
				}),
			};
			reltab.relocations.push(relocation);
		}
		Ok(reltab)
	}

	pub fn new(symtab_idx: SectionIndex, link_section: SectionIndex) -> Self {
		let mut relo_table = RelocationTable32 {
			idx: SectionIndex(0),
			name: "".to_string(),
			header: SectionHeader32::new(SectionKind::Rel),
			relocations: Vec::new()
		};
		relo_table.header.sh_addralign = 4;
		relo_table.header.sh_link = symtab_idx.0;
		relo_table.header.sh_flags |= constants::SHF_INFO_LINK;
		relo_table.header.sh_info = link_section.0;
		relo_table.header.sh_entsize = 8;

		relo_table
	}

	pub fn add_relocation(&mut self, relo: Relocation32) -> () {
		self.relocations.push(relo);
		self.header.sh_size += 8;
	}

	pub fn as_bytes(&self, endian: Endianness) -> Vec<u8> {
		let mut bytes: Vec<u8> = Vec::new();
		for relo in self.relocations.iter() {
			match endian {
				Endianness::Little => {
					bytes.extend(relo.r_offset.to_le_bytes());
					bytes.extend(relo.r_info.to_le_bytes());
				},
				Endianness::Big => {
					bytes.extend(relo.r_offset.to_be_bytes());
					bytes.extend(relo.r_info.to_be_bytes());
				}
			}
		}
		bytes
	}
}

pub mod constants {

	pub const ELF_MAGIC: [u8; 4] = [ 0x7f, b'E', b'L', b'F' ];

	pub const ET_NONE: u16 = 0x0000;
	pub const ET_REL: u16 = 0x0001;
	pub const ET_EXEC: u16 = 0x0002;
	pub const ET_DYN: u16 = 0x0003;
	pub const ET_CORE: u16 = 0x0004;
	pub const ET_LOOS: u16 = 0xfefe;
	pub const ET_HIOS: u16 = 0xfefd;
	pub const ET_LOPROC: u16 = 0xff00;
	pub const ET_HIPROC: u16 = 0xffff;

	pub const SHT_NULL: u32 = 0x00000000;
	pub const SHT_PROGBITS: u32 = 0x00000001;
	pub const SHT_SYMTAB: u32 = 0x00000002;
	pub const SHT_STRTAB: u32 = 0x00000003;
	pub const SHT_RELA: u32 = 0x00000004;
	pub const SHT_HASH: u32 = 0x00000005;
	pub const SHT_DYNAMIC: u32 = 0x00000006;
	pub const SHT_NOTE: u32 = 0x00000007;
	pub const SHT_NOBITS: u32 = 0x00000008;
	pub const SHT_REL: u32 = 0x00000009;
	pub const SHT_SHLIB: u32 = 0x0000000a;
	pub const SHT_DYNSYM: u32 = 0x0000000b;
	pub const SHT_INIT_ARRAY: u32 = 0x0000000e;
	pub const SHT_FINI_ARRAY: u32 = 0x0000000f;
	pub const SHT_PREINIT_ARRAY: u32 = 0x00000010;
	pub const SHT_GROUP: u32 = 0x00000011;
	pub const SHT_SYMTAB_SHNDX: u32 = 0x00000012;
	pub const SHT_LOOS: u32 = 0x60000000;
	pub const SHT_HIOS: u32 = 0x6fffffff;
	pub const SHT_LOPROC: u32 = 0x70000000;
	pub const SHT_HIPROC: u32 = 0x7fffffff;
	pub const SHT_LOUSER: u32 = 0x80000000;
	pub const SHT_HIUSER: u32 = 0xffffffff;
	#[allow(unused)] pub const SHT_GNUVERDEF: u32 = 0x6ffffffd;
	#[allow(unused)] pub const SHT_GNUVERNEED: u32 = 0x6ffffffe;
	#[allow(unused)] pub const SHT_GNUVERSYM: u32 = 0x6fffffff;
	#[allow(unused)] pub const SHT_ARM_EXIDX: u32 = 0x70000001;

	#[allow(unused)] pub const SHF_WRITE: u32 = 1 << 0;
	#[allow(unused)] pub const SHF_ALLOC: u32 = 1 << 1;
	#[allow(unused)] pub const SHF_EXECINSTR: u32 = 1 << 2;
	#[allow(unused)] pub const SHF_MERGE: u32 = 1 << 4;
	#[allow(unused)] pub const SHF_STRINGS: u32 = 1 << 5;
	#[allow(unused)] pub const SHF_INFO_LINK: u32 = 1 << 6;
	#[allow(unused)] pub const SHF_LINK_ORDER: u32 = 1 << 7;
	#[allow(unused)] pub const SHF_OS_NONCONFORMING: u32 = 1 << 8;
	#[allow(unused)] pub const SHF_GROUP: u32 = 1 << 9;
	#[allow(unused)] pub const SHF_TLS: u32 = 1 << 10;
	#[allow(unused)] pub const SHF_MASKOS: u32 = 0x0ff00000;
	#[allow(unused)] pub const SHF_MASKPROC: u32 = 0xf0000000;
	#[allow(unused)] pub const SHF_ORDERED: u32 = 0x40000000;
	#[allow(unused)] pub const SHF_EXCLUDE: u32 = 0x80000000;

	pub const SHN_UNDEF: u16 = 0x0000;
	#[allow(unused)] pub const SHN_LORESERVE: u16 = 0xff00;
	#[allow(unused)] pub const SHN_LOPROC: u16 = 0xff00;
	#[allow(unused)] pub const SHN_HIPROC: u16 = 0xff1f;
	#[allow(unused)] pub const SHN_LOOS: u16 = 0xff20;
	#[allow(unused)] pub const SHN_HIOS: u16 = 0xff3f;
	#[allow(unused)] pub const SHN_ABS: u16 = 0xfff1;
	#[allow(unused)] pub const SHN_COMMON: u16 = 0xfff2;
	#[allow(unused)] pub const SHN_HIRESERVE: u16 = 0xffff;
	#[allow(unused)] pub const SHN_XINDEX: u16 = 0xffff;

	pub const PT_NULL: u32 = 0x00000000;
	pub const PT_LOAD: u32 = 0x00000001;
	pub const PT_DYNAMIC: u32 = 0x00000002;
	pub const PT_INTERP: u32 = 0x00000003;
	pub const PT_NOTE: u32 = 0x00000004;
	pub const PT_SHLIB: u32 = 0x00000005;
	pub const PT_PHDR: u32 = 0x00000006;
	pub const PT_TLS: u32 = 0x00000007;
	pub const PT_LOOS: u32 = 0x60000000;
	pub const PT_HIOS: u32 = 0x6fffffff;
	pub const PT_LOPROC: u32 = 0x70000000;
	pub const PT_HIPROC: u32 = 0x7fffffff;
	#[allow(unused)] pub const PT_ARM_EXIDX: u32 = 0x70000001;

	#[allow(unused)] pub const R_ARM_THM_JUMP24: u8 = 0x1e;
	//pub const R_ARM_THM_CALL: u32 = 0x00;
}