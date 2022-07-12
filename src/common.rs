use std::io::{Error as IOError};
use std::path::PathBuf;

pub enum ToolchainStage {
	Compiler,
	Assembler,
	Linker,
}

pub struct ToolchainOperation {
	pub stage: ToolchainStage,
	pub cmd: String,
	pub cmd_args: String,
	pub sources: Vec<String>,
	pub output: String,
}
impl ToolchainOperation {
	pub fn execute(&self) -> Result<(), IOError> {
		let output_path = PathBuf::from(&self.output);
		std::fs::create_dir_all(output_path.parent().unwrap()).unwrap();
	
		/* TODO: check / restrict commands that can be executed */
		let mut cmd = std::process::Command::new(&self.cmd);
		cmd.args(self.cmd_args.split(" "));
		cmd.args([ "-o", output_path.to_str().unwrap() ]);
		cmd.args(&self.sources);
	
		match cmd.output() {
			Ok(x) => {
				if x.status.success() {
					Ok(())
				} else {
					Err(IOError::new(std::io::ErrorKind::Other, String::from_utf8(x.stderr).unwrap()))
				}
			},
			Err(e) => Err(e),
		}
	}
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ReturnType {
	Bxlr,
	PopT1,
	PopT2,
	PopT3
}

#[derive(Debug)]
pub struct ReturnOperation {
	pub rtype: ReturnType,
	pub section_idx: usize,
	pub offset: usize,
}

//#[macro_export]
macro_rules! print_msg {
	($color:literal, $code:expr, $x:literal) => {
		println!("{}[ {} ]{}{}\x1b[0m", $color, $code, space_fill!($code.len() + 4, 10), $x)
	};
	($color:literal, $code:expr, $x:literal, $($y:expr), *) => {
		println!("{}[ {} ]{}{}\x1b[0m", $color, $code, space_fill!($code.len() + 4, 10), format!($x, $($y), *))
	};
}
//#[macro_export]
macro_rules! print_ok {
	($($x:tt) *) => {
		print_msg!("\x1b[32m", " OK ", $($x)*);
	};
}
//#[macro_export]
macro_rules! print_hint {
	($($x:tt) *) => {
		print_msg!("\x1b[33m", "HINT", $($x)*)
	};
}
//#[macro_export]
macro_rules! print_skip {
	($($x:tt) *) => {
		print_msg!("\x1b[33m", "SKIP", $($x)*)
	};
}
//#[macro_export]
macro_rules! print_err {
	($($x:tt) *) => {
		print_msg!("\x1b[31m", "FAIL", $($x)*)
	};
}
macro_rules! print_err_panic {
	($($x:tt) *) => {{
		print_msg!("\x1b[31m", "FAIL", $($x)*);
		panic!();
	}};
}

//#[macro_export]
macro_rules! space_fill {
	($len:expr, $til:literal) => {
		if $len <= $til {
			" ".repeat($til - $len)
		} else {
			" ".repeat($len % $til)
		}
	};
}

macro_rules! clear_bit {
	($value:expr, $bit:literal) => {
		$value &= !(1 << $bit)
	};
}

macro_rules! set_bit {
	($value:expr, $bit:literal) => {
		$value |= (1 << $bit)
	};
}