use std::io::{Read};
use std::path::PathBuf;
use std::fs::{File};
use json::{self, JsonValue};

use crate::common::*;

pub struct WorkflowConfig {
	pub cfi_framework_precompiled: bool,
	pub cfi_compile_operation: Option<ToolchainOperation>,
	pub cfi_framework_source: Option<PathBuf>,

	pub process_targets: Vec<PathBuf>,
	pub skip_invalid_targets: bool,

	pub replace_bx_lr: bool,
	pub replace_pop_t1: bool,
	pub replace_pop_t2: bool,
	pub replace_pop_t3: bool,
	pub xor_in_prolog: bool,
}
impl WorkflowConfig {
	pub fn new() -> Self {
		WorkflowConfig {
			cfi_framework_precompiled: false,
			cfi_compile_operation: None,
			cfi_framework_source: None,
			process_targets: Vec::new(),
			skip_invalid_targets: false,

			replace_bx_lr: false,
			replace_pop_t1: false,
			replace_pop_t2: false,
			replace_pop_t3: false,
			xor_in_prolog: false,
		}
	}
}

pub fn parse_cfg_file(path: &str) -> WorkflowConfig {
	let cfg_path = match std::fs::canonicalize(path) {
		Ok(p) => p,
		Err(e) => { print_err!("Specified config file path is invalid ({})", e); panic!(); },
	};

	print_hint!("Config file: {}", cfg_path.to_str().unwrap());
	let mut cfg_file = File::open(cfg_path).unwrap();
	let mut cfg_str: String = String::new();
	cfg_file.read_to_string(&mut cfg_str).unwrap();

	let cfg_parsed = json::parse(&cfg_str[..]).unwrap();
	if !cfg_parsed.is_object() {
		print_err!("Config file has unsupported structure: root node is not a json object!");
		panic!();
	}
	if !cfg_parsed.has_key("cfi-framework") {
		print_err!("Config file does not have mandatory 'cfi-framework' section! You need to specify the CFI details!");
		panic!();
	}

	let mut workflow_cfg = WorkflowConfig::new();
	for (node_key, node_val) in cfg_parsed.entries() {
		match node_key {
			"cfi-framework" => {
				match node_val["pre-compiled"] {
					JsonValue::Boolean(true) => {
						workflow_cfg.cfi_framework_source = Some(PathBuf::from(
							node_val["source"]
								.as_str()
								.expect("Mandatory 'source' parameter does not exist or is invalid!")
						).canonicalize().unwrap());
						workflow_cfg.cfi_framework_precompiled = true;
					},
					JsonValue::Boolean(false) => {
						workflow_cfg.cfi_compile_operation = Some(ToolchainOperation {
							stage: match node_val["source-type"].as_str().expect("Mandatory 'source-type' invalid!") {
								"asm" => ToolchainStage::Assembler,
								"c" => ToolchainStage::Compiler,
								_ => panic!("")
							},
							cmd: String::from(node_val["compiler"].as_str().expect("Valid compiler must be specified!")),
							cmd_args: String::from(node_val["compile-args"].as_str().unwrap_or("")),
							sources: node_val["source"]
								.as_str()
								.expect("Mandatory 'source' parameter does not exist or is invalid!")
								.split(" ")
								.map(|x| String::from(x))
								.collect::<Vec<String>>(),
							output: String::from(node_val["output-file"].as_str().expect("Output file path must be specified!")),
						});
					},
					_ => print_err_panic!("Missing parameter 'pre-compiled' in config file!"),
				}
			},
			"process-targets" => {
				if node_val["skip_invalid"].is_boolean() && node_val["skip_invalid"].as_bool().unwrap_or(false) == true {
					workflow_cfg.skip_invalid_targets = true;
				}
				if node_val["files"].is_array() {
					for f in node_val["files"].members() {
						workflow_cfg.process_targets.push( PathBuf::from( f.as_str().unwrap() ).canonicalize().unwrap() );
					}
				} else {
					print_err_panic!("'files' node in 'process-targets' in config file must be an array!");
				}
			},
			"instrumentalization" => {
				workflow_cfg.replace_bx_lr = node_val["bx_lr"].as_bool().unwrap_or(false);
				workflow_cfg.replace_pop_t1 = node_val["pop_encoding_t1"].as_bool().unwrap_or(false);
				workflow_cfg.replace_pop_t2 = node_val["pop_encoding_t2"].as_bool().unwrap_or(false);
				workflow_cfg.replace_pop_t3 = node_val["pop_encoding_t3"].as_bool().unwrap_or(false);
				workflow_cfg.xor_in_prolog = node_val["xor_in_prolog"].as_bool().unwrap_or(false);
			}
			_ => () /* Just skip unsupported nodes! 🤷‍♂️ */
		};
	}
	
	workflow_cfg
}