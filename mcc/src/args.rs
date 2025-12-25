use crate::{CompileStage, Optimizations};
use asm::x86::Target;
use clap::Parser;
use std::path::PathBuf;

#[derive(Parser)]
pub struct Args {
    /// Path to the c file we're compiling.
    pub file: PathBuf,
    /// [Debug] amount to compile
    pub stage: Option<CompileStage>,
    /// Compile to object file
    pub compile: bool,
    /// Operating System we're targetting
    #[arg(value_enum,default_value_t = if cfg!(target_os = "macos") {Target::Darwin} else{Target::Linux})]
    pub target: Target,
    /// Optimizations enabled (currently all disabled)
    #[command(flatten)]
    pub opts: Optimizations,
    /// Don't delete intermediate assembly
    #[arg(short = 'S', long = "keep-asm")]
    pub keep_asm: bool,
}
