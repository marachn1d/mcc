use clap::Parser;

use mcc::CompileStage;
use asm::x86::Target;
use std::fmt;
use std::io;

use mcc::CVersion;
use mcc::Config;
use mcc::Optimizations;
use mcc::CONFIG;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;

fn main() -> Result<(), MCCError> {
    let args = Args::parse();


    let _ = CONFIG.set(Config {
        stage: args.stage,
        version: CVersion::C23,
        opt: args.opts,
        target:args.target,
        emit_asm:args.keep_asm,
    });

    let output = args.file.with_extension("i");
    let preprocessed_file = preprocess(&args.file, output).map_err(MCCError::Preprocess)?;

    if let Some(object_file) = mcc::compile(preprocessed_file).map_err(MCCError::Compile)? && !args.keep_asm{
        assemble(&object_file, &args).map_err(MCCError::Assemble)
    }else{
        Ok(())
    }
}

#[derive(Parser)]
struct Args {
    file: PathBuf,
    #[arg(value_enum, default_value_t = CompileStage::Codegen, long = "stage")]
    stage: CompileStage,
    #[arg(short = 's', alias="s")]
    keep_asm:bool,

    #[arg(short = 'c', )]
    compile_object:bool,


    #[arg(value_enum, default_value_t)]
    target: Target,
    #[command(flatten)]
    opts: Optimizations,
}




fn preprocess(input: &Path, mut output: PathBuf) -> Result<PathBuf, io::Error> {
    output.set_extension("i");
    Command::new("gcc")
        .args(["-E", "-P"])
        .arg(input)
        .arg("-o")
        .arg(&output)
        .status()?;
    Ok(output)
}

fn assemble(input: &Path, args: &Args) -> Result<(), io::Error> {
    let output = input.with_extension(if args.compile_object { "o" } else { "" });
    let mut command = Command::new("gcc");
    command.arg(input).arg("-o").arg(&output);
    if args.compile_object {
        command.arg("-c");
    }

    command.status().map(|_| ())
}

#[derive(Debug)]
pub enum MCCError {
    Usage,
    Preprocess(io::Error),
    Compile(mcc::Error),
    Assemble(io::Error),
}

impl fmt::Display for MCCError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Usage => write!(f, "Usage: mcc [input] [output]\n Arguments: \n\t[input]:The File to Compile\n\t[output] Optional output, if [input] ends in .c, compiles to output with .c"),
            _ => todo!()
        }
    }
}
