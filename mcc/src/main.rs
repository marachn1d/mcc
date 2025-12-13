use asm::x86::Target;
use mcc::CompileStage;
use std::fmt;
use std::io;

use mcc::args::Args;
use mcc::CVersion;
use mcc::Config;
use mcc::Optimizations;
use mcc::CONFIG;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;

fn main() -> Result<(), MCCError> {
    let Some(args) = Args::parse() else {
        return Err(MCCError::Usage);
    };

    let _ = CONFIG.set(Config {
        stage: args.stage,
        version: CVersion::C23,
        opt: args.opts,
        target: args.target,
    });

    let output = args.file.with_extension("i");
    let preprocessed_file = preprocess(&args.file, output).map_err(MCCError::Preprocess)?;
    let object_file = mcc::compile(preprocessed_file).map_err(MCCError::Compile)?;
    if CONFIG.get().unwrap().stage.is_none() {
        assemble(&object_file, &args).map_err(MCCError::Assemble)
    } else {
        Ok(())
    }
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
    let output = input.with_extension(if args.compile { "o" } else { "" });
    let mut command = Command::new("gcc");
    command.arg(input).arg("-o").arg(&output);
    if args.compile {
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
            Self::Usage => write!(
                f,
                "Usage: mcc [input] [output]\n Arguments: \n\t[input]:The File to Compile\n\t[output] Optional output, if [input] ends in .c, compiles to output with .c"
            ),
            _ => todo!(),
        }
    }
}
