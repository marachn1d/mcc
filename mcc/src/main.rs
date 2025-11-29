use asm::x86::Target;
use mcc::CompileStage;
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

struct Args {
    file: PathBuf,
    stage: Option<CompileStage>,
    compile: bool,
    target: Target,
    opts: Optimizations,
}

impl Args {
    fn parse() -> Option<Self> {
        let mut path: Option<PathBuf> = None;
        let mut stage: Option<CompileStage> = None;
        let mut keep_asm = false;
        let mut compile: bool = false;
        let mut opts = Optimizations::default();
        let mut target = if cfg!(target_os = "linux") {
            Target::Linux
        } else {
            Target::Darwin
        };

        let mut args = std::env::args();
        args.next();

        for arg in args {
            match arg.as_str() {
                "--lex" => {
                    if !Self::try_update(&mut stage, CompileStage::Lex) {
                        return None;
                    }
                }
                "--parse" => {
                    if !Self::try_update(&mut stage, CompileStage::Parse) {
                        return None;
                    }
                }
                "--codegen" => {
                    if !Self::try_update(&mut stage, CompileStage::Codegen) {
                        return None;
                    }
                }
                "--tacky" => {
                    if !Self::try_update(&mut stage, CompileStage::Tacky) {
                        return None;
                    }
                }
                "--validate" => {
                    if !Self::try_update(&mut stage, CompileStage::Validate) {
                        return None;
                    }
                }
                "--fold-constants" => {
                    opts.constant_folding = true;
                }
                "--propogate-copies" => {
                    opts.copy_propogation = true;
                }
                "--eliminate-unreachable-code" => {
                    opts.unreachable_code = true;
                }

                "--eliminate-dead-stores" => {
                    opts.dead_store = true;
                }
                "--optimize" => opts = Optimizations::all(),
                "--linux" => target = Target::Linux,
                "--darwin" | "--macos" => target = Target::Darwin,
                "-S" => {
                    if keep_asm {
                        return None;
                    }
                    keep_asm = true;
                }
                "-c" => {
                    if compile {
                        return None;
                    }
                    compile = true;
                }
                new_path => {
                    if !Self::try_update(&mut path, new_path.into()) {
                        return None;
                    }
                }
            };
        }
        path.map(|file| Self {
            file,
            stage,
            compile,
            opts,
            target,
        })
    }

    fn try_update<T>(option: &mut Option<T>, new: T) -> bool {
        if option.is_none() {
            *option = Some(new);
            true
        } else {
            false
        }
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
            Self::Usage => write!(f, "Usage: mcc [input] [output]\n Arguments: \n\t[input]:The File to Compile\n\t[output] Optional output, if [input] ends in .c, compiles to output with .c"),
            _ => todo!()
        }
    }
}
