use std::env;
use std::fmt;
use std::io;

use mcc::CompileStage;

use mcc::CVersion;
use mcc::Config;
use mcc::CONFIG;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;

/*
#[derive(PartialEq, Eq, Copy, Clone, Debug)]
pub enum Arg {
    Lex,
    Parse,
    Codegen,
    Compile,
    Tacky,
    Validate,
}
*/

fn main() -> Result<(), MCCError> {
    let input = parse_args().map_err(|()| MCCError::Usage)?;
    let output = input.with_extension("i");
    let preprocessed_file = preprocess(&input, output).map_err(MCCError::Preprocess)?;
    let object_file = mcc::compile(preprocessed_file).map_err(MCCError::Compile)?;
    if CONFIG.get().unwrap().stage.is_none() {
        assemble(&object_file).map_err(MCCError::Assemble)
    } else {
        Ok(())
    }
}

fn parse_args() -> Result<PathBuf, ()> {
    let mut args = env::args();
    // file name
    args.next();
    let mut stage: Option<CompileStage> = None;
    let mut version = CVersion::C17;
    let mut path: Option<PathBuf> = None;

    for arg in args.map(|x| Arg::parse(&x)) {
        let Some(arg) = arg else {
            return Err(());
        };
        match arg {
            Arg::Path(p) => {
                if path.is_some() {
                    return Err(());
                }
                path = Some(p);
            }
            Arg::Stage(s) => {
                if stage.is_some() {
                    return Err(());
                }
                stage = Some(s);
            }
            Arg::Version(v) => {
                if version == CVersion::C23 {
                    return Err(());
                }
                version = v;
            }
        }
    }

    let _ = CONFIG.set(Config { stage, version });
    let path = path.ok_or(())?;
    Ok(path)
}

enum Arg {
    Path(PathBuf),
    Stage(CompileStage),
    Version(CVersion),
}

impl Arg {
    fn parse(arg: &str) -> Option<Self> {
        match arg {
            "--lex" => Some(Self::Stage(CompileStage::Lex)),
            "--parse" => Some(Self::Stage(CompileStage::Parse)),
            "--codegen" => Some(Self::Stage(CompileStage::Codegen)),
            "-S" => Some(Self::Stage(CompileStage::Compile)),
            "--tacky" => Some(Self::Stage(CompileStage::Tacky)),
            "--validate" => Some(Self::Stage(CompileStage::Validate)),
            "--C23" => Some(Self::Version(CVersion::C23)),
            "--C17" => Some(Self::Version(CVersion::C17)),
            _ => {
                let file = PathBuf::from(arg);
                file.is_file().then_some(Self::Path(file))
            }
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

fn assemble(input: &Path) -> Result<(), io::Error> {
    let output = input.with_extension("");
    Command::new("gcc")
        .arg(input)
        .arg("-o")
        .arg(&output)
        .status()
        .map(|_| ())
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
