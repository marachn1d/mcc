use std::env;
use std::fmt;
use std::io;

use std::path::Path;
use std::path::PathBuf;
use std::process::Command;

mod compiler;

fn main() -> Result<(), MCCError> {
    let (input, arg) = parse_args().map_err(|_| MCCError::Usage)?;
    let output = input.with_extension("i");
    let preprocessed_file = preprocess(&input, output).map_err(MCCError::Preprocess)?;
    let object_file = compiler::compile(preprocessed_file, arg).map_err(MCCError::Compile)?;
    if Some(Arg::Compile) != arg {
        assemble(object_file).map_err(MCCError::Assemble)
    } else {
        Ok(())
    }
}

fn parse_args() -> Result<(PathBuf, Option<Arg>), ()> {
    let mut args = env::args().map(PathBuf::from);
    // file name
    args.next();
    let input = args.next().ok_or(())?;
    let argument = Arg::parse(args.next().as_deref());
    Ok((input, argument))
}

#[derive(PartialEq, Eq, Copy, Clone)]
pub enum Arg {
    Lex,
    Parse,
    Codegen,
    Compile,
}

impl Arg {
    fn parse(arg: Option<&Path>) -> Option<Self> {
        let arg = arg?;
        let Ok(arg) = <&str>::try_from(arg.as_os_str()) else {
            return None;
        };
        match arg {
            "--lex" => Some(Arg::Lex),
            "--parse" => Some(Arg::Parse),
            "--codegen" => Some(Arg::Codegen),
            "-S" => Some(Arg::Compile),
            _ => None,
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

fn assemble(input: PathBuf) -> Result<(), io::Error> {
    let output = input.with_extension("");
    Command::new("gcc")
        .arg(&input)
        .arg("-o")
        .arg(&output)
        .status()
        .map(|_| ())
}

#[derive(Debug)]
pub enum MCCError {
    Usage,
    Preprocess(io::Error),
    Compile(compiler::Error),
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
