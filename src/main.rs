use mcc::CVersion;
use mcc::CompileStage;
use mcc::Config;
use mcc::CONFIG;
use std::fs;
use std::io;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;

fn main() -> Result<(), Error> {
    let args = Args::parse()?;

    let _ = CONFIG.set(Config {
        stage: args.stage,
        version: CVersion::C23,
    });
    let output = args.file.with_extension("i");
    let preprocessed_file = preprocess(&args.file, output).map_err(Error::Preprocess)?;
    let Some(object_file) = mcc::compile(&preprocessed_file).map_err(Error::Compile)? else {
        return Ok(());
    };
    let _ = fs::remove_file(&preprocessed_file);
    if CONFIG.get().unwrap().stage.is_none() {
        assemble(&object_file, &args).map_err(Error::Assemble)
    } else {
        Ok(())
    }
}

struct Args {
    file: PathBuf,
    stage: Option<CompileStage>,
    compile: bool,
}

impl Args {
    fn parse() -> Result<Self, Error> {
        let mut path: Option<PathBuf> = None;
        let mut stage: Option<CompileStage> = None;
        let mut keep_asm = false;
        let mut compile: bool = false;

        let mut args = std::env::args();
        args.next();

        for arg in args {
            match arg.as_str() {
                "--lex" => Self::try_update(&mut stage, CompileStage::Lex)?,
                "--parse" => Self::try_update(&mut stage, CompileStage::Parse)?,
                "--codegen" => Self::try_update(&mut stage, CompileStage::Codegen)?,
                "--tacky" => Self::try_update(&mut stage, CompileStage::Tacky)?,
                "--validate" => Self::try_update(&mut stage, CompileStage::Validate)?,
                "-S" => {
                    if keep_asm {
                        return Err(Error::Usage);
                    }
                    keep_asm = true;
                }
                "-c" => {
                    if compile {
                        return Err(Error::Usage);
                    }
                    compile = true;
                }
                new_path => Self::try_update(&mut path, new_path.into())?,
            };
        }
        path.map_or(Err(Error::Usage), |file| {
            Ok(Self {
                file,
                stage,
                compile,
            })
        })
    }

    fn try_update<T>(option: &mut Option<T>, new: T) -> Result<(), Error> {
        if option.is_none() {
            *option = Some(new);
            Ok(())
        } else {
            Err(Error::Usage)
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

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Usage: mcc [input] [output]\n Arguments: \n\t[input]:The File to Compile\n\t[output] Optional output, if [input] ends in .c, compiles to output with .c")]
    Usage,
    #[error("Error Preprocessing: {0}")]
    Preprocess(io::Error),
    #[error("Error in Compilation: {0}")]
    Compile(mcc::Error),
    #[error("Error in Assembling: {0}")]
    Assemble(io::Error),
}
