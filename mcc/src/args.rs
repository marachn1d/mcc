use crate::{CompileStage, Optimizations};
use asm::x86::Target;
use std::path::PathBuf;

pub struct Args {
    pub file: PathBuf,
    pub stage: Option<CompileStage>,
    pub compile: bool,
    pub target: Target,
    pub opts: Optimizations,
}

impl Args {
    pub fn parse() -> Option<Self> {
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
