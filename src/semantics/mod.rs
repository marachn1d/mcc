mod check_labels;
mod resolve;
mod switch_cases;
use crate::parse::Program;
pub use check_labels::check as check_labels;
mod label_loops;
pub use resolve::resolve;

pub fn check(mut program: Program) -> Result<Program, Error> {
    let vars = resolve(&mut program).map_err(Error::Resolve)?;

    check_labels::check(&mut program, &vars).map_err(Error::Label)?;

    label_loops::label(&mut program).map_err(Error::Loops)?;
    Ok(program)
}

#[derive(Debug)]
pub enum Error {
    Label(LabelError),
    Loops(LoopError),
    Resolve(ResolveError),
}

pub use check_labels::Error as LabelError;
use label_loops::Error as LoopError;
pub use resolve::Error as ResolveError;
