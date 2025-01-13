mod tacky;

use super::lex::Identifier;
use super::parse::Constant as AstConstant;
use super::parse::Expression as AstExpression;
use super::parse::Function as AstFunction;
use super::parse::Program as AstProgram;
use super::parse::Statement as AstStatement;
use super::parse::UnaryOperator;
use std::fmt::{self, Display, Formatter};
use std::io::Write;

pub fn generate(program: AstProgram, emit_asm: bool) -> Box<[u8]> {
    let tacky = tacky::emit(program);
    if emit_asm {
        let bad_ast = convert_pass::convert_tacky(tacky);
        let good_ast = fix_pass::fix_ast(bad_ast);
        emit_program(good_ast)
    } else {
        Box::new([])
    }
}

struct Program<T>(Function<T>);

fn emit_program(program: Program<Operand>) -> Box<[u8]> {
    emit_function(program.0)
}

struct Function<T> {
    name: Identifier,
    body: Box<[Instruction<T>]>,
}

fn emit_function(function: Function<Operand>) -> Box<[u8]> {
    let mut bytes = Vec::new();
    let _ = write!(
        bytes,
        "\t.globl {name}\n{name}:\n\tpushq %rbp\n\tmovq %rsp, %rbp",
        name = format_args!("_{}", function.name)
    );
    for instruction in &function.body {
        let _ = write!(bytes, "\n\t");
        instruction.emit(&mut bytes);
    }
    bytes.into()
}

enum Instruction<T> {
    Mov {
        src: T,
        dst: T,
    },
    Unary {
        operator: AsmUnaryOperator,
        operand: T,
    },
    AllocateStack(isize),
    Ret,
}

impl Instruction<Operand> {
    fn emit(&self, writer: &mut impl Write) {
        let _ = match self {
            Self::Mov { src, dst } => write!(writer, "movl  {src}, {dst}"),
            Self::Ret => write!(writer, "movq %rbp, %rsp\npopq %rbp\nret"),
            Self::Unary { operator, operand } => {
                operator.emit(writer);
                write!(writer, " {operand}")
            }
            Self::AllocateStack(amnt) => {
                write!(writer, " subq ${amnt}, %rsp")
            }
        };
    }
}

mod convert_pass {
    use super::{tacky, Function, Identifier, Instruction, Operand, Program, Register};
    use std::rc::Rc;
    pub enum PseudoOperand {
        Normal(Operand),
        PseudoRegister(Rc<Identifier>),
    }
    pub fn convert_tacky(program: tacky::Program) -> Program<PseudoOperand> {
        Program(convert_function(program.0))
    }

    fn convert_function(
        tacky::Function { name, body }: tacky::Function,
    ) -> Function<PseudoOperand> {
        let mut instructions = Vec::new();
        for op in body {
            convert_instruction(op, &mut instructions);
        }

        Function {
            name,
            body: instructions.into(),
        }
    }

    fn convert_instruction(
        instruction: tacky::Instruction,
        instructions: &mut Vec<Instruction<PseudoOperand>>,
    ) {
        use tacky::Instruction as TackyOp;
        match instruction {
            TackyOp::Return(var) => {
                let src = var.into();
                let dst = Register::AX;
                let dst = Operand::Register(dst);
                let dst = PseudoOperand::Normal(dst);
                instructions.push(Instruction::Mov { src, dst });
                instructions.push(Instruction::Ret);
            }
            TackyOp::Unary {
                op,
                source: src,
                dst,
            } => {
                instructions.push(Instruction::Mov {
                    src: src.into(),
                    dst: dst.clone().into(),
                });
                instructions.push(Instruction::Unary {
                    operator: op.into(),
                    operand: dst.into(),
                });
            }
        };
    }

    impl From<tacky::Value> for PseudoOperand {
        fn from(value: tacky::Value) -> Self {
            match value {
                tacky::Value::Constant(num) => Self::Normal(Operand::Imm(num)),
                tacky::Value::Var(name) => Self::PseudoRegister(name),
            }
        }
    }

    impl From<Rc<Identifier>> for PseudoOperand {
        fn from(value: Rc<Identifier>) -> Self {
            Self::PseudoRegister(value)
        }
    }
}

mod fix_pass {
    use super::convert_pass::PseudoOperand;
    use super::Register;
    use super::{Function, Identifier, Instruction, Operand, Program};
    use std::collections::HashMap;
    use std::rc::Rc;

    pub fn fix_ast(program: Program<PseudoOperand>) -> Program<Operand> {
        let main = fix_function(program.0);
        Program(main)
    }

    fn fix_function(function: Function<PseudoOperand>) -> Function<Operand> {
        let mut stack_frame = StackFrame::default();
        let mut body: Vec<Instruction<Operand>> = Vec::with_capacity(function.body.len() + 1);
        body.push(Instruction::AllocateStack(0));

        for op in get_iter(function.body) {
            fix_instruction(op, &mut stack_frame, &mut body);
        }

        if stack_frame.size == 0 {
            // eww eww eww sorry sorry sorry
            body.remove(0);
        } else {
            body[0] = Instruction::AllocateStack(stack_frame.size);
        }

        Function {
            name: function.name,
            body: body.into(),
        }
    }

    fn get_iter<T>(boxed_slice: Box<[T]>) -> std::vec::IntoIter<T> {
        <Box<[T]> as IntoIterator>::into_iter(boxed_slice)
    }
    #[derive(Default)]
    struct StackFrame {
        map: HashMap<Rc<Identifier>, isize>,
        size: isize,
    }

    impl StackFrame {
        fn get(&mut self, ident: &Rc<Identifier>) -> isize {
            if let Some(offset) = self.map.get(ident) {
                *offset
            } else {
                self.size -= 4;
                self.map.insert(ident.clone(), self.size);
                self.size
            }
        }
    }

    fn fix_instruction(
        op: Instruction<PseudoOperand>,
        stack_frame: &mut StackFrame,
        vec: &mut Vec<Instruction<Operand>>,
    ) {
        type BadOp = Instruction<PseudoOperand>;
        type GoodOp = Instruction<Operand>;
        match op {
            BadOp::Mov { src, dst } => {
                let src = fix_register(src, stack_frame);
                let dst = fix_register(dst, stack_frame);
                // we can't have the stack in source, so replace it with two instructions
                if let Operand::Stack(_) = src {
                    let temp_register = Operand::Register(Register::R10d);
                    vec.push(GoodOp::Mov {
                        src,
                        dst: temp_register,
                    });
                    vec.push(GoodOp::Mov {
                        src: temp_register,
                        dst,
                    });
                } else {
                    vec.push(GoodOp::Mov { src, dst });
                }
            }
            BadOp::Unary { operator, operand } => {
                let operand = fix_register(operand, stack_frame);
                vec.push(GoodOp::Unary { operator, operand })
            }
            BadOp::AllocateStack(n) => vec.push(GoodOp::AllocateStack(n)),
            BadOp::Ret => vec.push(GoodOp::Ret),
        };
    }

    fn fix_register(operand: PseudoOperand, stack_frame: &mut StackFrame) -> Operand {
        match operand {
            PseudoOperand::Normal(o) => o,
            PseudoOperand::PseudoRegister(name) => {
                let offset = stack_frame.get(&name);
                Operand::Stack(offset)
            }
        }
    }
}

enum AsmUnaryOperator {
    Not,
    Neg,
}

impl AsmUnaryOperator {
    fn emit(&self, writer: &mut impl Write) {
        let _ = writer.write_all(match self {
            Self::Not => b"notl",
            Self::Neg => b"negl",
        });
    }
}

impl From<UnaryOperator> for AsmUnaryOperator {
    fn from(op: UnaryOperator) -> Self {
        match op {
            UnaryOperator::Negate => Self::Neg,
            UnaryOperator::Complement => Self::Not,
        }
    }
}

#[derive(Clone, Copy)]
enum Operand {
    Imm(u64),
    Register(Register),
    Stack(isize),
}

#[derive(Clone, Copy)]
enum Register {
    AX,
    R10d,
}

impl Display for Operand {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Imm(val) => write!(f, "${}", val),
            Self::Register(r) => r.fmt(f),
            Self::Stack(n) => write!(f, "{}(%rbp)", n),
        }
    }
}

impl Display for Register {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_str(match self {
            Self::AX => "%eax",
            Self::R10d => "%r10d",
        })
    }
}
