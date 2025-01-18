mod tacky;

use super::lex::Identifier;
use super::parse::BinaryOperator;
use super::parse::Constant as AstConstant;
use super::parse::Expression as AstExpression;
use super::parse::Factor as AstFactor;
use super::parse::Function as AstFunction;
use super::parse::Program as AstProgram;
use super::parse::Statement as AstStatement;
use super::parse::UnaryOperator;

use super::parse::Binary as AstBinary;
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

#[derive(Clone)]
enum Instruction<T> {
    Mov {
        src: T,
        dst: T,
    },
    Unary {
        operator: AsmUnaryOperator,
        operand: T,
    },
    Binary {
        operator: AsmBinaryOperator,
        src_op: T,
        dst_op: T,
    },
    AllocateStack(isize),
    Ret,
    Idiv {
        divisor: T,
    },
    Cdq,
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
                write!(writer, "subq ${amnt}, %rsp")
            }
            Self::Binary {
                operator,
                src_op,
                dst_op,
            } => {
                operator.emit(writer);
                write!(writer, " {src_op}, {dst_op}")
            }
            Self::Idiv { divisor } => {
                write!(writer, "idivl {divisor}")
            }
            Self::Cdq => {
                write!(writer, "cdq")
            }
        };
    }
}

mod convert_pass {
    use super::{
        tacky, BinaryOperator, Function, Identifier, Instruction, Operand, Program, Register,
    };
    use std::rc::Rc;
    #[derive(Clone)]
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
            TackyOp::Binary {
                operator,
                source_1,
                source_2,
                dst,
            } => {
                convert_binary(
                    operator,
                    source_1.into(),
                    source_2.into(),
                    dst.into(),
                    instructions,
                );
            }
            TackyOp::Return(var) => {
                let src = var.into();
                let dst = Register::Ax;
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

    use super::AsmBinaryOperator;
    fn convert_binary(
        op: BinaryOperator,
        source_1: PseudoOperand,
        source_2: PseudoOperand,
        dst: PseudoOperand,
        instructions: &mut Vec<Instruction<PseudoOperand>>,
    ) {
        /*instructions.push(Instruction::Mov {
            src: source_1.into(),
            dst: dst.clone().into(),
        });*/

        use Instruction as Op;
        let ops: &[Instruction<PseudoOperand>] = match op {
            // if its add subtrcat or multiply, then the only difference is the binary operator, so
            // I use an if elif else to handle it
            operator @ (BinaryOperator::Add
            | BinaryOperator::Subtract
            | BinaryOperator::Multiply) => {
                let operator = if operator == BinaryOperator::Add {
                    AsmBinaryOperator::Add
                } else if operator == BinaryOperator::Subtract {
                    AsmBinaryOperator::Sub
                } else {
                    AsmBinaryOperator::Mult
                };
                &[
                    Op::Mov {
                        src: source_1,
                        dst: dst.clone(),
                    },
                    Op::Binary {
                        operator,
                        src_op: source_2,
                        dst_op: dst,
                    },
                ]
            }
            operator @ (BinaryOperator::Divide | BinaryOperator::Remainder) => {
                // IDiv stores the result of division in AX, and the remiainder in Dx, but
                // otherwise the Remainder and Division instructions are the same
                let desired_register = if operator == BinaryOperator::Divide {
                    Register::Ax.into()
                } else {
                    Register::Dx.into()
                };
                &[
                    Op::Mov {
                        src: source_1.clone(),
                        dst: Register::Ax.into(),
                    },
                    Op::Cdq,
                    Op::Idiv { divisor: source_2 },
                    Op::Mov {
                        src: Register::Ax.into(),
                        dst: desired_register,
                    },
                ]
            }
        };
        instructions.extend_from_slice(ops);
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

    impl From<Register> for PseudoOperand {
        fn from(r: Register) -> Self {
            Self::Normal(Operand::Register(r))
        }
    }
}

mod fix_pass {
    use super::convert_pass::PseudoOperand;
    use super::AsmBinaryOperator;
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
                    let temp_register = Operand::Register(Register::R10);
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
            BadOp::Binary {
                operator,
                src_op,
                dst_op,
            } => {
                let mut src_op = fix_register(src_op, stack_frame);
                let dst_op = fix_register(dst_op, stack_frame);
                if let Operand::Stack(src_addr) = src_op {
                    let temp_register = Operand::Register(Register::R10);
                    vec.push(GoodOp::Mov {
                        src: Operand::Stack(src_addr),
                        dst: temp_register,
                    });
                    src_op = temp_register;
                }

                if operator == AsmBinaryOperator::Mult {
                    if let Operand::Stack(dst_addr) = dst_op {
                        let temp_register = Operand::Register(Register::R11);
                        vec.push(GoodOp::Mov {
                            src: Operand::Stack(dst_addr),
                            dst: temp_register,
                        });
                        vec.push(GoodOp::Binary {
                            operator: AsmBinaryOperator::Mult,
                            src_op,
                            dst_op: temp_register,
                        });

                        vec.push(GoodOp::Mov {
                            src: temp_register,
                            dst: Operand::Stack(dst_addr),
                        });
                        return;
                    }
                }
                vec.push(GoodOp::Binary {
                    operator,
                    src_op,
                    dst_op,
                })
            }
            BadOp::Idiv { divisor } => {
                let mut divisor = fix_register(divisor, stack_frame);
                if let Operand::Imm(value) = divisor {
                    let temp_register = Operand::Register(Register::R10);
                    vec.push(GoodOp::Mov {
                        src: Operand::Imm(value),
                        dst: temp_register,
                    });
                    divisor = temp_register;
                }
                vec.push(GoodOp::Idiv { divisor })
            }
            BadOp::AllocateStack(n) => vec.push(GoodOp::AllocateStack(n)),
            BadOp::Ret => vec.push(GoodOp::Ret),
            BadOp::Cdq => vec.push(GoodOp::Cdq),
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

#[derive(Clone, Copy, PartialEq, Eq)]
enum AsmUnaryOperator {
    Not,
    Neg,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum AsmBinaryOperator {
    Add,
    Sub,
    Mult,
}

impl AsmUnaryOperator {
    fn emit(&self, writer: &mut impl Write) {
        let _ = writer.write_all(match self {
            Self::Not => b"notl",
            Self::Neg => b"negl",
        });
    }
}

impl AsmBinaryOperator {
    fn emit(&self, writer: &mut impl Write) {
        let _ = writer.write_all(match self {
            Self::Add => b"addl",
            Self::Sub => b"subl",
            Self::Mult => b"imull",
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
    Ax,
    Dx,
    R10,
    R11,
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
            Self::Ax => "%eax",
            Self::Dx => "%edx",
            Self::R10 => "%r10d",
            Self::R11 => "%r11d",
        })
    }
}
