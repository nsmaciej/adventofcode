mod common;
use common::*;

use std::str::FromStr;
use std::str::SplitAsciiWhitespace;

type Register = char;
type Value = i64;

enum EvalResult {
    Blocked,
    Continue,
    Send(Value),
}

#[derive(Debug)]
enum Operand {
    Reg(Register),
    Imm(Value),
}

#[derive(Debug)]
enum Instruction {
    Snd(Register),
    Set(Register, Operand),
    Add(Register, Operand),
    Mul(Register, Operand),
    Mod(Register, Operand),
    Rcv(Register),
    Jgz(Operand, Operand),
}

impl FromStr for Operand {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use Operand::*;
        Ok(s.parse()
            .map(Imm)
            .unwrap_or_else(|_| Reg(s.chars().next().unwrap())))
    }
}

struct InstructionParser<'a> {
    split: SplitAsciiWhitespace<'a>,
}

impl<'a> InstructionParser<'a> {
    fn new(instruction: &'a str) -> InstructionParser<'a> {
        InstructionParser {
            split: instruction.split_ascii_whitespace(),
        }
    }

    fn mnemonic(&mut self) -> &'a str {
        self.split.next().unwrap()
    }

    fn reg(&mut self) -> Register {
        self.split.next().unwrap().chars().next().unwrap()
    }

    fn op(&mut self) -> Operand {
        self.split.next().unwrap().parse().unwrap()
    }
}

impl FromStr for Instruction {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use Instruction::*;
        let mut parser = InstructionParser::new(s);
        let r = match parser.mnemonic() {
            "snd" => Snd(parser.reg()),
            "set" => Set(parser.reg(), parser.op()),
            "add" => Add(parser.reg(), parser.op()),
            "mul" => Mul(parser.reg(), parser.op()),
            "mod" => Mod(parser.reg(), parser.op()),
            "rcv" => Rcv(parser.reg()),
            "jgz" => Jgz(parser.op(), parser.op()),
            _ => panic!("Impossible instruction"),
        };
        Ok(r)
    }
}

trait IpcState {
    fn rcv(&mut self) -> Option<Value>;
}

#[derive(Debug)]
struct Vm<Q> {
    pc: Value,
    registers: HashMap<char, Value>,
    ipc_state: Q,
}

impl<Q> Vm<Q>
where
    Q: IpcState,
{
    fn operand(&self, operand: &Operand) -> Value {
        match operand {
            Operand::Reg(reg) => self.registers[reg],
            Operand::Imm(imm) => *imm,
        }
    }

    fn reg(&self, reg: char) -> Value {
        *self.registers.get(&reg).unwrap_or(&0)
    }

    fn eval(&mut self, instruction: &Instruction) -> EvalResult {
        use Instruction::*;
        match instruction {
            Snd(reg) => {
                self.pc += 1;
                return EvalResult::Send(self.reg(*reg));
            }
            Set(reg, op) => {
                self.registers.insert(*reg, self.operand(op));
            }
            Add(reg, op) => {
                let v = self.reg(*reg) + self.operand(op);
                self.registers.insert(*reg, v);
            }
            Mul(reg, op) => {
                let v = self.reg(*reg) * self.operand(op);
                self.registers.insert(*reg, v);
            }
            Mod(reg, op) => {
                let v = self.reg(*reg) % self.operand(op);
                self.registers.insert(*reg, v);
            }
            Rcv(reg) => match self.ipc_state.rcv() {
                Some(v) => {
                    self.registers.insert(*reg, v);
                }
                None => return EvalResult::Blocked,
            },
            Jgz(reg, op) => {
                self.pc += if self.operand(reg) > 0 {
                    self.operand(op)
                } else {
                    1
                };
                return EvalResult::Continue;
            }
        }
        self.pc += 1;
        return EvalResult::Continue;
    }

    fn empty(vm_id: Value, ipc_state: Q) -> Vm<Q> {
        let mut registers = HashMap::new();
        registers.insert('p', vm_id);
        Vm {
            pc: 0,
            registers,
            ipc_state,
        }
    }
}

#[derive(Debug)]
struct Part1Ipc {
    last_sound: Value,
}

impl IpcState for Part1Ipc {
    fn rcv(&mut self) -> Option<Value> {
        Some(self.last_sound)
    }
}

#[derive(Debug)]
struct Part2Ipc {
    values_sent: usize,
    queue: VecDeque<Value>,
}

impl IpcState for Part2Ipc {
    fn rcv(&mut self) -> Option<Value> {
        // println!("rcv {:?}", self.queue.front());
        self.queue.pop_front()
    }
}

impl Part2Ipc {
    fn snd(&mut self, value: Value) {
        self.values_sent += 1;
        self.queue.push_back(value)
    }

    fn empty() -> Part2Ipc {
        Part2Ipc {
            values_sent: 0,
            queue: VecDeque::new(),
        }
    }
}

fn part1(instructions: &[Instruction]) -> Value {
    let mut vm = Vm::empty(0, Part1Ipc { last_sound: 0 });
    loop {
        let current = &instructions[vm.pc as usize];
        if matches!(current, Instruction::Rcv(_)) && vm.ipc_state.last_sound != 0 {
            return vm.ipc_state.last_sound;
        }
        match vm.eval(current) {
            EvalResult::Blocked => unreachable!(),
            EvalResult::Continue => {}
            EvalResult::Send(v) => vm.ipc_state.last_sound = v,
        }
    }
}

fn part2(instructions: &[Instruction]) -> usize {
    let mut vm1 = Vm::empty(0, Part2Ipc::empty());
    let mut vm2 = Vm::empty(1, Part2Ipc::empty());
    loop {
        let mut blocked = 0;
        match vm1.eval(&instructions[vm1.pc as usize]) {
            EvalResult::Blocked => blocked += 1,
            EvalResult::Continue => {}
            EvalResult::Send(v) => vm2.ipc_state.snd(v),
        }
        match vm2.eval(&instructions[vm2.pc as usize]) {
            EvalResult::Blocked => blocked += 1,
            EvalResult::Continue => {}
            EvalResult::Send(v) => vm1.ipc_state.snd(v),
        }
        if blocked >= 2 {
            return vm1.ipc_state.values_sent;
        }
    }
}

fn main() {
    let instructions: Vec<Instruction> = aoc_input_lines(18)
        .iter()
        .map(|x| x.parse().unwrap())
        .collect();
    println!("{}", part1(&instructions));
    println!("{}", part2(&instructions));
}
