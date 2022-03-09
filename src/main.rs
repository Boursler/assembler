use std::str::FromStr;

struct Instruction {
    operation: Ops,
    dst: Operand,
    src1: Operand,
    debuginfo: DebugInfo,
}

enum Ops {
    Add,
    Sub,
    Mul,
    Xor,
    And,
    Mov,
}

struct Register {
    index: u8,
    size: u8, // log_2(num_reg_bytes)
}

const rax: Register = Register { index: 0, size: 3 };
const rcx: Register = Register { index: 1, size: 3 };
const rdx: Register = Register { index: 2, size: 3 };
const rbx: Register = Register { index: 3, size: 3 };
const rsp: Register = Register { index: 4, size: 3 };
const rbp: Register = Register { index: 5, size: 3 };
const rsi: Register = Register { index: 6, size: 3 };
const rdi: Register = Register { index: 7, size: 3 };
const r8: Register = Register { index: 8, size: 3 };
const r9: Register = Register { index: 9, size: 3 };
const r10: Register = Register { index: 10, size: 3 };
const r11: Register = Register { index: 11, size: 3 };
const r12: Register = Register { index: 12, size: 3 };
const r13: Register = Register { index: 13, size: 3 };
const r14: Register = Register { index: 14, size: 3 };
const r15: Register = Register { index: 15, size: 3 };

const raxd: Register = Register { index: 0, size: 2 };
const rcxd: Register = Register { index: 1, size: 2 };
const rdxd: Register = Register { index: 2, size: 2 };
const rbxd: Register = Register { index: 3, size: 2 };
const rspd: Register = Register { index: 4, size: 2 };
const rbpd: Register = Register { index: 5, size: 2 };
const rsid: Register = Register { index: 6, size: 2 };
const rdid: Register = Register { index: 7, size: 2 };
const r8d: Register = Register { index: 8, size: 2 };
const r9d: Register = Register { index: 9, size: 2 };
const r10d: Register = Register { index: 10, size: 2 };
const r11d: Register = Register { index: 11, size: 2 };
const r12d: Register = Register { index: 12, size: 2 };
const r13d: Register = Register { index: 13, size: 2 };
const r14d: Register = Register { index: 14, size: 2 };
const r15d: Register = Register { index: 15, size: 2 };

const eax: Register = raxd;
const ecx: Register = rcxd;
const edx: Register = rdxd;
const ebx: Register = rbxd;
const esp: Register = rspd;
const ebp: Register = rbpd;
const esi: Register = rsid;
const edi: Register = rdid;

const raxw: Register = Register { index: 0, size: 1 };
const rcxw: Register = Register { index: 1, size: 1 };
const rdxw: Register = Register { index: 2, size: 1 };
const rbxw: Register = Register { index: 3, size: 1 };
const rspw: Register = Register { index: 4, size: 1 };
const rbpw: Register = Register { index: 5, size: 1 };
const rsiw: Register = Register { index: 6, size: 1 };
const rdiw: Register = Register { index: 7, size: 1 };
const r8w: Register = Register { index: 8, size: 1 };
const r9w: Register = Register { index: 9, size: 1 };
const r10w: Register = Register { index: 10, size: 1 };
const r11w: Register = Register { index: 11, size: 1 };
const r12w: Register = Register { index: 12, size: 1 };
const r13w: Register = Register { index: 13, size: 1 };
const r14w: Register = Register { index: 14, size: 1 };
const r15w: Register = Register { index: 15, size: 1 };

const ax: Register = raxw;
const cx: Register = rcxw;
const dx: Register = rdxw;
const bx: Register = rbxw;
const sp: Register = rspw;
const bp: Register = rbpw;
const si: Register = rsiw;
const di: Register = rdiw;

const raxb: Register = Register { index: 0, size: 0 };
const rcxb: Register = Register { index: 1, size: 0 };
const rdxb: Register = Register { index: 2, size: 0 };
const rbxb: Register = Register { index: 3, size: 0 };
const rspb: Register = Register { index: 4, size: 0 };
const rbpb: Register = Register { index: 5, size: 0 };
const rsib: Register = Register { index: 6, size: 0 };
const rdib: Register = Register { index: 7, size: 0 };
const r8b: Register = Register { index: 8, size: 0 };
const r9b: Register = Register { index: 9, size: 0 };
const r10b: Register = Register { index: 10, size: 0 };
const r11b: Register = Register { index: 11, size: 0 };
const r12b: Register = Register { index: 12, size: 0 };
const r13b: Register = Register { index: 13, size: 0 };
const r14b: Register = Register { index: 14, size: 0 };
const r15b: Register = Register { index: 15, size: 0 };

const al: Register = raxb;
const cl: Register = rcxb;
const dl: Register = rdxb;
const bl: Register = rbxb;

struct MemOperand {
    source: Register,
    index: Register,
    scale: u8,
    base: u32,
}
enum Operand {
    Unused,
    Register,
    Imm(u32),
    MemOperand,
}

struct DebugInfo {
    line: String,
    line_num: usize,
}

fn main() {
    println!("Hello, world!");
}

impl FromStr for Ops {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, String> {
        match s {
            "add" => Ok(Ops::Add),
            "sub" => Ok(Ops::Sub),
            "mul" => Ok(Ops::Mul),
            "xor" => Ok(Ops::Xor),
            "and" => Ok(Ops::And),
            "mov" => Ok(Ops::Mov),
            _ => Err(format!("unkown op {}", s)),
        }
    }
}

impl FromStr for Register {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, String> {
        match s {
            "rax" => Ok(rax),
            "rcx" => Ok(rcx),
            "rdx" => Ok(rdx),
            "rbx" => Ok(rbx),
            "rsp" => Ok(rsp),
            "rbp" => Ok(rbp),
            "rsi" => Ok(rsi),
            "rdi" => Ok(rdi),
            "r8" => Ok(r8),
            "r9" => Ok(r9),
            "r10" => Ok(r10),
            "r11" => Ok(r11),
            "r12" => Ok(r12),
            "r13" => Ok(r13),
            "r14" => Ok(r14),
            "r15" => Ok(r15),
            //32-bit reg
            "raxd" => Ok(raxd),
            "rcxd" => Ok(rcxd),
            "rdxd" => Ok(rdxd),
            "rbxd" => Ok(rbxd),
            "rspd" => Ok(rspd),
            "rbpd" => Ok(rbpd),
            "rsid" => Ok(rsid),
            "rdid" => Ok(rdid),
            "r8d" => Ok(r8d),
            "r9d" => Ok(r9d),
            "r10d" => Ok(r10d),
            "r11d" => Ok(r11d),
            "r12d" => Ok(r12d),
            "r13d" => Ok(r13d),
            "r14d" => Ok(r14d),
            "r15d" => Ok(r15d),
            _ => Err(format!(" not an expected register {}", s)),
        }
    }
}
