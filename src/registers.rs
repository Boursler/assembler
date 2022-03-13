#![allow(non_upper_case_globals)]
use std::str::FromStr;

#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Debug)]
enum RegisterType {
    GPR,
}

#[repr(C, align(4))]
#[derive(Copy, Clone, PartialEq, Debug)]
pub struct Register {
    r#type: RegisterType,
    index: u8,
    size: u8, // log_2(num_reg_bytes)
}

impl FromStr for Register {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, String> {
        match s {
            //64-bit general purpose registers
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

            //32-bit general purpose registers
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
            "eax" => Ok(eax),
            "ecx" => Ok(ecx),
            "edx" => Ok(edx),
            "ebx" => Ok(ebx),
            "esp" => Ok(esp),
            "ebp" => Ok(ebp),
            "esi" => Ok(esi),
            "edi" => Ok(edi),

            //16-bit general purpose registers
            "raxw" => Ok(raxw),
            "rcxw" => Ok(rcxw),
            "rdxw" => Ok(rdxw),
            "rbxw" => Ok(rbxw),
            "rspw" => Ok(rspw),
            "rbpw" => Ok(rbpw),
            "rsiw" => Ok(rsiw),
            "rdiw" => Ok(rdiw),
            "r8w" => Ok(r8w),
            "r9w" => Ok(r9w),
            "r10w" => Ok(r10w),
            "r11w" => Ok(r11w),
            "r12w" => Ok(r12w),
            "r13w" => Ok(r13w),
            "r14w" => Ok(r14w),
            "r15w" => Ok(r15w),
            "ax" => Ok(ax),
            "cx" => Ok(cx),
            "dx" => Ok(dx),
            "bx" => Ok(bx),
            "sp" => Ok(sp),
            "bp" => Ok(bp),
            "si" => Ok(si),
            "di" => Ok(di),

            //8-bit registers
            "raxb" => Ok(raxb),
            "rcxb" => Ok(rcxb),
            "rdxb" => Ok(rdxb),
            "rbxb" => Ok(rbxb),
            "rspb" => Ok(rspb),
            "rbpb" => Ok(rbpb),
            "rsib" => Ok(rsib),
            "rdib" => Ok(rdib),
            "r8b" => Ok(r8b),
            "r9b" => Ok(r9b),
            "r10b" => Ok(r10b),
            "r11b" => Ok(r11b),
            "r12b" => Ok(r12b),
            "r13b" => Ok(r13b),
            "r14b" => Ok(r14b),
            "r15b" => Ok(r15b),
            "al" => Ok(al),
            "cl" => Ok(cl),
            "dl" => Ok(dl),
            "bl" => Ok(bl),
            _ => Err(format!("Unknown register: '{}'", s)),
        }
    }
}

pub const rax: Register = Register {
    r#type: RegisterType::GPR,
    index: 0,
    size: 3,
};
pub const rcx: Register = Register {
    r#type: RegisterType::GPR,
    index: 1,
    size: 3,
};
pub const rdx: Register = Register {
    r#type: RegisterType::GPR,
    index: 2,
    size: 3,
};
pub const rbx: Register = Register {
    r#type: RegisterType::GPR,
    index: 3,
    size: 3,
};
pub const rsp: Register = Register {
    r#type: RegisterType::GPR,
    index: 4,
    size: 3,
};
pub const rbp: Register = Register {
    r#type: RegisterType::GPR,
    index: 5,
    size: 3,
};
pub const rsi: Register = Register {
    r#type: RegisterType::GPR,
    index: 6,
    size: 3,
};
pub const rdi: Register = Register {
    r#type: RegisterType::GPR,
    index: 7,
    size: 3,
};
pub const r8: Register = Register {
    r#type: RegisterType::GPR,
    index: 8,
    size: 3,
};
pub const r9: Register = Register {
    r#type: RegisterType::GPR,
    index: 9,
    size: 3,
};
pub const r10: Register = Register {
    r#type: RegisterType::GPR,
    index: 10,
    size: 3,
};
pub const r11: Register = Register {
    r#type: RegisterType::GPR,
    index: 11,
    size: 3,
};
pub const r12: Register = Register {
    r#type: RegisterType::GPR,
    index: 12,
    size: 3,
};
pub const r13: Register = Register {
    r#type: RegisterType::GPR,
    index: 13,
    size: 3,
};
pub const r14: Register = Register {
    r#type: RegisterType::GPR,
    index: 14,
    size: 3,
};
pub const r15: Register = Register {
    r#type: RegisterType::GPR,
    index: 15,
    size: 3,
};

pub const raxd: Register = Register {
    r#type: RegisterType::GPR,
    index: 0,
    size: 2,
};
pub const rcxd: Register = Register {
    r#type: RegisterType::GPR,
    index: 1,
    size: 2,
};
pub const rdxd: Register = Register {
    r#type: RegisterType::GPR,
    index: 2,
    size: 2,
};
pub const rbxd: Register = Register {
    r#type: RegisterType::GPR,
    index: 3,
    size: 2,
};
pub const rspd: Register = Register {
    r#type: RegisterType::GPR,
    index: 4,
    size: 2,
};
pub const rbpd: Register = Register {
    r#type: RegisterType::GPR,
    index: 5,
    size: 2,
};
pub const rsid: Register = Register {
    r#type: RegisterType::GPR,
    index: 6,
    size: 2,
};
pub const rdid: Register = Register {
    r#type: RegisterType::GPR,
    index: 7,
    size: 2,
};
pub const r8d: Register = Register {
    r#type: RegisterType::GPR,
    index: 8,
    size: 2,
};
pub const r9d: Register = Register {
    r#type: RegisterType::GPR,
    index: 9,
    size: 2,
};
pub const r10d: Register = Register {
    r#type: RegisterType::GPR,
    index: 10,
    size: 2,
};
pub const r11d: Register = Register {
    r#type: RegisterType::GPR,
    index: 11,
    size: 2,
};
pub const r12d: Register = Register {
    r#type: RegisterType::GPR,
    index: 12,
    size: 2,
};
pub const r13d: Register = Register {
    r#type: RegisterType::GPR,
    index: 13,
    size: 2,
};
pub const r14d: Register = Register {
    r#type: RegisterType::GPR,
    index: 14,
    size: 2,
};
pub const r15d: Register = Register {
    r#type: RegisterType::GPR,
    index: 15,
    size: 2,
};

pub const eax: Register = raxd;
pub const ecx: Register = rcxd;
pub const edx: Register = rdxd;
pub const ebx: Register = rbxd;
pub const esp: Register = rspd;
pub const ebp: Register = rbpd;
pub const esi: Register = rsid;
pub const edi: Register = rdid;

pub const raxw: Register = Register {
    r#type: RegisterType::GPR,
    index: 0,
    size: 1,
};
pub const rcxw: Register = Register {
    r#type: RegisterType::GPR,
    index: 1,
    size: 1,
};
pub const rdxw: Register = Register {
    r#type: RegisterType::GPR,
    index: 2,
    size: 1,
};
pub const rbxw: Register = Register {
    r#type: RegisterType::GPR,
    index: 3,
    size: 1,
};
pub const rspw: Register = Register {
    r#type: RegisterType::GPR,
    index: 4,
    size: 1,
};
pub const rbpw: Register = Register {
    r#type: RegisterType::GPR,
    index: 5,
    size: 1,
};
pub const rsiw: Register = Register {
    r#type: RegisterType::GPR,
    index: 6,
    size: 1,
};
pub const rdiw: Register = Register {
    r#type: RegisterType::GPR,
    index: 7,
    size: 1,
};
pub const r8w: Register = Register {
    r#type: RegisterType::GPR,
    index: 8,
    size: 1,
};
pub const r9w: Register = Register {
    r#type: RegisterType::GPR,
    index: 9,
    size: 1,
};
pub const r10w: Register = Register {
    r#type: RegisterType::GPR,
    index: 10,
    size: 1,
};
pub const r11w: Register = Register {
    r#type: RegisterType::GPR,
    index: 11,
    size: 1,
};
pub const r12w: Register = Register {
    r#type: RegisterType::GPR,
    index: 12,
    size: 1,
};
pub const r13w: Register = Register {
    r#type: RegisterType::GPR,
    index: 13,
    size: 1,
};
pub const r14w: Register = Register {
    r#type: RegisterType::GPR,
    index: 14,
    size: 1,
};
pub const r15w: Register = Register {
    r#type: RegisterType::GPR,
    index: 15,
    size: 1,
};

pub const ax: Register = raxw;
pub const cx: Register = rcxw;
pub const dx: Register = rdxw;
pub const bx: Register = rbxw;
pub const sp: Register = rspw;
pub const bp: Register = rbpw;
pub const si: Register = rsiw;
pub const di: Register = rdiw;

pub const raxb: Register = Register {
    r#type: RegisterType::GPR,
    index: 0,
    size: 0,
};
pub const rcxb: Register = Register {
    r#type: RegisterType::GPR,
    index: 1,
    size: 0,
};
pub const rdxb: Register = Register {
    r#type: RegisterType::GPR,
    index: 2,
    size: 0,
};
pub const rbxb: Register = Register {
    r#type: RegisterType::GPR,
    index: 3,
    size: 0,
};
pub const rspb: Register = Register {
    r#type: RegisterType::GPR,
    index: 4,
    size: 0,
};
pub const rbpb: Register = Register {
    r#type: RegisterType::GPR,
    index: 5,
    size: 0,
};
pub const rsib: Register = Register {
    r#type: RegisterType::GPR,
    index: 6,
    size: 0,
};
pub const rdib: Register = Register {
    r#type: RegisterType::GPR,
    index: 7,
    size: 0,
};
pub const r8b: Register = Register {
    r#type: RegisterType::GPR,
    index: 8,
    size: 0,
};
pub const r9b: Register = Register {
    r#type: RegisterType::GPR,
    index: 9,
    size: 0,
};
pub const r10b: Register = Register {
    r#type: RegisterType::GPR,
    index: 10,
    size: 0,
};
pub const r11b: Register = Register {
    r#type: RegisterType::GPR,
    index: 11,
    size: 0,
};
pub const r12b: Register = Register {
    r#type: RegisterType::GPR,
    index: 12,
    size: 0,
};
pub const r13b: Register = Register {
    r#type: RegisterType::GPR,
    index: 13,
    size: 0,
};
pub const r14b: Register = Register {
    r#type: RegisterType::GPR,
    index: 14,
    size: 0,
};
pub const r15b: Register = Register {
    r#type: RegisterType::GPR,
    index: 15,
    size: 0,
};

pub const al: Register = raxb;
pub const cl: Register = rcxb;
pub const dl: Register = rdxb;
pub const bl: Register = rbxb;

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_register_from_str() {
        assert_eq!("rax".parse::<Register>(), Ok(rax));
        assert_eq!("eax".parse::<Register>(), Ok(raxd));
        assert_eq!("ax".parse::<Register>(), Ok(raxw));
        assert_eq!("al".parse::<Register>(), Ok(raxb));

        assert_eq!("r13".parse::<Register>(), Ok(r13));
        assert_eq!("r13d".parse::<Register>(), Ok(r13d));
        assert_eq!("r13w".parse::<Register>(), Ok(r13w));
        assert_eq!("r13b".parse::<Register>(), Ok(r13b));
    }
}
