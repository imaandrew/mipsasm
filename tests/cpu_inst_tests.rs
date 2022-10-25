mod common;
use common::{asm, disasm};
use regex::Regex;

macro_rules! test {
    ($name:ident, $inst:expr, $expected:expr) => {
        #[test]
        fn $name() {
            let inst = asm($inst);
            assert_eq!(inst, vec![$expected]);
            assert_eq!(disasm(&inst), $inst);
        }
    };
}

macro_rules! lbl_test {
    ($name:ident, $inst:expr, $expected:expr) => {
        #[test]
        fn $name() {
            let inst = asm($inst);
            assert_eq!(inst, vec![$expected]);
            let re = Regex::new(r"\w+:\n").unwrap();
            if let Some(x) = re.find_iter($inst).last() {
                let y = re.replace($inst, "");
                assert_eq!(
                    disasm(&inst),
                    y.replace(&x.as_str().replace(":\n", ""), "-0x1")
                );
                return;
            }

            assert_eq!(disasm(&inst), $inst);
        }
    };
}

#[test]
fn test_abs() {
    let inst = asm("abs $a0, $a1");
    assert_eq!(inst, vec![0x00050fc3, 0x00a12026, 0x00812023]);
}

test!(test_add, "add $a0, $a1, $a2", 0x00a62020);
test!(test_addi, "addi $a0, $a1, 0x8", 0x20a40008);
test!(test_addiu, "addiu $a0, $a1, 0x8", 0x24a40008);
test!(test_addu, "addu $a0, $a1, $a2", 0x00a62021);
test!(test_and, "and $a0, $a1, $a2", 0x00a62024);
test!(test_andi, "andi $a0, $a1, 0x8", 0x30a40008);

#[test]
fn test_b() {
    let inst = asm("B:\nb B");
    assert_eq!(inst, vec![0x1000ffff]);
}

#[test]
fn test_bal() {
    let inst = asm("BAL:\nbal BAL");
    assert_eq!(inst, vec![0x0411ffff]);
}

lbl_test!(test_bc0f, "BC0F:\nbc0f BC0F", 0x4100ffff);
lbl_test!(test_bc1f, "BC1F:\nbc1f BC1F", 0x4500ffff);
lbl_test!(test_bc0fl, "BC0FL:\nbc0fl BC0FL", 0x4102ffff);
lbl_test!(test_bc1fl, "BC1FL:\nbc1fl BC1FL", 0x4502ffff);
lbl_test!(test_bc0t, "BC0T:\nbc0t BC0T", 0x4101ffff);
lbl_test!(test_bc1t, "BC1T:\nbc1t BC1T", 0x4501ffff);
lbl_test!(test_bc0tl, "BC0TL:\nbc0tl BC0TL", 0x4103ffff);
lbl_test!(test_bc1tl, "BC1TL:\nbc1tl BC1TL", 0x4503ffff);

lbl_test!(test_beq, "BEQ:\nbeq $a0, $a1, BEQ", 0x1085ffff);
lbl_test!(test_beql, "BEQL:\nbeql $a0, $a1, BEQL", 0x5085ffff);

#[test]
fn test_beqz() {
    let inst = asm("BEQZ:\nbeqz $a0, BEQZ");
    assert_eq!(inst, vec![0x1080ffff]);
}

#[test]
fn test_beqzl() {
    let inst = asm("BEQZL:\nbeqzl $a0, BEQZL");
    assert_eq!(inst, vec![0x5080ffff]);
}

#[test]
fn test_bge() {
    let inst = asm("BGE:\nbge $a0, $a1, BGE");
    assert_eq!(inst, vec![0x0085082a, 0x1020fffe]);
}

#[test]
fn test_bgel() {
    let inst = asm("BGEL:\nbgel $a0, $a1, BGEL");
    assert_eq!(inst, vec![0x0085082a, 0x5020fffe]);
}

#[test]
fn test_bgeu() {
    let inst = asm("BGEU:\nbgeu $a0, $a1, BGEU");
    assert_eq!(inst, vec![0x0085082b, 0x1020fffe]);
}

#[test]
fn test_bgeul() {
    let inst = asm("BGEUL:\nbgeul $a0, $a1, BGEUL");
    assert_eq!(inst, vec![0x0085082b, 0x5020fffe]);
}

lbl_test!(test_bgez, "BGEZ:\nbgez $a0, BGEZ", 0x0481ffff);
lbl_test!(test_bgezal, "BGEZAL:\nbgezal $a0, BGEZAL", 0x0491ffff);
lbl_test!(test_bgezall, "BGEZALL:\nbgezall $a0, BGEZALL", 0x0493ffff);
lbl_test!(test_bgezl, "BGEZL:\nbgezl $a0, BGEZL", 0x0483ffff);

#[test]
fn test_bgt() {
    let inst = asm("BGT:\nbgt $a0, $a1, BGT");
    assert_eq!(inst, vec![0x00a4082a, 0x1420fffe]);
}

#[test]
fn test_bgtl() {
    let inst = asm("BGTL:\nbgtl $a0, $a1, BGTL");
    assert_eq!(inst, vec![0x00a4082a, 0x5420fffe]);
}

#[test]
fn test_bgtu() {
    let inst = asm("BGTU:\nbgtu $a0, $a1, BGTU");
    assert_eq!(inst, vec![0x00a4082b, 0x1420fffe]);
}

#[test]
fn test_bgtul() {
    let inst = asm("BGTUL:\nbgtul $a0, $a1, BGTUL");
    assert_eq!(inst, vec![0x00a4082b, 0x5420fffe]);
}

lbl_test!(test_bgtz, "BGTZ:\nbgtz $a0, BGTZ", 0x1c80ffff);
lbl_test!(test_bgtzl, "BGTZL:\nbgtzl $a0, BGTZL", 0x5c80ffff);

#[test]
fn test_ble() {
    let inst = asm("BLE:\nble $a0, $a1, BLE");
    assert_eq!(inst, vec![0x00a4082a, 0x1020fffe]);
}

#[test]
fn test_blel() {
    let inst = asm("BLEL:\nblel $a0, $a1, BLEL");
    assert_eq!(inst, vec![0x00a4082a, 0x5020fffe]);
}

#[test]
fn test_bleu() {
    let inst = asm("BLEU:\nbleu $a0, $a1, BLEU");
    assert_eq!(inst, vec![0x00a4082b, 0x1020fffe]);
}

#[test]
fn test_bleul() {
    let inst = asm("BLEUL:\nbleul $a0, $a1, BLEUL");
    assert_eq!(inst, vec![0x00a4082b, 0x5020fffe]);
}

lbl_test!(test_blez, "BLEZ:\nblez $a0, BLEZ", 0x1880ffff);
lbl_test!(test_blezl, "BLEZL:\nblezl $a0, BLEZL", 0x5880ffff);

#[test]
fn test_blt() {
    let inst = asm("BLT:\nblt $a0, $a1, BLT");
    assert_eq!(inst, vec![0x0085082a, 0x1420fffe]);
}

#[test]
fn test_bltl() {
    let inst = asm("BLTL:\nbltl $a0, $a1, BLTL");
    assert_eq!(inst, vec![0x0085082a, 0x5420fffe]);
}

#[test]
fn test_bltu() {
    let inst = asm("BLTU:\nbltu $a0, $a1, BLTU");
    assert_eq!(inst, vec![0x0085082b, 0x1420fffe]);
}

#[test]
fn test_bltul() {
    let inst = asm("BLTUL:\nbltul $a0, $a1, BLTUL");
    assert_eq!(inst, vec![0x0085082b, 0x5420fffe]);
}

lbl_test!(test_bltz, "BLTZ:\nbltz $a0, BLTZ", 0x0480ffff);
lbl_test!(test_bltzal, "BLTZAL:\nbltzal $a0, BLTZAL", 0x0490ffff);
lbl_test!(test_bltzall, "BLTZALL:\nbltzall $a0, BLTZALL", 0x0492ffff);
lbl_test!(test_bltzl, "BLTZL:\nbltzl $a0, BLTZL", 0x0482ffff);

lbl_test!(test_bne, "BNE:\nbne $a0, $a1, BNE", 0x1485ffff);
lbl_test!(test_bnel, "BNEL:\nbnel $a0, $a1, BNEL", 0x5485ffff);

#[test]
fn test_bnez() {
    let inst = asm("BNEZ:\nbnez $a0, BNEZ");
    assert_eq!(inst, vec![0x1480ffff]);
}

#[test]
fn test_bnezl() {
    let inst = asm("BNEZL:\nbnezl $a0, BNEZL");
    assert_eq!(inst, vec![0x5480ffff]);
}

test!(test_break, "break", 0x0000000d);
test!(test_break_imm, "break 0x20", 0x0000080d);
test!(test_cache, "cache 0x14, 0x10($a0)", 0xbc940010);
test!(test_cfc0, "cfc0 $a0, Context", 0x40442000);
test!(test_cfc1, "cfc1 $a0, $fa1", 0x44447000);

#[test]
fn test_clear() {
    let inst = asm("clear $a0");
    assert_eq!(inst, vec![0x00002021]);
}

test!(test_ctc0, "ctc0 $a0, Context", 0x40c42000);
test!(test_ctc1, "ctc1 $a0, $fa1", 0x44c47000);

#[test]
fn test_dabs() {
    let inst = asm("dabs $a0, $a1");
    assert_eq!(inst, vec![0x00050ffb, 0x00a12026, 0x0081202e]);
}

test!(test_dadd, "dadd $a0, $a1, $a2", 0x00a6202c);
test!(test_daddi, "daddi $a0, $a1, 0x8", 0x60a40008);
test!(test_daddiu, "daddiu $a0, $a1, 0x8", 0x64a40008);
test!(test_daddu, "daddu $a0, $a1, $a2", 0x00a6202d);
test!(test_ddiv, "ddiv $a0, $a1", 0x0085001e);

// #[test]
// fn test_ddiv_pseudo() {
//     let inst = asm("ddiv $a0, $a1, $a2");
//     assert_eq!(inst, vec![0x00c001f4, 0x00a6001e, 0x6401ffff, 0x14c10004, 0x64010001, 0x00010ffc, 0x012101b4, 0x00002012]);
// }

test!(test_ddivu, "ddivu $a0, $a1", 0x0085001f);

#[test]
fn test_ddivu_pseudo() {
    let inst = asm("ddivu $a0, $a1, $a2");
    assert_eq!(inst, vec![0x00c001f4, 0x00a6001f, 0x00002012]);
}

test!(test_div, "div $a0, $a1", 0x0085001a);
test!(test_divu, "divu $a0, $a1", 0x0085001b);

#[test]
fn test_div_pseudo() {
    let inst = asm("div $a0, $a1, $a2");
    assert_eq!(
        inst,
        vec![
            0x00c001f4, 0x00a6001a, 0x3c01ffff, 0x3421ffff, 0x14c10003, 0x3c018000, 0x00a101b4,
            0x00002012
        ]
    );
}

#[test]
fn test_divu_pseudo() {
    let inst = asm("divu $a0, $a1, $a2");
    assert_eq!(inst, vec![0x00c001f4, 0x00a6001b, 0x00002012]);
}

#[test]
fn test_dli_pos() {
    let inst = asm("dli $a0, 0x123456789abcdef0");
    assert_eq!(
        inst,
        vec![0x3c041234, 0x34845678, 0x00042438, 0x34849abc, 0x00042438, 0x3484def0]
    );
}

#[test]
fn test_dli_neg() {
    let inst = asm("dli $a0, -423023405674592");
    assert_eq!(
        inst,
        vec![0x3c04fffe, 0x34847f43, 0x00042438, 0x3484302c, 0x00042438, 0x34844fa0]
    );
}

test!(test_dmfc0, "dmfc0 $a0, Context", 0x40242000);
test!(test_dmfc1, "dmfc1 $a0, $fa1", 0x44247000);

#[test]
fn test_dmove() {
    let inst = asm("dmove $a0, $a1");
    assert_eq!(inst, vec![0x00a0202d]);
}

#[test]
fn test_dmul() {
    let inst = asm("dmul $a0, $a1, $a2");
    assert_eq!(inst, vec![0x00a6001c, 0x00002012]);
}

#[test]
fn test_dmulu() {
    let inst = asm("dmulu $a0, $a1, $a2");
    assert_eq!(inst, vec![0x00a6001d, 0x00002012]);
}

#[test]
fn test_dmulo() {
    let inst = asm("dmulo $a0, $a1, $a2");
    assert_eq!(
        inst,
        vec![0x00a6001c, 0x00002012, 0x000427ff, 0x00000810, 0x008101b6, 0x00002012]
    );
}

#[test]
fn test_dmulou() {
    let inst = asm("dmulou $a0, $a1, $a2");
    assert_eq!(inst, vec![0x00a6001d, 0x00000810, 0x00002012, 0x002001b6]);
}

test!(test_dmult, "dmult $a0, $a1", 0x0085001c);
test!(test_dmultu, "dmultu $a0, $a1", 0x0085001d);
test!(test_dmtc0, "dmtc0 $a0, Context", 0x40a42000);
test!(test_dmtc1, "dmtc1 $a0, $fa1", 0x44a47000);

#[test]
fn test_dneg() {
    let inst = asm("dneg $a0, $a1");
    assert_eq!(inst, vec![0x0005202e]);
}

#[test]
fn test_dnegu() {
    let inst = asm("dnegu $a0, $a1");
    assert_eq!(inst, vec![0x0005202f]);
}

//#[test]
//fn test_drem() {
//    let inst = asm("drem $a0, $a1, $a2");
//    assert_eq!(inst, vec![0x00c001f4, 0x00a6001e, 0x6401ffff, 0x14c10004, 0x64010001, 0x00010ffc, 0x012101b4, 0x00002010]);
//}

#[test]
fn test_dremu() {
    let inst = asm("dremu $a0, $a1, $a2");
    assert_eq!(inst, vec![0x00c001f4, 0x00a6001f, 0x00002010]);
}

#[test]
fn test_drol() {
    let inst = asm("drol $a0, $a1, $a2");
    assert_eq!(inst, vec![0x0006082f, 0x00250816, 0x00c52014, 0x00812025]);
}

#[test]
fn test_dror() {
    let inst = asm("dror $a0, $a1, $a2");
    assert_eq!(inst, vec![0x0006082f, 0x00250814, 0x00c52016, 0x00812025]);
}

test!(test_dsll, "dsll $a0, $a1, 0x1f", 0x000527f8);
test!(test_dsll32, "dsll32 $a0, $a1, 0x1f", 0x000527fc);
test!(test_dsllv, "dsllv $a0, $a1, $a2", 0x00c52014);
test!(test_dsra, "dsra $a0, $a1, 0x1f", 0x000527fb);
test!(test_dsra32, "dsra32 $a0, $a1, 0x1f", 0x000527ff);
test!(test_dsrav, "dsrav $a0, $a1, $a2", 0x00c52017);
test!(test_dsrl, "dsrl $a0, $a1, 0x1f", 0x000527fa);
test!(test_dsrl32, "dsrl32 $a0, $a1, 0x1f", 0x000527fe);
test!(test_dsrlv, "dsrlv $a0, $a1, $a2", 0x00c52016);
test!(test_dsub, "dsub $a0, $a1, $a2", 0x00a6202e);

#[test]
fn test_dsubi() {
    let inst = asm("dsubi $a0, $a1, 0x8");
    assert_eq!(inst, vec![0x60a4fff8]);
}

#[test]
fn test_dsubiu() {
    let inst = asm("dsubiu $a0, $a1, 0x8");
    assert_eq!(inst, vec![0x64a4fff8]);
}

test!(test_dsubu, "dsubu $a0, $a1, $a2", 0x00a6202f);
test!(test_eret, "eret", 0x42000018);
test!(test_j, "j 0x80000000", 0x08000000);
test!(test_jal, "jal 0x80000000", 0x0c000000);
test!(test_jr, "jr $ra", 0x03e00008);
test!(test_jalr, "jalr $ra", 0x03e0f809);
test!(test_jalr_two_args, "jalr $a0, $a1", 0x00a02009);
test!(test_lb, "lb $a0, 0x10($a1)", 0x80a40010);
test!(test_lbu, "lbu $a0, 0x10($a1)", 0x90a40010);
test!(test_ld, "ld $a0, 0x10($a1)", 0xdca40010);
test!(test_ldc1, "ldc1 $ft0, 0x8($a0)", 0xd4840008);
test!(test_ldl, "ldl $a0, 0x10($a1)", 0x68a40010);
test!(test_ldr, "ldr $a0, 0x10($a1)", 0x6ca40010);
test!(test_lh, "lh $a0, 0x10($a1)", 0x84a40010);
test!(test_lhu, "lhu $a0, 0x10($a1)", 0x94a40010);
test!(test_ll, "ll $a0, 0x10($a1)", 0xc0a40010);
test!(test_lld, "lld $a0, 0x10($a1)", 0xd0a40010);
test!(test_lwc1, "lwc1 $ft0, 0x8($a0)", 0xc4840008);
test!(test_lwu, "lwu $a0, 0x10($a1)", 0x9ca40010);

#[test]
fn test_lli() {
    let inst = asm("lli $a0, 0x10");
    assert_eq!(inst, vec![0x34040010]);
}

#[test]
fn test_li() {
    let inst = asm("li $a0, 0x12345678");
    assert_eq!(inst, vec![0x3c041234, 0x34845678]);
}

test!(test_lui, "lui $a0, 0x8000", 0x3c048000);
test!(test_lw, "lw $a0, 0x10($a1)", 0x8ca40010);
test!(test_lwl, "lwl $a0, 0x10($a1)", 0x88a40010);
test!(test_lwr, "lwr $a0, 0x10($a1)", 0x98a40010);

#[test]
fn test_move() {
    let inst = asm("move $a0, $a1");
    assert_eq!(inst, vec![0x00052021]);
}

test!(test_mfc0, "mfc0 $a0, Context", 0x40042000);
test!(test_mfc1, "mfc1 $a0, $fa1", 0x44047000);
test!(test_mfhi, "mfhi $a0", 0x00002010);
test!(test_mflo, "mflo $a0", 0x00002012);

#[test]
fn test_mul() {
    let inst = asm("mul $a0, $a1, $a2");
    assert_eq!(inst, vec![0x00a60018, 0x00002012]);
}

#[test]
fn test_mulu() {
    let inst = asm("mulu $a0, $a1, $a2");
    assert_eq!(inst, vec![0x00a60019, 0x00002012]);
}

#[test]
fn test_mulo() {
    let inst = asm("mulo $a0, $a1, $a2");
    assert_eq!(
        inst,
        vec![0x00a60018, 0x00002012, 0x000427c3, 0x00000810, 0x008101b6, 0x00002012]
    );
}

#[test]
fn test_mulou() {
    let inst = asm("mulou $a0, $a1, $a2");
    assert_eq!(inst, vec![0x00a60019, 0x00000810, 0x00002012, 0x002001b6]);
}

test!(test_mult, "mult $a0, $a1", 0x00850018);
test!(test_multu, "multu $a0, $a1", 0x00850019);
test!(test_mtc0, "mtc0 $a0, Context", 0x40842000);
test!(test_mtc1, "mtc1 $a0, $fa1", 0x44847000);
test!(test_mthi, "mthi $a0", 0x00800011);
test!(test_mtlo, "mtlo $a0", 0x00800013);

#[test]
fn test_neg() {
    let inst = asm("neg $a0, $a1");
    assert_eq!(inst, vec![0x00052022]);
}

#[test]
fn test_negu() {
    let inst = asm("negu $a0, $a1");
    assert_eq!(inst, vec![0x00052023]);
}

#[test]
fn test_nop() {
    let inst = asm("nop");
    assert_eq!(inst, vec![0x00000000]);
}

test!(test_nor, "nor $a0, $a1, $a2", 0x00a62027);

#[test]
fn test_not() {
    let inst = asm("not $a0, $a1");
    assert_eq!(inst, vec![0x00a02027]);
}

test!(test_or, "or $a0, $a1, $a2", 0x00a62025);
test!(test_ori, "ori $a0, $a1, 0x8", 0x34a40008);

#[test]
fn test_rem() {
    let inst = asm("rem $a0, $a1, $a2");
    assert_eq!(
        inst,
        vec![
            0x00c001f4, 0x00a6001a, 0x3c01ffff, 0x3421ffff, 0x14c10003, 0x3c018000, 0x00a101b4,
            0x00002010
        ]
    );
}

#[test]
fn test_remu() {
    let inst = asm("remu $a0, $a1, $a2");
    assert_eq!(inst, vec![0x00c001f4, 0x00a6001b, 0x00002010]);
}

test!(test_sb, "sb $a0, 0x10($a1)", 0xa0a40010);
test!(test_sc, "sc $a0, 0x10($a1)", 0xe0a40010);
test!(test_scd, "scd $a0, 0x10($a1)", 0xf0a40010);
test!(test_sd, "sd $a0, 0x10($a1)", 0xfca40010);
test!(test_sdc1, "sdc1 $ft0, 0x8($a0)", 0xf4840008);
test!(test_sdl, "sdl $a0, 0x10($a1)", 0xb0a40010);
test!(test_sdr, "sdr $a0, 0x10($a1)", 0xb4a40010);

#[test]
fn test_seq() {
    let inst = asm("seq $a0, $a1, $a2");
    assert_eq!(inst, vec![0x00a62026, 0x2c840001]);
}

#[test]
fn test_sge() {
    let inst = asm("sge $a0, $a1, $a2");
    assert_eq!(inst, vec![0x00a6202a, 0x38840001]);
}

#[test]
fn test_sgeu() {
    let inst = asm("sgeu $a0, $a1, $a2");
    assert_eq!(inst, vec![0x00a6202b, 0x38840001]);
}

#[test]
fn test_sgt() {
    let inst = asm("sgt $a0, $a1, $a2");
    assert_eq!(inst, vec![0x00c5202a]);
}

#[test]
fn test_sgtu() {
    let inst = asm("sgtu $a0, $a1, $a2");
    assert_eq!(inst, vec![0x00c5202b]);
}

test!(test_sh, "sh $a0, 0x10($a1)", 0xa4a40010);

#[test]
fn test_sle() {
    let inst = asm("sle $a0, $a1, $a2");
    assert_eq!(inst, vec![0x00c5202a, 0x38840001]);
}

#[test]
fn test_sleu() {
    let inst = asm("sleu $a0, $a1, $a2");
    assert_eq!(inst, vec![0x00c5202b, 0x38840001]);
}

test!(test_sll, "sll $a0, $a1, 0x1f", 0x000527c0);
test!(test_sllv, "sllv $a0, $a1, $a2", 0x00c52004);
test!(test_slt, "slt $a0, $a1, $a2", 0x00a6202a);
test!(test_slti, "slti $a0, $a1, 0x8", 0x28a40008);
test!(test_sltiu, "sltiu $a0, $a1, 0x8", 0x2ca40008);
test!(test_sltu, "sltu $a0, $a1, $a2", 0x00a6202b);

#[test]
fn test_sne() {
    let inst = asm("sne $a0, $a1, $a2");
    assert_eq!(inst, vec![0x00a62026, 0x0004202b]);
}

test!(test_sra, "sra $a0, $a1, 0x1f", 0x000527c3);
test!(test_srav, "srav $a0, $a1, $a2", 0x00c52007);
test!(test_srl, "srl $a0, $a1, 0x1f", 0x000527c2);
test!(test_srlv, "srlv $a0, $a1, $a2", 0x00c52006);
test!(test_sub, "sub $a0, $a1, $a2", 0x00a62022);

#[test]
fn test_subi() {
    let inst = asm("subi $a0, $a1, 0x8");
    assert_eq!(inst, vec![0x20a4fff8]);
}

#[test]
fn test_subiu() {
    let inst = asm("subiu $a0, $a1, 0x8");
    assert_eq!(inst, vec![0x24a4fff8]);
}

test!(test_subu, "subu $a0, $a1, $a2", 0x00a62023);
test!(test_sw, "sw $a0, 0x10($a1)", 0xaca40010);
test!(test_swc1, "swc1 $ft0, 0x8($a0)", 0xe4840008);
test!(test_swl, "swl $a0, 0x10($a1)", 0xa8a40010);
test!(test_swr, "swr $a0, 0x10($a1)", 0xb8a40010);
test!(test_sync, "sync", 0x0000000f);
test!(test_syscall, "syscall", 0x0000000c);
test!(test_syscall_imm, "syscall 0x20", 0x0000080c);
test!(test_teq, "teq $a0, $a1", 0x00850034);
test!(test_teqi, "teqi $a0, 0x20", 0x048c0020);
test!(test_tge, "tge $a0, $a1", 0x00850030);
test!(test_tgei, "tgei $a0, 0x20", 0x04880020);
test!(test_tgeiu, "tgeiu $a0, 0x20", 0x04890020);
test!(test_tgeu, "tgeu $a0, $a1", 0x00850031);
test!(test_tlbp, "tlbp", 0x42000008);
test!(test_tlbr, "tlbr", 0x42000001);
test!(test_tlbwi, "tlbwi", 0x42000002);
test!(test_tlbwr, "tlbwr", 0x42000006);
test!(test_tlt, "tlt $a0, $a1", 0x00850032);
test!(test_tlti, "tlti $a0, 0x20", 0x048a0020);
test!(test_tltiu, "tltiu $a0, 0x20", 0x048b0020);
test!(test_tltu, "tltu $a0, $a1", 0x00850033);
test!(test_tne, "tne $a0, $a1", 0x00850036);
test!(test_tnei, "tnei $a0, 0x20", 0x048e0020);
test!(test_xor, "xor $a0, $a1, $a2", 0x00a62026);
test!(test_xori, "xori $a0, $a1, 0x8", 0x38a40008);

// Float instructions
test!(test_abs_s, "abs.s $fa0, $fa1", 0x46007305);
test!(test_abs_d, "abs.d $fa0, $fa1", 0x46207305);
test!(test_add_s, "add.s $fa0, $fa1, $ft0", 0x46047300);
test!(test_add_d, "add.d $fa0, $fa1, $ft0", 0x46247300);
test!(test_c_eq_s, "c.eq.s $fa1, $ft0", 0x46047032);
test!(test_c_eq_d, "c.eq.d $fa1, $ft0", 0x46247032);
test!(test_c_f_s, "c.f.s $fa1, $ft0", 0x46047030);
test!(test_c_f_d, "c.f.d $fa1, $ft0", 0x46247030);
test!(test_c_le_s, "c.le.s $fa1, $ft0", 0x4604703e);
test!(test_c_le_d, "c.le.d $fa1, $ft0", 0x4624703e);
test!(test_c_lt_s, "c.lt.s $fa1, $ft0", 0x4604703c);
test!(test_c_lt_d, "c.lt.d $fa1, $ft0", 0x4624703c);
test!(test_c_nge_s, "c.nge.s $fa1, $ft0", 0x4604703d);
test!(test_c_nge_d, "c.nge.d $fa1, $ft0", 0x4624703d);
test!(test_c_ngl_s, "c.ngl.s $fa1, $ft0", 0x4604703b);
test!(test_c_ngl_d, "c.ngl.d $fa1, $ft0", 0x4624703b);
test!(test_c_ngle_s, "c.ngle.s $fa1, $ft0", 0x46047039);
test!(test_c_ngle_d, "c.ngle.d $fa1, $ft0", 0x46247039);
test!(test_c_ngt_s, "c.ngt.s $fa1, $ft0", 0x4604703f);
test!(test_c_ngt_d, "c.ngt.d $fa1, $ft0", 0x4624703f);
test!(test_c_ole_s, "c.ole.s $fa1, $ft0", 0x46047036);
test!(test_c_ole_d, "c.ole.d $fa1, $ft0", 0x46247036);
test!(test_c_olt_s, "c.olt.s $fa1, $ft0", 0x46047034);
test!(test_c_olt_d, "c.olt.d $fa1, $ft0", 0x46247034);
test!(test_c_seq_s, "c.seq.s $fa1, $ft0", 0x4604703a);
test!(test_c_seq_d, "c.seq.d $fa1, $ft0", 0x4624703a);
test!(test_c_sf_s, "c.sf.s $fa1, $ft0", 0x46047038);
test!(test_c_sf_d, "c.sf.d $fa1, $ft0", 0x46247038);
test!(test_c_ueq_s, "c.ueq.s $fa1, $ft0", 0x46047033);
test!(test_c_ueq_d, "c.ueq.d $fa1, $ft0", 0x46247033);
test!(test_c_ule_s, "c.ule.s $fa1, $ft0", 0x46047037);
test!(test_c_ule_d, "c.ule.d $fa1, $ft0", 0x46247037);
test!(test_c_ult_s, "c.ult.s $fa1, $ft0", 0x46047035);
test!(test_c_ult_d, "c.ult.d $fa1, $ft0", 0x46247035);
test!(test_c_un_s, "c.un.s $fa1, $ft0", 0x46047031);
test!(test_c_un_d, "c.un.d $fa1, $ft0", 0x46247031);
test!(test_ceil_l_s, "ceil.l.s $fa0, $fa1", 0x4600730a);
test!(test_ceil_l_d, "ceil.l.d $fa0, $fa1", 0x4620730a);
test!(test_ceil_w_s, "ceil.w.s $fa0, $fa1", 0x4600730e);
test!(test_ceil_w_d, "ceil.w.d $fa0, $fa1", 0x4620730e);
test!(test_cvt_d_l, "cvt.d.l $fa0, $fa1", 0x46a07321);
test!(test_cvt_d_s, "cvt.d.s $fa0, $fa1", 0x46007321);
test!(test_cvt_d_w, "cvt.d.w $fa0, $fa1", 0x46807321);
test!(test_cvt_l_d, "cvt.l.d $fa0, $fa1", 0x46207325);
test!(test_cvt_l_s, "cvt.l.s $fa0, $fa1", 0x46007325);
test!(test_cvt_s_d, "cvt.s.d $fa0, $fa1", 0x46207320);
test!(test_cvt_s_l, "cvt.s.l $fa0, $fa1", 0x46a07320);
test!(test_cvt_s_w, "cvt.s.w $fa0, $fa1", 0x46807320);
test!(test_cvt_w_d, "cvt.w.d $fa0, $fa1", 0x46207324);
test!(test_cvt_w_s, "cvt.w.s $fa0, $fa1", 0x46007324);
test!(test_div_s, "div.s $fa0, $fa1, $ft0", 0x46047303);
test!(test_div_d, "div.d $fa0, $fa1, $ft0", 0x46247303);
test!(test_floor_l_s, "floor.l.s $fa0, $fa1", 0x4600730b);
test!(test_floor_l_d, "floor.l.d $fa0, $fa1", 0x4620730b);
test!(test_floor_w_s, "floor.w.s $fa0, $fa1", 0x4600730f);
test!(test_floor_w_d, "floor.w.d $fa0, $fa1", 0x4620730f);
test!(test_mul_s, "mul.s $fa0, $fa1, $ft0", 0x46047302);
test!(test_mul_d, "mul.d $fa0, $fa1, $ft0", 0x46247302);
test!(test_mov_s, "mov.s $fa0, $fa1", 0x46007306);
test!(test_mov_d, "mov.d $fa0, $fa1", 0x46207306);
test!(test_neg_s, "neg.s $fa0, $fa1", 0x46007307);
test!(test_neg_d, "neg.d $fa0, $fa1", 0x46207307);
test!(test_round_l_s, "round.l.s $fa0, $fa1", 0x46007308);
test!(test_round_l_d, "round.l.d $fa0, $fa1", 0x46207308);
test!(test_round_w_s, "round.w.s $fa0, $fa1", 0x4600730c);
test!(test_round_w_d, "round.w.d $fa0, $fa1", 0x4620730c);
test!(test_sqrt_s, "sqrt.s $fa0, $fa1", 0x46007304);
test!(test_sqrt_d, "sqrt.d $fa0, $fa1", 0x46207304);
test!(test_sub_s, "sub.s $fa0, $fa1, $ft0", 0x46047301);
test!(test_sub_d, "sub.d $fa0, $fa1, $ft0", 0x46247301);
test!(test_trunc_l_s, "trunc.l.s $fa0, $fa1", 0x46007309);
test!(test_trunc_l_d, "trunc.l.d $fa0, $fa1", 0x46207309);
test!(test_trunc_w_s, "trunc.w.s $fa0, $fa1", 0x4600730d);
test!(test_trunc_w_d, "trunc.w.d $fa0, $fa1", 0x4620730d);
