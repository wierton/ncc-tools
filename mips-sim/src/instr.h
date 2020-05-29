#define make_label(l) \
  l:
#define make_entry()
#define make_exit() make_label(exit)

/*
 * ABS.D ADD.D ADDIU ADD.S ADDU AND ANDI BC1F BC1T BEQ
 * BGEZ BGEZAL BGTZ BLEZ BLTZ BLTZAL BNE BREAK C.EQ.D
 * C.EQ.S CFC1 C.LE.D C.LE.S C.LT.D C.LT.S CLZ CTC1
 * C.ULE.D C.ULE.S C.ULT.D C.ULT.S C.UN.D C.UN.S
 * CVT.D.S CVT.D.W CVT.S.D CVT.S.W DIV DIV.D DIV.S DIVU
 * EXT INS J JAL JALR JR LB LBU LDC1 LH LHU LL LUI LW
 * LWC1 LWL LWR MADD MADDU MFC1 MFHC1 MFHI MFLO MOV.D
 * MOVF MOVF.D MOVF.S MOVN MOVN.D MOVN.S MOV.S MOVT
 * MOVT.D MOVT.S MOVZ MOVZ.S MSUB MTC1 MTHC1 MTHI MTLO
 * MUL MUL.D MUL.S MULT MULTU NEG.D NOR OR ORI PREF RDHWR
 * ROR RORV SB SC SDC1 SEB SEH SH SLL SLLV SLT SLTI SLTIU
 * SLTU SQRT.D SQRT.S SRA SRAV SRL SRLV SUB.D SUB.S SUBU
 * SW SWC1 SWL SWR SYNC SYSCALL TEQ TRUNC.W.D TRUNC.W.S
 * WSBH XOR XORI
 * */

#define make_exec_handler(name) \
  goto inst_end;                \
  make_label(name)
#define prepare_delayslot() \
  cpu.is_delayslot = true;  \
  cpu.pc += 4;              \
  goto exit;

typedef union {
  struct {
    uint32_t lo, hi;
  };
  uint64_t val;
  int64_t sval;
} L64_t;

/* clang-format off */
/* R-type */
static const void *special_table[64] = {
    /* 0x00 */ &&sll, &&inv, &&srl, &&sra,
    /* 0x04 */ &&sllv, &&inv, &&srlv, &&srav,
    /* 0x08 */ &&jr, &&jalr, &&movz, &&movn,
    /* 0x0c */ &&syscall, &&inv, &&inv, &&inv,
    /* 0x10 */ &&mfhi, &&mthi, &&mflo, &&mtlo,
    /* 0x14 */ &&inv, &&inv, &&inv, &&inv,
    /* 0x18 */ &&mult, &&multu, &&divide, &&divu,
    /* 0x1c */ &&inv, &&inv, &&inv, &&inv,
    /* 0x20 */ &&add, &&addu, &&sub, &&subu,
    /* 0x24 */ &&and_, &&or_, &&xor_, &&nor,
    /* 0x28 */ &&inv, &&inv, &&slt, &&sltu,
    /* 0x2c */ &&inv, &&inv, &&inv, &&inv,
    /* 0x30 */ &&inv, &&inv, &&inv, &&inv,
    /* 0x34 */ &&inv, &&inv, &&inv, &&inv,
    /* 0x38 */ &&inv, &&inv, &&inv, &&inv,
    /* 0x3c */ &&inv, &&inv, &&inv, &&inv,
};

static const void *special2_table[64] = {
    /* 0x00 */ &&madd, &&maddu, &&mul, &&inv,
    /* 0x04 */ &&msub, &&msubu, &&inv, &&inv,
    /* 0x08 */ &&inv, &&inv, &&inv, &&inv,
    /* 0x0c */ &&inv, &&inv, &&inv, &&inv,
    /* 0x10 */ &&inv, &&inv, &&inv, &&inv,
    /* 0x14 */ &&inv, &&inv, &&inv, &&inv,
    /* 0x18 */ &&inv, &&inv, &&inv, &&inv,
    /* 0x1c */ &&inv, &&inv, &&inv, &&inv,
    /* 0x20 */ &&clz, &&clo, &&inv, &&inv,
    /* 0x24 */ &&inv, &&inv, &&inv, &&inv,
    /* 0x28 */ &&inv, &&inv, &&inv, &&inv,
    /* 0x2c */ &&inv, &&inv, &&inv, &&inv,
    /* 0x30 */ &&inv, &&inv, &&inv, &&inv,
    /* 0x34 */ &&inv, &&inv, &&inv, &&inv,
    /* 0x38 */ &&inv, &&inv, &&inv, &&inv,
    /* 0x3c */ &&inv, &&inv, &&inv, &&inv,
};

static const void *special3_table[64] = {
    /* 0x00 */ &&ext, &&inv, &&mul, &&inv,
    /* 0x04 */ &&ins, &&inv, &&inv, &&inv,
    /* 0x08 */ &&inv, &&inv, &&inv, &&inv,
    /* 0x0c */ &&inv, &&inv, &&inv, &&inv,
    /* 0x10 */ &&inv, &&inv, &&inv, &&inv,
    /* 0x14 */ &&inv, &&inv, &&inv, &&inv,
    /* 0x18 */ &&inv, &&inv, &&inv, &&inv,
    /* 0x1c */ &&inv, &&inv, &&inv, &&inv,
    /* 0x20 */ &&exec_bshfl, &&inv, &&inv, &&inv,
    /* 0x24 */ &&inv, &&inv, &&inv, &&inv,
    /* 0x28 */ &&inv, &&inv, &&inv, &&inv,
    /* 0x2c */ &&inv, &&inv, &&inv, &&inv,
    /* 0x30 */ &&inv, &&inv, &&inv, &&inv,
    /* 0x34 */ &&inv, &&inv, &&inv, &&inv,
    /* 0x38 */ &&inv, &&inv, &&inv, &&inv,
    /* 0x3c */ &&inv, &&inv, &&inv, &&inv,
};

/* shamt */
static const void *bshfl_table[64] = {
    /* 0x00 */ &&inv, &&inv, &&wsbh, &&inv,
    /* 0x04 */ &&inv, &&inv, &&inv, &&inv,
    /* 0x08 */ &&inv, &&inv, &&inv, &&inv,
    /* 0x0c */ &&inv, &&inv, &&inv, &&inv,
    /* 0x10 */ &&seb, &&inv, &&inv, &&inv,
    /* 0x14 */ &&inv, &&inv, &&inv, &&inv,
    /* 0x18 */ &&seh, &&inv, &&inv, &&inv,
    /* 0x1c */ &&inv, &&inv, &&inv, &&inv,
};

/* I-type */
static const void *regimm_table[64] = {
    /* 0x00 */ &&bltz, &&bgez, &&bltzl, &&bgezl,
    /* 0x04 */ &&inv, &&inv, &&inv, &&inv,
    /* 0x08 */ &&inv, &&inv, &&inv, &&inv,
    /* 0x0c */ &&inv, &&inv, &&inv, &&inv,
    /* 0x10 */ &&bltzal, &&bgezal, &&bltzall, &&bgezall,
    /* 0x14 */ &&inv, &&inv, &&inv, &&inv,
    /* 0x18 */ &&inv, &&inv, &&inv, &&inv,
    /* 0x1c */ &&inv, &&inv, &&inv, &&inv,
};

/* I-type */
static const void *opcode_table[64] = {
    /* 0x00 */ &&exec_special, &&exec_regimm, &&j, &&jal,
    /* 0x04 */ &&beq, &&bne, &&blez, &&bgtz,
    /* 0x08 */ &&addi, &&addiu, &&slti, &&sltiu,
    /* 0x0c */ &&andi, &&ori, &&xori, &&lui,
    /* 0x10 */ &&inv, &&inv, &&inv, &&inv,
    /* 0x14 */ &&beql, &&bnel, &&blezl, &&bgtzl,
    /* 0x18 */ &&inv, &&inv, &&inv, &&inv,
    /* 0x1c */ &&exec_special2, &&inv, &&inv, &&exec_special3,
    /* 0x20 */ &&lb, &&lh, &&lwl, &&lw,
    /* 0x24 */ &&lbu, &&lhu, &&lwr, &&inv,
    /* 0x28 */ &&sb, &&sh, &&swl, &&sw,
    /* 0x2c */ &&inv, &&inv, &&swr, &&inv,
    /* 0x30 */ &&inv, &&inv, &&inv, &&inv,
    /* 0x34 */ &&inv, &&inv, &&inv, &&inv,
    /* 0x38 */ &&inv, &&inv, &&inv, &&inv,
    /* 0x3c */ &&inv, &&inv, &&inv, &&inv,
};

/* clang-format on */
make_entry() {
  cpu.gpr[0] = 0;
#if CONFIG_INSTR_LOG
  instr_enqueue_instr(inst.val);
#endif
  goto *opcode_table[inst.op];
}

make_exec_handler(exec_special) {
  goto *special_table[inst.func];
}
make_exec_handler(exec_special2) {
  goto *special2_table[inst.func];
}
make_exec_handler(exec_special3) {
  goto *special3_table[inst.func];
}
make_exec_handler(exec_bshfl) {
  goto *bshfl_table[inst.shamt];
}

make_exec_handler(exec_regimm) {
  goto *regimm_table[inst.rt];
}

make_exec_handler(inv) {
  uint32_t instr = vaddr_read(cpu.pc, 4);
  uint8_t *p = (uint8_t *)&instr;
  printf(
      "invalid opcode(pc = 0x%08x): %02x %02x %02x %02x "
      "...\n",
      cpu.pc, p[0], p[1], p[2], p[3]);
  abort();
}

/* temporary strategy: store timer registers in C0 */
make_exec_handler(syscall) {
  switch (cpu.gpr[R_v0]) {
  case 1: printf("%d", cpu.gpr[R_a0]); break;
  case 4: {
    char *ptr = (char *)vaddr_map(cpu.gpr[R_a0], 0);
    printf("%s", ptr);
  } break;
  case 5: {
    int value = 0;
    scanf("%d", &value);
    cpu.gpr[R_v0] = value;
  } break;
  case 8: {
    int len = cpu.gpr[R_a1];
    char *ptr = (char *)vaddr_map(cpu.gpr[R_a0], len);
    assert(ptr);
    for (int i = 0; i < len; i++) ptr[i] = getchar();
  } break;
  case 11: putchar(cpu.gpr[R_a0]); break;
  case 12: cpu.gpr[R_a0] = getchar(); break;
  case 10: exit(0); break;
  case 17: exit(0); break;
  default: eprintf("unimplemented syscall\n");
  }
}

#define R_SIMPLE(name, op, t)                       \
  make_exec_handler(name) {                         \
    assert(inst.shamt == 0);                        \
    cpu.gpr[inst.rd] =                              \
        (t)cpu.gpr[inst.rs] op(t) cpu.gpr[inst.rt]; \
  }

R_SIMPLE(or_, |, uint32_t)
R_SIMPLE(xor_, ^, uint32_t)
R_SIMPLE(and_, &, uint32_t)
R_SIMPLE(addu, +, uint32_t)
R_SIMPLE(subu, -, uint32_t)
R_SIMPLE(mul, *, uint32_t)
R_SIMPLE(slt, <, int32_t)
R_SIMPLE(sltu, <, uint32_t)

make_exec_handler(add) {
  assert(inst.shamt == 0);
  L64_t ret;
  ret.val = (int64_t)(int32_t)cpu.gpr[inst.rs] +
            (int64_t)(int32_t)cpu.gpr[inst.rt];
  if ((ret.hi & 0x1) != ((ret.lo >> 31) & 1)) {
    assert(0 && "add overflow\n");
  } else {
    cpu.gpr[inst.rd] = ret.lo;
  }
}

make_exec_handler(sub) {
  assert(inst.shamt == 0);
  L64_t ret;
  ret.val = (int64_t)(int32_t)cpu.gpr[inst.rs] -
            (int64_t)(int32_t)cpu.gpr[inst.rt];
  if ((ret.hi & 0x1) != ((ret.lo >> 31) & 1)) {
    assert(0 && "sub overflow\n");
  } else {
    cpu.gpr[inst.rd] = ret.lo;
  }
}

make_exec_handler(nor) {
  assert(inst.shamt == 0);
  cpu.gpr[inst.rd] = ~(cpu.gpr[inst.rs] | cpu.gpr[inst.rt]);
}

#undef R_SIMPLE

make_exec_handler(clz) {
  if (cpu.gpr[inst.rs] == 0) {
    cpu.gpr[inst.rd] = 32;
  } else {
    cpu.gpr[inst.rd] = __builtin_clz(cpu.gpr[inst.rs]);
  }
}

make_exec_handler(clo) {
  uint32_t in = cpu.gpr[inst.rs];
  uint32_t cnt = 0;
  uint32_t b = 0x80000000;
  while ((in & b) != 0) {
    cnt++;
    b >>= 1;
  }
  cpu.gpr[inst.rd] = cnt;
}

make_exec_handler(madd) {
  assert(inst.rd == 0 && inst.shamt == 0);
  L64_t hilo;
  hilo.hi = cpu.hi;
  hilo.lo = cpu.lo;
  hilo.sval += (int64_t)(int32_t)cpu.gpr[inst.rs] *
               (int64_t)(int32_t)cpu.gpr[inst.rt];
  cpu.hi = hilo.hi;
  cpu.lo = hilo.lo;
}

make_exec_handler(maddu) {
  assert(inst.rd == 0 && inst.shamt == 0);
  L64_t hilo;
  hilo.hi = cpu.hi;
  hilo.lo = cpu.lo;
  hilo.val += (uint64_t)cpu.gpr[inst.rs] *
              (uint64_t)cpu.gpr[inst.rt];
  cpu.hi = hilo.hi;
  cpu.lo = hilo.lo;
}

make_exec_handler(msub) {
  assert(inst.rd == 0 && inst.shamt == 0);
  L64_t hilo;
  hilo.hi = cpu.hi;
  hilo.lo = cpu.lo;
  hilo.sval -= (int64_t)(int32_t)cpu.gpr[inst.rs] *
               (int64_t)(int32_t)cpu.gpr[inst.rt];
  cpu.hi = hilo.hi;
  cpu.lo = hilo.lo;
}

make_exec_handler(msubu) {
  assert(inst.rd == 0 && inst.shamt == 0);
  L64_t hilo;
  hilo.hi = cpu.hi;
  hilo.lo = cpu.lo;
  hilo.val -= (uint64_t)cpu.gpr[inst.rs] *
              (uint64_t)cpu.gpr[inst.rt];
  cpu.hi = hilo.hi;
  cpu.lo = hilo.lo;
}

make_exec_handler(mult) {
  assert(inst.rd == 0 && inst.shamt == 0);
  int64_t prod = (int64_t)(int32_t)cpu.gpr[inst.rs] *
                 (int64_t)(int32_t)cpu.gpr[inst.rt];
  cpu.lo = (uint32_t)prod;
  cpu.hi = (uint32_t)(prod >> 32);
}

make_exec_handler(multu) {
  assert(inst.rd == 0 && inst.shamt == 0);
  uint64_t prod = (uint64_t)cpu.gpr[inst.rs] *
                  (uint64_t)cpu.gpr[inst.rt];
  cpu.lo = (uint32_t)prod;
  cpu.hi = (uint32_t)(prod >> 32);
}

make_exec_handler(divide) {
  assert(inst.rd == 0 && inst.shamt == 0);
  cpu.lo =
      (int32_t)cpu.gpr[inst.rs] / (int32_t)cpu.gpr[inst.rt];
  cpu.hi =
      (int32_t)cpu.gpr[inst.rs] % (int32_t)cpu.gpr[inst.rt];
}

make_exec_handler(divu) {
  assert(inst.rd == 0 && inst.shamt == 0);
  cpu.lo = cpu.gpr[inst.rs] / cpu.gpr[inst.rt];
  cpu.hi = cpu.gpr[inst.rs] % cpu.gpr[inst.rt];
}

make_exec_handler(sll) {
  assert(inst.rs == 0);
  cpu.gpr[inst.rd] = cpu.gpr[inst.rt] << inst.shamt;
}

make_exec_handler(sllv) {
  assert(inst.shamt == 0);
  cpu.gpr[inst.rd] = cpu.gpr[inst.rt]
                     << (cpu.gpr[inst.rs] & 0x1f);
}

make_exec_handler(sra) {
  assert(inst.rs == 0);
  cpu.gpr[inst.rd] =
      (int32_t)cpu.gpr[inst.rt] >> inst.shamt;
}

make_exec_handler(srav) {
  assert(inst.shamt == 0);
  cpu.gpr[inst.rd] = (int32_t)cpu.gpr[inst.rt] >>
                     (cpu.gpr[inst.rs] & 0x1f);
}

make_exec_handler(srl) {
  if ((inst.rs & 0x1) == 0x1) {
    /* rotr */
    uint32_t rt_val = cpu.gpr[inst.rt];
    uint32_t sa = inst.shamt;
    cpu.gpr[inst.rd] =
        (rt_val >> sa) | (rt_val << (32 - sa));
  } else {
    assert(inst.rs == 0);
    cpu.gpr[inst.rd] = cpu.gpr[inst.rt] >> inst.shamt;
  }
}

make_exec_handler(srlv) {
  if ((inst.shamt & 0x1) == 0x1) {
    /* rotrv */
    uint32_t rt_val = cpu.gpr[inst.rt];
    uint32_t sa = cpu.gpr[inst.rs] & 0x1f;
    cpu.gpr[inst.rd] =
        (rt_val >> sa) | (rt_val << (32 - sa));
  } else {
    assert(inst.shamt == 0);
    cpu.gpr[inst.rd] =
        cpu.gpr[inst.rt] >> (cpu.gpr[inst.rs] & 0x1f);
  }
}

make_exec_handler(movn) {
  assert(inst.shamt == 0);
  if (cpu.gpr[inst.rt] != 0)
    cpu.gpr[inst.rd] = cpu.gpr[inst.rs];
}

make_exec_handler(movz) {
  assert(inst.shamt == 0);
  if (cpu.gpr[inst.rt] == 0)
    cpu.gpr[inst.rd] = cpu.gpr[inst.rs];
}

make_exec_handler(mfhi) {
  assert(inst.rs == 0 && inst.rt == 0 && inst.shamt == 0);
  cpu.gpr[inst.rd] = cpu.hi;
}

make_exec_handler(mthi) {
  assert(inst.rt == 0 && inst.rd == 0 && inst.shamt == 0);
  cpu.hi = cpu.gpr[inst.rs];
}

make_exec_handler(mflo) {
  assert(inst.rs == 0 && inst.rt == 0 && inst.shamt == 0);
  cpu.gpr[inst.rd] = cpu.lo;
}

make_exec_handler(mtlo) {
  assert(inst.rt == 0 && inst.rd == 0 && inst.shamt == 0);
  cpu.lo = cpu.gpr[inst.rs];
}

make_exec_handler(lui) {
  assert(inst.rs == 0);
  cpu.gpr[inst.rt] = inst.uimm << 16;
}

make_exec_handler(addi) {
  // should throw exception
  L64_t ret;
  ret.val = (int64_t)(int32_t)cpu.gpr[inst.rs] +
            (int64_t)(int32_t)inst.simm;
  if ((ret.hi & 0x1) != ((ret.lo >> 31) & 1)) {
    assert(0 && "addi overflow\n");
  } else {
    cpu.gpr[inst.rt] = ret.lo;
  }
}

make_exec_handler(addiu) {
  cpu.gpr[inst.rt] = cpu.gpr[inst.rs] + inst.simm;
}

make_exec_handler(andi) {
  cpu.gpr[inst.rt] = cpu.gpr[inst.rs] & inst.uimm;
}

make_exec_handler(ori) {
  cpu.gpr[inst.rt] = cpu.gpr[inst.rs] | inst.uimm;
}

make_exec_handler(xori) {
  cpu.gpr[inst.rt] = cpu.gpr[inst.rs] ^ inst.uimm;
}

make_exec_handler(sltiu) {
  cpu.gpr[inst.rt] = cpu.gpr[inst.rs] < (uint32_t)inst.simm;
}

make_exec_handler(slti) {
  cpu.gpr[inst.rt] = (int32_t)cpu.gpr[inst.rs] < inst.simm;
}

#define CHECK_ALIGNED_ADDR(align, addr)   \
  assert((((addr) & (align - 1)) == 0) && \
         "memory access addr is not aligned")

#define CHECK_ALIGNED_ADDR_AdEL CHECK_ALIGNED_ADDR
#define CHECK_ALIGNED_ADDR_AdES CHECK_ALIGNED_ADDR

make_exec_handler(swl) {
  uint32_t waddr = cpu.gpr[inst.rs] + inst.simm;
  int idx = waddr & 0x3;
  int len = idx + 1;
  uint32_t wdata = cpu.gpr[inst.rt] >> ((3 - idx) * 8);

  vaddr_write((waddr >> 2) << 2, len, wdata);
}

make_exec_handler(swr) {
  uint32_t waddr = cpu.gpr[inst.rs] + inst.simm;
  int len = 4 - (waddr & 0x3);
  uint32_t wdata = cpu.gpr[inst.rt];

  vaddr_write(waddr, len, wdata);
}

make_exec_handler(sw) {
  CHECK_ALIGNED_ADDR_AdES(4, cpu.gpr[inst.rs] + inst.simm);
  vaddr_write(
      cpu.gpr[inst.rs] + inst.simm, 4, cpu.gpr[inst.rt]);
}

make_exec_handler(sh) {
  CHECK_ALIGNED_ADDR_AdES(2, cpu.gpr[inst.rs] + inst.simm);
  vaddr_write(
      cpu.gpr[inst.rs] + inst.simm, 2, cpu.gpr[inst.rt]);
}

make_exec_handler(sb) {
  CHECK_ALIGNED_ADDR_AdES(1, cpu.gpr[inst.rs] + inst.simm);
  vaddr_write(
      cpu.gpr[inst.rs] + inst.simm, 1, cpu.gpr[inst.rt]);
}

make_exec_handler(lwl) {
  uint32_t raddr = cpu.gpr[inst.rs] + inst.simm;
  int len = (raddr & 0x3) + 1;
  uint32_t rdata = vaddr_read((raddr >> 2) << 2, len);

  if (len < 4)
    cpu.gpr[inst.rt] =
        rdata << ((4 - len) * 8) |
        ((uint32_t)cpu.gpr[inst.rt] << (len * 8)) >>
            (len * 8);
  else
    cpu.gpr[inst.rt] = rdata;
}

make_exec_handler(lwr) {
  uint32_t raddr = cpu.gpr[inst.rs] + inst.simm;
  int idx = raddr & 0x3;
  int len = 4 - idx;
  uint32_t rdata = vaddr_read(raddr, len);
  if (len < 4)
    cpu.gpr[inst.rt] =
        (rdata << idx * 8) >> (idx * 8) |
        ((uint32_t)cpu.gpr[inst.rt] >> (len * 8))
            << (len * 8);
  else
    cpu.gpr[inst.rt] = (rdata << idx * 8) >> (idx * 8);
}

make_exec_handler(lw) {
  CHECK_ALIGNED_ADDR_AdEL(4, cpu.gpr[inst.rs] + inst.simm);
  uint32_t rdata =
      vaddr_read(cpu.gpr[inst.rs] + inst.simm, 4);
  cpu.gpr[inst.rt] = rdata;
}

make_exec_handler(lb) {
  CHECK_ALIGNED_ADDR_AdEL(1, cpu.gpr[inst.rs] + inst.simm);
  uint32_t rdata = (int32_t)(int8_t)vaddr_read(
      cpu.gpr[inst.rs] + inst.simm, 1);
  cpu.gpr[inst.rt] = rdata;
}

make_exec_handler(lbu) {
  CHECK_ALIGNED_ADDR_AdEL(1, cpu.gpr[inst.rs] + inst.simm);
  uint32_t rdata =
      vaddr_read(cpu.gpr[inst.rs] + inst.simm, 1);
  cpu.gpr[inst.rt] = rdata;
}

make_exec_handler(lh) {
  CHECK_ALIGNED_ADDR_AdEL(2, cpu.gpr[inst.rs] + inst.simm);
  uint32_t rdata = (int32_t)(int16_t)vaddr_read(
      cpu.gpr[inst.rs] + inst.simm, 2);
  cpu.gpr[inst.rt] = rdata;
}

make_exec_handler(lhu) {
  CHECK_ALIGNED_ADDR_AdEL(2, cpu.gpr[inst.rs] + inst.simm);
  uint32_t rdata =
      vaddr_read(cpu.gpr[inst.rs] + inst.simm, 2);
  cpu.gpr[inst.rt] = rdata;
}

//////////////////////////////////////////////////////////////
//                      likely branch //
//////////////////////////////////////////////////////////////
make_exec_handler(beql) {
  if (cpu.gpr[inst.rs] == cpu.gpr[inst.rt]) {
    cpu.br_target = cpu.pc + (inst.simm << 2) + 4;
    prepare_delayslot();
  } else {
    cpu.br_target = cpu.pc + 8;
    cpu.pc += 4;
  }
}

make_exec_handler(bnel) {
  if (cpu.gpr[inst.rs] != cpu.gpr[inst.rt]) {
    cpu.br_target = cpu.pc + (inst.simm << 2) + 4;
    prepare_delayslot();
  } else {
    cpu.br_target = cpu.pc + 8;
    cpu.pc += 4;
  }
}

make_exec_handler(blezl) {
  assert(inst.rt == 0);
  if ((int32_t)cpu.gpr[inst.rs] <= 0) {
    cpu.br_target = cpu.pc + (inst.simm << 2) + 4;
    prepare_delayslot();
  } else {
    cpu.br_target = cpu.pc + 8;
    cpu.pc += 4;
  }
}

make_exec_handler(bgtzl) {
  if ((int32_t)cpu.gpr[inst.rs] > 0) {
    cpu.br_target = cpu.pc + (inst.simm << 2) + 4;
    prepare_delayslot();
  } else {
    cpu.br_target = cpu.pc + 8;
    cpu.pc += 4;
  }
}

make_exec_handler(bltzl) {
  if ((int32_t)cpu.gpr[inst.rs] < 0) {
    cpu.br_target = cpu.pc + (inst.simm << 2) + 4;
    prepare_delayslot();
  } else {
    cpu.br_target = cpu.pc + 8;
    cpu.pc += 4;
  }
}

make_exec_handler(bgezl) {
  if ((int32_t)cpu.gpr[inst.rs] >= 0) {
    cpu.br_target = cpu.pc + (inst.simm << 2) + 4;
    prepare_delayslot();
  } else {
    cpu.br_target = cpu.pc + 8;
    cpu.pc += 4;
  }
}

make_exec_handler(bgezall) {
  cpu.gpr[31] = cpu.pc + 8;
  if ((int32_t)cpu.gpr[inst.rs] >= 0) {
    cpu.br_target = cpu.pc + (inst.simm << 2) + 4;
    prepare_delayslot();
  } else {
    cpu.br_target = cpu.pc + 8;
    cpu.pc += 4;
  }
}

make_exec_handler(bltzall) {
  cpu.gpr[31] = cpu.pc + 8;
  if ((int32_t)cpu.gpr[inst.rs] < 0) {
    cpu.br_target = cpu.pc + (inst.simm << 2) + 4;
    prepare_delayslot();
  } else {
    cpu.br_target = cpu.pc + 8;
    cpu.pc += 4;
  }
}

//////////////////////////////////////////////////////////////
//                      unlikely branch //
//////////////////////////////////////////////////////////////
make_exec_handler(beq) {
  if (cpu.gpr[inst.rs] == cpu.gpr[inst.rt])
    cpu.br_target = cpu.pc + (inst.simm << 2) + 4;
  else
    cpu.br_target = cpu.pc + 8;
  prepare_delayslot();
}

make_exec_handler(bne) {
  if (cpu.gpr[inst.rs] != cpu.gpr[inst.rt])
    cpu.br_target = cpu.pc + (inst.simm << 2) + 4;
  else
    cpu.br_target = cpu.pc + 8;
  prepare_delayslot();
}

make_exec_handler(blez) {
  assert(inst.rt == 0);
  if ((int32_t)cpu.gpr[inst.rs] <= 0)
    cpu.br_target = cpu.pc + (inst.simm << 2) + 4;
  else
    cpu.br_target = cpu.pc + 8;
  prepare_delayslot();
}

make_exec_handler(bgtz) {
  if ((int32_t)cpu.gpr[inst.rs] > 0)
    cpu.br_target = cpu.pc + (inst.simm << 2) + 4;
  else
    cpu.br_target = cpu.pc + 8;
  prepare_delayslot();
}

make_exec_handler(bltz) {
  if ((int32_t)cpu.gpr[inst.rs] < 0)
    cpu.br_target = cpu.pc + (inst.simm << 2) + 4;
  else
    cpu.br_target = cpu.pc + 8;
  prepare_delayslot();
}

make_exec_handler(bgez) {
  if ((int32_t)cpu.gpr[inst.rs] >= 0)
    cpu.br_target = cpu.pc + (inst.simm << 2) + 4;
  else
    cpu.br_target = cpu.pc + 8;
  prepare_delayslot();
}

make_exec_handler(bgezal) {
  cpu.gpr[31] = cpu.pc + 8;
  if ((int32_t)cpu.gpr[inst.rs] >= 0)
    cpu.br_target = cpu.pc + (inst.simm << 2) + 4;
  else
    cpu.br_target = cpu.pc + 8;
  prepare_delayslot();
}

make_exec_handler(bltzal) {
  cpu.gpr[31] = cpu.pc + 8;
  if ((int32_t)cpu.gpr[inst.rs] < 0)
    cpu.br_target = cpu.pc + (inst.simm << 2) + 4;
  else
    cpu.br_target = cpu.pc + 8;
  prepare_delayslot();
}

make_exec_handler(jal) {
  cpu.gpr[31] = cpu.pc + 8;
  cpu.br_target = (cpu.pc & 0xf0000000) | (inst.addr << 2);
  prepare_delayslot();
}

make_exec_handler(jalr) {
  assert(inst.rt == 0 && inst.shamt == 0);
  cpu.gpr[inst.rd] = cpu.pc + 8;
  cpu.br_target = cpu.gpr[inst.rs];
  prepare_delayslot();
}

make_exec_handler(j) {
  cpu.br_target = (cpu.pc & 0xf0000000) | (inst.addr << 2);
  prepare_delayslot();
}

make_exec_handler(jr) {
  assert(inst.rt == 0 && inst.rd == 0);
  cpu.br_target = cpu.gpr[inst.rs];
  prepare_delayslot();
}

make_exec_handler(seb) {
  cpu.gpr[inst.rd] = (int32_t)(int8_t)cpu.gpr[inst.rt];
}

make_exec_handler(seh) {
  cpu.gpr[inst.rd] = (int32_t)(int16_t)cpu.gpr[inst.rt];
}

make_exec_handler(wsbh) {
  uint32_t rt_val = cpu.gpr[inst.rt];
  cpu.gpr[inst.rd] = ((rt_val & 0x00FF0000) << 8) |
                     ((rt_val & 0xFF000000) >> 8) |
                     ((rt_val & 0x000000FF) << 8) |
                     ((rt_val & 0x0000FF00) >> 8);
}

make_exec_handler(ins) {
  uint32_t lsb = inst.shamt;
  uint32_t msb = inst.rd;
  assert(lsb <= msb);
  uint32_t rs_val = cpu.gpr[inst.rs];
  uint32_t rt_val = cpu.gpr[inst.rt];
  uint32_t mask = (1ull << (msb - lsb + 1)) - 1;

  cpu.gpr[inst.rt] =
      (rt_val & ~(mask << lsb)) | ((rs_val & mask) << lsb);
}

make_exec_handler(ext) {
  uint32_t lsb = inst.shamt;
  uint32_t msbd = inst.rd;
  uint32_t size = msbd + 1;
  uint32_t rs_val = cpu.gpr[inst.rs];
  uint32_t mask = (1ull << size) - 1;

  cpu.gpr[inst.rt] = ((rs_val & (mask << lsb)) >> lsb);
}

make_label(inst_end) {
  if (cpu.is_delayslot) {
    cpu.pc = cpu.br_target;
    cpu.is_delayslot = false;
  } else {
    cpu.pc += 4;
  }
  /* fall through */
}

make_exit() {}
