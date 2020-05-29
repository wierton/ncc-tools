#include <assert.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define HALT_PC 0xBFC00380

typedef uint32_t paddr_t;
typedef uint32_t vaddr_t;

struct {
  uint32_t gpr[32];
  uint32_t hi, lo;
  uint32_t pc;

  vaddr_t br_target;
  bool is_delayslot;
} cpu;

typedef struct {
  union {
    uint32_t val;
    // R-type
    struct {
      uint32_t func : 6;
      uint32_t shamt : 5;
      uint32_t rd : 5;
      uint32_t rt : 5;
      uint32_t rs : 5;
      uint32_t op : 6;
    };

    uint32_t uimm : 16; // I-type

    int32_t simm : 16; // SI-type

    uint32_t addr : 26; // J-type
    uint32_t sel : 3;   // MFC0
  };
} Inst; // Instruction

/* clang-format off */
const char *regs[32] = {
  "0 ", "at", "v0", "v1",
  "a0", "a1", "a2", "a3",
  "t0", "t1", "t2", "t3",
  "t4", "t5", "t6", "t7",
  "s0", "s1", "s2", "s3",
  "s4", "s5", "s6", "s7",
  "t8", "t9", "k0", "k1",
  "gp", "sp", "fp", "ra"
};
/* clang-format on */

#define DDR_SIZE (128 * 1024 * 1024) // 0x08000000

static uint8_t ddr[DDR_SIZE];

static inline __attribute__((always_inline)) uint32_t
vaddr_read(paddr_t addr, int len) {
  assert(addr < DDR_SIZE && addr + len < DDR_SIZE);
  return *((uint32_t *)((void *)ddr + addr)) &
         (~0u >> ((4 - len) << 3));
}

static inline __attribute__((always_inline)) void
vaddr_write(paddr_t addr, int len, uint32_t data) {
  assert(addr < DDR_SIZE && addr + len < DDR_SIZE);
  memcpy((uint8_t *)ddr + addr, &data, len);
}

int init_cpu(vaddr_t entry) {
  cpu.gpr[29] = DDR_SIZE - 4; // set sp
  cpu.gpr[31] = HALT_PC;
  return 0;
}

/* Simulate how the CPU works. */
void cpu_exec() {
  while (true) {
    assert((cpu.pc & 0x3) == 0);

    Inst inst = {.val = vaddr_read(cpu.pc, 4)};

#include "instr.h"

    if (cpu.pc == HALT_PC) break;
  }
}

int sh(const char *fmt, ...) {
  static char buffer[1024];
  va_list ap;
  va_start(ap, fmt);
  vsnprintf(buffer, sizeof(buffer) - 1, fmt, ap);
  va_end(ap);

  return system(buffer);
}

int main(int argc, const char *argv[]) {
  if (argc <= 1) {
    printf("usage: ./mips-sim *.[sS]");
    return 0;
  }

  sh("mips-linux-gnu-as %s -EL -o %s.o", argv[1], argv[1]);
  sh("mips-linux-gnu-ld -entry main -Ttext=0x1000 %s.o -EL "
     "-o %s.elf",
      argv[1], argv[1]);
  return 0;
}
