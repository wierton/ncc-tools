#ifndef MIPSEMU_H
#define MIPSEMU_H

#include <cstring>
#include <memory>
#include <string>
#include <vector>

#define IGNORE_CERTAIN_EXCEPTION

enum class MipsEmuEx {
  INVOP,
  IF,
  LOAD,
  STORE,
  TIMEOUT,
  OVERFLOW,
};

class MipsEmu {
  typedef uint32_t paddr_t;
  typedef uint32_t vaddr_t;

  struct {
    uint32_t gpr[32];
    uint32_t hi, lo;
    uint32_t pc;

    vaddr_t br_target;
    bool is_delayslot;
  } cpu = {0};

  static constexpr uint32_t DDR_SIZE = 128 * 1024 * 1024;
  std::unique_ptr<uint8_t[]> ddr;

private:
  inline __attribute__((always_inline)) uint32_t vaddr_read(
      paddr_t addr, int len) {
    if (!(addr < DDR_SIZE && addr + len < DDR_SIZE))
#ifdef IGNORE_CERTAIN_EXCEPTION
      return 0;
#else
      abort();
#endif
    return *((uint32_t *)((char *)ddr.get() + addr)) &
           (~0u >> ((4 - len) << 3));
  }

  inline __attribute__((always_inline)) void vaddr_write(
      paddr_t addr, int len, uint32_t data) {
    if (!(addr < DDR_SIZE && addr + len < DDR_SIZE))
#ifdef IGNORE_CERTAIN_EXCEPTION
      return;
#else
      abort();
#endif
    memcpy((uint8_t *)ddr.get() + addr, &data, len);
  }

  void *vaddr_map(uint32_t addr, uint32_t len) {
    if (!(addr < DDR_SIZE && addr + len < DDR_SIZE))
      return nullptr;
    return (void *)(ddr.get() + addr);
  }

  uint32_t load_elf(const std::string &elf_file);
  void print_registers(uint32_t instr);

private:
  uint32_t counter = 0;
  uint32_t limit = -1u;
  MipsEmuEx exception;
  std::vector<int> *printedNumbers = nullptr;

public:
  MipsEmu() : ddr(new uint8_t[DDR_SIZE]) {}

  void setInstLimit(uint32_t limit = -1u) {
    this->limit = limit;
  }
  MipsEmuEx getException() const { return exception; }
  void collectPrintedNumbers(std::vector<int> &numbers) {
    this->printedNumbers = &numbers;
  }

  std::string compile(const std::string &source_file);
  int run(const std::string &elf_file);
};

#endif
