#include <stdio.h>
#include "MipsEmu.h"

int main(int argc, const char *argv[]) {
  if (argc <= 1) {
    printf("usage: irsim *.[sS]\n");
    return 0;
  }

  MipsEmu emu;
  emu.setInstLimit(-1u);
  std::string elf = emu.compile(argv[1]);
  int code = emu.run(elf);
  if (code != 0) {
    switch (emu.getException()) {
    case MipsEmuEx::INVOP:
      printf("invalid instruction\n");
      break;
    case MipsEmuEx::IF:
      printf("invalid addr while fetching instruction\n");
      break;
    case MipsEmuEx::STORE:
      printf("invalid store addr\n");
      break;
    case MipsEmuEx::LOAD:
      printf("invalid load addr\n");
      break;
    case MipsEmuEx::TIMEOUT:
      printf("run out of instructions\n");
      break;
    case MipsEmuEx::OVERFLOW:
      printf("arithmetic overflow\n");
      break;
    }
  }
  return 0;
}
