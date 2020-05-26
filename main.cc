#include "irsim.h"

#include <fstream>
#include <stdio.h>
#include <string>

int main(int argc, const char *argv[]) {
  if (argc <= 1) {
    printf("usage: irsim [*.ir]\n");
    return 0;
  }

  // printf("load %s\n", argv[1]);
  std::ifstream ifs(argv[1]);

  if (!ifs.good()) {
    printf("'%s' no such file\n", argv[1]);
    return 0;
  }

  using namespace irsim;
  Compiler compiler;
  auto prog = compiler.compile(ifs);
  prog->setInstsLimit(-1u);
  prog->setMemoryLimit(128 * 1024 * 1024);
  auto code = prog->run(compiler.getFunction("main"));
  if (code == 0 ||
      prog->getException() == Exception::NONE) {
    printf("return %d, execute %d instructions\n", code,
        prog->getInstCounter());
  } else {
    switch (prog->getException()) {
    case Exception::IF:
      printf(
          "fetch instruction error (labels used but not "
          "defined)\n");
      break;
    case Exception::LOAD:
      printf("memory load exception\n");
      break;
    case Exception::STORE:
      printf("memory store exception\n");
      break;
    case Exception::DIV_ZERO:
      printf("divided by zero exception\n");
      break;
    case Exception::TIMEOUT:
      printf("run out of instructions\n");
      break;
    case Exception::OOM:
      printf("run out of memory\n");
      break;
    case Exception::ABORT:
      printf("abort (function not return)\n");
      break;
    case Exception::INVOP:
      printf("invalid instruction (bug of irsim)\n");
      break;
    case Exception::STACK_LIMIT:
      printf(
          "exceed the stack limit (depth 65536, max 65536 "
          "words per stack frame)\n");
      break;
    default: break;
    }
  }
  return 0;
}
