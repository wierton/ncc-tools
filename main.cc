#include "irsim.h"
#include "fmt/printf.h"

#include <string>
#include <fstream>

int main(int argc, const char *argv[]) {
  if (argc <= 1) {
    fmt::printf("usage: irsim [*.ir]\n");
    return 0;
  }

  fmt::printf("load %s\n", argv[1]);
  std::ifstream ifs(argv[1]);

  if (!ifs.good()) {
    fmt::printf("'%s' no such file\n", argv[1]);
    return 0;
  }

  using namespace irsim;
  Compiler compiler;
  auto prog = compiler.compile(ifs);
  prog->setInstsLimit(10 * 10000 * 10000);
  prog->setMemoryLimit(128 * 1024 * 1024);
  auto code = prog->run(compiler.getFunction("main"));
  printf("ret with %d, reason %d\n", code, (int)prog->exception);
  return 0;
}
