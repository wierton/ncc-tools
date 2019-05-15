#include <cassert>
#include <cstdint>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <map>
#include <memory>
#include <regex>
#include <string>
#include <tuple>
#include <vector>

enum class stmt {
  begin,
  label = stmt::begin,
  func,
  assign,
  add,
  sub,
  mul,
  div,
  takeaddr,
  deref,
  deref_assign,
  goto_,
  branch,
  ret,
  dec,
  arg,
  call,
  param,
  read,
  write,
  end,
};

enum class op {
  abort, // as 0
  call,  // nativa call, not function call
  li,
  mov,
  add,
  sub,
  mul,
  div,
  takeaddr,
  deref,
  br,
  cond_br,
  lt,
  le,
  eq,
  ge,
  gt,
  ne,
  alloca,
  inc_esp,
  ret,
  read,
  write,
  quit,
};

using VarType = uintptr_t;
using VarMap = std::map<std::string, std::unique_ptr<VarType>>;
using TransitionBlock = std::array<int, 1024>;

class Program {
  std::vector<std::unique_ptr<TransitionBlock>> codes;
  TransitionBlock *curblk;
  int *textptr;

  std::vector<std::unique_ptr<int[]>> mempool;

  /* running context */
  std::vector<int> stack;
  int *esp;

public:
  int *get_textptr() const { return textptr; }
  void check_eof(unsigned N) {
    if (textptr + N + 2 >= &(*curblk)[curblk->size()]) {
      curblk = new TransitionBlock;
      codes.push_back(std::unique_ptr<TransitionBlock>(curblk));
      *textptr++ = (int)op::br;
      *textptr++ = (uint64_t) & (curblk->at(0));
      *textptr++ = (uint64_t) & (curblk->at(0)) >> 32;
    }
  }

  template <class... Args>
  int *gen_inst(op opc, Args... args) {
    constexpr unsigned N = sizeof...(args);
    check_eof(N + 1);
    auto oldptr = textptr;
    *textptr++ = (int)opc;
    for (int v : std::array<int, N>{static_cast<int>(args)...}) { *textptr++ = v; }
    return oldptr;
  }

  int *gen_br(int *target) {
    assert(target);
    unsigned int lo = (uint64_t)target;
    unsigned int hi = (uint64_t)target >> 32;
    return gen_inst(op::br, lo, hi);
  }

  int *gen_cond_br(int cond, int *target) {
    assert(target);
    unsigned int lo = (uint64_t)target;
    unsigned int hi = (uint64_t)target >> 32;
    return gen_inst(op::br, cond, lo, hi);
  }

  void run(int *eip);
};

void Program::run(int *eip) {
  esp = &stack[0];

  for (;;) {
    int opc = *eip++;
    int to = 0, lhs = 0, rhs = 0;
    switch ((op)opc) {
    case op::abort: std::cerr << "unexpected instruction\n"; break;
    case op::call: {
      uintptr_t ptrlo = *eip++;
      uintptr_t ptrhi = *eip++;
      void *f = (void *)(ptrlo | ptrhi << 32);
      ((void (*)(int *, int *))f)(eip, esp);
    } break;
    case op::li: {
      to = *eip++;
      lhs = *eip++;
      esp[to] = lhs;
    } break;
    case op::mov:
      /* esp[*eip++] = esp[*eip++]; // WARNING: undefined behavior */
      to = *eip++;
      lhs = *eip++;
      esp[to] = esp[lhs];
      break;
    case op::add:
      to = *eip++;
      lhs = *eip++;
      rhs = *eip++;
      esp[to] = esp[lhs] + esp[rhs];
      break;
    case op::sub:
      to = *eip++;
      lhs = *eip++;
      rhs = *eip++;
      esp[to] = esp[lhs] - esp[rhs];
      break;
    case op::mul:
      to = *eip++;
      lhs = *eip++;
      rhs = *eip++;
      esp[to] = esp[lhs] * esp[rhs];
      break;
    case op::div:
      to = *eip++;
      lhs = *eip++;
      rhs = *eip++;
      esp[to] = esp[lhs] / esp[rhs];
      break;
    case op::takeaddr:
      to = *eip++;
      lhs = *eip++;
      esp[to] = lhs;
      break;
    case op::deref:
      to = *eip++;
      lhs = *eip++;
      memcpy(&esp[to], (char *)esp + lhs, sizeof(int));
      break;
    case op::br: {
      uint64_t ptrlo = *eip++;
      uint64_t ptrhi = *eip++;
      eip = (int *)(ptrlo | ptrhi << 32);
    } break;
    case op::cond_br: {
      int cond = *eip++;
      if (cond) {
        uint64_t ptrlo = *eip++;
        uint64_t ptrhi = *eip++;
        eip = (int *)(ptrlo | ptrhi << 32);
      }
    } break;
    case op::lt:
      to = *eip++;
      lhs = *eip++;
      rhs = *eip++;
      esp[to] = esp[lhs] < esp[rhs];
      break;
    case op::le:
      to = *eip++;
      lhs = *eip++;
      rhs = *eip++;
      esp[to] = esp[lhs] <= esp[rhs];
      break;
    case op::eq:
      to = *eip++;
      lhs = *eip++;
      rhs = *eip++;
      esp[to] = esp[lhs] == esp[rhs];
      break;
    case op::ge:
      to = *eip++;
      lhs = *eip++;
      rhs = *eip++;
      esp[to] = esp[lhs] >= esp[rhs];
      break;
    case op::gt:
      to = *eip++;
      lhs = *eip++;
      rhs = *eip++;
      esp[to] = esp[lhs] > esp[rhs];
      break;
    case op::ne:
      to = *eip++;
      lhs = *eip++;
      rhs = *eip++;
      esp[to] = esp[lhs] != esp[rhs];
      break;
    case op::ret: {
      int retval = esp[*eip++];
      to = esp[-1];
      esp += esp[0];
      esp[to] = retval;
    } break;
    case op::alloca:
      to = *eip++;
      if (esp + to >= &stack[stack.size()]) {
        ptrdiff_t diff = esp - &stack[0];
        assert(diff > 0);
        stack.resize(diff + to + 4);
        esp = &stack[diff];
      }
      break;
    case op::read: abort(); break;
    case op::write:
      to = *eip++;
      std::cout << esp[to] << "\n";
      break;
    case op::quit: std::cout << "quit the program\n"; return;
    }
  }
}

class Compiler {
  int stack_size;
  int args_size;

  std::map<std::string, int> vars;
  std::map<std::string, int *> funcs;
  std::map<std::string, int *> labels;

  static std::map<stmt, std::regex> patterns;

public:
  Compiler() : stack_size(1), args_size(-3) {}

  void clear_env() {
    stack_size = 1;
    args_size = -3;
    labels.clear();
  }

  int *getFunction(const std::string &fname) {
	return funcs[fname];
  }

  int getVar(const std::string &name, unsigned size = 4) {
    auto it = vars.find(name);
    if (it == vars.end())
      std::tie(it, std::ignore) =
          vars.insert(std::pair<std::string, int>{
              name, stack_size += (size / sizeof(int))});
    return it->second;
  }

  int newTemp() { return stack_size; }

  int newArg() {
    stack_size += 4;
    return stack_size - 4;
  }

  int getParam(const std::string &name) {
    auto it = vars.find(name);
    if (it == vars.end())
      std::tie(it, std::ignore) =
          vars.insert(std::pair<std::string, int>{name, args_size--});
    return it->second;
  }

  std::unique_ptr<Program> compile(std::istream &is);
};

std::map<stmt, std::regex> Compiler::patterns{
    {stmt::label, std::regex(R"(^\s*LABEL\s+(\w+)\s*:\s*$)",
                             std::regex_constants::icase)},
    {stmt::func, std::regex(R"(^\s*FUNCTION\s+(\w+)\s*:\s*$)",
                            std::regex_constants::icase)},
    {stmt::assign, std::regex(R"(^\s*(\w+)\s*:=\s*(\w+)\s*$)",
                              std::regex_constants::icase)},
    {stmt::add,
     std::regex(R"(^\s*(\w+)\s*:=\s*(\w+)\s++\s+(\w+)\s*$)",
                std::regex_constants::icase)},
    {stmt::sub,
     std::regex(R"(^\s*(\w+)\s*:=\s*(\w+)\s+-\s+(\w+)\s*$)",
                std::regex_constants::icase)},
    {stmt::mul,
     std::regex(R"(^\s*(\w+)\s*:=\s*(\w+)\s+*\s+(\w+)\s*$)",
                std::regex_constants::icase)},
    {stmt::div,
     std::regex(R"(^\s*(\w+)\s*:=\s*(\w+)\s+/\s+(\w+)\s*$)",
                std::regex_constants::icase)},
    {stmt::takeaddr, std::regex(R"(^\s*(\w+)\s*:=\s*&(\w+)\s*$)",
                                std::regex_constants::icase)},
    {stmt::deref, std::regex(R"(^\s*(\w+)\s*:=\s*\*(\w+)\s*$)",
                             std::regex_constants::icase)},
    {stmt::deref_assign, std::regex(R"(^\s**(\w+)\s+:=\s+(\w+)\s*$)",
                                    std::regex_constants::icase)},
    {stmt::goto_, std::regex(R"(^\s*GOTO\s+(\w+)\s*$)",
                             std::regex_constants::icase)},
    {stmt::branch,
     std::regex(
         R"(^\s*IF\s+(\w+)\s*(<|>|<=|>=|==|!=)\s*(\w+)\s+GOTO\s+(\w+)\s*$)",
         std::regex_constants::icase)},
    {stmt::ret, std::regex(R"(^\s*RETURN\s+(\w+)\s*$)",
                           std::regex_constants::icase)},
    {stmt::dec, std::regex(R"(^\s*DEC\s+(\w+)\s+(\d+)\s*$)",
                           std::regex_constants::icase)},
    {stmt::arg, std::regex(R"(^\s*ARG\s+(\w+)\s*$)",
                           std::regex_constants::icase)},
    {stmt::call, std::regex(R"(^\s*(\w+)\s*:=\s*CALL\s+(\w+)\s*$)",
                            std::regex_constants::icase)},
    {stmt::param, std::regex(R"(^\s*PARAM\s+(\w+)\s*$)",
                             std::regex_constants::icase)},
    {stmt::read, std::regex(R"(^\s*READ\s+(\w+)\s*$)",
                            std::regex_constants::icase)},
    {stmt::write, std::regex(R"(^\s*WRITE\s+(\w+)\s*$)",
                             std::regex_constants::icase)},
};

std::unique_ptr<Program> Compiler::compile(std::istream &is) {
  int *curf = nullptr;
  auto prog = std::make_unique<Program>();
  while ((is.peek(), is.good())) {
    std::string line;
    std::getline(is, line);

    int m1[] = {0};
    int m2[] = {0, 1};
    int m3[] = {0, 1, 2};
    int m4[] = {0, 1, 2, 3};
    auto end = std::sregex_token_iterator();

    /* stmt label */
    auto it = std::sregex_token_iterator(line.begin(), line.end(),
                                         patterns[stmt::label], m1);
    if (it != end) {
      auto label = *it++;
      labels[label] = prog->get_textptr();
      continue;
    }

    /* stmt func */
    it = std::sregex_token_iterator(line.begin(), line.end(),
                                    patterns[stmt::func], m1);
    if (it != end) {
      auto f = *it++;
      prog->gen_inst(op::abort);
      if (curf) {
        curf[1] = stack_size;
        clear_env();
      }
      curf = prog->gen_inst(op::alloca, 0);
      continue;
    }

    /* stmt assign */
    it = std::sregex_token_iterator(line.begin(), line.end(),
                                    patterns[stmt::assign], m2);
    if (it != end) {
      auto x = *it++;
      auto y = *it++;
      prog->gen_inst(op::mov, getVar(x), getVar(y));
      continue;
    }

    /* stmt add */
    std::map<stmt, op> m{
        {stmt::add, op::add},
        {stmt::sub, op::sub},
        {stmt::mul, op::mul},
        {stmt::div, op::div},
    };
    for (auto [s, o] : m) {
      it = std::sregex_token_iterator(line.begin(), line.end(),
                                      patterns[s], m3);
      if (it != end) {
        auto x = *it++;
        auto y = *it++;
        auto z = *it++;
        prog->gen_inst(o, getVar(x), getVar(y), getVar(z));
        continue;
      }
    }

    /* stmt assign */
    it = std::sregex_token_iterator(line.begin(), line.end(),
                                    patterns[stmt::takeaddr], m3);
    if (it != end) {
      auto x = *it++;
      auto y = *it++;
      prog->gen_inst(op::takeaddr, getVar(x), getVar(y));
      continue;
    }

    /* stmt assign */
    it = std::sregex_token_iterator(line.begin(), line.end(),
                                    patterns[stmt::deref], m3);
    if (it != end) {
      auto x = *it++;
      auto y = *it++;
      prog->gen_inst(op::deref, getVar(x), getVar(y));
      continue;
    }

    it = std::sregex_token_iterator(line.begin(), line.end(),
                                    patterns[stmt::deref_assign], m3);
    if (it != end) {
      auto x = *it++;
      auto y = *it++;
      auto tmp = newTemp();
      prog->gen_inst(op::deref, tmp, getVar(x));
      prog->gen_inst(op::mov, tmp, getVar(y));
      continue;
    }

    it = std::sregex_token_iterator(line.begin(), line.end(),
                                    patterns[stmt::goto_], m3);
    if (it != end) {
      auto label = labels[*it++];
      assert(label);
      unsigned int lo = (uint64_t)label;
      unsigned int hi = (uint64_t)label >> 32;
      prog->gen_inst(op::br, lo, hi);
      continue;
    }

    it = std::sregex_token_iterator(line.begin(), line.end(),
                                    patterns[stmt::branch], m4);
    if (it != end) {
      auto x = *it++;
      auto opc = *it++;
      auto y = *it++;
      auto label = *it++;

      static std::map<std::string, op> s2op{
          {"<", op::lt},  {">", op::gt},  {"<=", op::le},
          {">=", op::ge}, {"==", op::eq}, {"!=", op::ne},
      };

      auto tmp = newTemp();
      prog->gen_inst(s2op[opc], tmp, getVar(x), getVar(y));
      prog->gen_cond_br(tmp, labels[label]);
      continue;
    }

    it = std::sregex_token_iterator(line.begin(), line.end(),
                                    patterns[stmt::ret], m4);
    if (it != end) {
      auto x = *it++;
      prog->gen_inst(op::ret, getVar(x));
      continue;
    }

    it = std::sregex_token_iterator(line.begin(), line.end(),
                                    patterns[stmt::dec], m2);
    if (it != end) {
      auto x = *it++;
      auto size = stoi(*it++);
      getVar(x, size);
    }

    it = std::sregex_token_iterator(line.begin(), line.end(),
                                    patterns[stmt::arg], m2);
    if (it != end) {
      auto x = *it++;
      auto arg = newArg();
      prog->gen_inst(op::mov, arg, getVar(x));
    }

    it = std::sregex_token_iterator(line.begin(), line.end(),
                                    patterns[stmt::call], m2);
    if (it != end) {
      auto to = *it++;
      auto f = *it++;

      auto lo = newArg();
      auto hi = newArg();
      auto ret = newArg();
      auto inc = newArg();

      prog->gen_inst(op::li, lo, (uint64_t)curf);
      prog->gen_inst(op::li, hi, (uint64_t)curf >> 32);
      prog->gen_inst(op::takeaddr, ret, getVar(to));
      prog->gen_inst(op::li, inc, inc);
      prog->gen_inst(op::inc_esp, inc);

      prog->gen_br(funcs[f]);
    }

    it = std::sregex_token_iterator(line.begin(), line.end(),
                                    patterns[stmt::param], m1);
    if (it != end) { getParam(*it++); }

    it = std::sregex_token_iterator(line.begin(), line.end(),
                                    patterns[stmt::read], m1);
    if (it != end) {
      auto x = *it++;
      prog->gen_inst(op::read, getVar(x));
    }

    it = std::sregex_token_iterator(line.begin(), line.end(),
                                    patterns[stmt::write], m1);
    if (it != end) {
      auto x = *it++;
      prog->gen_inst(op::write, getVar(x));
    }
  }
  return prog;
}

int main() {
  std::ifstream ifs("a.ir");

  Compiler compiler;
  auto prog = compiler.compile(ifs);
  prog->run(compiler.getFunction("main"));
  return 0;
}
