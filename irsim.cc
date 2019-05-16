#include <cassert>
#include <climits>
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

#include "fmt/printf.h"

#define LOGIR
// #define DEBUG

template <class T>
int ptr_hi(const T *ptr) {
  return static_cast<int>(reinterpret_cast<uintptr_t>(ptr) >> 32);
}

template <class T>
int ptr_lo(const T *ptr) {
  return static_cast<int>(reinterpret_cast<uintptr_t>(ptr));
}

template <class T>
T *lohi_to_ptr(uint32_t lo, uint32_t hi) {
  uintptr_t ptr = ((uint64_t)hi << 32) | lo;
  return reinterpret_cast<T *>(ptr);
}

enum class Stmt {
  begin,
  label = Stmt::begin,
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

enum class Opc {
  abort, // as 0
  call,  // native call, not function call
  lai,
  la,
  ld,
  st,
  inc_esp,
  li,
  mov,
  add,
  sub,
  mul,
  div,
  br,
  cond_br,
  lt,
  le,
  eq,
  ge,
  gt,
  ne,
  alloca,
  ret,
  read,
  write,
  quit,
};

std::map<Opc, std::string> opc_to_string{
    {Opc::abort, "abort"},     {Opc::call, "call"}, {Opc::lai, "lai"},
    {Opc::la, "la"},           {Opc::ld, "ld"},     {Opc::st, "st"},
    {Opc::inc_esp, "inc_esp"}, {Opc::li, "li"},     {Opc::mov, "mov"},
    {Opc::add, "add"},         {Opc::sub, "sub"},   {Opc::mul, "mul"},
    {Opc::div, "div"},         {Opc::br, "br"},     {Opc::cond_br, "cond_br"},
    {Opc::lt, "lt"},           {Opc::le, "le"},     {Opc::eq, "eq"},
    {Opc::ge, "ge"},           {Opc::gt, "gt"},     {Opc::ne, "ne"},
    {Opc::alloca, "alloca"},   {Opc::ret, "ret"},   {Opc::read, "read"},
    {Opc::write, "write"},     {Opc::quit, "quit"},
};

using TransitionBlock = std::array<int, 10>;

class Program {
  std::vector<std::unique_ptr<TransitionBlock>> codes;
  TransitionBlock *curblk;
  int *textptr;

  std::vector<std::unique_ptr<int[]>> mempool;

  /* running context */
  std::vector<int> stack;
  int *esp;
  int *curf;

  friend class Compiler;

public:
  Program() {
    curblk = new TransitionBlock;
    codes.push_back(std::unique_ptr<TransitionBlock>(curblk));
    textptr = &curblk->at(0);

    curf = gen_inst(Opc::quit, Opc::quit);
  }

  int *get_textptr() const { return textptr; }
  void check_eof(unsigned N) {
    if (textptr + N + 2 >= &(*curblk)[curblk->size()]) {
      curblk = new TransitionBlock;
      codes.push_back(std::unique_ptr<TransitionBlock>(curblk));
      *textptr++ = (int)Opc::br;
      *textptr++ = ptr_lo(&(curblk->at(0)));
      *textptr++ = ptr_hi(&(curblk->at(0)));
      textptr = &curblk->at(0);
    }
  }

  template <class... Args>
  int *gen_inst(Opc opc, Args... args) {
    constexpr unsigned N = sizeof...(args);
    check_eof(N + 1);
    auto oldptr = textptr;
    *textptr++ = (int)opc;
    for (int v : std::array<int, N>{static_cast<int>(args)...}) {
      *textptr++ = v;
    }

#ifdef DEBUG
    fmt::printf("  %p: %s", fmt::ptr(oldptr), opc_to_string[opc]);
    for (int v : std::array<int, N>{static_cast<int>(args)...}) {
      fmt::printf("0x%x ", v);
    }
    fmt::printf("\n");
#endif
    return oldptr;
  }

  int *gen_br(int *target) {
    return gen_inst(Opc::br, ptr_lo(target), ptr_hi(target));
  }

  int *gen_cond_br(int cond, int *target) {
    return gen_inst(Opc::cond_br, cond, ptr_lo(target), ptr_hi(target));
  }

  void run(int *eip);
};

void Program::run(int *eip) {
  bool is_quit = false;

  auto lo = 0, hi = 1, ret = 2, inc = 3;
  int _start[] = {
      (int)Opc::alloca,
      4,
      (int)Opc::li,
      lo,
      ptr_lo(&_start[19]),
      (int)Opc::li,
      hi,
      ptr_hi(&_start[19]),
      (int)Opc::li,
      ret,
      0,
      (int)Opc::li,
      inc,
      inc,
      (int)Opc::inc_esp,
      inc,
      (int)Opc::br,
      ptr_lo(eip),
      ptr_hi(eip),
      (int)Opc::quit,
      0,
  };

  eip = &_start[0];
  esp = &stack[0];

  for (; !is_quit;) {
#ifdef DEBUG
    auto oldeip = eip;
#endif
    int opc = *eip++;
    int from, to;
    int lhs, rhs;
    int constant;

#ifdef DEBUG
    fmt::printf("stack:\n");
    constexpr int step = 6;
    for (auto i = 0u; i < stack.size(); i += step) {
      fmt::printf("%02d: ", i);
      for (auto j = i; j < i + step && j < stack.size(); j++) {
        fmt::printf("%08x ", stack[j]);
      }
      fmt::printf("\n");
    }
#endif

    switch ((Opc)opc) {
    case Opc::abort: fmt::printf("unexpected instruction\n"); break;
    case Opc::call: {
      int ptrlo = *eip++;
      int ptrhi = *eip++;
      int nr_args = *eip++;
      using F = void(int *, int *);
      F *f = lohi_to_ptr<F>(ptrlo, ptrhi);
      f(eip, esp);
      eip += nr_args;
    } break;
    case Opc::lai: {
      to = *eip++;
      from = *eip++;
      esp[to] = ((int)(esp - &stack[0]) + from) * 4;
#ifdef DEBUG
      fmt::printf("%p: lai %d, %d\n", fmt::ptr(oldeip), to, from);
#endif
    } break;
    case Opc::la: {
      to = *eip++;
      from = esp[*eip++];
      esp[to] = ((int)(esp - &stack[0]) + from) * 4;
#ifdef DEBUG
      fmt::printf("%p: la %d, (%d)=%d\n", fmt::ptr(oldeip), to, eip[-1], from);
#endif
    } break;
    case Opc::ld: {
      to = *eip++;
      from = *eip++;
#ifdef DEBUG
      fmt::printf("%p: ld %d, (%d)=%d\n", fmt::ptr(oldeip), to, from,
                  esp[from]);
#endif
      memcpy(&esp[to], (char *)&stack[0] + esp[from], sizeof(int));
    } break;
    case Opc::st: {
      to = *eip++;
      from = *eip++;
      memcpy((char *)&stack[0] + esp[to], &esp[from], sizeof(int));
#ifdef DEBUG
      fmt::printf("%p: st (%d)=%d, %d\n", fmt::ptr(oldeip), to, esp[to], from);
#endif
    } break;
    case Opc::li: {
      to = *eip++;
      lhs = *eip++;
      esp[to] = lhs;
#ifdef DEBUG
      if (lhs < 0 || lhs > 256)
        fmt::printf("%p: li %d %08x\n", fmt::ptr(oldeip), to, lhs);
      else
        fmt::printf("%p: li %d %d\n", fmt::ptr(oldeip), to, lhs);
#endif
    } break;
    case Opc::mov:
      /* esp[*eip++] = esp[*eip++]; // WARNING: undefined behavior */
      to = *eip++;
      lhs = *eip++;
      esp[to] = esp[lhs];
#ifdef DEBUG
      fmt::printf("%p: mov %d %d\n", fmt::ptr(oldeip), to, lhs);
#endif
      break;
    case Opc::add:
      to = *eip++;
      lhs = *eip++;
      rhs = *eip++;
      esp[to] = esp[lhs] + esp[rhs];
#ifdef DEBUG
      fmt::printf("%p: add %d, %d, %d\n", fmt::ptr(oldeip), to, lhs, rhs);
#endif
      break;
    case Opc::sub:
      to = *eip++;
      lhs = *eip++;
      rhs = *eip++;
      esp[to] = esp[lhs] - esp[rhs];
#ifdef DEBUG
      fmt::printf("%p: sub %d, %d, %d\n", fmt::ptr(oldeip), to, lhs, rhs);
#endif
      break;
    case Opc::mul:
      to = *eip++;
      lhs = *eip++;
      rhs = *eip++;
      esp[to] = esp[lhs] * esp[rhs];
#ifdef DEBUG
      fmt::printf("%p: mul %d, %d, %d\n", fmt::ptr(oldeip), to, lhs, rhs);
#endif
      break;
    case Opc::div:
      to = *eip++;
      lhs = *eip++;
      rhs = *eip++;
      esp[to] = esp[lhs] / esp[rhs];
#ifdef DEBUG
      fmt::printf("%p: div %d, %d, %d\n", fmt::ptr(oldeip), to, lhs, rhs);
#endif
      break;
    case Opc::br: {
      uint64_t ptrlo = *eip++;
      uint64_t ptrhi = *eip++;
#ifdef DEBUG
      fmt::printf("%p: br %p\n", fmt::ptr(oldeip),
                  lohi_to_ptr<void>(ptrlo, ptrhi));
#endif
      eip = lohi_to_ptr<int>(ptrlo, ptrhi);
    } break;
    case Opc::cond_br: {
      int cond = esp[*eip++];
      uint64_t ptrlo = *eip++;
      uint64_t ptrhi = *eip++;
#ifdef DEBUG
      fmt::printf("%p: cond %d br %p\n", fmt::ptr(oldeip), cond,
                  lohi_to_ptr<void>(ptrlo, ptrhi));
#endif
      if (cond) { eip = lohi_to_ptr<int>(ptrlo, ptrhi); }
    } break;
    case Opc::lt:
      to = *eip++;
      lhs = *eip++;
      rhs = *eip++;
      esp[to] = esp[lhs] < esp[rhs];
#ifdef DEBUG
      fmt::printf("%p: lt %d, %d, %d\n", fmt::ptr(oldeip), to, lhs, rhs);
#endif
      break;
    case Opc::le:
      to = *eip++;
      lhs = *eip++;
      rhs = *eip++;
      esp[to] = esp[lhs] <= esp[rhs];
#ifdef DEBUG
      fmt::printf("%p: le %d, %d, %d\n", fmt::ptr(oldeip), to, lhs, rhs);
#endif
      break;
    case Opc::eq:
      to = *eip++;
      lhs = *eip++;
      rhs = *eip++;
      esp[to] = esp[lhs] == esp[rhs];
#ifdef DEBUG
      fmt::printf("%p: eq %d, %d, %d\n", fmt::ptr(oldeip), to, lhs, rhs);
#endif
      break;
    case Opc::ge:
      to = *eip++;
      lhs = *eip++;
      rhs = *eip++;
      esp[to] = esp[lhs] >= esp[rhs];
#ifdef DEBUG
      fmt::printf("%p: ge %d, %d, %d\n", fmt::ptr(oldeip), to, lhs, rhs);
#endif
      break;
    case Opc::gt:
      to = *eip++;
      lhs = *eip++;
      rhs = *eip++;
      esp[to] = esp[lhs] > esp[rhs];
#ifdef DEBUG
      fmt::printf("%p: gt %d, %d, %d\n", fmt::ptr(oldeip), to, lhs, rhs);
#endif
      break;
    case Opc::ne:
      to = *eip++;
      lhs = *eip++;
      rhs = *eip++;
      esp[to] = esp[lhs] != esp[rhs];
#ifdef DEBUG
      fmt::printf("%p: ne %d, %d, %d\n", fmt::ptr(oldeip), to, lhs, rhs);
#endif
      break;
    case Opc::inc_esp: constant = *eip++;
#ifdef DEBUG
      fmt::printf("%p: inc_esp %d\n", fmt::ptr(oldeip), to);
#endif
      esp += constant;
      break;
    case Opc::ret: {
#ifdef DEBUG
      int *oldesp = esp;
#endif
      int retval = esp[*eip++];
      to = esp[-1];
      eip = lohi_to_ptr<int>(esp[-3], esp[-2]);
      esp -= esp[0];
      esp[to] = retval;
#ifdef DEBUG
      fmt::printf("%p: ret %d, %d, dec %d, br %p\n", fmt::ptr(oldeip), retval,
                  to, esp[0], lohi_to_ptr<void>(oldesp[-3], oldesp[-2]));
#endif
    } break;
    case Opc::alloca: to = *eip++;
#ifdef DEBUG
      fmt::printf("%p: alloca %d\n", fmt::ptr(oldeip), to);
#endif
      if (esp + to >= &stack[stack.size()]) {
        ptrdiff_t diff = esp - &stack[0];
        assert(diff >= 0);
        stack.resize(diff + to + 4);
        esp = &stack[diff];
      }
      break;
    case Opc::read:
      fmt::printf("please input a number: ");
      to = *eip++;
      std::cin >> esp[to];
      break;
    case Opc::write:
      to = *eip++;
      fmt::printf("%d\n", esp[to]);
      break;
    case Opc::quit:
      is_quit = true;
      fmt::printf("quit the program\n");
      break;
    default:
      fmt::printf("unexpected opc %d\n", opc);
      abort();
      break;
    }
  }
}

class Compiler {
  int stack_size;
  int args_size;

  std::map<std::string, int> vars;
  std::map<std::string, int *> funcs;
  std::map<std::string, int *> labels;

  std::map<std::string, std::vector<int *>> backfill_labels;

  static std::map<Stmt, bool (Compiler::*)(Program *, const std::string &)>
      handlers;

  static int m1[];
  static int m2[];
  static int m3[];
  static int m4[];

  int primary_exp(Program *prog, const std::string &tok, int to = INT_MAX) {
    if (tok[0] == '#') {
      if (to == INT_MAX) to = newTemp();
      prog->gen_inst(Opc::li, to, std::stoi(&tok[1]));
      return to;
    } else if (tok[0] == '&') {
      auto var = getVar(&tok[1]);
      if (to == INT_MAX) to = newTemp();
      prog->gen_inst(Opc::lai, to, var);
      return to;
    } else if (tok[0] == '*') {
      auto var = getVar(&tok[1]);
      if (to == INT_MAX) to = newTemp();
      prog->gen_inst(Opc::ld, to, var);
      return to;
    } else {
      if (to != INT_MAX) {
        prog->gen_inst(Opc::mov, to, getVar(tok));
        return to;
      } else {
        return getVar(tok);
      }
    }
  }

  bool handle_label(Program *, const std::string &line);
  bool handle_func(Program *, const std::string &line);
  bool handle_assign(Program *, const std::string &line);
  bool handle_arith(Program *, const std::string &line);
  bool handle_takeaddr(Program *, const std::string &line);
  bool handle_deref(Program *, const std::string &line);
  bool handle_deref_assign(Program *, const std::string &line);
  bool handle_goto_(Program *, const std::string &line);
  bool handle_branch(Program *, const std::string &line);
  bool handle_ret(Program *, const std::string &line);
  bool handle_dec(Program *, const std::string &line);
  bool handle_arg(Program *, const std::string &line);
  bool handle_call(Program *, const std::string &line);
  bool handle_param(Program *, const std::string &line);
  bool handle_read(Program *, const std::string &line);
  bool handle_write(Program *, const std::string &line);

public:
  Compiler() { clear_env(); }

  void clear_env() {
    stack_size = 1;
    args_size = -4;
    labels.clear();
  }

  int *getFunction(const std::string &fname) { return funcs[fname]; }

  int getVar(const std::string &name, unsigned size = 1) {
    auto it = vars.find(name);
    if (it == vars.end()) {
      std::tie(it, std::ignore) =
          vars.insert(std::pair<std::string, int>{name, stack_size});
      stack_size += size;
    }
    return it->second;
  }

  int newTemp() {
    stack_size++;
    return stack_size - 1;
  }

  int newArg() {
    stack_size++;
    return stack_size - 1;
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

std::map<Stmt, bool (Compiler::*)(Program *, const std::string &)>
    Compiler::handlers{
        {Stmt::label, &Compiler::handle_label},
        {Stmt::func, &Compiler::handle_func},
        {Stmt::assign, &Compiler::handle_assign},
        {Stmt::add, &Compiler::handle_arith},
        {Stmt::sub, &Compiler::handle_arith},
        {Stmt::mul, &Compiler::handle_arith},
        {Stmt::div, &Compiler::handle_arith},
        {Stmt::takeaddr, &Compiler::handle_takeaddr},
        {Stmt::deref, &Compiler::handle_deref},
        {Stmt::deref_assign, &Compiler::handle_deref_assign},
        {Stmt::goto_, &Compiler::handle_goto_},
        {Stmt::branch, &Compiler::handle_branch},
        {Stmt::ret, &Compiler::handle_ret},
        {Stmt::dec, &Compiler::handle_dec},
        {Stmt::arg, &Compiler::handle_arg},
        {Stmt::call, &Compiler::handle_call},
        {Stmt::param, &Compiler::handle_param},
        {Stmt::read, &Compiler::handle_read},
        {Stmt::write, &Compiler::handle_write},
    };

int Compiler::m1[] = {1};
int Compiler::m2[] = {1, 2};
int Compiler::m3[] = {1, 2, 3};
int Compiler::m4[] = {1, 2, 3, 4};

/* stmt label */
bool Compiler::handle_label(Program *prog, const std::string &line) {
  static std::regex pat(R"(^\s*LABEL\s+(\w+)\s*:\s*$)");
  auto it = std::sregex_token_iterator(line.begin(), line.end(), pat, m1);

  if (it == std::sregex_token_iterator()) return false;

  auto label = *it++;
  auto label_ptr = prog->get_textptr();
  labels[label] = label_ptr;
#ifdef DEBUG
  fmt::printf("add label %s, %p\n", label.str(), fmt::ptr(label_ptr));
#endif
  for (auto *ptr : backfill_labels[label]) {
    ptr[0] = ptr_lo(label_ptr);
    ptr[1] = ptr_hi(label_ptr);
  }
  backfill_labels.erase(label);
  return true;
}

/* stmt func */
bool Compiler::handle_func(Program *prog, const std::string &line) {
  static std::regex pat(R"(^\s*FUNCTION\s+(\w+)\s*:\s*$)");
  auto it = std::sregex_token_iterator(line.begin(), line.end(), pat, m1);
  if (it == std::sregex_token_iterator()) return false;

  auto f = *it++;

  prog->gen_inst(Opc::abort); // last function should manually ret
  funcs[f] = prog->get_textptr();

  if (prog->curf[0] == (int)Opc::alloca) {
    prog->curf[1] = stack_size + 1;
    clear_env();
  }
  prog->curf = prog->gen_inst(Opc::alloca, 0);
  return true;
}

bool Compiler::handle_assign(Program *prog, const std::string &line) {
  std::regex pat(R"(^\s*(\w+)\s*:=\s*(#[\+\-]?\d+|[&\*]?\w+)\s*$)");
  auto it = std::sregex_token_iterator(line.begin(), line.end(), pat, m2);
  if (it == std::sregex_token_iterator()) return false;

  auto x = getVar(*it++);
  primary_exp(prog, *it++, x);
  return true;
}

bool Compiler::handle_arith(Program *prog, const std::string &line) {
  static std::regex pat(
      R"(^\s*(\w+)\s*:=\s*(#[\+\-]?\d+|[&\*]?\w+)\s*(\+|\-|\*|\/)\s*(#[\+\-]?\d+|[&\*]?\w+)\s*$)");

  static std::map<std::string, Opc> m{
      {"+", Opc::add},
      {"-", Opc::sub},
      {"*", Opc::mul},
      {"/", Opc::div},
  };

  auto it = std::sregex_token_iterator(line.begin(), line.end(), pat, m4);
  if (it == std::sregex_token_iterator()) return false;

  auto x = getVar(*it++);
  auto y = primary_exp(prog, *it++);
  auto op = *it++;
  auto z = primary_exp(prog, *it++);

  prog->gen_inst(m[op], x, y, z);
  return true;
}

bool Compiler::handle_takeaddr(Program *prog, const std::string &line) {
  static std::regex pat(R"(^\s*(\w+)\s*:=\s*&\s*(\w+)\s*$)");
  /* stmt assign */
  auto it = std::sregex_token_iterator(line.begin(), line.end(), pat, m2);
  if (it == std::sregex_token_iterator()) return false;
  auto x = *it++;
  auto y = *it++;
  prog->gen_inst(Opc::li, getVar(x), getVar(y));
  return true;
}

bool Compiler::handle_deref(Program *prog, const std::string &line) {
  static std::regex pat(R"(^\s*(\w+)\s*:=\s*\*\s*(\w+)\s*$)");
  /* stmt assign */
  auto it = std::sregex_token_iterator(line.begin(), line.end(), pat, m3);
  if (it == std::sregex_token_iterator()) return false;
  auto x = getVar(*it++);
  auto y = getVar(*it++);
  auto tmp = newTemp();
  prog->gen_inst(Opc::lai, tmp, y);
  prog->gen_inst(Opc::ld, x, y);
  return true;
}

bool Compiler::handle_deref_assign(Program *prog, const std::string &line) {
  static std::regex pat(
      R"(^\s*\*(\w+)\s+:=\s+(#[\+\-]?\d+|[&\*]?\w+)\s*$)");
  auto it = std::sregex_token_iterator(line.begin(), line.end(), pat, m3);
  if (it == std::sregex_token_iterator()) return false;

  auto x = getVar(*it++);
  auto y = primary_exp(prog, *it++);
  prog->gen_inst(Opc::st, x, y);
  return true;
}

bool Compiler::handle_goto_(Program *prog, const std::string &line) {
  static std::regex pat(R"(^\s*GOTO\s+(\w+)\s*$)");
  auto it = std::sregex_token_iterator(line.begin(), line.end(), pat, m3);
  if (it == std::sregex_token_iterator()) { return false; }

  auto label = *it++;
  auto label_ptr = labels[label];
  auto code = prog->gen_inst(Opc::br, ptr_lo(label_ptr), ptr_hi(label_ptr));
  if (!label_ptr) { backfill_labels[label].push_back(code + 1); }
  return true;
}

bool Compiler::handle_branch(Program *prog, const std::string &line) {
  static std::regex pat(
      R"(^\s*IF\s+(#[\+\-]?\d+|[&\*]?\w+)\s*(<|>|<=|>=|==|!=)\s*(#[\+\-]?\d+|[&\*]?\w+)\s+GOTO\s+(\w+)\s*$)");
  auto it = std::sregex_token_iterator(line.begin(), line.end(), pat, m4);
  if (it == std::sregex_token_iterator()) return false;

  auto x = primary_exp(prog, *it++);
  auto opc = *it++;
  auto y = primary_exp(prog, *it++);

  auto label = *it++;
  auto label_ptr = labels[label];

  static std::map<std::string, Opc> s2op{
      {"<", Opc::lt},  {">", Opc::gt},  {"<=", Opc::le},
      {">=", Opc::ge}, {"==", Opc::eq}, {"!=", Opc::ne},
  };

  auto tmp = newTemp();
  prog->gen_inst(s2op[opc], tmp, x, y);
  auto code = prog->gen_cond_br(tmp, label_ptr);
  if (!label_ptr) { backfill_labels[label].push_back(code + 2); }
  return true;
}

bool Compiler::handle_ret(Program *prog, const std::string &line) {
  static std::regex pat(
      R"(^\s*RETURN\s+(#[\+\-]?\d+|[&\*]?\w+)\s*$)");
  auto it = std::sregex_token_iterator(line.begin(), line.end(), pat, m4);
  if (it == std::sregex_token_iterator()) return false;

  auto x = primary_exp(prog, *it++);
  prog->gen_inst(Opc::ret, x);
  return true;
}

bool Compiler::handle_dec(Program *prog, const std::string &line) {
  static std::regex pat(R"(^\s*DEC\s+(\w+)\s+(\d+)\s*$)");
  auto it = std::sregex_token_iterator(line.begin(), line.end(), pat, m2);
  if (it == std::sregex_token_iterator()) return false;

  auto x = *it++;
  auto size = stoi(*it++);
  getVar(x, (size + 3) / 4);
  return true;
}

bool Compiler::handle_arg(Program *prog, const std::string &line) {
  static std::regex pat(R"(^\s*ARG\s+(#[\+\-]?\d+|[&\*]?\w+)\s*$)");
  auto it = std::sregex_token_iterator(line.begin(), line.end(), pat, m2);
  if (it == std::sregex_token_iterator()) return false;

  auto arg = newArg();
  primary_exp(prog, *it++, arg);
  return true;
}

bool Compiler::handle_call(Program *prog, const std::string &line) {
  static std::regex pat(R"(^\s*(\w+)\s*:=\s*CALL\s+(\w+)\s*$)");
  auto it = std::sregex_token_iterator(line.begin(), line.end(), pat, m2);
  if (it == std::sregex_token_iterator()) return false;
  auto to = *it++;
  auto f = *it++;

  auto lo = newArg();
  auto hi = newArg();
  auto ret = newArg();
  auto inc = newArg();

  auto *la_lo = prog->gen_inst(Opc::li, lo, 0);
  auto *la_hi = prog->gen_inst(Opc::li, hi, 0);
  prog->gen_inst(Opc::li, ret, getVar(to));
  prog->gen_inst(Opc::li, inc, inc);
  prog->gen_inst(Opc::inc_esp, inc);
  prog->gen_br(funcs[f]);

  auto *text_ptr = prog->get_textptr();
  la_lo[2] = ptr_lo(text_ptr);
  la_hi[2] = ptr_hi(text_ptr);

  return true;
}

bool Compiler::handle_param(Program *prog, const std::string &line) {
  static std::regex pat(R"(^\s*PARAM\s+(\w+)\s*$)");
  auto it = std::sregex_token_iterator(line.begin(), line.end(), pat, m1);
  if (it == std::sregex_token_iterator()) return false;
  getParam(*it++);
  return true;
}

bool Compiler::handle_read(Program *prog, const std::string &line) {
  static std::regex pat(R"(^\s*READ\s+(\w+)\s*$)");
  auto it = std::sregex_token_iterator(line.begin(), line.end(), pat, m1);
  if (it == std::sregex_token_iterator()) return false;

  auto x = *it++;
  prog->gen_inst(Opc::read, getVar(x));
  return true;
}

bool Compiler::handle_write(Program *prog, const std::string &line) {
  static std::regex pat(R"(^\s*WRITE\s+(#[\+\-]?\d+|[&\*]?\w+)\s*$)");
  auto it = std::sregex_token_iterator(line.begin(), line.end(), pat, m1);
  if (it == std::sregex_token_iterator()) return false;

  auto x = primary_exp(prog, *it++);
  prog->gen_inst(Opc::write, x);
  return true;
}

void log_curir(int *eip, int *esp) {
  const char *s = lohi_to_ptr<char>(eip[0], eip[1]);
  fmt::printf("IR> %s\n", s, s);
}

std::unique_ptr<Program> Compiler::compile(std::istream &is) {
#ifdef LOGIR
  static std::vector<std::unique_ptr<char[]>> lines;
#endif
  auto prog = std::make_unique<Program>();
  unsigned lineno = 0;
  while ((is.peek(), is.good())) {
    bool suc = false;
    lineno++;
    std::string line;
    std::getline(is, line);

#ifdef LOGIR
    char *ir = new char[line.size() + 1];
    strcpy(ir, line.c_str());
    lines.push_back(std::unique_ptr<char[]>(ir));
    prog->gen_inst(Opc::call, ptr_lo((void *)log_curir),
                   ptr_hi((void *)log_curir), 2, ptr_lo(ir), ptr_hi(ir));
#endif

#ifdef DEBUG
    fmt::printf("compile %s\n", line);
#endif

    if (line.find_first_not_of("\r\n\v\f\t ") == line.npos) { continue; }

    for (int i = (int)Stmt::begin; i < (int)Stmt::end; i++)
      if ((this->*handlers[(Stmt)i])(&*prog, line)) {
        suc = true;
        break;
      }

    if (suc) continue;

    fmt::printf("syntax error at line %d: %s\n", lineno, line);
  }

  if (prog->curf[0] == (int)Opc::alloca) { prog->curf[1] = stack_size + 1; }
  return prog;
}

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

  Compiler compiler;
  auto prog = compiler.compile(ifs);
  prog->run(compiler.getFunction("main"));
  return 0;
}
