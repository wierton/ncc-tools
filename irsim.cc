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

#include "irsim.h"

#define LOGIR
#define DEBUG
// #define SAFE_POINTER

namespace irsim {

/* clang-format off */
static std::map<Opc, std::string> opc_to_string{
    {Opc::abort, "abort"},     {Opc::helper, "helper"},
    {Opc::la, "la"},
    {Opc::ld, "ld"},           {Opc::st, "st"},
    {Opc::updsp, "updsp"},     {Opc::li, "li"},
    {Opc::mov, "mov"},         {Opc::add, "add"},
    {Opc::sub, "sub"},         {Opc::mul, "mul"},
    {Opc::div, "div"},         {Opc::jmp, "jmp"},
    {Opc::br, "br"},           {Opc::lt, "lt"},
    {Opc::le, "le"},           {Opc::eq, "eq"},
    {Opc::ge, "ge"},           {Opc::gt, "gt"},
    {Opc::ne, "ne"},           {Opc::alloca, "alloca"},
    {Opc::call, "call"},       {Opc::ret, "ret"},
    {Opc::read, "read"},       {Opc::write, "write"},
    {Opc::quit, "quit"},
};
/* clang-format on */

#ifdef SAFE_POINTER
template <class T>
struct SafePointer {
  T *ptr;
  size_t left;
  size_t right;

  void check(int i) const {
    assert(i != INT_MIN);
    if (i < 0) {
      assert((unsigned)-i <= left);
    } else {
      assert((unsigned)i < right);
    }
  }

public:
  SafePointer(T *ptr, size_t right, size_t left = 0)
      : ptr(ptr), left(left), right(right) {}

  operator T * const() { return ptr; }

  T &operator[](int i) const {
    check(i);
    return ptr[i];
  }

  SafePointer &operator+=(int inc) {
    check(inc);
    ptr += inc;
    left += inc;
    right -= inc;
    return *this;
  }

  SafePointer &operator-=(int inc) {
    assert(inc != INT_MIN);
    return this->operator+=(-inc);
  }
};
#else
template <class T>
T *SafePointer(T *ptr, size_t right, size_t left = 0) {
  return ptr;
}
#endif

int Program::run(int *eip) {
  std::vector<int *> frames;
  std::vector<int> args;

  auto ret = 2, inc = 3;
  /* clang-format off */
  int _start[] = {
      (int)Opc::alloca, 4,
      (int)Opc::li, ret, 0,
      (int)Opc::li, inc, inc,
      (int)Opc::updsp, inc,
      (int)Opc::call, ptr_lo(eip), ptr_hi(eip),
      (int)Opc::quit, 0,
  };
  /* clang-format on */

  eip = &_start[0];
  auto esp = SafePointer<int>(&stack[0], stack.size());

  while (true) {
#ifdef DEBUG
    auto oldeip = eip;
#endif
    if (eip == nullptr) {
      exception = Exception::IF;
      return -1;
    }

    int opc = *eip++;
    int from, to;
    int lhs, rhs;
    int constant;

#ifdef DEBUG
    printf("stack:\n");
    auto sp_value = (ptrdiff_t)(esp - &stack[0]);
    constexpr int step = 6;
    for (auto i = 0u; i < stack.size(); i += step) {
      printf("%02d:", i);
      for (auto j = i; j < i + step && j < stack.size();
           j++) {
        if (j == sp_value) {
          printf(">%08x<", stack[j]);
        } else {
          printf(" %08x ", stack[j]);
        }
      }
      printf("\n");
    }
#endif

    switch ((Opc)opc) {
    case Opc::abort:
      printf("unexpected instruction\n");
      exception = Exception::ABORT;
#ifdef DEBUG
      printf("%p: abort\n", oldeip);
#endif
      return -1;
    case Opc::helper: {
      int ptrlo = *eip++;
      int ptrhi = *eip++;
      int nr_args = *eip++;
      using F = void(int *, int *);
      F *f = lohi_to_ptr<F>(ptrlo, ptrhi);
      f(eip, esp);
      eip += nr_args;
#ifdef DEBUG
      printf("%p: helper %p\n", oldeip, (void *)f);
#endif
    } break;
    case Opc::arg: to = *eip++; args.push_back(esp[to]);
#ifdef DEBUG
      printf("%p: arg %d\n", oldeip, to);
#endif
      break;
    case Opc::param:
      to = *eip++;
      esp[to] = args.back();
      args.pop_back();
#ifdef DEBUG
      printf("%p: param %d\n", oldeip, to);
#endif
      break;
    case Opc::la: {
      to = *eip++;
      from = *eip++;
      esp[to] = ((int)(esp - &stack[0]) + from) * 4;
#ifdef DEBUG
      printf("%p: la %d, %d\n", oldeip, to, from);
#endif
    } break;
    case Opc::ld: {
      to = *eip++;
      from = *eip++;
      /* esp[to] = stack[esp[from]] */
#ifdef DEBUG
      printf("%p: ld %d, (%d)=%d\n", oldeip, to, from,
          esp[from]);
#endif
      if ((unsigned)esp[from] + sizeof(int) >=
          sizeof(int) * stack.size()) {
        exception = Exception::LOAD;
        return -1;
      }
      memcpy(&esp[to], (char *)&stack[0] + esp[from],
          sizeof(int));
    } break;
    case Opc::st: {
      to = *eip++;
      from = *eip++;
      /* stack[esp[to]] = esp[from] */
      if ((unsigned)esp[to] + sizeof(int) >=
          sizeof(int) * stack.size()) {
        exception = Exception::STORE;
        return -1;
      }
      memcpy((char *)&stack[0] + esp[to], &esp[from],
          sizeof(int));
#ifdef DEBUG
      printf("%p: st (%d)=%d, %d\n", oldeip, to, esp[to],
          from);
#endif
    } break;
    case Opc::li: {
      to = *eip++;
      lhs = *eip++;
      esp[to] = lhs;
#ifdef DEBUG
      if (lhs < 0 || lhs > 256)
        printf("%p: li %d %08x\n", oldeip, to, lhs);
      else
        printf("%p: li %d %d\n", oldeip, to, lhs);
#endif
    } break;
    case Opc::mov:
      /* esp[*eip++] = esp[*eip++]; // WARNING: undefined
       * behavior */
      to = *eip++;
      lhs = *eip++;
#ifdef DEBUG
      printf("%p: mov %d %d\n", oldeip, to, lhs);
#endif
      esp[to] = esp[lhs];
      break;
    case Opc::add:
      to = *eip++;
      lhs = *eip++;
      rhs = *eip++;
      esp[to] = esp[lhs] + esp[rhs];
#ifdef DEBUG
      printf("%p: add %d, %d, %d\n", oldeip, to, lhs, rhs);
#endif
      break;
    case Opc::sub:
      to = *eip++;
      lhs = *eip++;
      rhs = *eip++;
      esp[to] = esp[lhs] - esp[rhs];
#ifdef DEBUG
      printf("%p: sub %d, %d, %d\n", oldeip, to, lhs, rhs);
#endif
      break;
    case Opc::mul:
      to = *eip++;
      lhs = *eip++;
      rhs = *eip++;
      esp[to] = esp[lhs] * esp[rhs];
#ifdef DEBUG
      printf("%p: mul %d, %d, %d\n", oldeip, to, lhs, rhs);
#endif
      break;
    case Opc::div:
      to = *eip++;
      lhs = *eip++;
      rhs = *eip++;
      if (esp[rhs] == 0) {
        exception = Exception::DIV_ZERO;
        return -1;
      } else {
        esp[to] = esp[lhs] / esp[rhs];
      }
#ifdef DEBUG
      printf("%p: div %d, %d, %d\n", oldeip, to, lhs, rhs);
#endif
      break;
    case Opc::jmp: {
      uint64_t ptrlo = *eip++;
      uint64_t ptrhi = *eip++;
#ifdef DEBUG
      printf("%p: jmp %p\n", oldeip,
          lohi_to_ptr<void>(ptrlo, ptrhi));
#endif
      eip = lohi_to_ptr<int>(ptrlo, ptrhi);
    } break;
    case Opc::br: {
      int cond = esp[*eip++];
      uint64_t ptrlo = *eip++;
      uint64_t ptrhi = *eip++;
#ifdef DEBUG
      printf("%p: cond %d br %p\n", oldeip, cond,
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
      printf("%p: lt %d, %d, %d\n", oldeip, to, lhs, rhs);
#endif
      break;
    case Opc::le:
      to = *eip++;
      lhs = *eip++;
      rhs = *eip++;
      esp[to] = esp[lhs] <= esp[rhs];
#ifdef DEBUG
      printf("%p: le %d, %d, %d\n", oldeip, to, lhs, rhs);
#endif
      break;
    case Opc::eq:
      to = *eip++;
      lhs = *eip++;
      rhs = *eip++;
      esp[to] = esp[lhs] == esp[rhs];
#ifdef DEBUG
      printf("%p: eq %d, %d, %d\n", oldeip, to, lhs, rhs);
#endif
      break;
    case Opc::ge:
      to = *eip++;
      lhs = *eip++;
      rhs = *eip++;
      esp[to] = esp[lhs] >= esp[rhs];
#ifdef DEBUG
      printf("%p: ge %d, %d, %d\n", oldeip, to, lhs, rhs);
#endif
      break;
    case Opc::gt:
      to = *eip++;
      lhs = *eip++;
      rhs = *eip++;
      esp[to] = esp[lhs] > esp[rhs];
#ifdef DEBUG
      printf("%p: gt %d, %d, %d\n", oldeip, to, lhs, rhs);
#endif
      break;
    case Opc::ne:
      to = *eip++;
      lhs = *eip++;
      rhs = *eip++;
      esp[to] = esp[lhs] != esp[rhs];
#ifdef DEBUG
      printf("%p: ne %d, %d, %d\n", oldeip, to, lhs, rhs);
#endif
      break;
    case Opc::updsp: constant = *eip++;
#ifdef DEBUG
      printf("%p: updsp %d\n", oldeip, to);
#endif
      esp += constant;
      break;
    case Opc::call: {
      int ptrlo = *eip++;
      int ptrhi = *eip++;
      int *target = lohi_to_ptr<int>(ptrlo, ptrhi);
      frames.push_back(eip);
      eip = target;
#ifdef DEBUG
      printf("%p: call %p\n", oldeip, eip);
#endif
    } break;
    case Opc::ret: {
      esp -= esp[0];
      assert(frames.size());
      eip = frames.back();
      frames.pop_back();
#ifdef DEBUG
      printf("%p: ret %p\n", oldeip, eip);
#endif
    } break;
    case Opc::alloca: {
      int size = *eip++;
#ifdef DEBUG
      printf("%p: alloca %d\n", oldeip, size);
#endif
      ptrdiff_t base = esp - &stack[0];
      assert(base >= 0 && (unsigned)base <= stack.size());
      unsigned newSize = base + size;
      if (newSize >= stack.size()) {
        if (newSize < memory_limit) {
          auto ns =
              std::min(2 * (newSize + 1), memory_limit);
          stack.resize(ns);
          esp = SafePointer<int>(
              &stack[base], ns - base, base);
        } else {
          exception = Exception::OOM;
          return -1;
        }
      }
    } break;
    case Opc::read:
      to = *eip++;
      esp[to] = io.read();
      break;
    case Opc::write: to = *eip++; io.write(esp[to]);
#ifdef DEBUG
      printf("%p: write %d\n", oldeip, to);
#endif
      break;
    case Opc::quit: return 0;
    case Opc::inst_begin:
      inst_counter++;
      if (inst_counter >= insts_limit) {
        exception = Exception::TIMEOUT;
        return -1;
      }
      break;
    default:
      printf("unexpected opc %d\n", opc);
      exception = Exception::INVOP;
      return -1;
    }
  }
  return 0;
}

/* clang-format off */
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
/* clang-format on */

int Compiler::primary_exp(
    Program *prog, const std::string &tok, int to) {
  if (tok[0] == '#') {
    if (to == INT_MAX) to = newTemp();
    prog->gen_inst(Opc::li, to, std::stoll(&tok[1]));
    return to;
  } else if (tok[0] == '&') {
    auto var = getVar(&tok[1]);
    if (to == INT_MAX) to = newTemp();
    prog->gen_inst(Opc::la, to, var);
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

/* stmt label */
bool Compiler::handle_label(
    Program *prog, const std::string &line) {
  static std::regex pat(R"(^\s*LABEL\s+(\w+)\s*:\s*$)");
  auto it = std::sregex_token_iterator(
      line.begin(), line.end(), pat, m1);

  if (it == std::sregex_token_iterator()) return false;

  auto label = *it++;
  auto label_ptr = prog->get_textptr();
  labels[label] = label_ptr;
#ifdef DEBUG
  printf(
      "add label %s, %p\n", label.str().c_str(), label_ptr);
#endif
  for (auto *ptr : backfill_labels[label]) {
    ptr[0] = ptr_lo(label_ptr);
    ptr[1] = ptr_hi(label_ptr);
  }
  backfill_labels.erase(label);
  return true;
}

/* stmt func */
bool Compiler::handle_func(
    Program *prog, const std::string &line) {
  static std::regex pat(R"(^\s*FUNCTION\s+(\w+)\s*:\s*$)");
  auto it = std::sregex_token_iterator(
      line.begin(), line.end(), pat, m1);
  if (it == std::sregex_token_iterator()) return false;

  auto f = *it++;

  prog->gen_inst(
      Opc::abort); // last function should manually ret
  funcs[f] = prog->get_textptr();

  if (prog->curf[0] == (int)Opc::alloca) {
    prog->curf[1] = stack_size + 1;
    clear_env();
  }
  prog->curf = prog->gen_inst(Opc::alloca, 0);
  return true;
}

bool Compiler::handle_assign(
    Program *prog, const std::string &line) {
  static std::regex pat(
      R"(^\s*(\w+)\s*:=\s*(#[\+\-]?\d+|[&\*]?\w+)\s*$)");
  auto it = std::sregex_token_iterator(
      line.begin(), line.end(), pat, m2);
  if (it == std::sregex_token_iterator()) return false;

  auto x = getVar(*it++);
  primary_exp(prog, *it++, x);
  return true;
}

bool Compiler::handle_arith(
    Program *prog, const std::string &line) {
  static std::regex pat(
      R"(^\s*(\w+)\s*:=\s*(#[\+\-]?\d+|[&\*]?\w+)\s*(\+|\-|\*|\/)\s*(#[\+\-]?\d+|[&\*]?\w+)\s*$)");

  static std::map<std::string, Opc> m{
      {"+", Opc::add},
      {"-", Opc::sub},
      {"*", Opc::mul},
      {"/", Opc::div},
  };

  auto it = std::sregex_token_iterator(
      line.begin(), line.end(), pat, m4);
  if (it == std::sregex_token_iterator()) return false;

  auto x = getVar(*it++);
  auto y = primary_exp(prog, *it++);
  auto op = *it++;
  auto z = primary_exp(prog, *it++);

  prog->gen_inst(m[op], x, y, z);
  return true;
}

bool Compiler::handle_takeaddr(
    Program *prog, const std::string &line) {
  static std::regex pat(
      R"(^\s*(\w+)\s*:=\s*&\s*(\w+)\s*$)");
  /* stmt assign */
  auto it = std::sregex_token_iterator(
      line.begin(), line.end(), pat, m2);
  if (it == std::sregex_token_iterator()) return false;
  auto x = *it++;
  auto y = *it++;
  prog->gen_inst(Opc::li, getVar(x), getVar(y));
  return true;
}

bool Compiler::handle_deref(
    Program *prog, const std::string &line) {
  static std::regex pat(
      R"(^\s*(\w+)\s*:=\s*\*\s*(\w+)\s*$)");
  /* stmt assign */
  auto it = std::sregex_token_iterator(
      line.begin(), line.end(), pat, m2);
  if (it == std::sregex_token_iterator()) return false;
  auto x = getVar(*it++);
  auto y = getVar(*it++);
  auto tmp = newTemp();
  prog->gen_inst(Opc::la, tmp, y);
  prog->gen_inst(Opc::ld, x, y);
  return true;
}

bool Compiler::handle_deref_assign(
    Program *prog, const std::string &line) {
  static std::regex pat(
      R"(^\s*\*(\w+)\s+:=\s+(#[\+\-]?\d+|[&\*]?\w+)\s*$)");
  auto it = std::sregex_token_iterator(
      line.begin(), line.end(), pat, m2);
  if (it == std::sregex_token_iterator()) return false;

  auto x = getVar(*it++);
  auto y = primary_exp(prog, *it++);
  prog->gen_inst(Opc::st, x, y);
  return true;
}

bool Compiler::handle_goto_(
    Program *prog, const std::string &line) {
  static std::regex pat(R"(^\s*GOTO\s+(\w+)\s*$)");
  auto it = std::sregex_token_iterator(
      line.begin(), line.end(), pat, m1);
  if (it == std::sregex_token_iterator()) { return false; }

  auto label = *it++;
  auto label_ptr = labels[label];
  auto code = prog->gen_inst(
      Opc::jmp, ptr_lo(label_ptr), ptr_hi(label_ptr));
  if (!label_ptr) {
    backfill_labels[label].push_back(code + 1);
  }
  return true;
}

bool Compiler::handle_branch(
    Program *prog, const std::string &line) {
  static std::regex pat(
      R"(^\s*IF\s+(#[\+\-]?\d+|[&\*]?\w+)\s*(<|>|<=|>=|==|!=)\s*(#[\+\-]?\d+|[&\*]?\w+)\s+GOTO\s+(\w+)\s*$)");
  auto it = std::sregex_token_iterator(
      line.begin(), line.end(), pat, m4);
  if (it == std::sregex_token_iterator()) return false;

  auto x = primary_exp(prog, *it++);
  auto opc = *it++;
  auto y = primary_exp(prog, *it++);

  auto label = *it++;
  auto label_ptr = labels[label];

  static std::map<std::string, Opc> s2op{
      {"<", Opc::lt},
      {">", Opc::gt},
      {"<=", Opc::le},
      {">=", Opc::ge},
      {"==", Opc::eq},
      {"!=", Opc::ne},
  };

  auto tmp = newTemp();
  prog->gen_inst(s2op[opc], tmp, x, y);
  auto code = prog->gen_br(tmp, label_ptr);
  if (!label_ptr) {
    backfill_labels[label].push_back(code + 2);
  }
  return true;
}

bool Compiler::handle_ret(
    Program *prog, const std::string &line) {
  static std::regex pat(
      R"(^\s*RETURN\s+(#[\+\-]?\d+|[&\*]?\w+)\s*$)");
  auto it = std::sregex_token_iterator(
      line.begin(), line.end(), pat, m1);
  if (it == std::sregex_token_iterator()) return false;

  auto x = primary_exp(prog, *it++);
  prog->gen_inst(Opc::mov, getRet(), x);
  prog->gen_inst(Opc::ret);
  return true;
}

bool Compiler::handle_dec(
    Program *prog, const std::string &line) {
  static std::regex pat(R"(^\s*DEC\s+(\w+)\s+(\d+)\s*$)");
  auto it = std::sregex_token_iterator(
      line.begin(), line.end(), pat, m2);
  if (it == std::sregex_token_iterator()) return false;

  auto x = *it++;
  auto size = stoi(*it++);
  getVar(x, (size + 3) / 4);
  return true;
}

bool Compiler::handle_arg(
    Program *prog, const std::string &line) {
  static std::regex pat(
      R"(^\s*ARG\s+(#[\+\-]?\d+|[&\*]?\w+)\s*$)");
  auto it = std::sregex_token_iterator(
      line.begin(), line.end(), pat, m1);
  if (it == std::sregex_token_iterator()) return false;

  auto tmp = primary_exp(prog, *it++);
  prog->gen_inst(Opc::arg, tmp);
  // auto *ptr = prog->gen_inst(Opc::mov, 0, tmp);
  // backfill_args.push_back(ptr);
  return true;
}

bool Compiler::handle_call(
    Program *prog, const std::string &line) {
  static std::regex pat(
      R"(^\s*(\w+)\s*:=\s*CALL\s+(\w+)\s*$)");
  auto it = std::sregex_token_iterator(
      line.begin(), line.end(), pat, m2);
  if (it == std::sregex_token_iterator()) return false;

  auto to = *it++;
  auto f = *it++;

  /* backfill args */
#if 0
  for (auto *arg : backfill_args) {
	assert (arg[0] == (int)Opc::mov);
	arg[1] = newArg();
  }
#endif

  backfill_args.clear();

  auto ret = newArg();
  printf("allocate %d for ret\n", ret);
  auto inc = newArg();
  printf("allocate %d for inc\n", inc);

  prog->gen_inst(Opc::li, inc, inc);
  prog->gen_inst(Opc::updsp, inc);
  prog->gen_call(funcs[f]);
  prog->gen_inst(Opc::mov, getVar(to), ret);
  return true;
}

bool Compiler::handle_param(
    Program *prog, const std::string &line) {
  static std::regex pat(R"(^\s*PARAM\s+(\w+)\s*$)");
  auto it = std::sregex_token_iterator(
      line.begin(), line.end(), pat, m1);
  if (it == std::sregex_token_iterator()) return false;
  prog->gen_inst(Opc::param, getVar(*it++));
  return true;
}

bool Compiler::handle_read(
    Program *prog, const std::string &line) {
  static std::regex pat(R"(^\s*READ\s+(\w+)\s*$)");
  auto it = std::sregex_token_iterator(
      line.begin(), line.end(), pat, m1);
  if (it == std::sregex_token_iterator()) return false;

  auto x = *it++;
  prog->gen_inst(Opc::read, getVar(x));
  return true;
}

bool Compiler::handle_write(
    Program *prog, const std::string &line) {
  static std::regex pat(
      R"(^\s*WRITE\s+(#[\+\-]?\d+|[&\*]?\w+)\s*$)");
  auto it = std::sregex_token_iterator(
      line.begin(), line.end(), pat, m1);
  if (it == std::sregex_token_iterator()) return false;

  auto x = primary_exp(prog, *it++);
  prog->gen_inst(Opc::write, x);
  return true;
}

void log_curir(int *eip, int *esp) {
  const char *s = lohi_to_ptr<char>(eip[0], eip[1]);
  printf("IR:%03d> %s\n", eip[2], s);
}

std::unique_ptr<Program> Compiler::compile(
    std::istream &is) {
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

    prog->gen_inst(Opc::inst_begin);

#ifdef LOGIR
    char *ir = new char[line.size() + 1];
    strcpy(ir, line.c_str());
    lines.push_back(std::unique_ptr<char[]>(ir));
    prog->gen_inst(Opc::helper, ptr_lo((void *)log_curir),
        ptr_hi((void *)log_curir), 3, ptr_lo(ir),
        ptr_hi(ir), lineno);
#endif

#ifdef DEBUG
    printf("compile %s\n", line.c_str());
#endif

    if (line.find_first_not_of("\r\n\v\f\t ") ==
        line.npos) {
      continue;
    }

    clearTemps();
    for (int i = (int)Stmt::begin; i < (int)Stmt::end;
         i++) {
      if ((this->*handlers[(Stmt)i])(&*prog, line)) {
        if (i == (int)Stmt::func) temps.clear();
        ;
        suc = true;
        break;
      }
    }

    if (suc) continue;

    printf("[IGNORED] syntax error at line %d: '%s'\n",
        lineno, line.c_str());
    /* IGNORED and continue */
  }

  if (prog->curf[0] == (int)Opc::alloca) {
    prog->curf[1] = stack_size + 2;
  }
  prog->gen_inst(Opc::abort);
  return prog;
}

} // namespace irsim
