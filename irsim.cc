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

// #define LOGIR
// #define DEBUG
// #define SAFE_POINTER

#include "irsim.h"

namespace irsim {

/* clang-format off */
std::map<Opc, std::string> opc_to_string{
    {Opc::abort, "abort"}, {Opc::helper, "helper"},
    {Opc::alloca, "alloca"}, {Opc::la, "la"},
    {Opc::ld, "ld"}, {Opc::st, "st"}, {Opc::li, "li"},
    {Opc::mov, "mov"}, {Opc::add, "add"}, {Opc::sub, "sub"},
    {Opc::mul, "mul"}, {Opc::div, "div"}, {Opc::jmp, "jmp"},
    {Opc::br, "br"}, {Opc::lt, "lt"}, {Opc::le, "le"},
    {Opc::eq, "eq"}, {Opc::ge, "ge"}, {Opc::gt, "gt"},
    {Opc::ne, "ne"}, {Opc::call, "call"}, {Opc::ret, "ret"},
    {Opc::mfcr, "mfcr"}, {Opc::mtcr, "mtcr"},
    {Opc::quit, "quit"}, {Opc::mark, "mark"},
};
/* clang-format on */

int Program::run(int *eip) {
  std::vector<int> args;
  std::vector<int *> frames;
  std::vector<std::vector<int>> stack;
  std::array<int, 6> ctrl_regs = {};

  /* clang-format off */
  int _start[] = {
      (int)Opc::alloca, 1,
      (int)Opc::call, ptr_lo(eip), ptr_hi(eip),
      (int)Opc::mfcr, 0, CR_RET,
      (int)Opc::quit, 0,
  };
  /* clang-format on */

  eip = &_start[0];

  while (true) {
#ifdef DEBUG
    auto oldeip = eip;
#endif
    if (eip == nullptr) {
      exception = Exception::IF;
      return -1;
    }

    int opc = *eip++;

#ifdef DEBUG
    printf("stack:\n");
    for (auto d = 0u; d < stack.size(); d++) {
      auto &s = stack[d];
      constexpr int step = 6;
      for (auto i = 0u; i < s.size(); i += step) {
        if (i == 0)
          printf("%02d:%02d:", d, i);
        else
          printf("  :%02d:", i);
        for (auto j = i; j < i + step && j < s.size();
             j++) {
          printf(" %08x", s[j]);
        }
        printf("\n");
      }
    }
    printf("crs:  ");
    for (auto i = 0u; i < ctrl_regs.size(); i++)
      printf(" %08x", ctrl_regs[i]);
    printf("\n");
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
    case Opc::la: {
      int to = *eip++;
      int from = *eip++;
      stack.back().at(to) =
          ((stack.size() - 1) << 16) | (from * 4);
      if (stack.size() >= 65536 || from >= 65536) {
        return -1;
      }
#ifdef DEBUG
      printf("%p: la %d, %d\n", oldeip, to, from);
#endif
    } break;
    case Opc::ld: {
      int to = *eip++;
      int from = *eip++;
      int sel = ((unsigned)from >> 16);
      int addr = addr & 0xFFFF;
      if ((int)stack.size() <= sel) {
        exception = Exception::LOAD;
        return -1;
      } else if ((int)stack[sel].size() * 4 <= addr + 4) {
        exception = Exception::LOAD;
        return -1;
      } else {
        uint8_t *from_ptr = (uint8_t *)&(stack[sel]) + addr;
        uint8_t *to_ptr = (uint8_t *)&stack.back().at(to);
        memcpy(to_ptr, from_ptr, sizeof(int));
      }
#ifdef DEBUG
      printf("%p: ld %d, (%d)=%d\n", oldeip, to, from,
          stack.back().at(from));
#endif
    } break;
    case Opc::st: {
      int to = *eip++;
      int from = *eip++;
      int sel = ((unsigned)to >> 16);
      int addr = addr & 0xFFFF;
      if ((int)stack.size() <= sel) {
        exception = Exception::LOAD;
        return -1;
      } else if ((int)stack[sel].size() * 4 <= addr + 4) {
        exception = Exception::LOAD;
        return -1;
      } else {
        uint8_t *from_ptr =
            (uint8_t *)&stack.back().at(from);
        uint8_t *to_ptr = (uint8_t *)&(stack[sel]) + addr;
        memcpy(to_ptr, from_ptr, sizeof(int));
      }
#ifdef DEBUG
      printf("%p: st (%d)=%d, %d\n", oldeip, to,
          stack.back().at(to), from);
#endif
    } break;
    case Opc::li: {
      int to = *eip++;
      int lhs = *eip++;
      stack.back().at(to) = lhs;
#ifdef DEBUG
      if (lhs < 0 || lhs > 256)
        printf("%p: li %d %08x\n", oldeip, to, lhs);
      else
        printf("%p: li %d %d\n", oldeip, to, lhs);
#endif
    } break;
    case Opc::mov: {
      int to = *eip++;
      int lhs = *eip++;
#ifdef DEBUG
      printf("%p: mov %d %d\n", oldeip, to, lhs);
#endif
      stack.back().at(to) = stack.back().at(lhs);
    } break;
    case Opc::add: {
      int to = *eip++;
      int lhs = *eip++;
      int rhs = *eip++;
      stack.back().at(to) =
          stack.back().at(lhs) + stack.back().at(rhs);
#ifdef DEBUG
      printf("%p: add %d, %d, %d\n", oldeip, to, lhs, rhs);
#endif
    } break;
    case Opc::sub: {
      int to = *eip++;
      int lhs = *eip++;
      int rhs = *eip++;
      stack.back().at(to) =
          stack.back().at(lhs) - stack.back().at(rhs);
#ifdef DEBUG
      printf("%p: sub %d, %d, %d\n", oldeip, to, lhs, rhs);
#endif
    } break;
    case Opc::mul: {
      int to = *eip++;
      int lhs = *eip++;
      int rhs = *eip++;
      stack.back().at(to) =
          stack.back().at(lhs) * stack.back().at(rhs);
#ifdef DEBUG
      printf("%p: mul %d, %d, %d\n", oldeip, to, lhs, rhs);
#endif
    } break;
    case Opc::div: {
      int to = *eip++;
      int lhs = *eip++;
      int rhs = *eip++;
      if (stack.back().at(rhs) == 0) {
        exception = Exception::DIV_ZERO;
        return -1;
      } else {
        stack.back().at(to) =
            stack.back().at(lhs) / stack.back().at(rhs);
      }
#ifdef DEBUG
      printf("%p: div %d, %d, %d\n", oldeip, to, lhs, rhs);
#endif
    } break;
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
      int cond = stack.back().at(*eip++);
      uint64_t ptrlo = *eip++;
      uint64_t ptrhi = *eip++;
#ifdef DEBUG
      printf("%p: cond %d br %p\n", oldeip, cond,
          lohi_to_ptr<void>(ptrlo, ptrhi));
#endif
      if (cond) { eip = lohi_to_ptr<int>(ptrlo, ptrhi); }
    } break;
    case Opc::lt: {
      int to = *eip++;
      int lhs = *eip++;
      int rhs = *eip++;
      stack.back().at(to) =
          stack.back().at(lhs) < stack.back().at(rhs);
#ifdef DEBUG
      printf("%p: lt %d, %d, %d\n", oldeip, to, lhs, rhs);
#endif
    } break;
    case Opc::le: {
      int to = *eip++;
      int lhs = *eip++;
      int rhs = *eip++;
      stack.back().at(to) =
          stack.back().at(lhs) <= stack.back().at(rhs);
#ifdef DEBUG
      printf("%p: le %d, %d, %d\n", oldeip, to, lhs, rhs);
#endif
    } break;
    case Opc::eq: {
      int to = *eip++;
      int lhs = *eip++;
      int rhs = *eip++;
      stack.back().at(to) =
          stack.back().at(lhs) == stack.back().at(rhs);
#ifdef DEBUG
      printf("%p: eq %d, %d, %d\n", oldeip, to, lhs, rhs);
#endif
    } break;
    case Opc::ge: {
      int to = *eip++;
      int lhs = *eip++;
      int rhs = *eip++;
      stack.back().at(to) =
          stack.back().at(lhs) >= stack.back().at(rhs);
#ifdef DEBUG
      printf("%p: ge %d, %d, %d\n", oldeip, to, lhs, rhs);
#endif
    } break;
    case Opc::gt: {
      int to = *eip++;
      int lhs = *eip++;
      int rhs = *eip++;
      stack.back().at(to) =
          stack.back().at(lhs) > stack.back().at(rhs);
#ifdef DEBUG
      printf("%p: gt %d, %d, %d\n", oldeip, to, lhs, rhs);
#endif
    } break;
    case Opc::ne: {
      int to = *eip++;
      int lhs = *eip++;
      int rhs = *eip++;
      stack.back().at(to) =
          stack.back().at(lhs) != stack.back().at(rhs);
#ifdef DEBUG
      printf("%p: ne %d, %d, %d\n", oldeip, to, lhs, rhs);
#endif
    } break;
    case Opc::call: {
      int ptrlo = *eip++;
      int ptrhi = *eip++;
      int *target = lohi_to_ptr<int>(ptrlo, ptrhi);
      frames.push_back(eip);
      stack.emplace_back();
      eip = target;
#ifdef DEBUG
      printf("%p: call %p\n", oldeip, eip);
#endif
    } break;
    case Opc::ret: {
      assert(stack.size() >= 1);
      stack.pop_back();
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
      if (stack.empty()) stack.emplace_back();
      stack.back().resize(size);
      unsigned used_mem = 0;
      for (auto &s : stack) used_mem += s.size();
      if (used_mem >= memory_limit) {
        exception = Exception::OOM;
        return -1;
      }
    } break;
    case Opc::mfcr: {
      int to = *eip++;
      int from = *eip++;
      switch (from) {
      case CR_COUNT:
        stack.back().at(to) = ctrl_regs.at(from);
#ifdef DEBUG
        printf("%p: mfcr %d, cr_count\n", oldeip, to);
#endif
        break;
      case CR_RET: stack.back().at(to) = ctrl_regs.at(from);
#ifdef DEBUG
        printf("%p: mfcr %d, cr_ret\n", oldeip, to);
#endif
        break;
      case CR_SERIAL: stack.back().at(to) = io.read();
#ifdef DEBUG
        printf("%p: mfcr %d, cr_serial\n", oldeip, to);
#endif
        break;
      case CR_ARG:
        stack.back().at(to) = args.back();
        args.pop_back();
#ifdef DEBUG
        printf("%p: mfcr %d, cr_arg\n", oldeip, to);
#endif
        break;
      default: abort();
      }
    } break;
    case Opc::mtcr: {
      int to = *eip++;
      int from = *eip++;
      switch (to) {
      case CR_COUNT:
#ifdef DEBUG
        printf("%p: mtcr cr_count, %d\n", oldeip, from);
#endif
        ctrl_regs.at(to) = stack.back().at(from);
        break;
      case CR_RET:
        ctrl_regs.at(to) = stack.back().at(from);
#ifdef DEBUG
        printf("%p: mtcr cr_ret, %d\n", oldeip, from);
#endif
        break;
      case CR_SERIAL: io.write(stack.back().at(from));
#ifdef DEBUG
        printf("%p: mtcr cr_serial, %d\n", oldeip, from);
#endif
        break;
      case CR_ARG: args.push_back(stack.back().at(from));
#ifdef DEBUG
        printf("%p: mtcr cr_arg, %d\n", oldeip, from);
#endif
        break;
      default: abort();
      }
    } break;
    case Opc::mark: ctrl_regs[CR_COUNT]++; break;
    case Opc::quit: return stack.back().at(*eip++);
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
#ifdef DEBUG
    printf("alloca %d\n", stack_size + 1);
#endif
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
  prog->gen_inst(Opc::la, getVar(x), getVar(y));
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
  prog->gen_inst(Opc::mtcr, CR_RET, x);
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
  prog->gen_inst(Opc::mtcr, CR_ARG, tmp);
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

  prog->gen_call(funcs[f]);
  prog->gen_inst(Opc::mfcr, getVar(to), CR_RET);
  return true;
}

bool Compiler::handle_param(
    Program *prog, const std::string &line) {
  static std::regex pat(R"(^\s*PARAM\s+(\w+)\s*$)");
  auto it = std::sregex_token_iterator(
      line.begin(), line.end(), pat, m1);
  if (it == std::sregex_token_iterator()) return false;
  prog->gen_inst(Opc::mfcr, getVar(*it++), CR_ARG);
  return true;
}

bool Compiler::handle_read(
    Program *prog, const std::string &line) {
  static std::regex pat(R"(^\s*READ\s+(\w+)\s*$)");
  auto it = std::sregex_token_iterator(
      line.begin(), line.end(), pat, m1);
  if (it == std::sregex_token_iterator()) return false;

  auto x = *it++;
  prog->gen_inst(Opc::mfcr, getVar(x), CR_SERIAL);
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
  prog->gen_inst(Opc::mtcr, CR_SERIAL, x);
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

    prog->gen_inst(Opc::mark);

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
#ifdef DEBUG
    printf("alloca %d\n", stack_size + 2);
#endif
    prog->curf[1] = stack_size + 2;
  }
  prog->gen_inst(Opc::abort);
  return prog;
}

} // namespace irsim
