#ifndef IRSIM_H
#define IRSIM_H

#include <memory>
#include <string>
#include <map>
#include <vector>
#include <utility>
#include <iostream>
#include <limits.h>

namespace irsim {

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
  inst_begin,
  helper, // native call
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
  call,
  ret,
  read,
  write,
  quit,
};

using TransitionBlock = std::array<int, 4 * 1024>;

class ProgramInput {
  std::istream *is;
  std::vector<int> *vec;

  friend class Program;

public:
  ProgramInput(std::istream &is) : is(&is), vec(nullptr) {}
  ProgramInput(std::vector<int> &vec) : is(nullptr), vec(&vec) {}
  ProgramInput(const ProgramInput &that) = default;

  int read() {
    if (vec) {
      auto ret = vec->back();
      vec->pop_back();
      return ret;
    } else {
      int ret;
      (*is) >> ret;
      return ret;
    }
  }
};

class ProgramOutput {
  std::ostream *os;
  std::vector<int> *vec;

  friend class Program;

public:
  ProgramOutput(std::ostream &os) : os(&os), vec(nullptr) {}
  ProgramOutput(std::vector<int> &vec) : os(nullptr), vec(&vec) {}
  ProgramOutput(const ProgramOutput &that) = default;

  void write(int v) {
    if (vec) {
      vec->push_back(v);
    } else {
      (*os) << v << "\n";
    }
  }
};

class ProgramIO : public ProgramInput, public ProgramOutput {
public:
  ProgramIO(ProgramInput in, ProgramOutput out)
      : ProgramInput(in), ProgramOutput(out) {}
};

class Program {
  ProgramIO io;

  unsigned inst_counter;

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
  Program() : io(std::cin, std::cout) {
    inst_counter = 0;
    curblk = new TransitionBlock;
    codes.push_back(std::unique_ptr<TransitionBlock>(curblk));
    textptr = &curblk->at(0);

    curf = gen_inst(Opc::quit, Opc::quit);
  }

  struct Exception {
    enum { LOAD, STORE, DIV_ZERO, MAX_INSTS, MAX_MEMORY } reason;
  } exception;

  unsigned getInstCounter() const { return inst_counter; }

  void setIO(ProgramIO io) { this->io = io; }
  void setInput(ProgramInput in) {
    static_cast<ProgramInput &>(this->io) = in;
  }
  void setOutput(ProgramOutput out) {
    static_cast<ProgramOutput &>(this->io) = out;
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

  int *gen_call(int *target) {
    return gen_inst(Opc::call, ptr_lo(target), ptr_hi(target));
  }

  int *gen_br(int *target) {
    return gen_inst(Opc::br, ptr_lo(target), ptr_hi(target));
  }

  int *gen_cond_br(int cond, int *target) {
    return gen_inst(Opc::cond_br, cond, ptr_lo(target),
                    ptr_hi(target));
  }

  int run(int *eip, uint64_t max = -1u);
};

class Compiler {
  int stack_size;
  int args_size;

  std::map<std::string, int> vars;
  std::map<std::string, int *> funcs;
  std::map<std::string, int *> labels;

  std::map<std::string, std::vector<int *>> backfill_labels;

  static std::map<Stmt,
                  bool (Compiler::*)(Program *, const std::string &)>
      handlers;

  static int m1[];
  static int m2[];
  static int m3[];
  static int m4[];

  int primary_exp(Program *prog, const std::string &tok,
                  int to = INT_MAX);

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
    args_size = -2;
	vars.clear();
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

} // namespace irsim

#endif
