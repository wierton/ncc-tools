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

template <class T>
uint32_t ptr_hi(const T *ptr) {
  return reinterpret_cast<uintptr_t>(ptr) >> 32;
}

template <class T>
uint32_t ptr_lo(const T *ptr) {
  return reinterpret_cast<uintptr_t>(ptr);
}

template <class T>
const T *lohi_to_ptr(uint32_t lo, uint32_t hi) {
  uintptr_t ptr = ((uint64_t)hi << 32) | lo;
  return reinterpret_cast<const T *>(ptr);
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
  call,  // nativa call, not function call
  li,
  mov,
  add,
  sub,
  mul,
  div,
  la,
  deref,
  deref_assign,
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

std::map<Opc, std::string> opc_to_string{
    {Opc::abort, "abort"},   {Opc::call, "call"},
    {Opc::li, "li"},         {Opc::mov, "mov"},
    {Opc::add, "add"},       {Opc::sub, "sub"},
    {Opc::mul, "mul"},       {Opc::div, "div"},
    {Opc::la, "la"},         {Opc::deref, "deref"},
    {Opc::br, "br"},         {Opc::cond_br, "cond_br"},
    {Opc::lt, "lt"},         {Opc::le, "le"},
    {Opc::eq, "eq"},         {Opc::ge, "ge"},
    {Opc::gt, "gt"},         {Opc::ne, "ne"},
    {Opc::alloca, "alloca"}, {Opc::inc_esp, "inc_esp"},
    {Opc::ret, "ret"},       {Opc::read, "read"},
    {Opc::write, "write"},   {Opc::quit, "quit"},
    {Opc::deref_assign, "deref_assign"},
};

using TransitionBlock = std::array<int, 1024>;

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
  Program() : curf(nullptr) {
    curblk = new TransitionBlock;
    codes.push_back(std::unique_ptr<TransitionBlock>(curblk));
    textptr = &curblk->at(0);
  }

  int *get_textptr() const { return textptr; }
  void check_eof(unsigned N) {
    if (textptr + N + 2 >= &(*curblk)[curblk->size()]) {
      curblk = new TransitionBlock;
      codes.push_back(std::unique_ptr<TransitionBlock>(curblk));
      *textptr++ = (int)Opc::br;
      *textptr++ = ptr_lo(&(curblk->at(0)));
      *textptr++ = ptr_hi(&(curblk->at(0)));
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

    std::cout << "  " << opc_to_string[opc] << " ";
    for (int v : std::array<int, N>{static_cast<int>(args)...}) {
      std::cout << std::hex << "0x" << (unsigned)v << " " << std::dec;
    }
    std::cout << "\n";
    return oldptr;
  }

  int *gen_br(int *target) {
    assert(target);
    return gen_inst(Opc::br, ptr_lo(target), ptr_hi(target));
  }

  int *gen_cond_br(int cond, int *target) {
    assert(target);
    return gen_inst(Opc::br, cond, ptr_lo(target), ptr_hi(target));
  }

  void run(int *eip);
};

void Program::run(int *eip) {
  esp = &stack[0];

  for (;;) {
    int opc = *eip++;
    int to = 0, lhs = 0, rhs = 0;
    switch ((Opc)opc) {
    case Opc::abort: std::cerr << "unexpected instruction\n"; break;
    case Opc::call: {
      uintptr_t ptrlo = *eip++;
      uintptr_t ptrhi = *eip++;
      void *f = (void *)(ptrlo | ptrhi << 32);
      ((void (*)(int *, int *))f)(eip, esp);
    } break;
    case Opc::li: {
      to = *eip++;
      lhs = *eip++;
      esp[to] = lhs;
    } break;
    case Opc::mov:
      /* esp[*eip++] = esp[*eip++]; // WARNING: undefined behavior */
      to = *eip++;
      lhs = *eip++;
      esp[to] = esp[lhs];
      break;
    case Opc::add:
      to = *eip++;
      lhs = *eip++;
      rhs = *eip++;
      esp[to] = esp[lhs] + esp[rhs];
      break;
    case Opc::sub:
      to = *eip++;
      lhs = *eip++;
      rhs = *eip++;
      esp[to] = esp[lhs] - esp[rhs];
      break;
    case Opc::mul:
      to = *eip++;
      lhs = *eip++;
      rhs = *eip++;
      esp[to] = esp[lhs] * esp[rhs];
      break;
    case Opc::div:
      to = *eip++;
      lhs = *eip++;
      rhs = *eip++;
      esp[to] = esp[lhs] / esp[rhs];
      break;
    case Opc::la:
      to = *eip++;
      lhs = *eip++;
      esp[to] = lhs;
      break;
	case Opc::deref_assign:
      to = *eip++;
      lhs = *eip++;
      memcpy((char *)esp + esp[to], &esp[lhs], sizeof(int));
      break;
    case Opc::deref:
      to = *eip++;
      lhs = *eip++;
      memcpy(&esp[to], (char *)esp + esp[lhs], sizeof(int));
      break;
    case Opc::br: {
      uint64_t ptrlo = *eip++;
      uint64_t ptrhi = *eip++;
      eip = (int *)(ptrlo | ptrhi << 32);
    } break;
    case Opc::cond_br: {
      int cond = *eip++;
      if (cond) {
        uint64_t ptrlo = *eip++;
        uint64_t ptrhi = *eip++;
        eip = (int *)(ptrlo | ptrhi << 32);
      }
    } break;
    case Opc::lt:
      to = *eip++;
      lhs = *eip++;
      rhs = *eip++;
      esp[to] = esp[lhs] < esp[rhs];
      break;
    case Opc::le:
      to = *eip++;
      lhs = *eip++;
      rhs = *eip++;
      esp[to] = esp[lhs] <= esp[rhs];
      break;
    case Opc::eq:
      to = *eip++;
      lhs = *eip++;
      rhs = *eip++;
      esp[to] = esp[lhs] == esp[rhs];
      break;
    case Opc::ge:
      to = *eip++;
      lhs = *eip++;
      rhs = *eip++;
      esp[to] = esp[lhs] >= esp[rhs];
      break;
    case Opc::gt:
      to = *eip++;
      lhs = *eip++;
      rhs = *eip++;
      esp[to] = esp[lhs] > esp[rhs];
      break;
    case Opc::ne:
      to = *eip++;
      lhs = *eip++;
      rhs = *eip++;
      esp[to] = esp[lhs] != esp[rhs];
      break;
    case Opc::ret: {
      int retval = esp[*eip++];
      to = esp[-1];
      esp += esp[0];
      esp[to] = retval;
    } break;
    case Opc::alloca:
      to = *eip++;
      if (esp + to >= &stack[stack.size()]) {
        ptrdiff_t diff = esp - &stack[0];
        assert(diff > 0);
        stack.resize(diff + to + 4);
        esp = &stack[diff];
      }
      break;
    case Opc::read: abort(); break;
    case Opc::write:
      to = *eip++;
      std::cout << esp[to] << "\n";
      break;
    case Opc::quit: std::cout << "quit the program\n"; return;
    }
  }
}

class Compiler {
  int stack_size;
  int args_size;

  std::map<std::string, int> vars;
  std::map<std::string, int *> funcs;
  std::map<std::string, int *> labels;

  static std::map<Stmt,
                  bool (Compiler::*)(Program *, const std::string &)>
      handlers;

  static int m1[];
  static int m2[];
  static int m3[];
  static int m4[];

  int primary_exp(Program *prog, const std::string &tok) {
	std::cout << "tok is " << tok << ", stack_size is " << stack_size << "\n";
    if (tok[0] == '#') {
      auto tmp = newTemp();
      prog->gen_inst(Opc::li, tmp, std::stoi(&tok[1]));
      return tmp;
    } else if (tok[0] == '&') {
	  auto var = getVar(&tok[1]);
      auto tmp = newTemp();
      prog->gen_inst(Opc::la, tmp, var);
      return tmp;
    } else if (tok[0] == '*') {
	  auto var = getVar(&tok[1]);
      auto tmp = newTemp();
      prog->gen_inst(Opc::deref, tmp, var);
      return tmp;
    } else {
      return getVar(tok);
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
    args_size = -3;
    labels.clear();
  }

  int *getFunction(const std::string &fname) { return funcs[fname]; }

  int getVar(const std::string &name, unsigned size = 1) {
    auto it = vars.find(name);
    if (it == vars.end()) {
	  std::cout << "var is " << name << "\n";
	  std::cout << "stack_size is " << stack_size << "\n";
      std::tie(it, std::ignore) =
          vars.insert(std::pair<std::string, int>{
              name, stack_size += size});
	  std::cout << "stack_size is " << stack_size << "\n";
	  std::cout << "it->second is " << it->second << "\n";
	}
#if 0
    std::cout << "==GET(" << name << ", " << size
              << ") = " << it->second << "\n"
              << ", stack.size = " << stack_size << "\n";
#endif
    return it->second;
  }

  int newTemp() {
    stack_size ++;
    return stack_size - 1;
  }

  int newArg() {
    stack_size ++;
    return stack_size - 1;
  }

  int getParam(const std::string &name) {
    auto it = vars.find(name);
    if (it == vars.end())
      std::tie(it, std::ignore) =
          vars.insert(std::pair<std::string, int>{name, args_size--});
#if 0
    std::cout << "==GET(" << name << ", " << 4 << ") = " << it->second
              << "\n";
#endif
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
  auto it =
      std::sregex_token_iterator(line.begin(), line.end(), pat, m1);

  if (it == std::sregex_token_iterator()) return false;

  auto label = *it++;
  labels[label] = prog->get_textptr();
  return true;
}

/* stmt func */
bool Compiler::handle_func(Program *prog, const std::string &line) {
  static std::regex pat(R"(^\s*FUNCTION\s+(\w+)\s*:\s*$)");
  auto it =
      std::sregex_token_iterator(line.begin(), line.end(), pat, m1);
  if (it == std::sregex_token_iterator()) return false;

  auto f = *it++;

  prog->gen_inst(Opc::abort); // last function should manually ret
  funcs[f] = prog->get_textptr();

  if (prog->curf) {
    prog->curf[1] = stack_size + 1;
    clear_env();
  }
  prog->curf = prog->gen_inst(Opc::alloca, 0);
  return true;
}

bool Compiler::handle_assign(Program *prog, const std::string &line) {
  std::regex pat(R"(^\s*(\w+)\s*:=\s*(#[\+\-]?\d+|[&\*]?\w+)\s*$)");
  auto it =
      std::sregex_token_iterator(line.begin(), line.end(), pat, m2);
  if (it == std::sregex_token_iterator()) return false;

  auto x = getVar(*it++);
  auto y = primary_exp(prog, *it++);
  prog->gen_inst(Opc::mov, x, y);
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

  auto it =
      std::sregex_token_iterator(line.begin(), line.end(), pat, m4);
  if (it == std::sregex_token_iterator()) return false;

  auto x = getVar(*it++);
  auto y = primary_exp(prog, *it++);
  auto op = *it++;
  auto z = primary_exp(prog, *it++);

  prog->gen_inst(m[op], x, y, z);
  return true;
}

bool Compiler::handle_takeaddr(Program *prog,
                               const std::string &line) {
  static std::regex pat(R"(^\s*(\w+)\s*:=\s*&\s*(\w+)\s*$)");
  /* stmt assign */
  auto it =
      std::sregex_token_iterator(line.begin(), line.end(), pat, m2);
  if (it == std::sregex_token_iterator()) return false;
  auto x = *it++;
  auto y = *it++;
  prog->gen_inst(Opc::la, getVar(x), getVar(y));
  return true;
}

bool Compiler::handle_deref(Program *prog, const std::string &line) {
  static std::regex pat(R"(^\s*(\w+)\s*:=\s*\*\s*(\w+)\s*$)");
  /* stmt assign */
  auto it =
      std::sregex_token_iterator(line.begin(), line.end(), pat, m3);
  if (it == std::sregex_token_iterator()) return false;
  auto x = *it++;
  auto y = *it++;
  prog->gen_inst(Opc::deref, getVar(x), getVar(y));
  return true;
}

bool Compiler::handle_deref_assign(Program *prog,
                                   const std::string &line) {
  static std::regex pat(
      R"(^\s*\*(\w+)\s+:=\s+(#[\+\-]?\d+|[&\*]?\w+)\s*$)");
  auto it =
      std::sregex_token_iterator(line.begin(), line.end(), pat, m3);
  if (it == std::sregex_token_iterator()) return false;

  auto x = getVar(*it++);
  auto y = primary_exp(prog, *it++);
  prog->gen_inst(Opc::deref_assign, x, y);
  return true;
}

bool Compiler::handle_goto_(Program *prog, const std::string &line) {
  static std::regex pat(R"(^\s*GOTO\s+(\w+)\s*$)");
  auto it =
      std::sregex_token_iterator(line.begin(), line.end(), pat, m3);
  if (it == std::sregex_token_iterator()) { return false; }

  auto label = labels[*it++];
  assert(label);
  prog->gen_inst(Opc::br, ptr_lo(label), ptr_hi(label));
  return true;
}

bool Compiler::handle_branch(Program *prog, const std::string &line) {
  static std::regex pat(
      R"(^\s*IF\s+(#[\+\-]?\d+|[&\*]?\w+)\s*(<|>|<=|>=|==|!=)\s*(#[\+\-]?\d+|[&\*]?\w+)\s+GOTO\s+(\w+)\s*$)");
  auto it =
      std::sregex_token_iterator(line.begin(), line.end(), pat, m4);
  if (it == std::sregex_token_iterator()) return false;

  auto x = primary_exp(prog, *it++);
  auto opc = *it++;
  auto y = primary_exp(prog, *it++);

  auto label = *it++;

  static std::map<std::string, Opc> s2op{
      {"<", Opc::lt},  {">", Opc::gt},  {"<=", Opc::le},
      {">=", Opc::ge}, {"==", Opc::eq}, {"!=", Opc::ne},
  };

  auto tmp = newTemp();
  prog->gen_inst(s2op[opc], tmp, x, y);
  prog->gen_cond_br(tmp, labels[label]);
  return true;
}

bool Compiler::handle_ret(Program *prog, const std::string &line) {
  static std::regex pat(R"(^\s*RETURN\s+(#[\+\-]?\d+|[&\*]?\w+)\s*$)");
  auto it =
      std::sregex_token_iterator(line.begin(), line.end(), pat, m4);
  if (it == std::sregex_token_iterator()) return false;

  auto x = *it++;
  prog->gen_inst(Opc::ret, primary_exp(prog, x));
  return true;
}

bool Compiler::handle_dec(Program *prog, const std::string &line) {
  static std::regex pat(R"(^\s*DEC\s+(\w+)\s+(\d+)\s*$)");
  auto it =
      std::sregex_token_iterator(line.begin(), line.end(), pat, m2);
  if (it == std::sregex_token_iterator()) return false;

  auto x = *it++;
  auto size = stoi(*it++);
  getVar(x, (size + 3) / 4);
  return true;
}

bool Compiler::handle_arg(Program *prog, const std::string &line) {
  static std::regex pat(R"(^\s*ARG\s+(#[\+\-]?\d+|[&\*]?\w+)\s*$)");
  auto it =
      std::sregex_token_iterator(line.begin(), line.end(), pat, m2);
  if (it == std::sregex_token_iterator()) return false;

  auto x = primary_exp(prog, *it++);
  auto arg = newArg();
  prog->gen_inst(Opc::mov, arg, x);
  return true;
}

bool Compiler::handle_call(Program *prog, const std::string &line) {
  static std::regex pat(R"(^\s*(\w+)\s*:=\s*CALL\s+(\w+)\s*$)");
  auto it =
      std::sregex_token_iterator(line.begin(), line.end(), pat, m2);
  if (it == std::sregex_token_iterator()) return false;
  auto to = *it++;
  auto f = *it++;

  auto lo = newArg();
  auto hi = newArg();
  auto ret = newArg();
  auto inc = newArg();

  prog->gen_inst(Opc::li, lo, ptr_lo(prog->curf));
  prog->gen_inst(Opc::li, hi, ptr_hi(prog->curf));
  prog->gen_inst(Opc::la, ret, getVar(to));
  prog->gen_inst(Opc::li, inc, inc);
  prog->gen_inst(Opc::inc_esp, inc);

  prog->gen_br(funcs[f]);
  return true;
}

bool Compiler::handle_param(Program *prog, const std::string &line) {
  static std::regex pat(R"(^\s*PARAM\s+(\w+)\s*$)");
  auto it =
      std::sregex_token_iterator(line.begin(), line.end(), pat, m1);
  if (it == std::sregex_token_iterator()) return false;
  getParam(*it++);
  return true;
}

bool Compiler::handle_read(Program *prog, const std::string &line) {
  static std::regex pat(R"(^\s*READ\s+(\w+)\s*$)");
  auto it =
      std::sregex_token_iterator(line.begin(), line.end(), pat, m1);
  if (it == std::sregex_token_iterator()) return false;

  auto x = *it++;
  prog->gen_inst(Opc::read, getVar(x));
  return true;
}

bool Compiler::handle_write(Program *prog, const std::string &line) {
  static std::regex pat(R"(^\s*WRITE\s+(#[\+\-]?\d+|[&\*]?\w+)\s*$)");
  auto it =
      std::sregex_token_iterator(line.begin(), line.end(), pat, m1);
  if (it == std::sregex_token_iterator()) return false;

  auto x = primary_exp(prog, *it++);
  prog->gen_inst(Opc::write, x);
  return true;
}

std::unique_ptr<Program> Compiler::compile(std::istream &is) {
  auto prog = std::make_unique<Program>();
  unsigned lineno = 0;
  while ((is.peek(), is.good())) {
    bool suc = false;
    lineno++;
    std::string line;
    std::getline(is, line);

	std::cout << line << "\n";
    for (int i = (int)Stmt::begin; i < (int)Stmt::end; i++)
      if ((this->*handlers[(Stmt)i])(&*prog, line)) {
        suc = true;
        break;
      }

    if (suc) continue;

    if (line.find_first_not_of("\r\n\v\f\t ") == line.npos) {
      continue;
    }

    std::cerr << "syntax error at line " << lineno << ": '" << line
              << "'\n";
  }
  return prog;
}

int main(int argc, const char *argv[]) {
  if (argc <= 1) {
    std::cerr << "usage: irsim [*.ir]\n";
    return 0;
  }

  std::clog << "load " << argv[1] << "\n";
  std::ifstream ifs(argv[1]);

  Compiler compiler;
  auto prog = compiler.compile(ifs);
  // prog->run(compiler.getFunction("main"));
  return 0;
}
