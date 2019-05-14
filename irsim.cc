#include <fstream>
#include <map>
#include <memory>
#include <regex>
#include <string>
#include <vector>

enum {
  stmt_begin,
  stmt_label = stmt_begin,
  stmt_func,
  stmt_assign,
  stmt_add,
  stmt_sub,
  stmt_mul,
  stmt_div,
  stmt_takeaddr,
  stmt_deref,
  stmt_deref_assign,
  stmt_goto,
  stmt_branch,
  stmt_return,
  stmt_dec,
  stmt_arg,
  stmt_call,
  stmt_param,
  stmt_read,
  stmt_write,
  stmt_end,
};

enum {
  op_mov,
  op_add,
  op_sub,
  op_mul,
  op_div,
  op_takeaddr,
  op_deref,
  op_br,
  op_lt,
  op_le,
  op_eq,
  op_ge,
  op_gt,
  op_ne,
  op_push,
  op_pop,
  op_write,
  op_alloc,
  op_quit,
};

using VarType = intptr_t;
using VarMap = std::map<std::string, std::unique_ptr<VarType>>;
using TransitionBlock = std::array<uintptr_t, 1024>;

class Program {
  std::vector<std::unique_ptr<TransitionBlock>> codes;
  TransitionBlock *curblk;
  uintptr_t *ptr;

public:
  uintptr_t *get_cur_ptr() const { return ptr; }
  void check_eof(unsigned N) {
    if (ptr + N + 2 >= curblk) {
      curblk = new TransitionBlock;
      codes.push_back(std::unique_ptr<TransitionBlock>(curblk));
    }
    *ptr++ = op_br;
    *ptr++ = &(curblk->at(0));
  }

  void gen1(unsigned op, VarType *ptr) {
    check_eof();
    *ptr++ = op;
    *ptr++ = (uintptr_t)ptr;
  }

  void gen2(unsigned op, VarType *ptr1, VarType *ptr2) {
    check_eof();
    *ptr++ = op;
    *ptr++ = (uintptr_t)ptr1;
    *ptr++ = (uintptr_t)ptr2;
  }

  void gen3(unsigned op, VarType *ptr1, VarType *ptr2,
            VarType *ptr3) {
    check_eof();
    *ptr++ = op;
    *ptr++ = (uintptr_t)ptr1;
    *ptr++ = (uintptr_t)ptr2;
    *ptr++ = (uintptr_t)ptr3;
  }
};

class Context {
  VarMap vars;
  std::map<std::string, uintptr_t> funcs;
  std::map<std::string, uintptr_t> labels;
  std::vector<std::unique_ptr<VarType>> pool;

  Program prog;

  static std::map<unsigned, std::regex> patterns;

public:
  VarType *getVal(const std::string &name) {
    auto &holder = vars[name];
    if (!holder) holder.reset(new VarType); // let it random
    return &*holder;
  }

  VarType *getParam(const std::string &name) {}

  VarType *newVal() {
    pool.push_back(std::make_unique<VarType>());
    return &pool.back();
  }

  void compile(std::istream &is);

  void run();
};

std::map<unsigned, const char *> Context::patterns{
    {stmt_label, std::regex(R"(^\s*LABEL\s+(\w+)\s*:\s*$)",
                            std::regex_constants::icase)},
    {stmt_func, std::regex(R"(^\s*FUNCTION\s+f\s*:\s*$)",
                           std::regex_constants::icase)},
    {stmt_assign, std::regex(R"(^\s*(\w+)\s*:=\s*(\w+)\s*$)",
                             std::regex_constants::icase)},
    {stmt_add, std::regex(R"(^\s*(\w+)\s*:=\s*(\w+)\s++\s+(\w+)\s*$)",
                          std::regex_constants::icase)},
    {stmt_sub, std::regex(R"(^\s*(\w+)\s*:=\s*(\w+)\s+-\s+(\w+)\s*$)",
                          std::regex_constants::icase)},
    {stmt_mul, std::regex(R"(^\s*(\w+)\s*:=\s*(\w+)\s+*\s+(\w+)\s*$)",
                          std::regex_constants::icase)},
    {stmt_div, std::regex(R"(^\s*(\w+)\s*:=\s*(\w+)\s+/\s+(\w+)\s*$)",
                          std::regex_constants::icase)},
    {stmt_takeaddr, std::regex(R"(^\s*(\w+)\s*:=\s*&(\w+)\s*$)",
                               std::regex_constants::icase)},
    {stmt_deref, std::regex(R"(^\s*(\w+)\s*:=\s*\*(\w+)\s*$)",
                            std::regex_constants::icase)},
    {stmt_deref_assign, std::regex(R"(^\s**(\w+)\s+:=\s+(\w+)\s*$)",
                                   std::regex_constants::icase)},
    {stmt_goto, std::regex(R"(^\s*GOTO\s+(\w+)\s*$)",
                           std::regex_constants::icase)},
    {stmt_branch,
     std::regex(
         R"(^\s*IF\s+(\w+)\s*(<|>|<=|>=|==|!=)\s*(\w+)\s+GOTO\s+(\w+)\s*$)",
         std::regex_constants::icase)},
    {stmt_return, std::regex(R"(^\s*RETURN\s+(\w+)\s*$)",
                             std::regex_constants::icase)},
    {stmt_dec, std::regex(R"(^\s*DEC\s+(\w+)\s+[si(\w+)e]\s*$)",
                          std::regex_constants::icase)},
    {stmt_arg, std::regex(R"(^\s*ARG\s+(\w+)\s*$)",
                          std::regex_constants::icase)},
    {stmt_call, std::regex(R"(^\s*(\w+)\s*:=\s*CALL\s+(\w+)\s*$)",
                           std::regex_constants::icase)},
    {stmt_param, std::regex(R"(^\s*PARAM\s+(\w+)\s*$)",
                            std::regex_constants::icase)},
    {stmt_read, std::regex(R"(^\s*READ\s+(\w+)\s*$)",
                           std::regex_constants::icase)},
    {stmt_write, std::regex(R"(^\s*WRITE\s+(\w+)\s*$)",
                            std::regex_constants::icase)},
};

void Context::compile(std::istream &is) {
  while ((ifs.peek(), ifs.good())) {
    std::string line;
    std::getline(ifs, line);

    int m0[] = {0};
    int m1[] = {0, 1};
    int m2[] = {0, 1, 2};
    auto end = std::sregex_token_iterator();

    /* stmt label */
    auto it = std::sregex_token_iterator(line.begin(), line.end(),
                                         patterns[stmt_label], m0);
    if (it != end) {
      auto label = *it++;
      labels[label] = prog.get_cur_ptr();
      continue;
    }

    /* stmt add */
    it = std::sregex_token_iterator(line.begin(), line.end(),
                                    patterns[stmt_add], m0);
    if (it != end) {
      auto x = *it++;
      auto y = *it++;
      auto z = *it++;
      gen3(op_add, getVar(x), getVar(y), getVar(z));
      continue;
    }

    /* stmt push */
    it = std::sregex_token_iterator(line.begin(), line.end(),
                                    patterns[stmt_arg], m0);
    if (it != end) {
      auto v = *it++;
      gen3(op_push, getVar(v));
      continue;
    }
  }
}

int main() {
  std::ifstream ifs("a.ir");

  return 0;
}
