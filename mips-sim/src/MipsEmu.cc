#include <assert.h>
#include <elf.h>
#include <fcntl.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "MipsEmu.h"

#define HALT_PC 0xBFC00380
#define eprintf(...) fprintf(stderr, ##__VA_ARGS__)
// #define DEBUG

#define Assert(cond, fmt, ...)                      \
  do {                                              \
    if (!(cond)) {                                  \
      eprintf("Assertion `%s' failed: \e[1;31m" fmt \
              "\e[0m\n",                            \
          #cond, ##__VA_ARGS__);                    \
      abort();                                      \
    }                                               \
  } while (0)


typedef struct {
  union {
    uint32_t val;
    // R-type
    struct {
      uint32_t func : 6;
      uint32_t shamt : 5;
      uint32_t rd : 5;
      uint32_t rt : 5;
      uint32_t rs : 5;
      uint32_t op : 6;
    };

    uint32_t uimm : 16; // I-type

    int32_t simm : 16; // SI-type

    uint32_t addr : 26; // J-type
    uint32_t sel : 3;   // MFC0
  };
} Inst; // Instruction

/* clang-format off */
const char *regs[32] = {
  "0 ", "at", "v0", "v1",
  "a0", "a1", "a2", "a3",
  "t0", "t1", "t2", "t3",
  "t4", "t5", "t6", "t7",
  "s0", "s1", "s2", "s3",
  "s4", "s5", "s6", "s7",
  "t8", "t9", "k0", "k1",
  "gp", "sp", "fp", "ra"
};

enum {
  R_zero, R_at, R_v0, R_v1,
  R_a0, R_a1, R_a2, R_a3,
  R_t0, R_t1, R_t2, R_t3,
  R_t4, R_t5, R_t6, R_t7,
  R_s0, R_s1, R_s2, R_s3,
  R_s4, R_s5, R_s6, R_s7,
  R_t8, R_t9, R_k0, R_k1,
  R_gp, R_sp, R_fp, R_ra,
};
/* clang-format on */

static int sh(const char *fmt, ...) {
  static char buffer[1024];
  va_list ap;
  va_start(ap, fmt);
  vsnprintf(buffer, sizeof(buffer) - 1, fmt, ap);
  va_end(ap);

  return system(buffer);
}

static size_t get_file_size(const char *img_file) {
  struct stat file_status;
  lstat(img_file, &file_status);
  if (S_ISLNK(file_status.st_mode)) {
    char *buf = (char *)malloc(file_status.st_size + 1);
    size_t size =
        readlink(img_file, buf, file_status.st_size);
    (void)size;
    buf[file_status.st_size] = 0;
    size = get_file_size(buf);
    free(buf);
    return size;
  } else {
    return file_status.st_size;
  }
}

static void *read_file(const char *filename) {
  size_t size = get_file_size(filename);
  int fd = open(filename, O_RDONLY);
  if (fd == -1) return NULL;

  // malloc buf which should be freed by caller
  void *buf = malloc(size);
  size_t len = 0;
  while (len < size) { len += read(fd, buf, size - len); }
  close(fd);
  return buf;
}

uint32_t MipsEmu::load_elf(const std::string &elf_file) {
  Assert(elf_file.size(), "Need an elf file");

  /* set symbol file to elf_file */
  const uint32_t elf_magic = 0x464c457f;

  size_t size = get_file_size(elf_file.c_str());
  void *buf = read_file(elf_file.c_str());
  Assert(buf, "file '%s' cannot be opened for read\n",
      elf_file.c_str());

  Elf32_Ehdr *elf = (Elf32_Ehdr *)buf;

  uint32_t elf_entry = elf->e_entry;

  uint32_t *p_magic = (uint32_t *)buf;
  Assert(*p_magic == elf_magic, "wrong file format");
  Assert(elf->e_ident[EI_CLASS] == ELFCLASS32,
      "not a 32-bit elf file");
  Assert(elf->e_ident[EI_DATA] == ELFDATA2LSB,
      "not a little endian elf file");
  Assert(elf->e_machine == EM_MIPS, "not a mips elf file");

  for (int i = 0; i < elf->e_phnum; i++) {
    size_t phdr_off = i * elf->e_phentsize + elf->e_phoff;
    Elf32_Phdr *ph = (Elf32_Phdr *)((char *)buf + phdr_off);
    Assert(phdr_off < size, "ELF32_Phdr out of file");
    Assert(ph->p_offset < size, "ELF32_Ph out of file");
    if (ph->p_type != PT_LOAD) { continue; }

    void *ptr = vaddr_map(ph->p_vaddr, ph->p_memsz);
    if (ptr) {
      memcpy(ptr, (char *)buf + ph->p_offset, ph->p_filesz);
      memset((char *)ptr + ph->p_filesz, 0,
          ph->p_memsz - ph->p_filesz);
    }
  }

  free(buf);
  return elf_entry;
}

void MipsEmu::print_registers(uint32_t instr) {
  eprintf("$pc:    0x%08x", cpu.pc);
  eprintf("   ");
  eprintf("$instr: 0x%08x", instr);
  eprintf("\n");
  eprintf("$hi:    0x%08x", cpu.hi);
  eprintf("   ");
  eprintf("$lo:    0x%08x", cpu.lo);
  eprintf("\n");

  for (int i = 0; i < 32; i++) {
    eprintf("$%s:0x%08x%c", regs[i], cpu.gpr[i],
        (i + 1) % 4 == 0 ? '\n' : ' ');
  }
}

const char *find_program(const char *name) {
  static char buffer[BUFSIZ];
  const char *s_beg = getenv("PATH");
  const char *s_end = s_beg;
  if (!s_beg) {
    s_beg = s_end = "/bin:/usr/bin:/usr/local/bin";
  }

  while (*s_end) {
    while (*s_end && *s_end != ':') s_end++;

    int n = s_end - s_beg;
    memcpy(buffer, s_beg, n);
    sprintf(buffer + n, "/%s", name);
    if (access(buffer, F_OK | X_OK) == 0) return buffer;

    if (!*s_end) break;
    s_end++;
    s_beg = s_end;
  }

  return NULL;
}

const char *check_and_find(const char *name) {
  const char *path = find_program(name);
  if (!path) {
    eprintf("%s is required !\n", name);
    eprintf(
        "install it by `sudo apt-get install "
        "gcc-mips-linux-gnu`\n");
    exit(0);
  }
  return path;
}

std::string MipsEmu::compile(const std::string &source) {
  const char *srcfile = source.c_str();
  if (access(srcfile, F_OK | R_OK) != 0) {
    eprintf("unable to open '%s' for reading\n", srcfile);
    return 0;
  }

  const char *as = check_and_find("mips-linux-gnu-as");
  int code = sh("%s %s -EL -o %s.o", as, srcfile, srcfile);
  if (code != 0) {
    eprintf("failed to compile %s\n", srcfile);
    return 0;
  }

  const char *ld = check_and_find("mips-linux-gnu-ld");
  code =
      sh("%s -entry main -Ttext=0x1000 %s.o -EL -o %s.elf",
          ld, srcfile, srcfile);
  if (code != 0) {
    eprintf("failed to link %s\n", srcfile);
    return 0;
  }

  static char buffer[1024];
  snprintf(buffer, 1023, "%s.elf", srcfile);
  return buffer;
}

int MipsEmu::run(const std::string &elf_file) {
  uint32_t entry = load_elf(elf_file);

  cpu.pc = entry;
  cpu.gpr[R_sp] = DDR_SIZE - 4; // set sp
  cpu.gpr[R_ra] = HALT_PC;

  while (true) {
    if ((cpu.pc & 0x3) != 0) {
      exception = MipsEmuEx::IF;
      return -1;
    }

    Inst inst = {.val = vaddr_read(cpu.pc, 4)};

#ifdef DEBUG
    print_registers(inst.val);
#endif

#include "instr.h"

    counter++;
    if (counter > limit) {
      exception = MipsEmuEx::TIMEOUT;
      return -1;
    }

    if (cpu.pc == HALT_PC) break;
  }
  return 0;
}
