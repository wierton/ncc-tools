OBJ_DIR := build
SRCS := src/cpu.c
OBJS := $(patsubst %.c,$(OBJ_DIR)/%.o,$(SRCS))
BIN  := $(OBJ_DIR)/mips-sim
CXX  := gcc

.DEFAULT_GOAL := $(BIN)
CXXFLAGS := -Iinclude -std=c11 -O0 -ggdb3 -Wall -MMD
-include $(OBJS:.o=.d)

$(BIN): $(OBJS)
	@echo "+ LNK $@"
	@$(CXX) $^ -o $@

$(OBJ_DIR)/%.o: %.c
	@echo "+ CC $<"
	@mkdir -p $(@D)
	@$(CXX) -c $(CXXFLAGS) $< -o $@

run: $(BIN)
	@$(BIN) test/test.S

gdb: $(BIN)
	gdb --args $(BIN) test/add.ir

clean:
	rm -rf $(OBJ_DIR)
