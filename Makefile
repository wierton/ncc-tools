OBJ_DIR := build
BIN := $(OBJ_DIR)/irsim

.DEFAULT_GOAL := $(BIN)
CXXFLAGS := -std=c++17 -O2 -ggdb3

$(BIN): irsim.cc
	@mkdir -p $(@D)
	@g++ $(CXXFLAGS) $^ -o $@

run: $(BIN)
	@$(BIN)

gdb: $(BIN)
	gdb $(BIN)
