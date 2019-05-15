OBJ_DIR := build
BIN := $(OBJ_DIR)/irsim

.DEFAULT_GOAL := $(BIN)
CXXFLAGS := -std=c++17 -O0 -ggdb3

$(BIN): irsim.cc
	@mkdir -p $(@D)
	@g++ $(CXXFLAGS) $^ -o $@

run: $(BIN)
	@$(BIN) test/sgn.ir

gdb: $(BIN)
	gdb --args $(BIN) test/add.ir

clean:
	rm -rf $(OBJ_DIR)
