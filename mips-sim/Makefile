OBJ_DIR := build
SRCS := $(wildcard src/*.cc)
OBJS := $(patsubst %.cc,$(OBJ_DIR)/%.o,$(SRCS))
BIN  := $(OBJ_DIR)/mips-sim
CXX  := g++

.DEFAULT_GOAL := $(BIN)
CXXFLAGS := -Iinclude -O0 -ggdb3 -Wall -MMD
-include $(OBJS:.o=.d)

$(BIN): $(OBJS)
	@echo "+ LNK $@"
	@$(CXX) $^ -o $@

$(OBJ_DIR)/%.o: %.cc
	@echo "+ CC $<"
	@mkdir -p $(@D)
	@$(CXX) -c $(CXXFLAGS) $< -o $@

run: $(BIN)
	@$(BIN) test/fib.S

clean:
	rm -rf $(OBJ_DIR)
	rm -rf test/*.elf
	rm -rf test/*.o
