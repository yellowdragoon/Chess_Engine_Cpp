CXX      ?= clang++
CXXFLAGS ?= -O3 -DNDEBUG -std=c++17 -march=native
WARNINGS  = -Wall -Wno-writable-strings -Wno-missing-braces -Wno-bitwise-op-parentheses -Wno-unused-const-variable -Wno-unused-function -Wno-vla-cxx-extension
EXE      ?= engine

SRCS = board.cpp
HDRS = board.h magic.h hce_params.h

$(EXE): $(SRCS) $(HDRS)
	$(CXX) $(CXXFLAGS) $(WARNINGS) $(SRCS) -o $@

.PHONY: test clean play

# Perft regression: node counts for standard test positions
test: $(EXE)
	./tests/perft.sh ./$(EXE)

# Play against the engine in the browser
play: $(EXE)
	cd tools && uv run play.py ../$(EXE)

clean:
	rm -f $(EXE)
