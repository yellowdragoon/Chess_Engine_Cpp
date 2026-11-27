CXX      ?= clang++
CXXFLAGS ?= -O3 -DNDEBUG -std=c++17 -march=native
WARNINGS  = -Wall -Wno-writable-strings -Wno-missing-braces -Wno-bitwise-op-parentheses -Wno-unused-const-variable -Wno-unused-function -Wno-vla-cxx-extension
EXE      ?= engine

SRCS = board.cpp
HDRS = board.h magic.h hce_params.h

$(EXE): $(SRCS) $(HDRS)
	$(CXX) $(CXXFLAGS) $(WARNINGS) $(SRCS) -o $@

.PHONY: test clean play test-play versions ladder

# Perft regression: node counts for standard test positions
test: $(EXE)
	./tests/perft.sh ./$(EXE)

# Play against the engine in the browser (the current build plus everything in bin/)
play: $(EXE) versions
	cd tools && uv run play.py ../$(EXE) ../bin

# Build every version in versions.txt into bin/ (skips ones already built)
versions:
	cd tools && uv run versions.py build

# Play each version against the previous one; writes docs/engine-history.md
ladder:
	cd tools && uv run ladder.py

# Tests for the tools: engine UCI moves, version builds, play server and browser UI (Playwright).
# The first run downloads headless Chromium (~100 MB) into Playwright's cache.
test-play: $(EXE)
	cd tools && uv run --group test playwright install chromium-headless-shell && ENGINE=../$(EXE) uv run --group test pytest tests -q

clean:
	rm -f $(EXE)
