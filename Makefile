CXX      ?= clang++
CXXFLAGS ?= -O3 -DNDEBUG -std=c++17 -march=native
WARNINGS  = -Wall -Wno-writable-strings -Wno-missing-braces -Wno-bitwise-op-parentheses -Wno-unused-const-variable -Wno-unused-function -Wno-vla-cxx-extension
EXE      ?= engine

# EVAL=nnue (default) embeds the network in NET; EVAL=hce builds the
# hand-crafted evaluation only.
EVAL     ?= nnue
NET      ?= nets/net.bin

SRCS = board.cpp
HDRS = board.h magic.h

ifeq ($(EVAL),nnue)
  DEFINES = -DUSE_NNUE=1
  NET_HDR = nnue_net.h
else
  DEFINES = -DUSE_NNUE=0
  NET_HDR =
endif

$(EXE): $(SRCS) $(HDRS) $(NET_HDR)
	$(CXX) $(CXXFLAGS) $(WARNINGS) $(DEFINES) $(SRCS) -o $@

# The network as a C array, regenerated whenever NET changes
nnue_net.h: $(NET)
	{ echo '// generated from $(NET) by make; do not edit'; \
	  echo 'alignas(64) static const unsigned char nnue_net_data[] = {'; \
	  xxd -i < $(NET); \
	  echo '};'; } > $@

.PHONY: test clean FORCE

# Perft regression: node counts for standard test positions
test: $(EXE)
	./tests/perft.sh ./$(EXE)

clean:
	rm -f $(EXE) nnue_net.h
