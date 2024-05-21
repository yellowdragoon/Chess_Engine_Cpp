#include <stdlib.h>
#include <iostream>
#include <cstdint>

typedef std::uint64_t U64;
#define get_bit(bitboard, square) (bitboard & (1ULL << square))
#define set_bit(bitboard, square) (bitboard |= (1ULL << square))
#define remove_bit(bitboard, square) (bitboard &= ~(1ULL << square))

// Little endian file-rank (LEFR) mapping implies following C++ enumeration:
// https://www.chessprogramming.org/Square_Mapping_Considerations

enum enumSquare {
  a1, a2, a3, a4, a5, a6, a7, a8,
  b1, b2, b3, b4, b5, b6, b7, b8,
  c1, c2, c3, c4, c5, c6, c7, c8,
  d1, d2, d3, d4, d5, d6, d7, d8,
  e1, e2, e3, e4, e5, e6, e7, e8,
  f1, f2, f3, f4, f5, f6, f7, f8,
  g1, g2, g3, g4, g5, g6, g7, g8,
  h1, h2, h3, h4, h5, h6, h7, h8
};

void print_bitboard(U64 board) {
  for (int rank = 7; rank >= 0; rank--)
  {
    std::cout << rank + 1 << " ";
    for (int file = 0; file < 8; file++)
    {
      std::cout << (get_bit(board, (8 * file + rank)) & board ? "| X " : "|   "); 
    }   
    std::cout << "|\n";
  }
  std::cout << "    a   b   c   d   e   f   g   h";
}

int main() {
  U64 sample {0};
  set_bit(sample, a2);
  sample |= (1ULL << g6);
  sample |= (1ULL << e3);
  remove_bit(sample, g6);
  remove_bit(sample, g6);
  print_bitboard(sample);
  return 0;
}
