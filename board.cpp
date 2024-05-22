#include <stdlib.h>
#include <iostream>
#include <cstdint>

typedef std::uint64_t U64;
#define get_bit(bitboard, square) (bitboard & (1ULL << square))
#define set_bit(bitboard, square) (bitboard |= (1ULL << square))
#define remove_bit(bitboard, square) (bitboard &= ~(1ULL << square))
// >> is -, << is +
#define shift_n(bitboard) (bitboard << 1)
#define shift_ne(bitboard) (bitboard << 9)
#define shift_e(bitboard) (bitboard << 8)
#define shift_se(bitboard) (bitboard << 7)
#define shift_s(bitboard) (bitboard >> 1)
#define shift_sw(bitboard) (bitboard >> 9)
#define shift_w(bitboard) (bitboard >> 8)
#define shift_nw(bitboard) (bitboard >> 7)

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

enum side {
  white, black
};

U64 pawn_attacks_table[2][64]; 

// Helper print function for bitboards
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
  std::cout << "    a   b   c   d   e   f   g   h\n";
}

// Generating attack mask for pawns
U64 pawn_attack_mask(int side, int square){
  U64 mask {0ULL};
  U64 pawn_bitboard {0ULL};
  set_bit(pawn_bitboard, square);

  switch (side)
  {
  case white:
    // Shift all bits 1 file forward and left/right
    mask |= (shift_nw(pawn_bitboard) | shift_ne(pawn_bitboard));
    break;
  
  case black:
    // Shift all bits 1 file backwards and left/right 
    mask |= (shift_sw(pawn_bitboard) | shift_se(pawn_bitboard));
    break;
  }

  return mask;
}

U64 init_leaping_pieces_tables() {
  return 0ULL;
}

int main() {
  U64 bitboard {0};
  set_bit(bitboard, a2);
  set_bit(bitboard, b3);
  set_bit(bitboard, d4);
  set_bit(bitboard, f6);
  print_bitboard(bitboard);
  print_bitboard((pawn_attack_mask(white, a2)));
  print_bitboard((pawn_attack_mask(white, g7)));
  print_bitboard((pawn_attack_mask(black, a2)));
  print_bitboard((pawn_attack_mask(black, e4)));
  print_bitboard((pawn_attack_mask(white, a8)));
  print_bitboard((pawn_attack_mask(white, h8)));
  print_bitboard((pawn_attack_mask(black, a1)));
  print_bitboard((pawn_attack_mask(black, h1)));
  return 0;
}
