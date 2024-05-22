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
// Knight directions
#define shift_n_ne(bitboard) (bitboard << 10)
#define shift_e_ne(bitboard) (bitboard << 17)
#define shift_e_se(bitboard) (bitboard << 15)
#define shift_s_se(bitboard) (bitboard << 6)
#define shift_s_sw(bitboard) (bitboard >> 10)
#define shift_w_sw(bitboard) (bitboard >> 17)
#define shift_w_nw(bitboard) (bitboard >> 15)
#define shift_n_nw(bitboard) (bitboard >> 6)

const U64 rank_1 {0x0101010101010101};
const U64 rank_2 {0x0202020202020202};
const U64 rank_12 {0x0303030303030303};
const U64 rank_7 {0x4040404040404040};
const U64 rank_8 {0x8080808080808080};
const U64 rank_78 {0xC0C0C0C0C0C0C0C0};

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
U64 knight_attacks_table[64];
U64 king_attacks_table[64];

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

// Generating attack mask for knights
U64 knight_attack_mask(int square){
  U64 mask {0ULL};
  U64 knight_bitboard {0ULL};
  set_bit(knight_bitboard, square);

  // Shift all bits for the 8 knight directions
  mask |= (
    shift_n_ne(knight_bitboard) & (~rank_12) | 
    shift_e_ne(knight_bitboard) & (~rank_1)  | 
    shift_e_se(knight_bitboard) & (~rank_8)  | 
    shift_s_se(knight_bitboard) & (~rank_78) | 
    shift_s_sw(knight_bitboard) & (~rank_78) | 
    shift_w_sw(knight_bitboard) & (~rank_8)  | 
    shift_w_nw(knight_bitboard) & (~rank_1)  | 
    shift_n_nw(knight_bitboard) & (~rank_12)
  );

  return mask;
}

// Generating attack mask for kings
U64 king_attack_mask(int square){
  U64 mask {0ULL};
  U64 king_bitboard {0ULL};
  set_bit(king_bitboard, square);

  // Shift all bits for the 8 king directions
  mask |= (
    shift_nw(king_bitboard) | shift_n(king_bitboard) | shift_ne(king_bitboard) & (~rank_1) |
    shift_sw(king_bitboard) | shift_s(king_bitboard) | shift_se(king_bitboard) & (~rank_8) |
    shift_w(king_bitboard)  | shift_e(king_bitboard)
  );

  return mask;
}

void init_leaping_pieces_tables() {
  for (int i = 0; i < 64; i++)
  {
    // Pawns
    pawn_attacks_table[white][i] = pawn_attack_mask(white, i);
    pawn_attacks_table[black][i] = pawn_attack_mask(black, i);

    // Knights
    knight_attacks_table[i] = knight_attack_mask(i);

    // Kings
    king_attacks_table[i] = king_attack_mask(i);
  }
}

int main() {
  init_leaping_pieces_tables();
  for (int i = 0; i < 64; i++)
  {
    print_bitboard(king_attacks_table[i]);
  }
  return 0;
}
