#include <stdlib.h>
#include <iostream>
#include <random>
#include <cstdint>
#include <string.h>

typedef std::uint64_t U64;
#define get_bit(bitboard, square) (bitboard & (1ULL << square))
#define set_bit(bitboard, square) (bitboard |= (1ULL << square))
#define remove_bit(bitboard, square) (bitboard &= ~(1ULL << square))

// GCC compiler builtin to optimally count number of bits in a ULL
#define count_bits(bitboard) __builtin_popcountll(bitboard)
// GCC compiler builtin to optimally get index of LS1B in a ULL
#define ls1b_index(bitboard) (__builtin_ffsll(bitboard) - 1);

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

// Precalculated attack tables
U64 pawn_attacks_table[2][64]; 
U64 knight_attacks_table[64];
U64 king_attacks_table[64];

// Precalculated bit counts for bishop/rook masks
const int bishop_mask_bit_counts[64] {
  6, 5, 5, 5, 5, 5, 5, 6, 
  5, 5, 5, 5, 5, 5, 5, 5, 
  5, 5, 7, 7, 7, 7, 5, 5, 
  5, 5, 7, 9, 9, 7, 5, 5, 
  5, 5, 7, 9, 9, 7, 5, 5, 
  5, 5, 7, 7, 7, 7, 5, 5, 
  5, 5, 5, 5, 5, 5, 5, 5, 
  6, 5, 5, 5, 5, 5, 5, 6
};

const int rook_mask_bit_counts[64] {
  12, 11, 11, 11, 11, 11, 11, 12, 
  11, 10, 10, 10, 10, 10, 10, 11, 
  11, 10, 10, 10, 10, 10, 10, 11, 
  11, 10, 10, 10, 10, 10, 10, 11, 
  11, 10, 10, 10, 10, 10, 10, 11, 
  11, 10, 10, 10, 10, 10, 10, 11, 
  11, 10, 10, 10, 10, 10, 10, 11, 
  12, 11, 11, 11, 11, 11, 11, 12
};

// Random number gen using Mersenne Twisters: https://en.wikipedia.org/wiki/Mersenne_Twister
const int rand_seed {424242};
std::mt19937 generator(rand_seed);
std::uniform_int_distribution<uint32_t> distribution(0, 0xFFFFFFFF);

unsigned int generate_random_number_U32() { return distribution(generator); }

// Random number gen for good candidate magic numbers: https://www.chessprogramming.org/Looking_for_Magics 
// Based on Tord Romstad's proposal to find magics
U64 generate_random_number_U64() {
  U64 u1, u2, u3, u4;

  // Slice top 16 bits of each random U32, used to randomise the U64
  u1 = (U64)(generate_random_number_U32()) & 0xFFFF;
  u2 = (U64)(generate_random_number_U32()) & 0xFFFF;
  u3 = (U64)(generate_random_number_U32()) & 0xFFFF; 
  u4 = (U64)(generate_random_number_U32()) & 0xFFFF;

  return u1 | (u2 << 16) | (u3 << 32) | (u4 << 48);
}

// We want candidates to have low number of nonzero bits
U64 generate_candidate_magic() {
  return generate_random_number_U64() & generate_random_number_U64() & generate_random_number_U64();
}

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

// Generating attack mask for bishops
U64 bishop_attack_mask(int square){
  U64 mask {0ULL};
  U64 bishop_bitboard {0ULL};
  set_bit(bishop_bitboard, square);

  int init_file { square / 8 };
  int init_rank { square % 8 };

  int f, r;

  // trace rays in 4 diagonal directions (up to edge)
  for (f = init_file + 1, r = init_rank + 1; f <= 6 && r <= 6; f++, r++) mask |= (1ULL << (f * 8 + r));
  for (f = init_file - 1, r = init_rank + 1; f >= 1 && r <= 6; f--, r++) mask |= (1ULL << (f * 8 + r));
  for (f = init_file + 1, r = init_rank - 1; f <= 6 && r >= 1; f++, r--) mask |= (1ULL << (f * 8 + r));
  for (f = init_file - 1, r = init_rank - 1; f >= 1 && r >= 1; f--, r--) mask |= (1ULL << (f * 8 + r));
  
  return mask;
}

// Generating attacks for bishops (slow) to use with magic bitboards later
// TODO: refactor offsets to arrays for less repetition
U64 generate_bishop_attacks_slow(int square, U64 blockers){
  U64 mask {0ULL};

  int init_file { square / 8 };
  int init_rank { square % 8 };

  int f, r;

  // trace rays in 4 diagonal directions (including edge)
  for (f = init_file + 1, r = init_rank + 1; f <= 7 && r <= 7; f++, r++) {
    int square = f * 8 + r;
    mask |= (1ULL << square);
    if ((blockers >> square) & 1ULL) break;
  }

  for (f = init_file - 1, r = init_rank + 1; f >= 0 && r <= 7; f--, r++) {
    int square = f * 8 + r;
    mask |= (1ULL << square);
    if ((blockers >> square) & 1ULL) break;
  }

  for (f = init_file + 1, r = init_rank - 1; f <= 7 && r >= 0; f++, r--) {
    int square = f * 8 + r;
    mask |= (1ULL << square);
    if ((blockers >> square) & 1ULL) break;
  }

  for (f = init_file - 1, r = init_rank - 1; f >= 0 && r >= 0; f--, r--) {
    int square = f * 8 + r;
    mask |= (1ULL << square);
    if ((blockers >> square) & 1ULL) break;
  }
  
  return mask;
}

// Generating attacks for rooks (slow) to use with magic bitboards later
U64 generate_rook_attacks_slow(int square, U64 blockers){
  U64 mask {0ULL};

  int init_file { square / 8 };
  int init_rank { square % 8 };

  int f, r;
  
  // trace rays in 4 orthogonal directions (including edge)
  for (f = init_file + 1, r = init_rank; f <= 7; f++) {
    int square = f * 8 + r;
    mask |= (1ULL << square);
    if ((blockers >> square) & 1ULL) break;
  }

  for (f = init_file - 1, r = init_rank; f >= 0; f--) {
    int square = f * 8 + r;
    mask |= (1ULL << square);
    if ((blockers >> square) & 1ULL) break;
  }

  for (f = init_file, r = init_rank + 1; r <= 7; r++) {
    int square = f * 8 + r;
    mask |= (1ULL << square);
    if ((blockers >> square) & 1ULL) break;
  }

  for (f = init_file, r = init_rank - 1; r >= 0; r--) {
    int square = f * 8 + r;
    mask |= (1ULL << square);
    if ((blockers >> square) & 1ULL) break;
  }
  
  return mask;
}

// Generating attack mask for rooks
U64 rook_attack_mask(int square){
  U64 mask {0ULL};
  U64 rook_bitboard {0ULL};
  set_bit(rook_bitboard, square);

  int init_file { square / 8 };
  int init_rank { square % 8 };

  int f, r;

  // trace rays in 4 orthogonal directions (up to edge)
  // TODO: replace with set_bit macro
  for (f = init_file + 1, r = init_rank; f <= 6; f++) mask |= (1ULL << (f * 8 + r));
  for (f = init_file - 1, r = init_rank; f >= 1; f--) mask |= (1ULL << (f * 8 + r));
  for (f = init_file, r = init_rank + 1; r <= 6; r++) mask |= (1ULL << (f * 8 + r));
  for (f = init_file, r = init_rank - 1; r >= 1; r--) mask |= (1ULL << (f * 8 + r));
  
  return mask;
}

/*
Passes in attack mask, index of relevant bits in the mask, and number of bits in the mask
For example, index 13 = b1101 so the blockers are at LS1B, LS3B, LS4B of the attackers mask
*/
U64 set_occupancy(int index, U64 attack_mask, int num_bits_in_mask){
  U64 occupancy = 0ULL;

  for (int i = 0; i < num_bits_in_mask; i++)
  {
    // Pop the ls1b index and conditionally set the occupancy bits
    int square = ls1b_index(attack_mask);
    remove_bit(attack_mask, square);

    if (index & (1ULL << i))
      set_bit(occupancy, square);
  }
  
  return occupancy;
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
  for (int i = 0; i < 10; i++)
  {
    print_bitboard(generate_candidate_magic());
  }

  return 0;
}
