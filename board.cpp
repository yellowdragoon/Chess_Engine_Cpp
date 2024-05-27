#include <stdlib.h>
#include <iostream>
#include <random>
#include <cstdint>
#include <string.h>
#include <string>
#include <unordered_map>
#include "board.h"
#include "magic.h"

typedef std::uint64_t U64;
#define get_bit(bitboard, square) ((bitboard) & (1ULL << (square)))
#define set_bit(bitboard, square) ((bitboard) |= (1ULL << (square)))
#define remove_bit(bitboard, square) ((bitboard) &= ~(1ULL << (square)))

// GCC compiler builtin to optimally count number of bits in a ULL
#define count_bits(bitboard) __builtin_popcountll(bitboard)
// GCC compiler builtin to optimally get index of LS1B in a ULL
#define ls1b_index(bitboard) (__builtin_ffsll(bitboard) - 1)

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

char * start_position_fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1 ";
char * test_fen = "r3k2r/1q2pppp/3p4/1ppPn3/n6P/2P4Q/PPBBPPP1/R3KR2 w Qk c6 0 1 ";

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

const char * index_to_square_name[] {
  "a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8",
  "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8",
  "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8",
  "d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8",
  "e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8",
  "f1", "f2", "f3", "f4", "f5", "f6", "f7", "f8",
  "g1", "g2", "g3", "g4", "g5", "g6", "g7", "g8",
  "h1", "h2", "h3", "h4", "h5", "h6", "h7", "h8"
};

enum side {
  white, black
};

enum magic_pieces {
  rook, bishop
};

// capitalized = white, lower = black
enum pieces {
  P, R, N, B, Q, K, p, r, n, b, q, k
};

// C-style string to easily convert from enum -> char
const char pieces_ascii[] {"PRNBQKprnbqk"};

// Mapping to easily convert from char -> enum
std::unordered_map<char, pieces> char_to_pieces = {
  {'P', P}, 
  {'R', R}, 
  {'N', N}, 
  {'B', B},
  {'Q', Q}, 
  {'K', K}, 
  {'p', p}, 
  {'r', r},
  {'n', n}, 
  {'b', b}, 
  {'q', q}, 
  {'k', k}
};

// Precalculated attack tables
U64 pawn_attacks_table[2][64]; 
U64 knight_attacks_table[64];
U64 king_attacks_table[64];

// Plain magic bitboards: https://www.chessprogramming.org/Magic_Bitboards
// Attack masks for bishop/rook
U64 bishop_masks[64];
U64 rook_masks[64];

// Precalculated attack tables for bishop/rook
U64 bishop_attacks_table[64][512];
U64 rook_attacks_table[64][4096];

// Variables to keep track of board state
// 6 bitboards for white pieces/pawns, 6 for black
U64 piece_bitboards[12] = {0ULL};

// bitboards for all occupied squares: white, black, both
U64 occupancy_bitboards[3] = {0ULL};

// Side to move
int side_to_move;

// Legal en passant square
int en_passant_square = -1;

// Castling rights - 4 bit number, (LSB -> MSB): White Kside, White Qside, Black Kside, Black Qside
int castle;

enum castle_flags{w_kside = 1, w_qside = 2, b_kside = 4, b_qside = 8};

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

void reset_board_state() {
  memset(piece_bitboards, 0, sizeof(piece_bitboards));
  memset(occupancy_bitboards, 0, sizeof(occupancy_bitboards));
  side_to_move = white;
  en_passant_square = -1;
  castle = 0;
}

void parse_fen(char * fen) {
  reset_board_state();
  // loop over board squares
  for (int square = 0; square < 64 && *fen && *fen != ' '; )
  {
    int file = square / 8;
    int rank = square % 8;

    int actual_square = ((rank + 1) * 8) - 1 - file;
    // match ascii pieces within FEN string
    if ((*fen >= 'b' && *fen <= 'r') || (*fen >= 'B' && *fen <= 'R'))
    {
        // init piece type
        int piece = char_to_pieces[*fen];
        
        // set piece on corresponding bitboard
        set_bit(piece_bitboards[piece], actual_square);
        
        // increment square and pointer to FEN string
        square++;
        fen++;
    }
      
    // match empty square numbers within FEN string
    else if (*fen >= '1' && *fen <= '8')
    {
        // init offset (convert char 0 to int 0)
        int offset = *fen - '0';
        
        // increment square and pointer to FEN string
        square += offset;
        fen++;
    }
    
    // match rank separator
    else if (*fen == '/')
    {
        // increment pointer to FEN string
        fen++;
    }
    else
    {
        fen++; // error
    }
  }

  // Move past first space, now side to move flag
  fen++;

  side_to_move = (*fen == 'w') ? white : black;

  // Move past side to move & space, handle castling rights
  fen += 2;

  while (*fen != ' ') {
    switch (*fen) {
      case 'K': castle |= w_kside; break;
      case 'Q': castle |= w_qside; break;
      case 'k': castle |= b_kside; break;
      case 'q': castle |= b_qside; break;
      case '-': break;
    }

    fen++;
  }

  // Move to en passant square
  fen++;

  if (*fen != '-') {
    int file = fen[0] - 'a';
    int rank = fen[1] - '0' - 1;
    en_passant_square = 8 * file + rank;
  }

  else{
    en_passant_square = -1;
  }

  // populate white occupancy bitboard
  for (int piece = P; piece <= K; piece++)
      occupancy_bitboards[white] |= piece_bitboards[piece];
  
  // populate black occupancy bitboard
  for (int piece = p; piece <= k; piece++)
      occupancy_bitboards[black] |= piece_bitboards[piece];

  // init all occupancies
  occupancy_bitboards[2] |= occupancy_bitboards[white];
  occupancy_bitboards[2] |= occupancy_bitboards[black];

  //printf("fen: '%s'\n", fen);
}

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

U64 find_magic_number(int square, int occupancy_bit_count, bool bishop) {
  U64 occupancies[4096];
  U64 attacks[4096];
  U64 used_attacks[4096];
  U64 attack_mask = bishop ? bishop_attack_mask(square) : rook_attack_mask(square);

  int occupancy_indices {1 << occupancy_bit_count};

  // Calculating the attacks for each configuration the slow way so we can test later with magic numbers
  for (int i = 0; i < occupancy_indices; i++)
  {
    occupancies[i] = set_occupancy(i, attack_mask, occupancy_bit_count);
    attacks[i] = bishop ? generate_bishop_attacks_slow(square, occupancies[i]) : 
                          generate_rook_attacks_slow(square, occupancies[i]);
  } 
  
  // Generating and testing magic numbers
  for(int count = 0; count < 100000000; count++) {
    U64 candidate = generate_candidate_magic();
    // Initial check to see if candidate is suitable
    if(count_bits((attack_mask * candidate) & 0xFF00000000000000ULL) < 6) continue;
    // Initially, all used attacks are 0
    memset(used_attacks, 0, sizeof(used_attacks));
    bool fail = false;
    for(int i = 0; !fail && i < occupancy_indices; i++) {
      int magic_index = (int)((occupancies[i] * candidate) >> (64 - occupancy_bit_count));

      // If the index isn't used so far then fill it with the current attack bitboard
      if(used_attacks[magic_index] == 0ULL) used_attacks[magic_index] = attacks[i];

      // Otherwise if a bad collision occurs we move on
      else if(used_attacks[magic_index] != attacks[i]) fail = true;
    }
    if(!fail){
      // printf("Found magic for square %d!\n", square);
      // for (int j = 0; j < 10; j++)
      // {
      //   print_bitboard(occupancies[j]);
      //   print_bitboard(attacks[j]);
      //   print_bitboard(used_attacks[(int)((occupancies[j] * candidate) >> (64 - occupancy_bit_count))]);
      //   printf("-------------------------\n");
      // }
      
      return candidate;
    } 
  }
  printf("Find magic number failed!\n");
  return 0ULL;
}

void init_magic_numbers() {
  // Find bishop magic numbers
  printf("Bishop magics:\n");
  for (int square = 0; square < 64; square++)
  {
    //bishop_magics[square] = find_magic_number(square, bishop_mask_bit_counts[square], bishop);
    printf("0x%llxULL,\n", find_magic_number(square, bishop_mask_bit_counts[square], bishop));
  }

  // Find rook magic numbers
  printf("Rook magics:\n");
  for (int square = 0; square < 64; square++)
  {
    printf("0x%llxULL,\n", find_magic_number(square, rook_mask_bit_counts[square], rook));
  }
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

// Print the entire board
void print_board() {
  for (int rank = 7; rank >= 0; rank--)
  {
    std::cout << rank + 1 << " ";
    for (int file = 0; file < 8; file++)
    {
      int piece = -1;
      for (int current_bb = P; current_bb <= k; current_bb++)
      {
        if(get_bit(piece_bitboards[current_bb], 8 * file + rank)) piece = current_bb;
      }

      if(piece == -1) std::cout << "|   ";
      else printf("| %c ", pieces_ascii[piece]);
    }   
    std::cout << "|\n";
  }
  std::cout << "    a   b   c   d   e   f   g   h\n";
  std::cout << "Side to move: " << ((side_to_move == white) ? "white" : "black") << "\n";
  std::cout << "En passant square: " << ((en_passant_square == -1) ? "none" : index_to_square_name[en_passant_square]) << "\n";
  printf("Castling rights: %c%c%c%c\n\n",
    (castle & w_kside) ? 'K' : '-',
    (castle & w_qside) ? 'Q' : '-',
    (castle & b_kside) ? 'k' : '-',
    (castle & b_qside) ? 'q' : '-'
  );
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
    ((shift_nw(king_bitboard) | shift_n(king_bitboard) | shift_ne(king_bitboard)) & (~rank_1)) |
    ((shift_sw(king_bitboard) | shift_s(king_bitboard) | shift_se(king_bitboard)) & (~rank_8)) |
    shift_w(king_bitboard)    | shift_e(king_bitboard)
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

// Fast bishop attacks generation
static inline U64 generate_bishop_attacks_magic(int square, U64 occupancy) {
  // Only get the relevant bits
  occupancy &= bishop_masks[square];
  // Multiply with magic & shift to get magic index
  occupancy *= bishop_magics[square];
  occupancy >>= 64 - bishop_mask_bit_counts[square];

  // Lookup in magic tables and return
  return bishop_attacks_table[square][occupancy];
}

// Fast bishop attacks generation
static inline U64 generate_rook_attacks_magic(int square, U64 occupancy) {
  // Only get the relevant bits
  occupancy &= rook_masks[square];
  // Multiply with magic & shift to get magic index
  occupancy *= rook_magics[square];
  occupancy >>= 64 - rook_mask_bit_counts[square];

  // Lookup in magic tables and return
  return rook_attacks_table[square][occupancy];
}

static inline U64 generate_queen_attacks(int square, U64 occupancy) {
  return generate_bishop_attacks_magic(square, occupancy) | generate_rook_attacks_magic(square, occupancy);
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

void init_sliding_attack_masks() {
  for (int square = 0; square < 64; square++)
  {
    bishop_masks[square] = bishop_attack_mask(square);
    rook_masks[square] = rook_attack_mask(square);
  }
}

void init_sliding_pieces_tables(bool bishop) {
  for (int square = 0; square < 64; square++)
  {
    U64 attack_mask = bishop ? bishop_masks[square] : rook_masks[square];

    int bits_in_mask = count_bits(attack_mask);
    int occupancy_indices = 1 << bits_in_mask;

    for (int i = 0; i < occupancy_indices; i++)
    {
      if(bishop) {
        U64 occupancy = set_occupancy(i, attack_mask, bits_in_mask);
        int magic_index = (occupancy * bishop_magics[square]) >> (64 - bishop_mask_bit_counts[square]);
        bishop_attacks_table[square][magic_index] = generate_bishop_attacks_slow(square, occupancy);
      }
      else {
        U64 occupancy = set_occupancy(i, attack_mask, bits_in_mask);
        int magic_index = (occupancy * rook_magics[square]) >> (64 - rook_mask_bit_counts[square]);
        rook_attacks_table[square][magic_index] = generate_rook_attacks_slow(square, occupancy);
      }
    }
  }
}

// Is square attacked by the given side?
static inline bool is_square_attacked(int square, int side) {

  // For white pawn to attack a square, must extend a black pawn attack mask from that square
  if((side == white) && (pawn_attacks_table[black][square] & piece_bitboards[P])) return true;

  // Vice versa for black pawns (slightly confusing!)
  if((side == black) && (pawn_attacks_table[white][square] & piece_bitboards[p])) return true;

  // Knight attacks
  if(knight_attacks_table[square] & (piece_bitboards[side == white ? N:n])) return true;

  // King attacks
  if(king_attacks_table[square] & (piece_bitboards[side == white ? K:k])) return true;

  // Bishop attacks
  if(generate_bishop_attacks_magic(square, occupancy_bitboards[2]) & (piece_bitboards[side == white ? B:b])) return true;

  // Rook attacks
  if(generate_rook_attacks_magic(square, occupancy_bitboards[2]) & (piece_bitboards[side == white ? R:r])) return true;

  // Queen attacks
  if(generate_queen_attacks(square, occupancy_bitboards[2]) & (piece_bitboards[side == white ? Q:q])) return true;

  return false;
}

// Print all squares attacked by the given side
void print_attacked_squares(int side) {
  for (int rank = 7; rank >= 0; rank--)
  {
    std::cout << rank + 1 << " ";
    for (int file = 0; file < 8; file++)
    {
      std::cout << (is_square_attacked((8 * file + rank), side) ? "| X " : "|   "); 
    }   
    std::cout << "|\n";
  }
  std::cout << "    a   b   c   d   e   f   g   h\n";
}

void init_all() {
  // Precalculated attack tables for leaping pieces
  init_leaping_pieces_tables();
  // Precalculated attack masks for bishop/rook
  init_sliding_attack_masks();
  // init_magic_numbers();
  // Populate the attack tables using magic numbers for bishop/rook
  init_sliding_pieces_tables(bishop);
  init_sliding_pieces_tables(rook);
}

int main() {
  init_all();
  parse_fen(test_fen);
  print_board();
  print_attacked_squares(white);
  print_attacked_squares(black);

  return 0;
}
