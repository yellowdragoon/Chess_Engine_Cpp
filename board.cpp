#include <stdlib.h>
#include <iostream>
#include <random>
#include <cstdint>
#include <string.h>
#include <string>
#include <unordered_map>
#include <chrono>
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
char * tricky_position_fen =  "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1 ";

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
  P, N, B, R, Q, K, p, n, b, r, q, k
};

// C-style string to easily convert from enum -> char
const char pieces_ascii[] {"PNBRQKpnbrqk"};

// Promoted pieces
std::unordered_map<int, char> promoted_pieces = {
  {Q, 'q'},
  {q, 'q'},
  {R, 'R'},
  {r, 'r'},
  {N, 'N'},
  {n, 'n'},
  {B, 'B'},
  {b, 'b'}
};

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

/*
                           castling   move     in      in
                              right update     binary  decimal

 king & rooks didn't move:     1111 & 1111  =  1111    15

        white king  moved:     1111 & 1100  =  1100    12
  white king's rook moved:     1111 & 1110  =  1110    14
 white queen's rook moved:     1111 & 1101  =  1101    13
     
         black king moved:     1111 & 0011  =  0011    3
  black king's rook moved:     1111 & 1011  =  1011    11
 black queen's rook moved:     1111 & 0111  =  0111    7

*/

const int castling_rights[64] = {
  13, 15, 15, 15, 15, 15, 15, 7, 
  15, 15, 15, 15, 15, 15, 15, 15, 
  15, 15, 15, 15, 15, 15, 15, 15, 
  15, 15, 15, 15, 15, 15, 15, 15, 
  12, 15, 15, 15, 15, 15, 15, 3, 
  15, 15, 15, 15, 15, 15, 15, 15, 
  15, 15, 15, 15, 15, 15, 15, 15, 
  14, 15, 15, 15, 15, 15, 15, 11
};

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

typedef struct {
  int moves[256];
  int count;
} moves;

/* Binary formatting of moves:

  0000 0000 0000 0000 0011 1111 [0x3f]     (6 bits) - Source square
  0000 0000 0000 1111 1100 0000 [0xfc0]    (6 bits) - Target square
  0000 0000 1111 0000 0000 0000 [0xf000]   (4 bits) - Piece
  0000 1111 0000 0000 0000 0000 [0xf0000]  (4 bits) - Promoted piece
  0001 0000 0000 0000 0000 0000 [0x100000] (1 bit ) - Capture flag
  0010 0000 0000 0000 0000 0000 [0x200000] (1 bit ) - Double push flag
  0100 0000 0000 0000 0000 0000 [0x400000] (1 bit ) - Enpassant flag
  1000 0000 0000 0000 0000 0000 [0x800000] (1 bit ) - Castling flag 
*/

#define encode_move(source, target, piece, promoted, capture, double_push, enpassant, castle) \
    (source) |            \
    (target << 6) |       \
    (piece << 12) |       \
    (promoted << 16) |    \
    (capture << 20) |     \
    (double_push << 21) | \
    (enpassant << 22) |   \
    (castle << 23)

#define get_move_source(move) ((move) & 0x3f)
#define get_move_target(move) (((move) & 0xfc0) >> 6)
#define get_move_piece(move) (((move) & 0xf000) >> 12)
#define get_move_promoted(move) (((move) & 0xf0000) >> 16)
#define get_move_capture(move) ((move) & 0x100000)
#define get_move_double_push(move) ((move) & 0x200000)
#define get_move_enpassant(move) ((move) & 0x400000)
#define get_move_castle(move) ((move) & 0x800000)

void add_move(moves *move_list, int move){
  move_list->moves[move_list->count] = move;
  move_list->count++;
}

void print_move(int move) {
  if(get_move_promoted(move)) {
    printf("%s%s%c", index_to_square_name[get_move_source(move)],
                      index_to_square_name[get_move_target(move)],
                      promoted_pieces[get_move_promoted(move)]);
  }
  else{
    printf("%s%s", index_to_square_name[get_move_source(move)],
                     index_to_square_name[get_move_target(move)]);
  }

}

void print_move_list(moves *move_list) {
  printf("\n    move    piece   capture   double    enpass    castling\n\n");
  for (int move_count = 0; move_count < move_list->count; move_count++)
  {
    int move = move_list->moves[move_count];
    // print move
    printf("    %s%s%c   %c       %d         %d         %d         %d\n", 
      index_to_square_name[get_move_source(move)],
      index_to_square_name[get_move_target(move)],
      get_move_promoted(move) ? promoted_pieces[get_move_promoted(move)] : ' ',
      pieces_ascii[get_move_piece(move)],
      get_move_capture(move) ? 1 : 0,
      get_move_double_push(move) ? 1 : 0,
      get_move_enpassant(move) ? 1 : 0,
      get_move_castle(move) ? 1 : 0);
  }
  // print total number of moves
  printf("\n\n    Total number of moves: %d\n\n", move_list->count);
}

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

void generate_moves(moves *move_list) {
  move_list->count = 0;

  int source_square;
  int target_square;

  U64 current_bitboard;
  U64 attacks;

  for (int piece = P; piece <= k; piece++)
  {
    current_bitboard = piece_bitboards[piece];

    // Generate white pawn/castling moves
    if(side_to_move == white) {
      if(piece == P) {
        while(current_bitboard) {
          source_square = ls1b_index(current_bitboard);
          target_square = source_square + 1;

          // If target on the board and target not occupied
          if((target_square <= h8) && (!get_bit(occupancy_bitboards[2], target_square))) {
            // Pawn promotion
            if(target_square % 8 == 7) {
              add_move(move_list, encode_move(source_square, target_square, piece, Q, 0, 0, 0, 0));
              add_move(move_list, encode_move(source_square, target_square, piece, R, 0, 0, 0, 0));
              add_move(move_list, encode_move(source_square, target_square, piece, N, 0, 0, 0, 0));
              add_move(move_list, encode_move(source_square, target_square, piece, B, 0, 0, 0, 0));
            }

            // Pawn pushes
            else{
              // Single push
              add_move(move_list, encode_move(source_square, target_square, piece, 0, 0, 0, 0, 0));

              // Double push
              if((source_square % 8 == 1) && !get_bit(occupancy_bitboards[2], target_square + 1)) {
                add_move(move_list, encode_move(source_square, target_square + 1, piece, 0, 0, 1, 0, 0));
              }
            }
          }

          // Captures
          attacks = pawn_attacks_table[white][source_square] & occupancy_bitboards[black];

          while(attacks) {
            target_square = ls1b_index(attacks);

            // Capture promotion
            if(target_square % 8 == 7) {
              add_move(move_list, encode_move(source_square, target_square, piece, Q, 1, 0, 0, 0));
              add_move(move_list, encode_move(source_square, target_square, piece, R, 1, 0, 0, 0));
              add_move(move_list, encode_move(source_square, target_square, piece, N, 1, 0, 0, 0));
              add_move(move_list, encode_move(source_square, target_square, piece, B, 1, 0, 0, 0));
            }
            else{
              add_move(move_list, encode_move(source_square, target_square, piece, 0, 1, 0, 0, 0));
            }

            remove_bit(attacks, target_square);
          }

          // En passant capture
          if(en_passant_square != -1) {
            U64 enpassant_attacks = pawn_attacks_table[white][source_square] & (1ULL << en_passant_square);

            if(enpassant_attacks) {
              int target_enpassant = ls1b_index(enpassant_attacks);
              add_move(move_list, encode_move(source_square, target_enpassant, piece, 0, 1, 0, 1, 0));
            }
          }

          remove_bit(current_bitboard, source_square);
        }
      }

      // Castling moves
      else if (piece == K) {
        // Castle short
        if(castle & w_kside){
          // Empty squares on g1, f1
          if(!get_bit(occupancy_bitboards[2], f1) && !get_bit(occupancy_bitboards[2], g1)){
            if(!is_square_attacked(e1, black) && !is_square_attacked(f1, black)){
              add_move(move_list, encode_move(e1, g1, piece, 0, 0, 0, 0, 1));
            }
          }
        }

        // Castle long
        if(castle & w_qside){
          // Empty squares on b1, c1, d1
          if(!get_bit(occupancy_bitboards[2], b1) && !get_bit(occupancy_bitboards[2], c1) && !get_bit(occupancy_bitboards[2], d1)){
            if(!is_square_attacked(d1, black) && !is_square_attacked(e1, black)){
              add_move(move_list, encode_move(e1, c1, piece, 0, 0, 0, 0, 1));
            }
          }
        }
      }
    }

    // Generate black pawn/castling moves
    else{
      if(piece == p) {
        while(current_bitboard) {
          source_square = ls1b_index(current_bitboard);
          target_square = source_square - 1;

          // If target on the board and target not occupied
          if((target_square >= a1) && (!get_bit(occupancy_bitboards[2], target_square))) {
            // Pawn promotion
            if(target_square % 8 == 0) {
              add_move(move_list, encode_move(source_square, target_square, piece, q, 0, 0, 0, 0));
              add_move(move_list, encode_move(source_square, target_square, piece, r, 0, 0, 0, 0));
              add_move(move_list, encode_move(source_square, target_square, piece, n, 0, 0, 0, 0));
              add_move(move_list, encode_move(source_square, target_square, piece, b, 0, 0, 0, 0));
            }

            // Pawn pushes
            else{
              // Single push
              add_move(move_list, encode_move(source_square, target_square, piece, 0, 0, 0, 0, 0));

              // Double push
              if((source_square % 8 == 6) && !get_bit(occupancy_bitboards[2], target_square - 1)) {
                add_move(move_list, encode_move(source_square, target_square - 1, piece, 0, 0, 1, 0, 0));
              }
            }
          }

          // Captures
          attacks = pawn_attacks_table[black][source_square] & occupancy_bitboards[white];

          while(attacks) {
            target_square = ls1b_index(attacks);

            // Capture promotions
            if(target_square % 8 == 0) {
              add_move(move_list, encode_move(source_square, target_square, piece, q, 1, 0, 0, 0));
              add_move(move_list, encode_move(source_square, target_square, piece, r, 1, 0, 0, 0));
              add_move(move_list, encode_move(source_square, target_square, piece, n, 1, 0, 0, 0));
              add_move(move_list, encode_move(source_square, target_square, piece, b, 1, 0, 0, 0));
            }
            // Normal capture
            else{
              add_move(move_list, encode_move(source_square, target_square, piece, 0, 1, 0, 0, 0));
            }

            remove_bit(attacks, target_square);
          }

          // En passant capture
          if(en_passant_square != -1) {
            U64 enpassant_attacks = pawn_attacks_table[black][source_square] & (1ULL << en_passant_square);

            if(enpassant_attacks) {
              int target_enpassant = ls1b_index(enpassant_attacks);
              add_move(move_list, encode_move(source_square, target_enpassant, piece, 0, 1, 0, 1, 0));
            }
          }

          remove_bit(current_bitboard, source_square);
        }
      }

      // Castling moves
      else if (piece == k) {
        // Castle short
        if(castle & b_kside){
          // Empty squares on g8, f8
          if(!get_bit(occupancy_bitboards[2], f8) && !get_bit(occupancy_bitboards[2], g8)){
            if(!is_square_attacked(e8, white) && !is_square_attacked(f8, white)){
              add_move(move_list, encode_move(e8, g8, piece, 0, 0, 0, 0, 1));
            }
          }
        }

        // Castle long
        if(castle & b_qside){
          // Empty squares on b8, c8, d8
          if(!get_bit(occupancy_bitboards[2], b8) && !get_bit(occupancy_bitboards[2], c8) && !get_bit(occupancy_bitboards[2], d8)){
            if(!is_square_attacked(d8, white) && !is_square_attacked(e8, white)){
              add_move(move_list, encode_move(e8, c8, piece, 0, 0, 0, 0, 1));
            }
          }
        }
      }
    }

    // Rook moves
    if((side_to_move == white) ? piece == R : piece == r) {
      while(current_bitboard){
        source_square = ls1b_index(current_bitboard);
        attacks = generate_rook_attacks_magic(source_square, occupancy_bitboards[2]) & ((side_to_move == white) ? ~occupancy_bitboards[white]: ~occupancy_bitboards[black]);

        while(attacks){
          target_square = ls1b_index(attacks);

          // Quiet move (needed for quiescence search later)
          if(!get_bit((side_to_move == white) ? occupancy_bitboards[black] : occupancy_bitboards[white], target_square)){
            add_move(move_list, encode_move(source_square, target_square, piece, 0, 0, 0, 0, 0));
          }
          // Capture move
          else{
            add_move(move_list, encode_move(source_square, target_square, piece, 0, 1, 0, 0, 0));
          }
          remove_bit(attacks, target_square);
        }
        remove_bit(current_bitboard, source_square);
      }
    }

    // Knight moves
    if((side_to_move == white) ? piece == N : piece == n) {
      while(current_bitboard){
        source_square = ls1b_index(current_bitboard);
        attacks = knight_attacks_table[source_square] & ((side_to_move == white) ? ~occupancy_bitboards[white]: ~occupancy_bitboards[black]);

        while(attacks){
          target_square = ls1b_index(attacks);

          // Quiet move (needed for quiescence search later)
          if(!get_bit((side_to_move == white) ? occupancy_bitboards[black] : occupancy_bitboards[white], target_square)){
            add_move(move_list, encode_move(source_square, target_square, piece, 0, 0, 0, 0, 0));
          }
          // Capture move
          else{
            add_move(move_list, encode_move(source_square, target_square, piece, 0, 1, 0, 0, 0));
          }
          remove_bit(attacks, target_square);
        }
        remove_bit(current_bitboard, source_square);
      }
    }

    // Bishop moves
    if((side_to_move == white) ? piece == B : piece == b) {
      while(current_bitboard){
        source_square = ls1b_index(current_bitboard);
        attacks = generate_bishop_attacks_magic(source_square, occupancy_bitboards[2]) & ((side_to_move == white) ? ~occupancy_bitboards[white]: ~occupancy_bitboards[black]);

        while(attacks){
          target_square = ls1b_index(attacks);

          // Quiet move (needed for quiescence search later)
          if(!get_bit((side_to_move == white) ? occupancy_bitboards[black] : occupancy_bitboards[white], target_square)){
            add_move(move_list, encode_move(source_square, target_square, piece, 0, 0, 0, 0, 0));
          }
          // Capture move
          else{
            add_move(move_list, encode_move(source_square, target_square, piece, 0, 1, 0, 0, 0));
          }
          remove_bit(attacks, target_square);
        }
        remove_bit(current_bitboard, source_square);
      }
    }

    // Queen moves
    if((side_to_move == white) ? piece == Q : piece == q) {
      while(current_bitboard){
        source_square = ls1b_index(current_bitboard);
        attacks = generate_queen_attacks(source_square, occupancy_bitboards[2]) & ((side_to_move == white) ? ~occupancy_bitboards[white]: ~occupancy_bitboards[black]);

        while(attacks){
          target_square = ls1b_index(attacks);

          // Quiet move (needed for quiescence search later)
          if(!get_bit((side_to_move == white) ? occupancy_bitboards[black] : occupancy_bitboards[white], target_square)){
            add_move(move_list, encode_move(source_square, target_square, piece, 0, 0, 0, 0, 0));
          }
          // Capture move
          else{
            add_move(move_list, encode_move(source_square, target_square, piece, 0, 1, 0, 0, 0));
          }
          remove_bit(attacks, target_square);
        }
        remove_bit(current_bitboard, source_square);
      }
    }

    // King moves
    if((side_to_move == white) ? piece == K : piece == k) {
      while(current_bitboard){
        source_square = ls1b_index(current_bitboard);
        attacks = king_attacks_table[source_square] & ((side_to_move == white) ? ~occupancy_bitboards[white]: ~occupancy_bitboards[black]);

        while(attacks){
          target_square = ls1b_index(attacks);

          // Quiet move (needed for quiescence search later)
          if(!get_bit((side_to_move == white) ? occupancy_bitboards[black] : occupancy_bitboards[white], target_square)){
            add_move(move_list, encode_move(source_square, target_square, piece, 0, 0, 0, 0, 0));
          }
          // Capture move
          else{
            add_move(move_list, encode_move(source_square, target_square, piece, 0, 1, 0, 0, 0));
          }
          remove_bit(attacks, target_square);
        }
        remove_bit(current_bitboard, source_square);
      }
    }
  }
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

// preserve board state
#define copy_board()                                                            \
    U64 bitboards_copy[12], occupancies_copy[3];                                \
    int side_copy, enpassant_copy, castle_copy;                                 \
    memcpy(bitboards_copy, piece_bitboards, sizeof(piece_bitboards));           \
    memcpy(occupancies_copy, occupancy_bitboards, sizeof(occupancy_bitboards)); \
    side_copy = side_to_move;                                                   \
    enpassant_copy = en_passant_square;                                         \
    castle_copy = castle;

// restore board state
#define take_back()                                                             \
    memcpy(piece_bitboards, bitboards_copy, sizeof(piece_bitboards));           \
    memcpy(occupancy_bitboards, occupancies_copy, sizeof(occupancy_bitboards)); \
    side_to_move = side_copy;                                                   \
    en_passant_square = enpassant_copy;                                         \
    castle = castle_copy;

// move types
enum {all_moves, only_captures};

// make move
int make_move(int move, int move_flag) {
  if(move_flag == all_moves) {
    copy_board();

    int source_square = get_move_source(move);
    int target_square = get_move_target(move);
    int piece = get_move_piece(move);
    int promoted = get_move_promoted(move);
    int capture = get_move_capture(move);
    int double_push = get_move_double_push(move);
    int enpassant = get_move_enpassant(move);
    int current_castle = get_move_castle(move);

    // move piece
    remove_bit(piece_bitboards[piece], source_square);
    set_bit(piece_bitboards[piece], target_square);

    // handling captures
    if(capture){
      int start_piece, end_piece;

      if(side_to_move == white){
        start_piece = p;
        end_piece = k;
      }
      else{
        start_piece = P;
        end_piece = K;
      }

      for (int current_piece = start_piece; current_piece <= end_piece; current_piece++)
      {
        if(get_bit(piece_bitboards[current_piece], target_square)){
          remove_bit(piece_bitboards[current_piece], target_square);
          break;
        }
      }
    }

    // handling promotions
    if(promoted){
      // erase existing pawn
      remove_bit(piece_bitboards[(side_to_move == white) ? P : p], target_square);
      set_bit(piece_bitboards[promoted], target_square);
    }

    // handling enpassant captures
    if(enpassant){
      // remove captured pawn
      (side_to_move == white) ? remove_bit(piece_bitboards[p], target_square - 1) : 
                                remove_bit(piece_bitboards[P], target_square + 1);
    }

    // reset enpassant square
    en_passant_square = -1;

    // handle double pawn push
    if(double_push){
      (side_to_move == white) ? (en_passant_square = target_square - 1) : (en_passant_square = target_square + 1);
    }

    // handling castling moves
    if(current_castle){
      switch (target_square)
      {
        // White kingside
        case g1:
          remove_bit(piece_bitboards[R], h1);
          set_bit(piece_bitboards[R], f1);
          break;
        
        // White queenside
        case c1:
          remove_bit(piece_bitboards[R], a1);
          set_bit(piece_bitboards[R], d1);
          break;

        // Black kingside
        case g8:
          remove_bit(piece_bitboards[r], h8);
          set_bit(piece_bitboards[r], f8);
          break;

        // Black queenside
        case c8:
          remove_bit(piece_bitboards[r], a8);
          set_bit(piece_bitboards[r], d8);
          break;
      }
    }

    // Update castling rights
    castle &= castling_rights[source_square];
    castle &= castling_rights[target_square];

    // Reset occupancies
    memset(occupancy_bitboards, 0, sizeof(occupancy_bitboards));

    // Set occupancies
    for (int piece = P; piece <= K; piece++)
    {
      occupancy_bitboards[white] |= piece_bitboards[piece];
    }

    for (int piece = p; piece <= k; piece++)
    {
      occupancy_bitboards[black] |= piece_bitboards[piece];
    }

    occupancy_bitboards[2] |= occupancy_bitboards[white];
    occupancy_bitboards[2] |= occupancy_bitboards[black];

    // Change side to move
    side_to_move ^= 1;

    // Make sure king is not in check
    if(is_square_attacked((side_to_move == white) ? ls1b_index(piece_bitboards[k]) : ls1b_index(piece_bitboards[K]), side_to_move)){
      take_back();
      // return illegal move
      return 0;
    }

    else{
      // legal move
      return 1;
    }
  }

  // capture moves
  else
  {
    // make sure move is the capture
    if (get_move_capture(move))
        make_move(move, all_moves);
    
    // otherwise the move is not a capture
    else
        // don't make it
        return 0;
  }
}

// Leaf nodes
U64 nodes = 0;

// Perft driver
void perft_driver(int depth){
  if(depth == 0){
    nodes++;
    return;
  }

  moves move_list[1];
  generate_moves(move_list);

  for (int move_count = 0; move_count < move_list->count; move_count++)
  {
    copy_board();
    if(!make_move(move_list->moves[move_count], all_moves)){
      continue;
    }

    perft_driver(depth - 1);
    take_back();
  }
}

// Perft test
void perft_test(int depth) {
  printf("\n     Performance test\n\n");
  
  // create move list instance
  moves move_list[1];
  
  // generate moves
  generate_moves(move_list);
  
  // init start time
  auto start = std::chrono::high_resolution_clock::now();
  
  // loop over generated moves
  for (int move_count = 0; move_count < move_list->count; move_count++)
  {   
    // preserve board state
    copy_board();
    
    // make move
    if (!make_move(move_list->moves[move_count], all_moves)){
      // skip to the next move
      continue;
    }

    
    // cummulative nodes
    U64 cummulative_nodes = nodes;
    
    // call perft driver recursively
    perft_driver(depth - 1);
    
    // old nodes
    U64 old_nodes = nodes - cummulative_nodes;
    
    // take back
    take_back();
    
    // print move
    printf("     move: %s%s%c  nodes: %ld\n", index_to_square_name[get_move_source(move_list->moves[move_count])],
                                              index_to_square_name[get_move_target(move_list->moves[move_count])],
                                              get_move_promoted(move_list->moves[move_count]) ? promoted_pieces[get_move_promoted(move_list->moves[move_count])] : ' ',
                                              old_nodes);
  }
  auto end = std::chrono::high_resolution_clock::now();
  
  // print results
  printf("\n    Depth: %d\n", depth);
  printf("    Nodes: %ld\n", nodes);
  printf("Time (ms): %llu\n\n", std::chrono::duration_cast<std::chrono::milliseconds>(end - start).count());
}

int material_score[12] = {
    100,      // white pawn score
    300,      // white knight scrore
    350,      // white bishop score
    500,      // white rook score
   1000,      // white queen score
  10000,      // white king score
   -100,      // black pawn score
   -300,      // black knight scrore
   -350,      // black bishop score
   -500,      // black rook score
  -1000,      // black queen score
 -10000,      // black king score
};

const int pawn_score[64] = 
{
  0,   0,   0,   5,  10,  20,  30,  90, 
  0,   0,   0,   5,  10,  20,  30,  90, 
  0,   0,   0,  10,  10,  20,  30,  90, 
  0,  -10,   5,  20,  20,  30,  40,  90,
  0,  -10,   5,  20,  20,  30,  40,  90,
  0,   0,   0,   5,  10,  20,  30,  90, 
  0,   0,   0,   5,  10,  20,  30,  90, 
  0,   0,   0,   5,  10,  20,  30,  90  
};

// knight positional score
const int knight_score[64] = 
{
  -5,  -5,  -5,  -5,  -5,  -5,  -5,  -5, 
  -10,   0,   5,  10,  10,   5,   0,   0,
    0,   0,  20,  20,  20,  20,   0,   0, 
    0,   0,  20,  30,  30,  10,   0,   0, 
    0,   0,  20,  30,  30,  10,   0,   0, 
    0,   0,  20,  20,  20,  20,   0,   0, 
  -10,   0,   5,  10,  10,   5,   0,   0,
  -5,  -5,  -5,  -5,  -5,  -5,  -5,  -5  
};

// bishop positional score
const int bishop_score[64] = 
{
     0,   0,   0,   0,   0,   0,   0,   0, 
     0,   30,  10,  0,  0,   0,   0,   0,  
    -10,  0,   0,  10,  10,   0,   0,   0, 
     0,   0,   0,  20,  20,   10,   0,   0,
     0,   0,   0,  20,  20,   10,   0,   0,
     -10, 0,   0,  10,  10,   0,   0,   0, 
     0,   30,  10,  0,   0,   0,   0,   0, 
     0,   0,   0,   0,   0,   0,   0,   0  
};

// rook positional score
const int rook_score[64] =
{
  0,  0,  0,  0,  0,  0, 50, 50,
  0,  0,  0,  0,  0,  0, 50, 50,
  0, 10, 10, 10, 10, 10, 50, 50,
  20, 20, 20, 20, 20, 20, 50, 50,
  20, 20, 20, 20, 20, 20, 50, 50,
  0, 10, 10, 10, 10, 10, 50, 50,
  0,  0,  0,  0,  0,  0, 50, 50,
  0,  0,  0,  0,  0,  0, 50, 50
};

// king positional score
const int king_score[64] = 
{
   0,   0,   0,   0,   0,   0,   0,   0,
   0,   5,   0,   5,   5,   5,   0,   0,
   5,   5,   5,  10,  10,   5,   5,   0,
   0,  -5,  10,  20,  20,  10,   5,   0,
  -15,  -5,  10,  20,  20,  10,   5,   0,
   0,   0,   5,  10,  10,   5,   5,   0,
   10,  5,   0,   5,   5,   5,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0
};

// mirror positional score tables for opposite side
const int mirror_score[128] =
{
  a8, a7, a6, a5, a4, a3, a2, a1,
  b8, b7, b6, b5, b4, b3, b2, b1,
  c8, c7, c6, c5, c4, c3, c2, c1,
  d8, d7, d6, d5, d4, d3, d2, d1,
  e8, e7, e6, e5, e4, e3, e2, e1,
  f8, f7, f6, f5, f4, f3, f2, f1,
  g8, g7, g6, g5, g4, g3, g2, g1,
  h8, h7, h6, h5, h4, h3, h2, h1
};

int evaluate()
{
    int score = 0;
    // current pieces bitboard copy
    U64 bitboard;
    int piece, square;
    
    // loop over piece bitboards
    for (int bb_piece = P; bb_piece <= k; bb_piece++)
    {
        // init piece bitboard copy
        bitboard = piece_bitboards[bb_piece];
        
        // loop over pieces within a bitboard
        while (bitboard)
        {
            piece = bb_piece;
            square = ls1b_index(bitboard);
            score += material_score[piece];

            // score positional piece scores
            switch (piece)
            {
                // evaluate white pieces
                case P: score += pawn_score[square]; break;
                case N: score += knight_score[square]; break;
                case B: score += bishop_score[square]; break;
                case R: score += rook_score[square]; break;
                case K: score += king_score[square]; break;

                // evaluate black pieces
                case p: score -= pawn_score[mirror_score[square]]; break;
                case n: score -= knight_score[mirror_score[square]]; break;
                case b: score -= bishop_score[mirror_score[square]]; break;
                case r: score -= rook_score[mirror_score[square]]; break;
                case k: score -= king_score[mirror_score[square]]; break;
            }

            // printf("%d %c %s\n", score, pieces_ascii[piece], index_to_square_name[square]);
            
            remove_bit(bitboard, square);
        }
    }
    
    // return final evaluation based on side
    return (side_to_move == white) ? score : -score;
}

// most valuable victim & less valuable attacker

/*
                          
    (Victims) Pawn Knight Bishop   Rook  Queen   King
  (Attackers)
        Pawn   105    205    305    405    505    605
      Knight   104    204    304    404    504    604
      Bishop   103    203    303    403    503    603
        Rook   102    202    302    402    502    602
       Queen   101    201    301    401    501    601
        King   100    200    300    400    500    600

*/

// MVV LVA [attacker][victim]
static int mvv_lva[12][12] = {
 	105, 205, 305, 405, 505, 605,  105, 205, 305, 405, 505, 605,
	104, 204, 304, 404, 504, 604,  104, 204, 304, 404, 504, 604,
	103, 203, 303, 403, 503, 603,  103, 203, 303, 403, 503, 603,
	102, 202, 302, 402, 502, 602,  102, 202, 302, 402, 502, 602,
	101, 201, 301, 401, 501, 601,  101, 201, 301, 401, 501, 601,
	100, 200, 300, 400, 500, 600,  100, 200, 300, 400, 500, 600,

	105, 205, 305, 405, 505, 605,  105, 205, 305, 405, 505, 605,
	104, 204, 304, 404, 504, 604,  104, 204, 304, 404, 504, 604,
	103, 203, 303, 403, 503, 603,  103, 203, 303, 403, 503, 603,
	102, 202, 302, 402, 502, 602,  102, 202, 302, 402, 502, 602,
	101, 201, 301, 401, 501, 601,  101, 201, 301, 401, 501, 601,
	100, 200, 300, 400, 500, 600,  100, 200, 300, 400, 500, 600
};


// half move counter
int ply;

// best move
int best_move;

// score moves
static inline int score_move(int move)
{
    // score capture move
    if (get_move_capture(move))
    {
        // init target piece
        int target_piece = P;
        
        // pick up bitboard piece index ranges depending on side
        int start_piece, end_piece;
        
        // pick up side to move
        if (side_to_move == white) { start_piece = p; end_piece = k; }
        else { start_piece = P; end_piece = K; }
        
        // loop over bitboards opposite to the current side to move
        for (int bb_piece = start_piece; bb_piece <= end_piece; bb_piece++)
        {
            // if there's a piece on the target square
            if (get_bit(piece_bitboards[bb_piece], get_move_target(move)))
            {
                // remove it from corresponding bitboard
                target_piece = bb_piece;
                break;
            }
        }
                
        // score move by MVV LVA lookup [source piece][target piece]
        return mvv_lva[get_move_piece(move)][target_piece];
    }
    
    // score quiet move
    else
    {
    
    }
    
    return 0;
}

// sort moves in descending order
static inline void sort_moves(moves *move_list)
{
  // move scores
  int move_scores[move_list->count];

  // score all the moves within a move list
  for (int count = 0; count < move_list->count; count++){
    // score move
    move_scores[count] = score_move(move_list->moves[count]);
  }
  // loop over current move within a move list
  for (int current_move = 0; current_move < move_list->count; current_move++)
  {
      // loop over next move within a move list
      for (int next_move = current_move + 1; next_move < move_list->count; next_move++)
      {
          // compare current and next move scores
          if (move_scores[current_move] < move_scores[next_move])
          {
              // swap scores
              int temp_score = move_scores[current_move];
              move_scores[current_move] = move_scores[next_move];
              move_scores[next_move] = temp_score;
              
              // swap moves
              int temp_move = move_list->moves[current_move];
              move_list->moves[current_move] = move_list->moves[next_move];
              move_list->moves[next_move] = temp_move;
          }
      }
  }
}

// print move scores
void print_move_scores(moves *move_list)
{
    printf("     Move scores:\n\n");
        
    // loop over moves within a move list
    for (int count = 0; count < move_list->count; count++)
    {
        printf("     move: ");
        print_move(move_list->moves[count]);
        printf(" score: %d\n", score_move(move_list->moves[count]));
    }
}

// quiescence search
static inline int quiescence(int alpha, int beta)
{
    nodes++;
    // evaluate position
    int evaluation = evaluate();
    
    // fail-hard beta cutoff
    if (evaluation >= beta)
    {
        // node (move) fails high
        return beta;
    }
    
    // found a better move
    if (evaluation > alpha)
    {
        // PV node (move)
        alpha = evaluation;
    }
    
    // create move list instance
    moves move_list[1];
    
    // generate moves
    generate_moves(move_list);
    
    // loop over moves within a movelist
    for (int count = 0; count < move_list->count; count++)
    {
        // preserve board state
        copy_board();
        
        // increment ply
        ply++;
        
        // make sure to make only legal moves
        if (make_move(move_list->moves[count], only_captures) == 0)
        {
            // decrement ply
            ply--;
            
            // skip to next move
            continue;
        }

        // score current move
        int score = -quiescence(-beta, -alpha);
        
        // decrement ply
        ply--;

        // take move back
        take_back();
        
        // fail-hard beta cutoff
        if (score >= beta)
        {
            // node (move) fails high
            return beta;
        }
        
        // found a better move
        if (score > alpha)
        {
            // PV node (move)
            alpha = score;
            
        }
    }
    
    // node (move) fails low
    return alpha;
}

// negamax alpha beta search
static inline int negamax(int alpha, int beta, int depth)
{
    // recurrsion escape condition
    if (depth == 0){
      // return evaluation
      return quiescence(alpha, beta);
    }

    // increment nodes count
    nodes++;

    // is king in check
    int in_check = is_square_attacked((side_to_move == white) ? ls1b_index(piece_bitboards[K]) : 
                                                                ls1b_index(piece_bitboards[k]),
                                                                side_to_move ^ 1);

    // legal moves counter
    int legal_moves = 0;
    
    // best move so far
    int best_sofar;
    
    // old value of alpha
    int old_alpha = alpha;
    
    // create move list instance
    moves move_list[1];
    
    // generate moves
    generate_moves(move_list);
    
    // loop over moves within a movelist
    for (int count = 0; count < move_list->count; count++)
    {
        // preserve board state
        copy_board();
        
        // increment ply
        ply++;
        
        // make sure to make only legal moves
        if (make_move(move_list->moves[count], all_moves) == 0)
        {
            // decrement ply
            ply--;
            
            // skip to next move
            continue;
        }

        legal_moves++;
        
        // score current move
        int score = -negamax(-beta, -alpha, depth - 1);
        
        // decrement ply
        ply--;

        // take move back
        take_back();
        
        // fail-hard beta cutoff
        if (score >= beta)
        {
            // node (move) fails high
            return beta;
        }
        
        // found a better move
        if (score > alpha)
        {
            // PV node (move)
            alpha = score;
            
            // if root move
            if (ply == 0)
            {
                // associate best move with the best score
                best_sofar = move_list->moves[count];
            }
        }
    }

    // we don't have any legal moves to make in the current postion
    if (legal_moves == 0)
    {
        // king is in check
        if (in_check){
          // return mating score (assuming closest distance to mating position)
          return -49000 + ply;
        }
        
        // king is not in check
        else {
          // return stalemate score
          return 0;
        }
    }
    
    // found better move
    if (old_alpha != alpha)
    {
      // init best move
      best_move = best_sofar;
    }
    
    // node (move) fails low
    return alpha;
}

// search position for the best move
void search_position(int depth)
{
    // find best move within a given position
    int score = negamax(-50000, 50000, depth);
    
    if (best_move)
    {
      printf("info score cp %d depth %d nodes %ld\n", score, depth, nodes);
  
      // best move placeholder
      printf("bestmove ");
      print_move(best_move);
      printf("\n");
    }
}

// UCI protocol
int parse_move(char * move_string){
  moves move_list[1];
  generate_moves(move_list);

  int source_square = 8 * (move_string[0] - 'a') + (move_string[1] - '1');
  int target_square = 8 * (move_string[2] - 'a') + (move_string[3] - '1');

  // printf("source square: %d %s\n", source_square, index_to_square_name[source_square]);
  // printf("target square: %d %s\n", target_square, index_to_square_name[target_square]);

  // loop over the moves within a move list
  for (int move_count = 0; move_count < move_list->count; move_count++)
  {
    // init move
    int move = move_list->moves[move_count];
    
    // make sure source & target squares are available within the generated move
    if (source_square == get_move_source(move) && target_square == get_move_target(move))
    {
      // init promoted piece
      int promoted_piece = get_move_promoted(move);
      
      // promoted piece is available
      if (promoted_piece)
      {
        // promoted to queen
        if ((promoted_piece == Q || promoted_piece == q) && move_string[4] == 'q')
          // return legal move
          return move;
        
        // promoted to rook
        else if ((promoted_piece == R || promoted_piece == r) && move_string[4] == 'r')
          // return legal move
          return move;
        
        // promoted to bishop
        else if ((promoted_piece == B || promoted_piece == b) && move_string[4] == 'b')
          // return legal move
          return move;
        
        // promoted to knight
        else if ((promoted_piece == N || promoted_piece == n) && move_string[4] == 'n')
          // return legal move
          return move;
        
        // continue the loop on possible wrong promotions (e.g. "e7e8f")
        continue;
      }
      
      // return legal move
      return move;
    }
  }
  return 0;
}

// parse UCI "position" command
void parse_position(char *command){
  // shift pointer to right of next token
  command += 9;
  char *current_char = command;

  // parse UCI "startpos" command
  if(strncmp(command, "startpos", 8) == 0){
    parse_fen(start_position_fen);
  }

  // parse UCI "fen" command
  else{
    current_char = strstr(command, "fen");

    if (current_char == NULL){
      parse_fen(start_position_fen);
    }

    // found fen substring
    else{
      current_char += 4;
      parse_fen(current_char);
    }
  }

  current_char = strstr(command, "moves");

  if(current_char != NULL){
    
    // move to next token
    current_char += 6;

    // parse all the moves
    while(*current_char){
      int move = parse_move(current_char);

      // if no more moves
      if(move == 0){
        break;
      }

      make_move(move, all_moves);

      // move pointer to end of current move
      while(*current_char && *current_char != ' ') {
        current_char++;
      }

      if(*current_char == '\0') break;

      // go to the next move
      current_char++;
    }
  }
  print_board();
}

// parse UCI "go" command
void parse_go(char *command){
  int depth = -1;

  char *current_depth = NULL;

  if(current_depth = strstr(command, "depth")){
    // convert string to int and assign result value to depth
    depth = atoi(current_depth + 6);
  }

  else{
    // default depth 6
    depth = 6;
  }
  search_position(depth);
}

// main UCI loop
void uci_loop(){
  // reset STDIN & STDOUT buffers
  setbuf(stdin, NULL);
  setbuf(stdout, NULL);

  // define user / GUI input buffer
  char input[2000];

  // print engine info
  printf("id name BBC\n");
  printf("id name Henry Li\n");
  printf("uciok\n");

  // main loop
  while(true){
    // reset user / GUI input
    memset(input, 0, sizeof(input));

    // make sure output reaches GUI
    fflush(stdout);

    // get user / GUI input
    if(!fgets(input, 2000, stdin)){
      continue;
    }

    // make sure input is available
    if(input[0] == '\n'){
      continue;
    }

    // parse UCI "isready" command
    if(strncmp(input, "isready", 7) == 0){
      printf("readyok\n");
      continue;
    }

    // parse UCI "position" command
    else if(strncmp(input, "position", 8) == 0){
      parse_position(input);
    }

    // parse UCI "ucinewgame" command
    else if(strncmp(input, "ucinewgame", 10) == 0){
      parse_position("position startpos");
    }

    // parse UCI "go" command
    else if(strncmp(input, "go", 2) == 0){
      parse_go(input);
    }

    // parse UCI "quit" command
    else if(strncmp(input, "quit", 4) == 0){
      // exit main loop
      break;
    }

    // parse UCI "uci" command
    else if(strncmp(input, "uci", 3) == 0){
      printf("id author BBC\n");
      printf("id name Henry Li\n");
      printf("uciok\n");
    }
  }
}

int main() {
  init_all();
    // parse_fen(start_position_fen);
    // perft_test(7);

    // debug mode variable
    int debug = 1;
    
    // if debugging
    if (debug)
    {
      // parse fen
      parse_fen(tricky_position_fen);
      print_board();
      //search_position(3);
      
      // create move list instance
      moves move_list[1];
      
      // generate moves
      generate_moves(move_list);
      
      // print move scores
      print_move_scores(move_list);

      sort_moves(move_list);

      print_move_scores(move_list);
    }
    
    else{
      uci_loop();
    }

  return 0;
}
