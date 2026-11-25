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
  {R, 'r'},
  {r, 'r'},
  {N, 'n'},
  {n, 'n'},
  {B, 'b'},
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

// Zobrist hashing: one random key per (piece, square), en passant square,
// castling-rights combination and side to move. A position's key is the XOR
// of the keys of everything in it.
U64 piece_keys[12][64];
U64 enpassant_keys[64];
U64 castle_keys[16];
U64 side_key;

// Zobrist key of the current position
U64 hash_key = 0ULL;

// Half-move clock for the fifty-move rule
int fifty = 0;

// Keys of every position reached so far in the game and the search,
// used to detect repetitions
U64 repetition_table[2048];
int repetition_index = 0;

void init_random_keys() {
  std::mt19937_64 rng(0x5EED1234ULL);
  for (int piece = P; piece <= k; piece++)
    for (int square = 0; square < 64; square++)
      piece_keys[piece][square] = rng();
  for (int square = 0; square < 64; square++)
    enpassant_keys[square] = rng();
  for (int i = 0; i < 16; i++)
    castle_keys[i] = rng();
  side_key = rng();
}

// Compute the Zobrist key of the current position from scratch
U64 generate_hash_key() {
  U64 key = 0ULL;
  for (int piece = P; piece <= k; piece++) {
    U64 bitboard = piece_bitboards[piece];
    while (bitboard) {
      int square = ls1b_index(bitboard);
      key ^= piece_keys[piece][square];
      bitboard &= bitboard - 1;
    }
  }
  if (en_passant_square != -1)
    key ^= enpassant_keys[en_passant_square];
  key ^= castle_keys[castle];
  if (side_to_move == black)
    key ^= side_key;
  return key;
}

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
    ((source) |               \
    ((target) << 6) |        \
    ((piece) << 12) |        \
    ((promoted) << 16) |     \
    ((capture) << 20) |      \
    ((double_push) << 21) |  \
    ((enpassant) << 22) |    \
    ((castle) << 23))

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
  fifty = 0;
  hash_key = 0ULL;
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

  // half-move clock (optional in FEN)
  fifty = 0;
  while (*fen && *fen != ' ') fen++;
  if (*fen == ' ') fifty = atoi(fen + 1);

  // populate white occupancy bitboard
  for (int piece = P; piece <= K; piece++)
      occupancy_bitboards[white] |= piece_bitboards[piece];
  
  // populate black occupancy bitboard
  for (int piece = p; piece <= k; piece++)
      occupancy_bitboards[black] |= piece_bitboards[piece];

  // init all occupancies
  occupancy_bitboards[2] |= occupancy_bitboards[white];
  occupancy_bitboards[2] |= occupancy_bitboards[black];

  // compute the position key and start a fresh game history
  hash_key = generate_hash_key();
  repetition_index = 0;
  repetition_table[0] = hash_key;
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
  // Zobrist keys
  init_random_keys();
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
    int side_copy, enpassant_copy, castle_copy, fifty_copy;                     \
    U64 hash_key_copy = hash_key;                                               \
    fifty_copy = fifty;                                                         \
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
    castle = castle_copy;                                                       \
    fifty = fifty_copy;                                                         \
    hash_key = hash_key_copy;

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

    // fifty-move rule: reset on pawn moves and captures
    if (piece == P || piece == p || capture) fifty = 0;
    else fifty++;

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

    // Recompute the position key. (Incremental updates would be faster;
    // from scratch keeps make_move simple and is cheap next to legality checks.)
    hash_key = generate_hash_key();

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
        return make_move(move, all_moves);
    
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
  nodes = 0;
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
    printf("     move: %s%s%c  nodes: %llu\n", index_to_square_name[get_move_source(move_list->moves[move_count])],
                                              index_to_square_name[get_move_target(move_list->moves[move_count])],
                                              get_move_promoted(move_list->moves[move_count]) ? promoted_pieces[get_move_promoted(move_list->moves[move_count])] : ' ',
                                              old_nodes);
  }
  auto end = std::chrono::high_resolution_clock::now();
  
  // print results
  printf("\n    Depth: %d\n", depth);
  printf("    Nodes: %llu\n", nodes);
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


// ---------------------------------------------------------------------------
// Search
// ---------------------------------------------------------------------------

#define MAX_PLY 128
#define INF 50000
#define MATE_VALUE 49000
// scores beyond this are mate scores
#define MATE_SCORE 48000

// distance from the root of the current node
int ply;

// killer moves [id][ply]: quiet moves that caused a beta cutoff at this ply
int killer_moves[2][MAX_PLY];

// history moves [piece][square]: quiet moves that raised alpha
int history_moves[12][64];

// triangular principal variation table
int pv_length[MAX_PLY];
int pv_table[MAX_PLY][MAX_PLY];

// ----- time control -----

long long start_time = 0;
long long soft_stop_time = 0;   // don't start a new iteration after this
long long hard_stop_time = 0;   // abort the search after this
bool time_set = false;
bool stopped = false;
U64 node_limit = 0;             // 0 = no limit
int current_iteration = 0;

static inline long long now_ms() {
  return std::chrono::duration_cast<std::chrono::milliseconds>(
    std::chrono::steady_clock::now().time_since_epoch()).count();
}

static inline void check_time() {
  // always finish depth 1 so there is a move to play
  if (current_iteration <= 1) return;
  if (time_set && now_ms() >= hard_stop_time) stopped = true;
  if (node_limit && nodes >= node_limit) stopped = true;
}

// ----- transposition table -----

enum { hash_exact, hash_alpha, hash_beta };
#define NO_HASH_ENTRY 100000

typedef struct {
  U64 key;
  int move;
  int score;
  short depth;
  short flag;
} tt_entry;

tt_entry *transposition_table = NULL;
U64 tt_entries = 0;

void init_tt(int megabytes) {
  free(transposition_table);
  U64 bytes = (U64)megabytes * 1024 * 1024;
  // round down to a power of two so we can index with a mask
  tt_entries = 1;
  while (tt_entries * 2 * sizeof(tt_entry) <= bytes) tt_entries *= 2;
  transposition_table = (tt_entry *)calloc(tt_entries, sizeof(tt_entry));
}

void clear_tt() {
  memset(transposition_table, 0, tt_entries * sizeof(tt_entry));
}

// Returns a score if the entry is deep enough to cut, else NO_HASH_ENTRY.
// Always reports the stored best move through *tt_move.
static inline int probe_tt(int alpha, int beta, int depth, int *tt_move) {
  tt_entry *entry = &transposition_table[hash_key & (tt_entries - 1)];
  if (entry->key != hash_key) return NO_HASH_ENTRY;
  *tt_move = entry->move;
  if (entry->depth < depth) return NO_HASH_ENTRY;

  // mate scores are stored relative to the node, convert back to the root
  int score = entry->score;
  if (score < -MATE_SCORE) score += ply;
  if (score > MATE_SCORE) score -= ply;

  if (entry->flag == hash_exact) return score;
  if (entry->flag == hash_alpha && score <= alpha) return alpha;
  if (entry->flag == hash_beta && score >= beta) return beta;
  return NO_HASH_ENTRY;
}

static inline void store_tt(int score, int depth, int flag, int move) {
  tt_entry *entry = &transposition_table[hash_key & (tt_entries - 1)];
  if (score < -MATE_SCORE) score -= ply;
  if (score > MATE_SCORE) score += ply;
  entry->key = hash_key;
  entry->move = move;
  entry->score = score;
  entry->depth = depth;
  entry->flag = flag;
}

// ----- move ordering -----

// Order: TT move, captures (MVV-LVA), killers, then history
static inline int score_move(int move, int tt_move)
{
    if (move == tt_move) return 30000;

    // score capture move
    if (get_move_capture(move))
    {
        // init target piece (en passant captures leave the target empty: pawn)
        int target_piece = P;

        // pick up bitboard piece index ranges depending on side
        int start_piece, end_piece;
        if (side_to_move == white) { start_piece = p; end_piece = k; }
        else { start_piece = P; end_piece = K; }

        // loop over bitboards opposite to the current side to move
        for (int bb_piece = start_piece; bb_piece <= end_piece; bb_piece++)
        {
            if (get_bit(piece_bitboards[bb_piece], get_move_target(move)))
            {
                target_piece = bb_piece;
                break;
            }
        }

        // score move by MVV LVA lookup [source piece][target piece]
        return mvv_lva[get_move_piece(move)][target_piece] + 10000;
    }

    // score quiet move
    if (killer_moves[0][ply] == move) return 9000;
    if (killer_moves[1][ply] == move) return 8000;
    return history_moves[get_move_piece(move)][get_move_target(move)];
}

// Selection sort step: bring the best remaining move to index `start`
static inline void pick_move(moves *move_list, int *scores, int start) {
  int best = start;
  for (int i = start + 1; i < move_list->count; i++)
    if (scores[i] > scores[best]) best = i;
  if (best != start) {
    int tmp_move = move_list->moves[start];
    move_list->moves[start] = move_list->moves[best];
    move_list->moves[best] = tmp_move;
    int tmp_score = scores[start];
    scores[start] = scores[best];
    scores[best] = tmp_score;
  }
}

// Has the current position occurred before since the last irreversible move?
static inline bool is_repetition() {
  int stop = repetition_index - fifty;
  if (stop < 0) stop = 0;
  for (int i = repetition_index - 2; i >= stop; i -= 2)
    if (repetition_table[i] == hash_key)
      return true;
  return false;
}

// ----- quiescence search -----

static inline int quiescence(int alpha, int beta)
{
    if ((nodes & 1023) == 0) check_time();
    if (stopped) return 0;

    nodes++;

    if (ply >= MAX_PLY - 1) return evaluate();

    // stand pat
    int evaluation = evaluate();
    if (evaluation >= beta) return beta;
    if (evaluation > alpha) alpha = evaluation;

    moves move_list[1];
    generate_moves(move_list);

    int scores[256];
    for (int i = 0; i < move_list->count; i++)
      scores[i] = score_move(move_list->moves[i], 0);

    for (int count = 0; count < move_list->count; count++)
    {
        pick_move(move_list, scores, count);

        copy_board();
        ply++;

        // only legal captures
        if (make_move(move_list->moves[count], only_captures) == 0)
        {
            ply--;
            continue;
        }

        int score = -quiescence(-beta, -alpha);

        ply--;
        take_back();

        if (stopped) return 0;

        if (score >= beta) return beta;
        if (score > alpha) alpha = score;
    }

    return alpha;
}

// ----- main search -----

static inline bool has_non_pawn_material(int side) {
  if (side == white)
    return piece_bitboards[N] | piece_bitboards[B] | piece_bitboards[R] | piece_bitboards[Q];
  return piece_bitboards[n] | piece_bitboards[b] | piece_bitboards[r] | piece_bitboards[q];
}

// negamax alpha beta search with PVS, null move pruning and late move reductions
static inline int negamax(int alpha, int beta, int depth)
{
    pv_length[ply] = ply;

    // draw by repetition (twofold inside the tree) or fifty-move rule
    if (ply && (is_repetition() || fifty >= 100))
      return 0;

    if (ply >= MAX_PLY - 1) return evaluate();

    bool pv_node = beta - alpha > 1;

    // transposition table cutoff (not on PV nodes, so the PV stays intact)
    int tt_move = 0;
    int tt_score = probe_tt(alpha, beta, depth, &tt_move);
    if (ply && !pv_node && tt_score != NO_HASH_ENTRY)
      return tt_score;

    // is king in check
    int in_check = is_square_attacked((side_to_move == white) ? ls1b_index(piece_bitboards[K]) :
                                                                ls1b_index(piece_bitboards[k]),
                                                                side_to_move ^ 1);

    // check extension
    if (in_check) depth++;

    if (depth <= 0) return quiescence(alpha, beta);

    if ((nodes & 1023) == 0) check_time();
    if (stopped) return 0;

    nodes++;

    // null move pruning: if passing still fails high, this node is very likely a cutoff
    if (!pv_node && !in_check && ply && depth >= 3 && has_non_pawn_material(side_to_move))
    {
        copy_board();
        ply++;
        repetition_table[++repetition_index] = hash_key;

        en_passant_square = -1;
        side_to_move ^= 1;
        hash_key = generate_hash_key();

        int score = -negamax(-beta, -beta + 1, depth - 1 - 2);

        ply--;
        repetition_index--;
        take_back();

        if (stopped) return 0;
        if (score >= beta) return beta;
    }

    moves move_list[1];
    generate_moves(move_list);

    int scores[256];
    for (int i = 0; i < move_list->count; i++)
      scores[i] = score_move(move_list->moves[i], tt_move);

    int legal_moves = 0;
    int best_move_here = 0;
    int hash_flag = hash_alpha;

    for (int count = 0; count < move_list->count; count++)
    {
        pick_move(move_list, scores, count);
        int move = move_list->moves[count];

        copy_board();
        ply++;

        if (make_move(move, all_moves) == 0)
        {
            ply--;
            continue;
        }

        repetition_table[++repetition_index] = hash_key;

        int score;
        bool quiet = !get_move_capture(move) && !get_move_promoted(move);

        if (legal_moves == 0)
        {
            // full window on the first move
            score = -negamax(-beta, -alpha, depth - 1);
        }
        else
        {
            // late move reduction for quiet moves ordered late
            if (legal_moves >= 4 && depth >= 3 && !in_check && quiet)
                score = -negamax(-alpha - 1, -alpha, depth - 2);
            else
                score = alpha + 1;

            // principal variation search: null window, re-search if it beats alpha
            if (score > alpha)
            {
                score = -negamax(-alpha - 1, -alpha, depth - 1);
                if (score > alpha && score < beta)
                    score = -negamax(-beta, -alpha, depth - 1);
            }
        }

        legal_moves++;

        ply--;
        repetition_index--;
        take_back();

        if (stopped) return 0;

        if (score > alpha)
        {
            alpha = score;
            best_move_here = move;
            hash_flag = hash_exact;

            if (quiet)
                history_moves[get_move_piece(move)][get_move_target(move)] += depth * depth;

            // update the principal variation
            pv_table[ply][ply] = move;
            for (int next = ply + 1; next < pv_length[ply + 1]; next++)
                pv_table[ply][next] = pv_table[ply + 1][next];
            pv_length[ply] = pv_length[ply + 1];

            // fail-hard beta cutoff
            if (score >= beta)
            {
                store_tt(beta, depth, hash_beta, move);
                if (quiet)
                {
                    killer_moves[1][ply] = killer_moves[0][ply];
                    killer_moves[0][ply] = move;
                }
                return beta;
            }
        }
    }

    // no legal moves: checkmate or stalemate
    if (legal_moves == 0)
        return in_check ? -MATE_VALUE + ply : 0;

    store_tt(alpha, depth, hash_flag, best_move_here);
    return alpha;
}

static void print_score(int score) {
  if (score > MATE_SCORE)
    printf("score mate %d", (MATE_VALUE - score) / 2 + 1);
  else if (score < -MATE_SCORE)
    printf("score mate %d", -(MATE_VALUE + score) / 2);
  else
    printf("score cp %d", score);
}

// last completed iteration's score, used by data generation
int last_search_score = 0;

// iterative deepening driver; prints UCI info lines and the best move
int search_position(int max_depth, bool print_info = true)
{
    nodes = 0;
    stopped = false;
    memset(killer_moves, 0, sizeof(killer_moves));
    memset(history_moves, 0, sizeof(history_moves));
    memset(pv_table, 0, sizeof(pv_table));
    memset(pv_length, 0, sizeof(pv_length));

    int best = 0;
    int best_score = 0;

    for (int depth = 1; depth <= max_depth; depth++)
    {
        current_iteration = depth;
        ply = 0;
        int score = negamax(-INF, INF, depth);

        // an aborted iteration can't be trusted; keep the previous result
        if (stopped) break;

        best = pv_table[0][0];
        best_score = score;

        if (print_info)
        {
            long long elapsed = now_ms() - start_time;
            printf("info depth %d ", depth);
            print_score(score);
            printf(" nodes %llu time %lld nps %llu pv", nodes, elapsed,
                   elapsed > 0 ? nodes * 1000 / elapsed : 0ULL);
            for (int i = 0; i < pv_length[0]; i++) {
              printf(" ");
              print_move(pv_table[0][i]);
            }
            printf("\n");
        }

        // no point searching deeper once a forced mate is found
        if (best_score > MATE_SCORE || best_score < -MATE_SCORE) break;
        if (time_set && now_ms() >= soft_stop_time) break;
        if (node_limit && nodes >= node_limit) break;
    }

    last_search_score = best_score;

    if (print_info)
    {
      printf("bestmove ");
      if (best) print_move(best);
      else printf("0000");
      printf("\n");
    }
    return best;
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
      if (repetition_index < 1900)
        repetition_table[++repetition_index] = hash_key;

      // move pointer to end of current move
      while(*current_char && *current_char != ' ') {
        current_char++;
      }

      if(*current_char == '\0') break;

      // go to the next move
      current_char++;
    }
  }
}

// parse UCI "go" command
static long long go_param(const char *command, const char *name, long long fallback) {
  const char *found = strstr(command, name);
  if (!found) return fallback;
  return atoll(found + strlen(name));
}

void parse_go(char *command){
  int depth = (int)go_param(command, " depth ", MAX_PLY - 1);
  long long movetime = go_param(command, " movetime ", -1);
  long long time_left = go_param(command, side_to_move == white ? " wtime " : " btime ", -1);
  long long increment = go_param(command, side_to_move == white ? " winc " : " binc ", 0);
  long long movestogo = go_param(command, " movestogo ", 30);
  node_limit = (U64)go_param(command, " nodes ", 0);

  const long long overhead = 20;
  start_time = now_ms();
  time_set = false;

  if (movetime > 0) {
    time_set = true;
    long long budget = movetime > overhead * 2 ? movetime - overhead : movetime / 2;
    soft_stop_time = hard_stop_time = start_time + budget;
  }
  else if (time_left >= 0) {
    time_set = true;
    if (movestogo <= 0) movestogo = 30;
    long long soft = time_left / movestogo + increment * 3 / 4;
    long long hard = soft * 3;
    long long cap = time_left - overhead > 1 ? (time_left - overhead) / 2 : 1;
    if (hard > cap) hard = cap;
    if (soft > hard) soft = hard;
    soft_stop_time = start_time + soft;
    hard_stop_time = start_time + hard;
  }

  // a bare "go" (no depth, time or node limit) can't be stopped yet, so cap it
  if (!time_set && !node_limit && !strstr(command, " depth ")) depth = 8;

  search_position(depth);
}

// main UCI loop
void uci_loop(){
  // reset STDIN & STDOUT buffers
  setbuf(stdin, NULL);
  setbuf(stdout, NULL);

  // define user / GUI input buffer
  char input[2000];

  // main loop
  while(true){
    // reset user / GUI input
    memset(input, 0, sizeof(input));

    // make sure output reaches GUI
    fflush(stdout);

    // get user / GUI input; stop on EOF
    if(!fgets(input, 2000, stdin)){
      break;
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
      clear_tt();
    }

    // parse UCI "setoption name Hash value <MB>"
    else if(strncmp(input, "setoption name Hash value ", 26) == 0){
      int megabytes = atoi(input + 26);
      if (megabytes < 1) megabytes = 1;
      if (megabytes > 4096) megabytes = 4096;
      init_tt(megabytes);
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
      printf("id name Chess_Engine_Cpp\n");
      printf("id author Henry Li\n");
      printf("option name Hash type spin default 64 min 1 max 4096\n");
      printf("uciok\n");
    }

    // non-UCI debug command: print the board
    else if(strncmp(input, "d", 1) == 0 && (input[1] == '\n' || input[1] == ' ')){
      print_board();
    }

    // non-UCI debug command: "perft <depth>" on the current position
    else if(strncmp(input, "perft", 5) == 0){
      perft_test(atoi(input + 6));
    }
  }
}

// ---------------------------------------------------------------------------
// Training data generation
// ---------------------------------------------------------------------------

// Write the current position as FEN. Squares are indexed 8 * file + rank.
void board_to_fen(char *out) {
  char *c = out;
  for (int rank = 7; rank >= 0; rank--) {
    int empty = 0;
    for (int file = 0; file < 8; file++) {
      int square = 8 * file + rank;
      int found = -1;
      for (int piece = P; piece <= k; piece++)
        if (get_bit(piece_bitboards[piece], square)) { found = piece; break; }
      if (found == -1) { empty++; continue; }
      if (empty) { *c++ = '0' + empty; empty = 0; }
      *c++ = pieces_ascii[found];
    }
    if (empty) *c++ = '0' + empty;
    if (rank) *c++ = '/';
  }
  *c++ = ' ';
  *c++ = side_to_move == white ? 'w' : 'b';
  *c++ = ' ';
  if (!castle) *c++ = '-';
  if (castle & w_kside) *c++ = 'K';
  if (castle & w_qside) *c++ = 'Q';
  if (castle & b_kside) *c++ = 'k';
  if (castle & b_qside) *c++ = 'q';
  *c++ = ' ';
  if (en_passant_square == -1) *c++ = '-';
  else c += snprintf(c, 3, "%s", index_to_square_name[en_passant_square]);
  snprintf(c, 16, " %d 1", fifty);
}

static inline bool side_in_check() {
  return is_square_attacked((side_to_move == white) ? ls1b_index(piece_bitboards[K]) :
                                                      ls1b_index(piece_bitboards[k]),
                                                      side_to_move ^ 1);
}

// Number of earlier occurrences of the current position since the last irreversible move
static int repetition_count() {
  int count = 0;
  int stop = repetition_index - fifty;
  if (stop < 0) stop = 0;
  for (int i = repetition_index - 2; i >= stop; i -= 2)
    if (repetition_table[i] == hash_key) count++;
  return count;
}

static bool insufficient_material() {
  U64 majors_and_pawns = piece_bitboards[P] | piece_bitboards[p] | piece_bitboards[R] |
                         piece_bitboards[r] | piece_bitboards[Q] | piece_bitboards[q];
  if (majors_and_pawns) return false;
  int minors = count_bits(piece_bitboards[N] | piece_bitboards[n] | piece_bitboards[B] | piece_bitboards[b]);
  return minors <= 1;
}

// Play one legal random move. Returns false if there is none.
static bool play_random_move(std::mt19937_64 &rng) {
  moves move_list[1];
  generate_moves(move_list);
  int legal[256], count = 0;
  for (int i = 0; i < move_list->count; i++) {
    copy_board();
    if (make_move(move_list->moves[i], all_moves)) {
      legal[count++] = move_list->moves[i];
      take_back();
    }
  }
  if (!count) return false;
  make_move(legal[rng() % count], all_moves);
  repetition_table[++repetition_index] = hash_key;
  return true;
}

// Self-play games; writes "fen | score | result" lines where score is in
// centipawns and result is 1.0 / 0.5 / 0.0, both from White's point of view.
// Only quiet positions are kept: side to move not in check, best move not a
// capture or promotion, and no mate score.
void datagen(const char *path, int games, int soft_nodes, U64 seed) {
  FILE *out = fopen(path, "a");
  if (!out) { perror(path); return; }

  std::mt19937_64 rng(seed);
  init_tt(16);
  time_set = false;

  struct sample { char fen[100]; int score; };
  static sample samples[1024];
  U64 total = 0;
  long long started = now_ms();

  for (int game = 0; game < games; game++) {
    // random opening: 8 or 9 plies so both sides get to move first
    parse_fen(start_position_fen);
    int random_plies = 8 + (rng() & 1);
    bool ok = true;
    for (int i = 0; i < random_plies && ok; i++) ok = play_random_move(rng);
    if (!ok) { game--; continue; }

    clear_tt();
    int count = 0;
    double result = -1;
    int winning_plies = 0;

    for (int game_ply = 0; game_ply < 400; game_ply++) {
      if (fifty >= 100 || repetition_count() >= 2 || insufficient_material()) { result = 0.5; break; }

      node_limit = soft_nodes;
      int move = search_position(MAX_PLY - 1, false);
      int score = last_search_score;               // side to move's view
      int white_score = side_to_move == white ? score : -score;

      if (!move) {                                  // no legal moves
        result = side_in_check() ? (side_to_move == white ? 0.0 : 1.0) : 0.5;
        break;
      }

      // adjudicate clearly decided games
      if (abs(score) >= 2500) winning_plies++; else winning_plies = 0;
      if (winning_plies >= 6) { result = white_score > 0 ? 1.0 : 0.0; break; }

      bool quiet = !get_move_capture(move) && !get_move_promoted(move);
      if (quiet && !side_in_check() && abs(score) < MATE_SCORE && count < 1024) {
        board_to_fen(samples[count].fen);
        samples[count].score = white_score;
        count++;
      }

      make_move(move, all_moves);
      if (repetition_index < 1900) repetition_table[++repetition_index] = hash_key;
    }
    if (result < 0) result = 0.5;                   // ply cap

    for (int i = 0; i < count; i++)
      fprintf(out, "%s | %d | %.1f\n", samples[i].fen, samples[i].score, result);
    total += count;

    if ((game + 1) % 50 == 0) {
      fflush(out);
      double secs = (now_ms() - started) / 1000.0;
      fprintf(stderr, "games %d  positions %llu  %.0f pos/s\n", game + 1, total, total / secs);
    }
  }
  node_limit = 0;
  fclose(out);
}

int main(int argc, char **argv) {
  init_all();
  init_tt(64);

  // "engine perft <depth> [fen]": print the perft node count and exit
  if (argc >= 3 && strcmp(argv[1], "perft") == 0) {
    char fen[256];
    snprintf(fen, sizeof(fen), "%s", argc >= 4 ? argv[3] : start_position_fen);
    parse_fen(fen);
    nodes = 0;
    perft_driver(atoi(argv[2]));
    printf("%llu\n", nodes);
    return 0;
  }

  // "engine datagen <file> <games> <soft nodes> <seed>": self-play training data
  if (argc >= 6 && strcmp(argv[1], "datagen") == 0) {
    datagen(argv[2], atoi(argv[3]), atoi(argv[4]), strtoull(argv[5], NULL, 10));
    return 0;
  }

  uci_loop();
  return 0;
}
