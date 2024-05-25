#include <stdlib.h>
#include <iostream>
#include <cstdint>

typedef std::uint64_t U64;
U64 bishop_attack_mask(int square);
U64 generate_bishop_attacks_slow(int square, U64 blockers);
U64 generate_rook_attacks_slow(int square, U64 blockers);
U64 rook_attack_mask(int square);
U64 set_occupancy(int index, U64 attack_mask, int num_bits_in_mask);
