#include <stdlib.h>
#include <iostream>
#include <cstdint>

typedef std::uint64_t U64;

void print_bitboard(U64 board) {
  for (int rank = 0; rank < 8; rank++)
  {
    for (int file = 0; file < 8; file++)
    {
      std::cout << " " << rank * 8 + file << " "; 
    }   
    std::cout << "\n";
  }
}

int main() {
  U64 sample {42};
  print_bitboard(sample);
  return 0;
}
