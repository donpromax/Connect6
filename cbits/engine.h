/* Connect6 search engine — C core called from Haskell via FFI.
 *
 * The board is row-major, n*n int8 cells: 0 = empty, 1 = Black, 2 = White.
 * c6_choose_moves writes the chosen cell indices (row*n + col) into `out`
 * (length >= stones) and returns how many it wrote.
 */
#ifndef CONNECT6_ENGINE_H
#define CONNECT6_ENGINE_H

#include <stdint.h>

int c6_choose_moves(const int8_t *board, int n, int winlen,
                    int player, int stones, int *out);

#endif /* CONNECT6_ENGINE_H */
