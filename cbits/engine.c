/* Connect6 search engine.
 *
 * Strategy:
 *   - Forcing shortcut: if a single stone completes six, play it.
 *   - Otherwise, an alpha-beta search that is aware of the two-stones-per-turn
 *     rule (the side to move only flips after both stones are placed). Search
 *     depth is measured in stones; iterative deepening runs deeper until a node
 *     budget is hit, so moves stay interactive. Leaves use a window-based threat
 *     evaluation. Because a line where the opponent can complete six scores as a
 *     loss, the search blocks every immediate threat (spending both stones when
 *     two must be blocked) and builds its own threats.
 */
#include "engine.h"
#include <string.h>

#define MAXN       19
#define MAXCELLS   (MAXN * MAXN)
#define WIN_SCORE  1000000000
#define NEG_INF    (-2000000000)
#define POS_INF    ( 2000000000)

#define BRANCH     10          /* candidate stones examined per node          */
#define MAX_DEPTH  6           /* iterative-deepening ceiling, in stones       */
#define NODE_BUDGET 350000L    /* search aborts past this many nodes           */

static const int DR4[4] = {0, 1, 1, 1};
static const int DC4[4] = {1, 0, 1, -1};

typedef struct {
  int     n, winlen;
  int8_t  b[MAXCELLS];
  long    nodes, budget;
  int     aborted;
  int     rootDepth;          /* depth of the current ID iteration, in stones */
} G;

static inline int  ON(const G *g, int r, int c) {
  return r >= 0 && r < g->n && c >= 0 && c < g->n;
}
static inline int8_t GET(const G *g, int r, int c) { return g->b[r * g->n + c]; }
static inline void   SET(G *g, int r, int c, int8_t v) { g->b[r * g->n + c] = v; }
static inline int    other(int p) { return p == 1 ? 2 : 1; }

/* Would placing `player` at empty (r,c) complete a run of winlen? */
static int makes_win(const G *g, int player, int r, int c) {
  for (int d = 0; d < 4; d++) {
    int dr = DR4[d], dc = DC4[d], cnt = 1;
    int rr = r + dr, cc = c + dc;
    while (ON(g, rr, cc) && GET(g, rr, cc) == player) { cnt++; rr += dr; cc += dc; }
    rr = r - dr; cc = c - dc;
    while (ON(g, rr, cc) && GET(g, rr, cc) == player) { cnt++; rr -= dr; cc -= dc; }
    if (cnt >= g->winlen) return 1;
  }
  return 0;
}

/* Weight of a clean length-winlen window holding k friendly stones. */
static int wweight(int winlen, int k) {
  if (k <= 0)          return 0;
  if (k >= winlen)     return 10000000;
  if (k == winlen - 1) return 100000;
  if (k == winlen - 2) return 1000;
  if (k == winlen - 3) return 100;
  if (k == winlen - 4) return 10;
  return 1;
}

/* Whole-board value from `root`'s perspective: own potential minus opponent's. */
static int eval_board(const G *g, int root) {
  int n = g->n, w = g->winlen;
  long sroot = 0, sopp = 0;
  for (int d = 0; d < 4; d++) {
    int dr = DR4[d], dc = DC4[d];
    for (int r = 0; r < n; r++) for (int c = 0; c < n; c++) {
      int er = r + (w - 1) * dr, ec = c + (w - 1) * dc;
      if (!ON(g, er, ec)) continue;
      int nb = 0, nw = 0, rr = r, cc = c;
      for (int k = 0; k < w; k++) {
        int8_t v = GET(g, rr, cc);
        if (v == 1) nb++; else if (v == 2) nw++;
        rr += dr; cc += dc;
      }
      int cr = (root == 1) ? nb : nw;
      int co = (root == 1) ? nw : nb;
      if (co == 0) sroot += wweight(w, cr);
      if (cr == 0) sopp  += wweight(w, co);
    }
  }
  long v = sroot - sopp;
  if (v > POS_INF) v = POS_INF;
  if (v < NEG_INF) v = NEG_INF;
  return (int) v;
}

/* Ordering value of a single empty cell for `player`: offense gained + defense. */
static int cell_score(const G *g, int player, int r, int c) {
  int opp = other(player), w = g->winlen, total = 0;
  for (int d = 0; d < 4; d++) {
    int dr = DR4[d], dc = DC4[d];
    for (int s = 0; s < w; s++) {
      int sr = r - s * dr, sc = c - s * dc;
      int er = sr + (w - 1) * dr, ec = sc + (w - 1) * dc;
      if (!ON(g, sr, sc) || !ON(g, er, ec)) continue;
      int me = 0, yo = 0, rr = sr, cc = sc;
      for (int k = 0; k < w; k++) {
        int8_t v = GET(g, rr, cc);
        if (v == player) me++; else if (v == opp) yo++;
        rr += dr; cc += dc;
      }
      if (yo == 0) total += wweight(w, me + 1) - wweight(w, me);
      if (me == 0) total += wweight(w, yo);
    }
  }
  return total;
}

/* Empty cells within Chebyshev distance 2 of a stone (centre if board empty). */
static int gen_candidates(const G *g, int *out) {
  int n = g->n, cnt = 0, any = 0;
  char near[MAXCELLS];
  memset(near, 0, (size_t) n * n);
  for (int r = 0; r < n; r++) for (int c = 0; c < n; c++) {
    if (GET(g, r, c) == 0) continue;
    any = 1;
    for (int dr = -2; dr <= 2; dr++) for (int dc = -2; dc <= 2; dc++) {
      int rr = r + dr, cc = c + dc;
      if (ON(g, rr, cc) && GET(g, rr, cc) == 0) near[rr * n + cc] = 1;
    }
  }
  if (!any) { out[0] = (n / 2) * n + (n / 2); return 1; }
  for (int i = 0; i < n * n; i++) if (near[i]) out[cnt++] = i;
  return cnt;
}

/* Move-order: bring the top `limit` cells (by cell_score) to the front. */
static void order_top(const G *g, int player, int *cand, int m, int limit) {
  int sc[MAXCELLS];
  for (int i = 0; i < m; i++)
    sc[i] = cell_score(g, player, cand[i] / g->n, cand[i] % g->n);
  int lim = m < limit ? m : limit;
  for (int i = 0; i < lim; i++) {
    int bj = i;
    for (int j = i + 1; j < m; j++) if (sc[j] > sc[bj]) bj = j;
    if (bj != i) {
      int t = sc[i]; sc[i] = sc[bj]; sc[bj] = t;
      t = cand[i]; cand[i] = cand[bj]; cand[bj] = t;
    }
  }
}

/* Value of the position from `root`'s view, with `toPlace` to drop one stone,
 * `stonesLeft` remaining in its turn, and `depth` stones of lookahead left. */
static int search(G *g, int toPlace, int stonesLeft, int depth,
                  int root, int alpha, int beta) {
  if (g->nodes++ > g->budget) { g->aborted = 1; return eval_board(g, root); }
  if (depth <= 0) return eval_board(g, root);

  int cand[MAXCELLS];
  int m = gen_candidates(g, cand);
  order_top(g, toPlace, cand, m, BRANCH);

  int maximizing = (toPlace == root);
  int best = maximizing ? NEG_INF : POS_INF;
  int limit = m < BRANCH ? m : BRANCH;

  for (int i = 0; i < limit; i++) {
    int r = cand[i] / g->n, c = cand[i] % g->n, val;
    if (makes_win(g, toPlace, r, c)) {
      int w = WIN_SCORE - (g->rootDepth - depth);   /* prefer sooner wins */
      val = (toPlace == root) ? w : -w;
    } else {
      int nP, nS;
      if (stonesLeft - 1 == 0) { nP = other(toPlace); nS = 2; }
      else                     { nP = toPlace;        nS = stonesLeft - 1; }
      SET(g, r, c, (int8_t) toPlace);
      val = search(g, nP, nS, depth - 1, root, alpha, beta);
      SET(g, r, c, 0);
    }
    if (maximizing) { if (val > best) best = val; if (best > alpha) alpha = best; }
    else            { if (val < best) best = val; if (best < beta)  beta  = best; }
    if (alpha >= beta || g->aborted) break;
  }
  return best;
}

/* Root: pick the best stone for `root` to place now, returning its value and
 * writing the cell to *bestCell. */
static int search_root(G *g, int stonesLeft, int depth, int root, int *bestCell) {
  int cand[MAXCELLS];
  int m = gen_candidates(g, cand);
  order_top(g, root, cand, m, BRANCH);

  int best = NEG_INF, alpha = NEG_INF;
  int limit = m < BRANCH ? m : BRANCH;
  *bestCell = cand[0];

  for (int i = 0; i < limit; i++) {
    int r = cand[i] / g->n, c = cand[i] % g->n, val;
    if (makes_win(g, root, r, c)) {
      val = WIN_SCORE - (g->rootDepth - depth);
    } else {
      int nP, nS;
      if (stonesLeft - 1 == 0) { nP = other(root); nS = 2; }
      else                     { nP = root;        nS = stonesLeft - 1; }
      SET(g, r, c, (int8_t) root);
      val = search(g, nP, nS, depth - 1, root, alpha, POS_INF);
      SET(g, r, c, 0);
    }
    if (val > best) { best = val; *bestCell = cand[i]; }
    if (best > alpha) alpha = best;
    if (g->aborted) break;
  }
  return best;
}

/* Pick a coordinated pair for `root` at one iterative-deepening depth. */
static void two_stones(G *g, int root, int depth, int *m1, int *m2) {
  search_root(g, 2, depth, root, m1);
  int r = *m1 / g->n, c = *m1 % g->n;
  if (makes_win(g, root, r, c)) {           /* winning first stone ends the game */
    int cand[MAXCELLS];
    int cnt = gen_candidates(g, cand);
    *m2 = (cand[0] == *m1 && cnt > 1) ? cand[1] : cand[0];
    return;
  }
  SET(g, r, c, (int8_t) root);
  search_root(g, 1, depth - 1, root, m2);
  SET(g, r, c, 0);
}

int c6_choose_moves(const int8_t *board, int n, int winlen,
                    int player, int stones, int *out) {
  if (n > MAXN) return 0;
  G g;
  g.n = n; g.winlen = winlen; g.budget = NODE_BUDGET;
  memcpy(g.b, board, (size_t) n * n);

  /* Forcing: take an immediate win if one exists. */
  int firstWin = -1, secondWin = -1;
  for (int r = 0; r < n && secondWin < 0; r++)
    for (int c = 0; c < n; c++)
      if (GET(&g, r, c) == 0 && makes_win(&g, player, r, c)) {
        if (firstWin < 0) firstWin = r * n + c;
        else { secondWin = r * n + c; break; }
      }
  if (firstWin >= 0) {
    out[0] = firstWin;
    if (stones >= 2) {
      if (secondWin >= 0) out[1] = secondWin;
      else {
        int cand[MAXCELLS];
        int cnt = gen_candidates(&g, cand);
        out[1] = (cand[0] == firstWin && cnt > 1) ? cand[1] : cand[0];
      }
    }
    return stones;
  }

  /* Iterative deepening; keep the deepest fully-completed result. */
  if (stones <= 1) {
    int saved = -1, cell;
    for (int d = 2; d <= MAX_DEPTH; d += 2) {
      g.nodes = 0; g.aborted = 0; g.rootDepth = d;
      search_root(&g, 1, d, player, &cell);
      if (!g.aborted || saved < 0) saved = cell;
      if (g.aborted) break;
    }
    out[0] = saved;
    return 1;
  } else {
    int s1 = -1, s2 = -1, a, b;
    for (int d = 2; d <= MAX_DEPTH; d += 2) {
      g.nodes = 0; g.aborted = 0; g.rootDepth = d;
      two_stones(&g, player, d, &a, &b);
      if (!g.aborted || s1 < 0) { s1 = a; s2 = b; }
      if (g.aborted) break;
    }
    out[0] = s1; out[1] = s2;
    return 2;
  }
}
