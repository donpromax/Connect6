/* Connect6 search engine.
 *
 *   - Forcing shortcut: if a single stone completes six, play it.
 *   - VCF (hard only): a forcing-move-only prover that looks for a guaranteed
 *     win (a turn that creates 3+ simultaneous winning points the opponent's two
 *     stones cannot all block). Conservative: it never claims a non-win.
 *   - Otherwise a negamax alpha-beta search, aware of the two-stones-per-turn
 *     rule (the side to move only flips after both stones are placed), sped up by
 *     a Zobrist transposition table and iterative deepening under a node budget.
 *
 * Difficulty (`level`) scales search depth/budget and enables VCF on hard.
 */
#include "engine.h"
#include <string.h>

#define MAXN        19
#define MAXCELLS    (MAXN * MAXN)
#define WIN_SCORE   1000000000
#define EVAL_CAP    250000000          /* eval clamp, well below WIN_SCORE      */
#define INF         2000000000

#define VCF_BRANCH  14                 /* forcing moves examined per VCF node   */
#define VCF_BUDGET  300000L

static const int DR4[4] = {0, 1, 1, 1};
static const int DC4[4] = {1, 0, 1, -1};

/* Difficulty presets. */
typedef struct { int branch, maxDepth; long budget; int vcf, vcfDepth; } Preset;
static const Preset PRESETS[3] = {
  /* easy   */ { 8,  2,     30000L, 0, 0 },
  /* medium */ { 10, 6,    250000L, 0, 0 },
  /* hard   */ { 12, 8,    900000L, 1, 8 },
};

typedef struct {
  int      n, winlen, branch;
  int8_t   b[MAXCELLS];
  long     nodes, budget;
  int      aborted, rootDepth;
  uint64_t hash;
} G;

static inline int    ON(const G *g, int r, int c) {
  return r >= 0 && r < g->n && c >= 0 && c < g->n;
}
static inline int8_t GET(const G *g, int r, int c) { return g->b[r * g->n + c]; }
static inline int    other(int p) { return p == 1 ? 2 : 1; }

/* ---- Zobrist hashing --------------------------------------------------- */

static uint64_t ZOB[3][MAXCELLS];
static uint64_t SIDE[3];
static uint64_t STK[4];
static int      zob_ready = 0;

static uint64_t splitmix64(uint64_t *s) {
  uint64_t z = (*s += 0x9E3779B97F4A7C15ULL);
  z = (z ^ (z >> 30)) * 0xBF58476D1CE4E5B9ULL;
  z = (z ^ (z >> 27)) * 0x94D049BB133111EBULL;
  return z ^ (z >> 31);
}
static void zob_init(void) {
  if (zob_ready) return;
  uint64_t s = 0x0123456789ABCDEFULL;   /* fixed seed -> reproducible */
  for (int p = 1; p < 3; p++) for (int i = 0; i < MAXCELLS; i++) ZOB[p][i] = splitmix64(&s);
  for (int p = 0; p < 3; p++) SIDE[p] = splitmix64(&s);
  for (int i = 0; i < 4; i++) STK[i] = splitmix64(&s);
  zob_ready = 1;
}

static inline void put(G *g, int cell, int player) {
  g->b[cell] = (int8_t) player;
  g->hash ^= ZOB[player][cell];
}
static inline void take(G *g, int cell, int player) {
  g->b[cell] = 0;
  g->hash ^= ZOB[player][cell];
}
static uint64_t board_hash(const G *g) {
  uint64_t h = 0;
  for (int i = 0; i < g->n * g->n; i++) { int v = g->b[i]; if (v) h ^= ZOB[v][i]; }
  return h;
}

/* ---- Transposition table ----------------------------------------------- */

#define TT_BITS  20
#define TT_SIZE  (1 << TT_BITS)
#define TT_MASK  (TT_SIZE - 1)
#define F_EXACT  0
#define F_LOWER  1
#define F_UPPER  2

typedef struct { uint64_t key; int value; short depth; unsigned char flag; int best; } TTEntry;
static TTEntry TT[TT_SIZE];

static inline uint64_t node_key(const G *g, int toPlace, int stonesLeft) {
  return g->hash ^ SIDE[toPlace] ^ STK[stonesLeft];
}

/* ---- Board primitives -------------------------------------------------- */

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

static int wweight(int winlen, int k) {
  if (k <= 0)          return 0;
  if (k >= winlen)     return 10000000;
  if (k == winlen - 1) return 100000;
  if (k == winlen - 2) return 1000;
  if (k == winlen - 3) return 100;
  if (k == winlen - 4) return 10;
  return 1;
}

/* Whole-board value from `root`'s perspective, clamped below WIN_SCORE. */
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
      int cr = (root == 1) ? nb : nw, co = (root == 1) ? nw : nb;
      if (co == 0) sroot += wweight(w, cr);
      if (cr == 0) sopp  += wweight(w, co);
    }
  }
  long v = sroot - sopp;
  if (v >  EVAL_CAP) v =  EVAL_CAP;
  if (v < -EVAL_CAP) v = -EVAL_CAP;
  return (int) v;
}

/* Ordering value of an empty cell for `player`: offense gained + defense. */
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
  char nearby[MAXCELLS];
  memset(nearby, 0, (size_t) n * n);
  for (int r = 0; r < n; r++) for (int c = 0; c < n; c++) {
    if (GET(g, r, c) == 0) continue;
    any = 1;
    for (int dr = -2; dr <= 2; dr++) for (int dc = -2; dc <= 2; dc++) {
      int rr = r + dr, cc = c + dc;
      if (ON(g, rr, cc) && GET(g, rr, cc) == 0) nearby[rr * n + cc] = 1;
    }
  }
  if (!any) { out[0] = (n / 2) * n + (n / 2); return 1; }
  for (int i = 0; i < n * n; i++) if (nearby[i]) out[cnt++] = i;
  return cnt;
}

/* Bring the top `limit` cells (by cell_score for `player`) to the front. */
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

/* ---- Negamax alpha-beta with transposition table ----------------------- */

/* Value from `toPlace`'s perspective. Same perspective/window for the second
 * stone of a turn; negate and flip the window when the turn passes. */
static int negamax(G *g, int toPlace, int stonesLeft, int depth, int alpha, int beta) {
  if (g->nodes++ > g->budget) { g->aborted = 1; return eval_board(g, toPlace); }
  if (depth <= 0) return eval_board(g, toPlace);

  int alphaOrig = alpha;
  uint64_t key = node_key(g, toPlace, stonesLeft);
  TTEntry *e = &TT[key & TT_MASK];
  int ttBest = -1;
  if (e->key == key) {
    ttBest = e->best;
    if (e->depth >= depth) {
      if (e->flag == F_EXACT) return e->value;
      else if (e->flag == F_LOWER) { if (e->value > alpha) alpha = e->value; }
      else                         { if (e->value < beta)  beta  = e->value; }
      if (alpha >= beta) return e->value;
    }
  }

  int cand[MAXCELLS];
  int m = gen_candidates(g, cand);
  order_top(g, toPlace, cand, m, g->branch);
  if (ttBest >= 0)
    for (int i = 0; i < m; i++)
      if (cand[i] == ttBest) { int t = cand[0]; cand[0] = cand[i]; cand[i] = t; break; }

  int limit = m < g->branch ? m : g->branch;
  int best = -INF, bestCell = cand[0];
  for (int i = 0; i < limit; i++) {
    int cell = cand[i], r = cell / g->n, c = cell % g->n, val;
    if (makes_win(g, toPlace, r, c)) {
      val = WIN_SCORE - (g->rootDepth - depth);
    } else {
      put(g, cell, toPlace);
      if (stonesLeft - 1 > 0) val =  negamax(g, toPlace, stonesLeft - 1, depth - 1, alpha, beta);
      else                    val = -negamax(g, other(toPlace), 2, depth - 1, -beta, -alpha);
      take(g, cell, toPlace);
    }
    if (val > best) { best = val; bestCell = cell; }
    if (best > alpha) alpha = best;
    if (alpha >= beta || g->aborted) break;
  }

  if (!g->aborted) {
    unsigned char flag = best <= alphaOrig ? F_UPPER : best >= beta ? F_LOWER : F_EXACT;
    e->key = key; e->value = best; e->depth = (short) depth; e->flag = flag; e->best = bestCell;
  }
  return best;
}

/* Root: best stone for `root` to place now, writing it to *bestCell. */
static int root_search(G *g, int root, int stonesLeft, int depth, int *bestCell) {
  uint64_t key = node_key(g, root, stonesLeft);
  TTEntry *e = &TT[key & TT_MASK];
  int ttBest = (e->key == key) ? e->best : -1;

  int cand[MAXCELLS];
  int m = gen_candidates(g, cand);
  order_top(g, root, cand, m, g->branch);
  if (ttBest >= 0)
    for (int i = 0; i < m; i++)
      if (cand[i] == ttBest) { int t = cand[0]; cand[0] = cand[i]; cand[i] = t; break; }

  int limit = m < g->branch ? m : g->branch;
  int best = -INF, alpha = -INF;
  *bestCell = cand[0];
  for (int i = 0; i < limit; i++) {
    int cell = cand[i], r = cell / g->n, c = cell % g->n, val;
    if (makes_win(g, root, r, c)) {
      val = WIN_SCORE - (g->rootDepth - depth);
    } else {
      put(g, cell, root);
      if (stonesLeft - 1 > 0) val =  negamax(g, root, stonesLeft - 1, depth - 1, alpha, INF);
      else                    val = -negamax(g, other(root), 2, depth - 1, -INF, -alpha);
      take(g, cell, root);
    }
    if (val > best) { best = val; *bestCell = cell; }
    if (best > alpha) alpha = best;
    if (g->aborted) break;
  }
  return best;
}

static void two_stones(G *g, int root, int depth, int *m1, int *m2) {
  root_search(g, root, 2, depth, m1);
  int cell = *m1, r = cell / g->n, c = cell % g->n;
  if (makes_win(g, root, r, c)) {
    int cand[MAXCELLS];
    int cnt = gen_candidates(g, cand);
    *m2 = (cand[0] == cell && cnt > 1) ? cand[1] : cand[0];
    return;
  }
  put(g, cell, root);
  root_search(g, root, 1, depth - 1, m2);
  take(g, cell, root);
}

/* ---- VCF: forced-win prover (forcing moves only) ----------------------- */

static long vcf_nodes;

/* Empty cells where `player` would immediately win. */
static int winning_points(const G *g, int player, int *out) {
  int cand[MAXCELLS];
  int m = gen_candidates(g, cand), cnt = 0;
  for (int i = 0; i < m; i++)
    if (makes_win(g, player, cand[i] / g->n, cand[i] % g->n)) out[cnt++] = cand[i];
  return cnt;
}

/* Empty cells that, once placed, give `player` a new winning point (a "four"). */
static int four_moves(G *g, int player, int *out) {
  int before[MAXCELLS];
  int nb = winning_points(g, player, before);
  int cand[MAXCELLS];
  int m = gen_candidates(g, cand), cnt = 0;
  for (int i = 0; i < m; i++) {
    int after[MAXCELLS];
    put(g, cand[i], player);
    int na = winning_points(g, player, after);
    take(g, cand[i], player);
    if (na > nb) out[cnt++] = cand[i];
  }
  return cnt;
}

static int vcf(G *g, int attacker, int depthTurns);

/* After the attacker has placed a forcing pair, is the win secured/forced? */
static int vcf_after_attacker(G *g, int attacker, int depthTurns) {
  int wp[MAXCELLS];
  int nw = winning_points(g, attacker, wp);
  if (nw >= 3) return 1;          /* two defender stones cannot block three     */
  if (nw != 2) return 0;          /* be conservative: only the unique forced     */
                                  /* two-block reply is provably forced          */
  /* Soundness: if the defender can win first, this is not a forced win. */
  int dwp[MAXCELLS];
  if (winning_points(g, other(attacker), dwp) >= 1) return 0;
  /* Defender must spend both stones blocking the two winning points. */
  put(g, wp[0], other(attacker));
  put(g, wp[1], other(attacker));
  int res = vcf(g, attacker, depthTurns - 1);
  take(g, wp[1], other(attacker));
  take(g, wp[0], other(attacker));
  return res;
}

/* Can `attacker` (to move, two stones) force a win within depthTurns? */
static int vcf(G *g, int attacker, int depthTurns) {
  if (vcf_nodes++ > VCF_BUDGET) return 0;
  int wp[MAXCELLS];
  if (winning_points(g, attacker, wp) >= 1) return 1;   /* play it and win       */
  if (depthTurns <= 0) return 0;

  int fm[MAXCELLS];
  int nf = four_moves(g, attacker, fm);
  int limit = nf < VCF_BRANCH ? nf : VCF_BRANCH;
  for (int i = 0; i < limit; i++)
    for (int j = i + 1; j < nf && j < limit; j++) {
      put(g, fm[i], attacker);
      put(g, fm[j], attacker);
      int res = vcf_after_attacker(g, attacker, depthTurns);
      take(g, fm[j], attacker);
      take(g, fm[i], attacker);
      if (res) return 1;
    }
  return 0;
}

/* Find the attacker's first forcing pair of a winning VCF line, if any. */
static int vcf_find(G *g, int attacker, int depthTurns, int *pa, int *pb) {
  vcf_nodes = 0;
  int fm[MAXCELLS];
  int nf = four_moves(g, attacker, fm);
  int limit = nf < VCF_BRANCH ? nf : VCF_BRANCH;
  for (int i = 0; i < limit; i++)
    for (int j = i + 1; j < nf && j < limit; j++) {
      put(g, fm[i], attacker);
      put(g, fm[j], attacker);
      int res = vcf_after_attacker(g, attacker, depthTurns);
      take(g, fm[j], attacker);
      take(g, fm[i], attacker);
      if (res) { *pa = fm[i]; *pb = fm[j]; return 1; }
    }
  return 0;
}

/* ---- Entry point ------------------------------------------------------- */

int c6_choose_moves(const int8_t *board, int n, int winlen,
                    int player, int stones, int level, int *out) {
  if (n > MAXN) return 0;
  zob_init();
  if (level < 0) level = 0;
  if (level > 2) level = 2;
  Preset ps = PRESETS[level];

  G g;
  g.n = n; g.winlen = winlen; g.branch = ps.branch;
  memcpy(g.b, board, (size_t) n * n);
  g.hash = board_hash(&g);

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

  /* VCF forced-win prover (hard only). */
  if (ps.vcf && stones >= 2) {
    int a = -1, b = -1;
    if (vcf_find(&g, player, ps.vcfDepth, &a, &b)) { out[0] = a; out[1] = b; return 2; }
  }

  /* Alpha-beta with iterative deepening; keep the deepest completed result. */
  memset(TT, 0, sizeof(TT));
  if (stones <= 1) {
    int saved = -1, cell;
    for (int d = 2; d <= ps.maxDepth; d += 2) {
      g.nodes = 0; g.aborted = 0; g.rootDepth = d; g.budget = ps.budget;
      root_search(&g, player, 1, d, &cell);
      if (!g.aborted || saved < 0) saved = cell;
      if (g.aborted) break;
    }
    out[0] = saved;
    return 1;
  } else {
    int s1 = -1, s2 = -1, a, b;
    for (int d = 2; d <= ps.maxDepth; d += 2) {
      g.nodes = 0; g.aborted = 0; g.rootDepth = d; g.budget = ps.budget;
      two_stones(&g, player, d, &a, &b);
      if (!g.aborted || s1 < 0) { s1 = a; s2 = b; }
      if (g.aborted) break;
    }
    out[0] = s1; out[1] = s2;
    return 2;
  }
}
