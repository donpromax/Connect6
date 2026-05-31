# Connect6 (六子棋)

Connect6 in Haskell with **three front-ends** over one shared, pure game core:
a plain CLI, a full-screen colored terminal UI, and a graphical window.

Connect6 is played on a Go board (19×19 here). **Black moves first and places a
single stone**; from then on **each player places two stones per turn**. The
first player to form an unbroken line of **six** stones — horizontally,
vertically, or diagonally — wins.

| Front-end | Executable | Looks like | Dependencies |
|-----------|-----------|------------|--------------|
| Terminal UI (recommended) | `connect6-tui` | Colored board, arrow-key cursor | GHC boot libs only |
| Plain CLI | `connect6` | Scrolling text prompts | GHC boot libs only |
| Graphical window | `connect6-gui` | Gloss/OpenGL window, mouse clicks | `gloss` (opt-in, `-fgui`) |

```
   1 2 3 4 5 6 7 8 9 ...        connect6-tui
 9 · · · · · · · · · ...        @ = Black (you)
10 · · · · · · · · @ ...        O = White (AI)
11 · · · · · · · · · ...        cursor highlighted, last move flagged
```

## Quick start (macOS / Linux)

You need **GHC** and **cabal**. The terminal and CLI front-ends use only GHC
boot libraries, so they build with no package downloads.

### Install the toolchain on macOS

The simplest route is [GHCup](https://www.haskell.org/ghcup/):

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
# then restart your shell, or: source ~/.ghcup/env
```

Or via Homebrew:

```bash
brew install ghcup        # then: ghcup install ghc && ghcup install cabal
```

### Build & play

```bash
cd Connect6

cabal run connect6-tui    # full-screen terminal UI  (recommended)
cabal run connect6        # plain CLI
cabal test                # run the test suite
```

`cabal build` compiles the core, CLI, and TUI; the graphical GUI is off by
default (see below).

## Controls

### Terminal UI (`connect6-tui`)

| Action | Keys |
|--------|------|
| Move cursor | Arrow keys, `WASD`, or `hjkl` |
| Place stone | `Space` / `Enter` |
| Undo last move | `u` |
| Difficulty (new game) | `1` easy / `2` medium / `3` hard |
| New game | `r` |
| Quit | `q` |

Play White (move second) with `cabal run connect6-tui -- --white`, and pick a
starting difficulty with `--easy` / `--medium` / `--hard` (default medium). The
cursor tile is highlighted; the most recent stones are flagged in orange. Needs
an interactive terminal (macOS Terminal and iTerm2 both work).

### Plain CLI (`connect6`)

You choose a side and a difficulty (Easy / Medium / Hard) at the start. Enter
moves as two 1-indexed numbers, `row col`, e.g. `10 10` (commas also accepted),
or type `u` to **undo** your last move. You are prompted once per stone.

### Graphical window (`connect6-gui`)

Click an intersection to place a stone. `U` undoes your last move; `1`/`2`/`3`
start a new game on easy/medium/hard; `R` restarts; `B`/`W` start a new game as
Black/White. Start at a difficulty with `--easy` / `--medium` / `--hard`.

The AI runs in a background thread, so your stone appears instantly and the
window stays responsive (showing an animated "AI is thinking…") while it
searches. Stone placements play a short sound via macOS's `afplay` (silently
skipped on systems without it). Built with `-threaded`; the engine FFI call is
`safe` so the search never blocks the render loop.

## The graphical GUI (Gloss)

The window front-end uses [Gloss](https://hackage.haskell.org/package/gloss),
which renders via OpenGL/GLUT. It is **opt-in** so the rest of the project never
needs those dependencies:

```bash
cabal update                              # once, to fetch the package index
cabal run -fgui connect6-gui              # build & launch the window
```

**macOS note:** GLUT ships as a system framework with macOS, so no Homebrew
package is required — `cabal run -fgui connect6-gui` builds out of the box.
(On Linux you need the GL/GLUT dev headers, e.g.
`sudo apt-get install freeglut3-dev libgl1-mesa-dev libglu1-mesa-dev`.)

### Reproducible builds with Stack

A `stack.yaml` pinned to LTS 16.31 (GHC 8.8.4) is included, which bundles a
compatible `gloss`:

```bash
stack run connect6-tui                       # terminal UI
stack run --flag connect6:gui connect6-gui   # graphical window
```

## Architecture

A single pure, UI-agnostic core is shared by every front-end:

```
src/Connect6/          core game logic (gloss-free, boot libs only)
├── Types.hs           Player, Cell, Board, GameConfig, GameState, Outcome
├── Board.hs           immutable board, placement, win detection
├── Game.hs            turn rules & state transitions (applyMove)
├── AI.hs              pure-Haskell reference AI (forcing rules + pair search)
├── Engine.hs          FFI binding to the C search engine
├── Render.hs          CLI board renderer
└── Input.hs           parse/validate CLI moves

cbits/engine.c         C alpha-beta search engine (the front-ends' AI)

app/Main.hs            plain CLI loop
app-tui/               terminal UI:  Tui.hs (pure render + keys) + Main.hs (IO loop)
app-gui/               graphical UI: Layout.hs (geometry) + Main.hs (Gloss)
test/Spec.hs           dependency-free test suite
```

Because the core is pure and the AI (`chooseMoves`) is a pure function, every
front-end resolves the AI's turn synchronously and shares identical rules.

### The AI

The front-ends play through a **C search engine** (`cbits/engine.c`) called from
Haskell via the FFI (`Connect6.Engine`). Keeping the hot search loop in C lets it
search several plies deep within an interactive time budget; C compiles with the
toolchain GHC already ships, so no extra dependency is needed.

The engine combines forcing rules with a **negamax alpha-beta search** that
understands the two-stones-per-turn rule (the side to move only flips after both
stones are placed):

1. **Win now.** If a stone completes six, play it.
2. **VCF (hard only).** A forced-win prover that explores **only forcing moves**
   (those that make a "four"), so it can prove a guaranteed kill cheaply — e.g. a
   turn that creates three simultaneous winning points the opponent's two stones
   can't all block. It is conservative: it never claims a win that isn't forced.
3. **Alpha-beta + iterative deepening**, sped up by a **Zobrist transposition
   table** (caches searched positions and orders moves by the cached best move,
   so the same budget searches deeper). Leaves use a whole-board threat
   evaluation split into **offense** and **defense**; a line the opponent can
   complete is scored as a loss, so the search blocks every immediate threat
   (both stones when two must be blocked) while building its own.

**Difficulty** scales the search: *Easy* (shallow, no planning), *Medium*
(deeper), *Hard* (deepest + VCF). A pure-Haskell reference AI (`Connect6.AI`,
forcing rules + a one-turn pair search) is kept for tests and as a fallback.

## Tests

`test/Spec.hs` is a dependency-free suite (PASS/FAIL lines, non-zero exit on
failure) covering win detection, immutable placement, bounds handling, the
opening/normal turn rules, and key AI behaviours (completing a win, blocking a
threat, rewarding adjacency). Run with `cabal test`.
