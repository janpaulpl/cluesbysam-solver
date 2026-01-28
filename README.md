# Clues Solver

SAT solver for [cluesbysam.com](https://cluesbysam.com/) puzzles.

## Build

### Dependencies

- [opam](https://opam.ocaml.org/)
- [z3](https://github.com/Z3Prover/z3)
- [gmp](https://doc.sagemath.org/html/en/reference/spkg/gmp.html)

```bash
opam init
eval $(opam env)
opam install dune z3 re ppx_deriving

dune build
```

## Create a Puzzle File

Create `puzzle.txt` with the names from today's puzzle:
```
A1, Ada, NO DESCRIPTION YET
A2, Bob, NO DESCRIPTION YET
A3, Cal, NO DESCRIPTION YET
A4, Dan, NO DESCRIPTION YET
A5, Eve, NO DESCRIPTION YET
B1, Fay, NO DESCRIPTION YET
B2, Gus, NO DESCRIPTION YET
B3, Hal, NO DESCRIPTION YET
B4, Ivy, NO DESCRIPTION YET
B5, Jan, NO DESCRIPTION YET
C1, Ken, NO DESCRIPTION YET
C2, Lea, NO DESCRIPTION YET
C3, Max, NO DESCRIPTION YET
C4, Nia, NO DESCRIPTION YET
C5, Ora, NO DESCRIPTION YET
D1, Pat, NO DESCRIPTION YET
D2, Ren, NO DESCRIPTION YET
D3, Sam, NO DESCRIPTION YET
D4, Tia, NO DESCRIPTION YET
D5, Uma, NO DESCRIPTION YET
```

Grid layout:
```
    A    B    C    D
1  A1   B1   C1   D1
2  A2   B2   C2   D2
3  A3   B3   C3   D3
4  A4   B4   C4   D4
5  A5   B5   C5   D5
```

## Example
```bash
dune exec clues_solver -- example_puzzle.txt
```

## Commands
```
add <name> <clue>    - Add a clue (e.g., add Eve I have 2 criminal neighbors)
set <name> innocent  - Mark as innocent
set <name> criminal  - Mark as criminal
deduce               - Find forced deductions
solve                - Find a solution
grid                 - Show grid
help                 - Show all commands
quit                 - Exit
```