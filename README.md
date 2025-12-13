# NimRepos

This repository hosts three subprojects that explore a new design for a Nim 3
compiler. Each subproject lives in its own directory and is managed via
`git subtree`, so keep changes isolated to the relevant folder when you commit.

## Layout
- `nimony/`: NJVL-based compiler implementation and tooling (builds the
  experimental `nimony` compiler).
- `nifspec/`: NIF bytecode specification documents.
- `nativenif/`: Native code backends and NIF assembler utilities.

## Prerequisites
A Nim 2.2.6 toolchain is available in this environment. Use it as the baseline
for building and testing until the Nim 3 toolchain stabilizes.

## Building nimony
1. Change into the `nimony` directory.
2. Build the toolchain with Nim 2.2.6:
   ```bash
   nim c -r src/hastur build all
   ```
   This produces the `bin/nimony` compiler and helper tools. Avoid committing
   built executables (e.g., the `src/hastur` binary); rebuild them locally when
   needed.
3. Run the sample hello program with the freshly built compiler:
   ```bash
   ./bin/nimony c -r hello.nim
   ```

## Notes
- When touching multiple subprojects, prefer separate commits per subtree to
  keep histories clean.
- Consult the documentation in each subdirectory (`doc/` or `README.md`) for
  project-specific details and design notes.
