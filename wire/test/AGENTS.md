# AGENTS.md (wire/test/)

Byte-level tests for `Pillar`, `Pillar_app` and `Xdp`, including order instructions. Each test
states its expected bytes explicitly. Promote expect output with `dune promote <file>` for the
one file that changed.

Run: `opam exec --switch 5.2.0+ox -- dune build --root .. @nyse-sim/runtest`.
