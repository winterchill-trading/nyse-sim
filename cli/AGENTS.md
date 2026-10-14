# AGENTS.md (cli/)

`nyse-sim-cli`, a Pillar client. It is the test driver of `../scripts/e2e.sh` and uses
`Pillar_client` from `../src`.

- Keep it a thin client: protocol code belongs in `../wire`, and the venue rules are checked by
  the server, not here.
- Build: `opam exec --switch 5.2.0+ox -- dune build --root .. @nyse-sim/all`.
