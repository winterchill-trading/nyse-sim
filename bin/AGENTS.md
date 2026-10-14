# AGENTS.md (bin/)

The `nyse-sim` server. `main.ml` parses flags (`-gateway-port`, `-feed-port`, `-http-port`,
`-symbols`, `-noise`, `-clock`, `-start-time`, `-speed`, the `-anchor-*` family) and starts
`Server`. Venue behaviour stays in `../src`.

- The anchor flags are the only way to attach a reference price from the command line.
- Build: `opam exec --switch 5.2.0+ox -- dune build --root .. @nyse-sim/all`.
