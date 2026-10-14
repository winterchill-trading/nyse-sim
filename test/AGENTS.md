# AGENTS.md (test/)

Venue, engine, feed and anchoring tests (expect tests and Quickcheck properties).

- Tests that need a clock use `Time_source.read_only` in APIs; keep the original source to advance.
- Do not use `Process.run_expect_no_output_exn`; it is a deprecation error on the OxCaml Async.
  Use `Unix.system_exn` in tests.
- Promote expect output with `dune promote <file>` for the one file that changed.

Run: `opam exec --switch 5.2.0+ox -- dune build --root .. @nyse-sim/runtest`.
