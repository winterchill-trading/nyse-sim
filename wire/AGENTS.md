# AGENTS.md (wire/)

The `nyse-sim.wire` library (`Nyse_sim_wire`): byte-level encodings with no venue logic.

- `Pillar` is the session layer (framing, sequence numbers); `Pillar_app` the gateway messages,
  with prices at scale 8; `Xdp` the feed packets and messages, little endian.
- A field that spans bit 62 or 63 must be read and written as `Int64`.
- Every format change needs a byte-level test in `test/`, which holds the byte-level tests.

Build: `opam exec --switch 5.2.0+ox -- dune build --root .. @nyse-sim/all`.
