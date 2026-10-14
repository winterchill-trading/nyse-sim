# AGENTS.md (wire/)

The `nyse-sim.wire` library (`Nyse_sim_wire`): byte-level encodings with no venue logic.

- `Pillar` is the session layer (framing, sequence numbers); `Pillar_app` the gateway messages,
  with prices at scale 8; `Xdp` the feed packets and messages, little endian.
- Every format change needs a byte-level test in `test/`. Expected bytes come from the published
  layout, not from the encoder's own output.

Build: `opam exec --switch 5.2.0+ox -- dune build --root .. @nyse-sim/all`.
