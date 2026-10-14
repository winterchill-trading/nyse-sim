# AGENTS.md (src/)

The `Nyse_sim` library: the venue and its network surface.

- `Venue` applies Pillar order rules on top of `Exchange_core_engine.Exchange`. Session state,
  LULD bands, pauses and auctions belong here, not in the engine.
- `Anchoring` wraps `price-anchor` for each anchored listing. Its clamp keeps the target inside
  the LULD bands.
- `Feed` publishes XDP messages in matching order: sequence reset, time references, symbol
  mappings, heartbeats, and refresh for late joiners. JSON subscribers get the same messages.
- `Gateway` and `Session_handler` hold one TCP session each; `Framing` is the stream framing.

Build: `opam exec --switch 5.2.0+ox -- dune build --root .. @nyse-sim/all`.
