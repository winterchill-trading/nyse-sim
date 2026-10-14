# AGENTS.md

Guide for AI coding agents working in this repository. Read this first; the subdirectory
`AGENTS.md` files add detail for `src/`, `wire/`, `bin/`, `cli/`, `scripts/` and `test/`.
The workspace-wide guide is `../oxcaml-claude-skills/AGENTS.md`.

## Purpose

`nyse-sim` is a local simulator of an NYSE Pillar equities venue: Pillar Gateway binary order
entry and the XDP Integrated Feed over TCP, built on the venue-neutral `exchange-core-engine`.
Prices are synthetic: noise traders around a random walk, optionally anchored to a reference.
It is a development target, not a replica of the exchange.

## Layout

| Path | Holds |
| --- | --- |
| `src/` | The `nyse-sim` library (`Nyse_sim`): `Venue`, `Listing`, `Anchoring`, `Market_event`, `Feed`, `Gateway`, `Session_handler`, `Framing`, `Server`, `Pillar_client` |
| `wire/` | The `nyse-sim.wire` library (`Nyse_sim_wire`): `Pillar` session layer, `Pillar_app` messages, `Xdp` packets |
| `wire/test/` | Byte-level tests of the wire formats |
| `bin/` | The `nyse-sim` server executable |
| `cli/` | `nyse-sim-cli`, a client used by the end-to-end script |
| `scripts/` | `e2e.sh` (end to end through the CLI) and `anchor-demo.sh` (anchored listings) |
| `test/` | Venue, engine, feed and anchoring tests |
| `.github/` | CI workflow |

## Build, test, run

Inside the workspace (sibling checkouts and the shared `5.2.0+ox` switch are required):

```sh
opam exec --switch 5.2.0+ox -- dune build --root .. @nyse-sim/all
opam exec --switch 5.2.0+ox -- dune build --root .. @nyse-sim/runtest
opam exec --switch 5.2.0+ox -- dune build --root .. @nyse-sim/doc
```

Parallel work: prefix the commands with `DUNE_BUILD_DIR=_build-nyse-sim`.

Run the server: default ports are gateway 4100, feed 4101 and HTTP 4102 (`GET /` shows the venue
summary, `/ws` the JSON feed). Clock modes: `always-open`, `real` (New York time with the 2026
holiday calendar) and `accelerated`.

Standalone: the sibling packages are not in the `winterchill` opam repository, so pin them from
GitHub in the order of `.github/workflows/ci.yml` first.

## Conventions

- Every module has an `.mli`. Code is `open! Core`, with labelled arguments and ppx_jane.
- Prices are at scale 8, the units of `Exchange_core.Price`. Do not convert them through floats.
- Venue rules (validation, session, LULD bands, pauses, short sale, auctions) live in `Venue`.
  Engine matching stays in `exchange-core-engine`.
- Formatting: `opam exec --switch 5.2.0+ox -- dune build --root .. @nyse-sim/fmt --auto-promote`.
  Never run `dune fmt` at the workspace root.

## Dependencies and dependents

- Depends on `fixed-decimal`, `exchange-core`, `exchange-core-engine`, `order-book`, `parquet`,
  `market-data`, `price-anchor` and `equities-core`.
- CI pin order (`ci.yml`): `fixed-decimal`, `exchange-core`, `order-book`,
  `exchange-core-engine` (same URL as `exchange-core`), `parquet`, `market-data`,
  `price-anchor`, `equities-core`.

## CI

`ci.yml` runs the `ox-setup` action with the eight pins above, then `dune build`, `dune test` and
the shared `obj-magic` workflow. Check a run with `gh run list --limit 1`, then at most one
`gh run watch <id> --exit-status`.

## Pitfalls

- NYSE Pillar puts Side at bits 60 to 63 of a 64-bit field, which OCaml ints cannot hold. Use
  `Int64`. `Bigstring.set_int64_le` sign-extends bit 62.
- Noise orders go through `Venue.with_engine` and skip the LULD checks, so an anchored walk can
  trade outside the bands. The anchor target is clamped 30 bps inside them.
- `Market_sim`'s half-life counts noise ticks on the wall clock, while series points follow venue
  time. On an accelerated clock the walk lags.
- `Order_book.iter` takes an `@ local` callback and needs `[@nontail]` at the call site.
- Inside `[%sexp ...]`, a field access must be parenthesised: `[%sexp ((f x).field : T)]`.
- `Time_source.create` is read-write; pass `Time_source.read_only ts` to APIs and keep the
  original to advance it.

## Where to read more

- `README.md`: layout, running, price anchor, fidelity, end-to-end, limitations.
- `../specs/repos/nyse-sim.md`, `../specs/design/price-anchor.md` and `../specs/design/simulators.md`.
- `../oxcaml-claude-skills/GOTCHAS.md`, section "TradFi simulators (nyse-sim, cboe-sim, cme-sim)".

## Never do

- Install or upgrade opam packages that change the compiler, `core`, `base`, `async` or
  `ppx_jane` on the shared switch. Run `opam install --switch 5.2.0+ox --dry-run <pkg>` first
  and stop if the plan touches any of them.
- Publish, tag or release, or point the simulator at a live exchange.
- Print secrets, or poll CI in a loop.
