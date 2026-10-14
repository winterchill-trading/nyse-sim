# AGENTS.md (scripts/)

- `e2e.sh` starts the simulator and drives it through `nyse-sim-cli`, end to end.
- `anchor-demo.sh` runs anchored listings: AAPL follows a fixed price, IBM a series file that
  ramps up 3% over ten venue minutes. The accelerated clock starts at 09:28 New York time, so
  the anchors wait through the pre-market and the opening auction before they pull the book.

Both scripts must pass `shellcheck`. They use local ports only.
