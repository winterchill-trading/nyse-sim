#!/usr/bin/env bash
# Anchored noise traders: AAPL follows a fixed price, IBM a series file ramping up 3%
# over ten venue minutes. The accelerated clock starts at 09:28 New York time, so the
# anchors wait through the end of the pre-market and the opening auction, then pull the
# books from the open. Prints the venue summary once a second.
#
#   HTTP_PORT   HTTP port (default 4102; the gateway and feed take free ports)
#   SECONDS_    how long to watch (default 20 real seconds, at 60 venue seconds each)
#   BIN_DIR     directory holding nyse-sim (default _build/install/default/bin)
#   SKIP_BUILD  set to skip `dune build @install`
set -euo pipefail
cd "$(dirname "$0")/.."

HTTP=${HTTP_PORT:-4102}
WATCH=${SECONDS_:-20}
BIN_DIR=${BIN_DIR:-_build/install/default/bin}
[ -n "${SKIP_BUILD:-}" ] || dune build @install

TMP=$(mktemp -d)
SIM_PID=
cleanup() {
  [ -z "$SIM_PID" ] || kill "$SIM_PID" 2>/dev/null || true
  wait 2>/dev/null || true
  rm -rf "$TMP"
}
trap cleanup EXIT

# One point a minute: 252.00 to 259.56 (+3%). Only the distances between points matter.
python3 - "$TMP/ibm.txt" <<'PY'
import sys
with open(sys.argv[1], "w") as f:
    f.write("# time price\n")
    for m in range(11):
        f.write("2026-10-12T13:%02d:00Z %.2f\n" % (30 + m, 252.00 * (1 + 0.003 * m)))
PY

"$BIN_DIR/nyse-sim" -gateway-port 0 -feed-port 0 -http-port "$HTTP" \
  -symbols AAPL,IBM -clock accelerated -start-time 09:28 -speed 60 \
  -anchor-fixed 195=AAPL -anchor-series "$TMP/ibm.txt=IBM" -anchor-half-life-ms 2000 \
  2>"$TMP/sim.log" &
SIM_PID=$!

for _ in $(seq 1 50); do
  curl -sf "http://localhost:$HTTP/" >/dev/null 2>&1 && break
  sleep 0.1
done

for _ in $(seq 1 "$WATCH"); do
  curl -sf "http://localhost:$HTTP/" | python3 -c '
import json, sys
d = json.load(sys.stdin)
row = [d["phase"]]
for s in d["symbols"]:
    a = s.get("anchor") or {}
    row.append("%s %s/%s anchor %s %s pulls %d waits %d" % (
        s["symbol"], s["bid"], s["ask"], a.get("price"),
        "on " if a.get("engaged") else "off", a.get("pulls", 0), a.get("waits", 0)))
print(" | ".join(row))'
  sleep 1
done
grep -E "session|anchor" "$TMP/sim.log" | sed 's/^[^ ]* [^ ]* //'
