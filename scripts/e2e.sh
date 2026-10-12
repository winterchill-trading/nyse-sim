#!/usr/bin/env bash
# End to end: start nyse-sim on high ports, trade with nyse-sim-cli over the Pillar
# gateway, watch the XDP feed, then run an accelerated session through the opening
# auction. Everything started here is stopped on exit.
#
#   GW_PORT, FEED_PORT, HTTP_PORT  ports (default 4100, 4101, 4102)
#   BIN_DIR    directory holding nyse-sim and nyse-sim-cli (default _build/install/default/bin)
#   SKIP_BUILD set to skip `dune build @install`
set -euo pipefail
cd "$(dirname "$0")/.."

GW=${GW_PORT:-4100}
FEED=${FEED_PORT:-4101}
HTTP=${HTTP_PORT:-4102}
BIN_DIR=${BIN_DIR:-_build/install/default/bin}
[ -n "${SKIP_BUILD:-}" ] || dune build @install

SIM="$BIN_DIR/nyse-sim"
CLI="$BIN_DIR/nyse-sim-cli"
TMP=$(mktemp -d)
PIDS=()

cleanup() {
  for pid in "${PIDS[@]}"; do kill "$pid" 2>/dev/null || true; done
  wait 2>/dev/null || true
  rm -rf "$TMP"
}
trap cleanup EXIT

for port in "$GW" "$FEED" "$HTTP"; do
  if lsof -iTCP:"$port" -sTCP:LISTEN >/dev/null 2>&1; then
    echo "port $port is already in use" >&2
    exit 1
  fi
done

start_sim() {
  "$SIM" -gateway-port "$GW" -feed-port "$FEED" -http-port "$HTTP" "$@" 2>"$TMP/sim.log" &
  SIM_PID=$!
  PIDS+=("$SIM_PID")
  for _ in $(seq 50); do
    nc -z 127.0.0.1 "$GW" 2>/dev/null && return 0
    sleep 0.1
  done
  echo "nyse-sim did not start" >&2
  cat "$TMP/sim.log" >&2
  exit 1
}

stop_sim() {
  kill "$SIM_PID"
  wait "$SIM_PID" 2>/dev/null || true
}

cli() { "$CLI" "$1" -host 127.0.0.1 "${@:2}"; }
gw() { cli "$1" -port "$GW" "${@:2}" | tee -a "$TMP/gw.txt"; }
feed() { cli "$1" -feed-port "$FEED" "${@:2}"; }

echo "== start: always-open session, noise traders"
start_sim -noise -seed 7
sleep 2

echo
echo "== logon (LoginResponse, StreamAvail per stream, REF symbol data)"
gw logon -user TRADER1

echo
echo "== AAPL book from a feed refresh"
feed book -symbol AAPL -depth 3

echo
echo "== orders while the feed is recorded"
feed feed -symbol AAPL -seconds 4 >"$TMP/feed.txt" &
FEED_PID=$!
PIDS+=("$FEED_PID")
sleep 0.5
echo "-- resting buy 100 @ 185.00"
gw order -symbol AAPL -side buy -qty 100 -price 185.00 -clordid 1001
echo "-- reserve sell 500 showing 100 @ 195.00"
gw order -symbol AAPL -side sell -qty 500 -price 195.00 -max-floor 100 -clordid 1002
echo "-- modify 1001 down to 60 shares"
gw modify -symbol AAPL -id 1001 -qty 60
echo "-- cancel/replace 1002 to 194.00"
gw order -symbol AAPL -side sell -qty 500 -price 194.00 -max-floor 100 -clordid 1003 -replace 1002
echo "-- marketable IOC buy 300 @ 191.00 (fills against the book)"
gw order -symbol AAPL -side buy -qty 300 -price 191.00 -tif ioc -clordid 1004
echo "-- cancel 1001"
gw cancel -symbol AAPL -id 1001
echo "-- rejected: off-tick price"
gw order -symbol AAPL -side buy -qty 100 -price 185.001 -clordid 1005
wait "$FEED_PID" || true
echo
echo "== the same orders on the feed: gateway OrderIDs and DealIDs match XDP order and trade ids"
ids=$(grep -o 'orderid=[0-9]*' "$TMP/gw.txt" | cut -d= -f2 | sort -u | paste -sd'|' -)
deals=$(grep -o 'deal=[0-9]*' "$TMP/gw.txt" | cut -d= -f2 | sort -u | paste -sd'|' -)
grep -E "(id|new)=(${ids:-x}) |trade=(${deals:-x}) " "$TMP/feed.txt" | head -40
echo "($(wc -l <"$TMP/feed.txt") AAPL feed lines in total, mostly noise traders)"

echo
echo "== JSON mirror of the feed: GET / and the first live order events on ws://127.0.0.1:$HTTP/ws"
curl -s "http://127.0.0.1:$HTTP/" | head -c 200
echo " ..."
(timeout 2 curl -s -N --http1.1 -H "Connection: Upgrade" -H "Upgrade: websocket" \
  -H "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==" -H "Sec-WebSocket-Version: 13" \
  "http://127.0.0.1:$HTTP/ws" || true) | grep -ao '{"seq[^}]*}' | grep -E 'add_order"|order_execution' | head -3
stop_sim

echo
echo "== accelerated clock: from 09:28 New York at 20x, through the opening auction"
start_sim -clock accelerated -start-time 09:28 -speed 20
feed feed -seconds 7 >"$TMP/open.txt" &
FEED_PID=$!
PIDS+=("$FEED_PID")
sleep 0.3
gw order -symbol IBM -side buy -qty 300 -price 251.00 -tif opg -clordid 2001
gw order -symbol IBM -side buy -qty 200 -price 249.00 -clordid 2002
gw order -user TRADER2 -symbol IBM -side sell -qty 400 -tif opg -clordid 2001
wait "$FEED_PID" || true
echo
echo "== feed around the open"
grep -E "IBM|SecurityStatus AAPL" "$TMP/open.txt" | grep -v SymbolIndexMapping | head -20
echo
echo "== TRADER1 GT replay after the open"
gw logon -user TRADER1 | grep -E "#[0-9]+ (OrderAck|Exec|ModCancelAck)"
echo
echo "== simulator log"
cat "$TMP/sim.log"
stop_sim
echo
echo "== done"
