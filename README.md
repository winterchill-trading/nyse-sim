# nyse-sim

A free, local stand-in for an NYSE Pillar equities venue that speaks the venue's real
public binary protocols over TCP:

- **Pillar Gateway binary order entry** (Pillar Stream Protocol session layer plus the
  gateway application messages) on the gateway port;
- **XDP Integrated Feed** (order-by-order depth, trades, imbalances, security status) on
  the feed port, the multicast packets carried over a plain TCP stream;
- the same feed as **JSON over a websocket** (`/ws` on the HTTP port), so tools can
  consume it without binary parsing.

Matching, clearing and background order flow come from the venue-neutral
[`exchange-core`](https://github.com/winterchill-trading/exchange-core) engine; US
equities rules (sessions, ticks, lots, auctions, order types, LULD, short sale
restriction) from
[`equities-core`](https://github.com/winterchill-trading/equities-core). The protocol
summaries the codecs follow are `equities-core/docs/nyse-pillar-gateway.md` and
`equities-core/docs/nyse-xdp-integrated-feed.md`.

> **There is no real market data here.** Nothing connects to NYSE and there is no free
> real-time source for this venue. The book moves because noise traders from the engine
> post, cancel and take around a random-walk reference price (`-noise`), which can be
> anchored to a reference price you choose (see [Price anchor](#price-anchor)). Prices,
> volumes and auctions are synthetic.

## Layout

| Path | What |
| --- | --- |
| `wire/` | library `nyse-sim.wire`: the codecs, usable by any client. `Le` (little-endian fields), `Stream_id`, `Order_instructions` (the 64-bit bitfield), `Pillar_app` (gateway messages), `Pillar` (session layer), `Xdp` (packets and messages), `Reason_code` |
| `src/` | library `nyse-sim`: `Listing`, `Venue` (order lifecycle over the engine), `Anchoring` (noise traders tied to a reference price), `Market_event`, `Feed` (XDP sequencing, refresh, JSON), `Gateway` (per-user streams), `Session_handler` (one TCP session), `Framing`, `Server`, `Pillar_client` |
| `bin/` | `nyse-sim`, the simulator |
| `cli/` | `nyse-sim-cli`, a client to play with it |
| `scripts/e2e.sh` | end-to-end run (output below) |
| `scripts/anchor-demo.sh` | anchored noise traders through the open |

## Running

```sh
dune build @install
dune exec nyse-sim -- -gateway-port 4100 -feed-port 4101 -http-port 4102 -noise
```

| Flag | Default | |
| --- | --- | --- |
| `-gateway-port` | 4100 | Pillar gateway (TCP) |
| `-feed-port` | 4101 | XDP feed (TCP) |
| `-http-port` | 4102 | `GET /` venue summary, `/ws` JSON feed |
| `-symbols` | all | e.g. `AAPL,MSFT,BRK.B`; listed: AAPL, MSFT, IBM, JPM, KO, XOM, BRK B, ZVZZT (test symbol) |
| `-noise` | off | noise traders, 4 per symbol |
| `-clock` | `always-open` | `always-open` (core session forever), `real` (New York wall clock and the 2026 holiday calendar), `accelerated` |
| `-start-time`, `-speed` | `09:25`, 60 | accelerated clock: New York time on the next trading day it starts at, venue seconds per real second |
| `-anchor-series FILE=SYM` | none | anchor a symbol to a series file (repeatable); turns noise on |
| `-anchor-fixed PRICE[,BPS]=SYM` | none | anchor a symbol to a fixed price with an optional drift in bps per day (repeatable) |
| `-anchor-scaled VENUE:SYMBOL=SYM,FACTOR` | none | anchor a symbol to a recorded crypto instrument times `FACTOR`, with `-anchor-norm DIR -anchor-from DATE [-anchor-to DATE]` |
| `-anchor-half-life-ms` | 1000 | mean-reversion half-life towards the anchor |

Phases follow `equities-core`'s `Session`: pre-market (early session) 04:00, core 09:30,
post-market (late session) 16:00 to 20:00. With `-clock accelerated -start-time 09:28
-speed 20` the opening auction runs about six seconds after start.

## Price anchor

There is no free real-time NYSE source, so there is no depth to shadow and no trade flow
to replay (the crypto simulators' anchor, [price-anchor](https://github.com/winterchill-trading/price-anchor),
does both). Here the noise traders stay the book's only liquidity, and their random walk
becomes mean-reverting around a reference price: each noise tick the walk moves by its
random step plus `k * (anchor - walk)` with `k = 1 - 0.5 ** (tick / half_life)`
(`Market_sim.set_reference ~half_life`).

`src/anchoring.ml` is a thin wrapper over `Price_anchor.Anchor`: one anchor per anchored
symbol, created with that symbol's `Market_sim` and its 4 noise traders, `half_life`
(`-anchor-half-life-ms`), `step_clock:false`, `levels:0`, `mark_interval` zero, a
`Price_anchor.Path.t` pulled on the venue's clock, `may_engage` from the session phase,
the imbalance periods and LULD pauses, `last_trade` from the venue, and `clamp` from the
LULD bands (`Price_anchor.Clamp.within_bands`). Only the flag parsing lives here.

| Reference | Flag | Notes |
| --- | --- | --- |
| Series file | `-anchor-series points.txt=IBM` | `time price` lines (nanoseconds or ISO 8601 with a zone; `#` comments), read by `Price_anchor.Source.of_series` into a `Path` (a file with points out of time order is refused). The first point applies when the simulator starts, the others at the same distances in venue time (so the accelerated clock replays them faster) |
| Fixed price | `-anchor-fixed 195,10=AAPL` | `Price_anchor.Path.fixed_of_string`: 195 drifting 10 bps per venue day, compounded |
| Recorded crypto | `-anchor-scaled binance:BTCUSDT=AAPL,0.0019 -anchor-norm data/norm -anchor-from 2026-10-12` | the recorder's normalized event logs through `Source.of_norm` and `Source.to_path ~min_interval:100ms`: book mid (L2 or BBO), else the last trade, the first change per 100 ms; scaled by the factor (`price_factor`) and rounded to the cent |

When the anchor pulls and when the venue owns the price:

- It pulls only in continuous core trading: core session, outside the closing imbalance
  period (15:50 on; never on the always-open clock), and not in a LULD pause.
- In the pre-market (including the opening imbalance period), during the auctions, in the
  post-market and while paused, the anchor waits: on the first such tick it hands the
  symbol back to the free walk (`Market_sim.clear_reference`). It resumes on the first
  core tick after the opening auction or the reopening, restarting the walk from the
  venue's last trade (the auction price, when there was one) and climbing back to the
  reference from there.
- LULD bands keep coming from the venue's own trades. The anchor's target is held 30 bps
  inside the bands, because noise orders reach the engine without the venue's LULD price
  checks. An anchor beyond a band climbs with it: trades near the band move the LULD
  reference (every 30 s, on a 1% move) and the next band lets the walk further, about 5%
  per 30 s, without a pause.
- `GET /` shows each anchored symbol's anchor price (the scaled reference at the last
  tick, before the clamp), whether it is engaged, and its `pulls`, `waits`, `releases`,
  `resumes` and `clamped` counts, next to the best bid and ask. Since the move to
  `Price_anchor.Anchor`, `pulls` counts reference changes set on the walk (a fixed price
  is set once per engagement, a series once per new point), not every tick; `releases`
  counts the first wait too (an anchor started in the pre-market shows 1); `clamped`
  counts the applied updates the bands moved.

`scripts/anchor-demo.sh` runs AAPL fixed at 195 and IBM on a +3% series from 09:28 on the
accelerated clock (one line per real second, 60 venue seconds each):

```
Pre_market | AAPL None/189.58 anchor 195 off pulls 0 waits 5 | IBM 250.19/250.48 anchor 252.76 off pulls 0 waits 5
Core | AAPL 190.64/191.46 anchor 195 on  pulls 1 waits 9 | IBM 250.04/250.46 anchor 254.27 on  pulls 2 waits 9
Core | AAPL 192.62/192.87 anchor 195 on  pulls 1 waits 9 | IBM 252.32/252.66 anchor 255.78 on  pulls 4 waits 9
Core | AAPL 194.38/194.77 anchor 195 on  pulls 1 waits 9 | IBM 256.44/256.53 anchor 258.8 on  pulls 8 waits 9
Core | AAPL 194.86/194.92 anchor 195 on  pulls 1 waits 9 | IBM 257.4/257.71 anchor 259.56 on  pulls 9 waits 9
Core | AAPL 194.96/195.15 anchor 195 on  pulls 1 waits 9 | IBM 259.38/None anchor 259.56 on  pulls 9 waits 9
```

AAPL is within 0.2% of its anchor about 8 real seconds (8 venue minutes) after the open
and inside a cent or two by 18 s; IBM tracks its ramp about one point behind and closes
on 259.56 once the series ends.

Limits: noise ticks run on the wall clock (250 ms), so the half-life is in real time
while series points follow venue time; on a fast accelerated clock the walk lags the
series. Raw minute files (`-anchor-raw`) are not read here: their normalizers live in the
venue client repositories, so rebuild `norm/` with the venue's feed binary first.

## How to play

```sh
CLI="dune exec nyse-sim-cli --"
$CLI logon -user TRADER1                     # login, streams, symbols, GT replay
$CLI book -symbol AAPL                       # book from a feed refresh + live updates
$CLI order -symbol AAPL -side buy -qty 100 -price 189.40 -clordid 1
$CLI order -symbol AAPL -side sell -qty 300 -price 190 -max-floor 100 -clordid 2
$CLI modify -symbol AAPL -id 1 -qty 50       # reduce quantity
$CLI order -symbol AAPL -side sell -qty 300 -price 189.90 -clordid 3 -replace 2
$CLI order -symbol AAPL -side buy -qty 200 -price 191 -tif ioc -clordid 4
$CLI cancel -symbol AAPL -id 1
$CLI feed -symbol AAPL -seconds 10           # decoded XDP, one line per message
```

`order` also takes `-tif day|ioc|opg|close`, no `-price` for a market order, `-alo`
(add liquidity only), `-side short|short-exempt`, `-session N` (TradingSessionID),
`-stp N` (SelfTradeType) and `-cod` (keep cancel on disconnect; the CLI turns it off by
default so orders survive the CLI exiting). Any username logs in; each gets its own TG,
GT and REF streams, kept for the life of the process, so `logon` replays your history.

Things to try: send an OPG order before the open with the accelerated clock and watch the
Imbalance messages and the uncross; post a bid at the LULD upper band (shown in Security
Status) and wait 15 s for the limit state to become a trading pause; trade ZVZZT down 10%
from its 10.00 close to switch on the short sale restriction.

## Fidelity

Compared with the two spec summaries. "Yes" means implemented as described there,
"partial" means implemented with the stated difference.

### Pillar Gateway

| Spec item | Status | Notes |
| --- | --- | --- |
| Little-endian types, `char`/`zchar` padding, Price scale 8 | yes | byte-exact tests against the documented offsets |
| Login / LoginResponse, version `"1.1"` | yes | any non-empty username, any password; status 24, 27, 81 |
| StreamAvail at login and every second (gateway heartbeat) | yes | TG (write), GT and REF (read) per user |
| Heartbeat: client every 1 s, drop after 5 s silence | yes | also "login timed out" after 5 s without a login |
| Open / OpenResponse, replay from `start_seq`, `end_seq` exclusive or unbounded | yes | GT and REF are full logs; status 18, 54, 84 |
| Close / CloseResponse | yes | |
| SeqMsg framing, TG sequence checking | partial | out-of-sequence TG messages get CloseResponse 82 and are dropped; the connection stays up. Lossy mode and throttling are ignored |
| REF: Equities Symbol Reference Data `0x0232` | yes | bytes 46-52 (undocumented) zero |
| REF: MPV class/level `0x0230`/`0x0231`, MPID/MMID config | no | the tick is the Reg NMS one: 0.01 at or above $1 |
| Session Configuration Request `0x0220` / Ack `0x0221` | partial | cancel on disconnect and session STP default are honoured; Ack bytes 35-38 echo the request (our reading of an undocumented tail) |
| New Order `0x0240`: limit, market, Day, IOC, OPG, on close | yes | market orders become IOC limits at the LULD band |
| ALO (ExtendedExecInst 1) | partial | rejected if it would take liquidity (no re-pricing) |
| MaxFloor reserve (OptionalOrderAddOn `0x0241`) | partial | displayed slice in the book, refreshed from the reserve when it fills; a refresh joins the back of the queue and keeps the OrderID |
| SelfTradeType 1-5 | partial | applied against the member's own resting orders that the new order's price would reach, before matching (not only when they are next in priority). Type 1 "none" still never trades with itself: the engine stops a taker at its own order |
| TradingSessionID | yes | orders not eligible for the next session are deleted at 09:30 and 16:00 |
| Cancel `0x0280` | yes | target by OrigClOrdID |
| Modify `0x0270` (reduce quantity, change between sell sides) | partial | the engine has no in-place reduction, so the order loses time priority (feed: Modify Order with PositionChange 1); OrderID kept |
| Cancel/Replace (`0x0240` with OrigClOrdID) | yes | new OrderID, Replace Order on the feed, Ack type 8 |
| Order Ack `0x0260` (AckType 1, 8) | yes | |
| Modify/Cancel Ack and UROUT `0x0271` (AckType 9, 11, 12) | yes | 12 (done for day) at the session ends |
| Execution Report `0x0290` | partial | DealID = XDP trade id for continuous trades; auction fills are reported per order, each with its own DealID (counterparties are not paired) |
| Application Layer Reject `0x0263` | yes | |
| Reason codes and liquidity indicators | partial | NYSE's values are in a spreadsheet not summarised; this simulator defines its own (`Reason_code`), liquidity `A`, `R`, `O`, `C`, `H` |
| Cancel on disconnect | yes | default on, switchable per user with `0x0220` |
| Maximum quantity 5,000,000 | yes | price ceilings per scale code are not checked |
| Out of scope in the spec (DMM, floor broker, risk, routing, pegs, retail, crosses, bust/correct, drop copy) | no | MinQty, ISO, pegs and non-displayed orders are rejected as unsupported |

### XDP Integrated Feed

| Spec item | Status | Notes |
| --- | --- | --- |
| Packet header, sequence numbers, max 1400 bytes | yes | one message per packet |
| Heartbeat packet once a second | partial | sent when the channel carried nothing else that second |
| Sequence Number Reset at start | yes | own packet, SeqNum 1, DeliveryFlag 12 |
| Source Time Reference once a second | yes | data messages carry only SourceTimeNS |
| Symbol Index Mapping per symbol at startup | yes | late joiners get them (and Security Status) as a refresh, DeliveryFlag 17-20 |
| Add (100), Modify (101), Delete (102), Execution (103), Replace (104) | yes | no Delete after a full execution or for a replace; price improvement shows as an execution at the resting price |
| Non-Displayed Trade (110) | partial | used for executions of auction-only (OPG / on-close) interest, buy side only; there are no hidden orders otherwise |
| Imbalance (105) | partial | opening 09:00-09:30 and closing 15:50-16:00, once a second when a figure changes; reference, paired, imbalance, market imbalance, side and indicative price are filled, the clearing prices, collars and freeze fields are zero |
| Security Status (34): sessions | yes | `E` early, `O` core, `L` late, `X` closed; at `X` all orders are gone without Delete messages |
| Security Status: LULD pause and resume, SSR | yes | `4`/`M` with the bands in Price1/Price2, `5` after the reopening auction; `A` when the short sale restriction activates |
| Refresh Request (15) answered with Add Order Refresh (106) | partial | layouts of 15 and 106 are not in the summary; 106 is the Add Order layout with type 106, 15 is this simulator's own (see `Xdp`) |
| Multicast, A/B lines, retransmission server | no | TCP stream instead (packets back to back, framed by PktSize) |
| Cross trades, corrections, RPI, stock summary | no | out of scope in the spec |

### Market model

| Item | Status | Notes |
| --- | --- | --- |
| Price-time matching, executions at the resting price | yes | `exchange-core` engine |
| Opening, closing and LULD reopening auctions | partial | `equities-core` `Auction` (max volume, min imbalance, nearest reference); auction fills are not settled in the engine's positions |
| LULD bands, re-pricing, limit state, 5 minute pause | yes | reference price refreshed every 30 s when the last sale moved 1% or more; CancelInsteadOfReprice honoured |
| Short sale restriction (Rule 201) | partial | activates on a 10% decline from the previous close; short sales at or below the best bid are rejected (not re-priced) |
| Market-wide circuit breakers | no | there is no index to watch |
| Fees | no | the Pillar execution report carries none; the engine charges notional fees internally |
| Accounts | partial | every username gets a large cash balance; no buying power or locate checks beyond the engine's margin |

## End to end

`scripts/e2e.sh` starts the simulator on ports 4100-4102 (override with `GW_PORT`,
`FEED_PORT`, `HTTP_PORT`), trades with the CLI, records the feed, runs an accelerated
morning through the opening auction and stops everything. Excerpt of a run:

```text
== AAPL book from a feed refresh
AAPL book (8 orders)
  ask     189.85       96  (1)
  ask     189.72      109  (1)
  ask     189.71       28  (1)
  ---
  bid     189.48      153  (1)
  bid     189.38       99  (1)
  bid     189.27      113  (1)

== orders while the feed is recorded
-- resting buy 100 @ 185.00
GT/1 #1 OrderAck type=1 sym=1 clordid=1001 orderid=177 px=185 qty=100 leaves=100 reason=0
-- reserve sell 500 showing 100 @ 195.00
GT/1 #2 OrderAck type=1 sym=1 clordid=1002 orderid=209 px=195 qty=500 leaves=500 reason=0
-- modify 1001 down to 60 shares
GT/1 #3 ModCancelAck type=9 sym=1 orderid=177 ref=814344049 orig=1001 qty=60 leaves=60 reason=0
-- cancel/replace 1002 to 194.00
GT/1 #4 OrderAck type=8 sym=1 clordid=1003 orderid=252 px=194 qty=500 leaves=500 reason=0
-- marketable IOC buy 300 @ 191.00 (fills against the book)
GT/1 #5 OrderAck type=1 sym=1 clordid=1004 orderid=268 px=191 qty=300 leaves=300 reason=0
GT/1 #6 Exec sym=1 clordid=1004 orderid=268 deal=20 px=189.53 qty=35 cum=35 leaves=265 liq=R
GT/1 #7 Exec sym=1 clordid=1004 orderid=268 deal=21 px=189.54 qty=143 cum=178 leaves=122 liq=R
GT/1 #8 Exec sym=1 clordid=1004 orderid=268 deal=22 px=189.67 qty=40 cum=218 leaves=82 liq=R
GT/1 #9 Exec sym=1 clordid=1004 orderid=268 deal=23 px=189.68 qty=14 cum=232 leaves=68 liq=R
GT/1 #10 Exec sym=1 clordid=1004 orderid=268 deal=24 px=189.69 qty=68 cum=300 leaves=0 liq=R
-- cancel 1001
GT/1 #11 ModCancelAck type=11 sym=1 orderid=177 ref=814345976 orig=1001 qty=60 leaves=0 reason=17
-- rejected: off-tick price
GT/1 #12 Reject type=1 sym=1 clordid=1005 reason=2

== the same orders on the feed: gateway OrderIDs and DealIDs match XDP order and trade ids
     272 11 AddOrder AAPL/24 id=177 buy 100 @ 185
     322 11 AddOrder AAPL/31 id=209 sell 100 @ 195
     359 11 ModifyOrder AAPL/39 id=177 60 @ 185 lost-priority
     402 11 ReplaceOrder AAPL/50 id=209 new=252 sell 100 @ 194
     434 11 OrderExecution AAPL/55 id=243 trade=20 35 @ 189.53
     435 11 OrderExecution AAPL/56 id=231 trade=21 143 @ 189.54
     436 11 OrderExecution AAPL/57 id=224 trade=22 40 @ 189.67
     437 11 OrderExecution AAPL/58 id=262 trade=23 14 @ 189.68
     438 11 OrderExecution AAPL/59 id=242 trade=24 68 @ 189.69

== JSON mirror of the feed: GET / and the first live order events on ws://127.0.0.1:4102/ws
{"seq":556,"type":"add_order","symbol":"MSFT","symbol_seq":57,"source_time_ns":213904000,"order_id":332,"side":"S","price":"419.74","volume":71}

== accelerated clock: from 09:28 New York at 20x, through the opening auction
GT/1 #1 OrderAck type=1 sym=3 clordid=2001 orderid=1000000001 px=251 qty=300 leaves=300 reason=0
GT/1 #2 OrderAck type=1 sym=3 clordid=2002 orderid=1 px=249 qty=200 leaves=200 reason=0
GT/2 #1 OrderAck type=1 sym=3 clordid=2001 orderid=1000000002 px=0 qty=400 leaves=400 reason=0

== feed around the open
      19 18 SecurityStatus IBM status=E halt=~ ssr=~
      20 11 Imbalance IBM type=M ref=250.3 paired=0 imbalance=300 side=B indicative=0
      22 11 AddOrder IBM/3 id=1 buy 200 @ 249
      23 11 Imbalance IBM type=M ref=250.3 paired=300 imbalance=100 side=S indicative=249
      28 11 NonDisplayedTrade IBM/5 trade=2000000001 300 @ 249
      29 11 OrderExecution IBM/6 id=1 trade=2000000002 100 @ 249
      32 11 SecurityStatus IBM status=O halt=~ ssr=~

== TRADER1 GT replay after the open
GT/1 #3 Exec sym=3 clordid=2001 orderid=1000000001 deal=2000000001 px=249 qty=300 cum=300 leaves=0 liq=O
GT/1 #4 Exec sym=3 clordid=2002 orderid=1 deal=2000000002 px=249 qty=100 cum=100 leaves=100 liq=O
```

Feed lines read `seq flag message`; `AAPL/24` is the symbol and its SymbolSeqNum.

## Limitations

- One XDP channel for all symbols, one gateway (any MIC in the login is accepted; the
  ack says `XNYS`).
- Auction-only and reserve orders live in the venue, not the engine: auction fills do not
  move engine positions, and hidden reserve shares cannot execute until displayed.
- The engine's order ids are reused as public ids, so ids are small integers and
  auction-only orders take ids from 1,000,000,000 up, auction trades from 2,000,000,000.
- State is in memory only; restarting the simulator starts a new day.
- Noise traders are not market makers: spreads and depth are whatever the random flow
  leaves, and they pause while a symbol is halted.
- Noise orders go to the engine directly, without the venue's LULD price checks; the
  price anchor keeps its target inside the bands, but the free walk can still trade
  through a band briefly before the limit state pauses the symbol.

## Development

```sh
dune build @all @runtest
dune fmt
```

The tests cover byte layouts and round trips of every message, the venue (lifecycle,
auctions, LULD, SSR, STP, reserve, sessions), the feed, the session state machine and a
gateway plus feed run over TCP.

CI installs the pinned siblings and the opam dependencies through the shared
[ox-setup](https://github.com/winterchill-trading/infra/blob/main/docs/ci.md) action, which
caches the built opam switch between runs.

## License

MIT
