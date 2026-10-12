(* The anchor on the venue's synthetic clock: it waits through the pre-market and the
   opening auction, pulls the noise traders' walk once the core session opens, and the
   LULD bands follow the venue's own trades. *)

open! Core
module H = Harness
module A = Nyse_sim.Anchoring
module Venue = Nyse_sim.Venue
module Market_sim = Exchange_core_engine.Market_sim
module Price = Exchange_core.Price
module Phase = Equities_core.Session.Us_equities.Phase

let aapl = Option.value_exn (Nyse_sim.Listing.find_by_id Nyse_sim.Listing.defaults 1)

(* The server's noise configuration for one listing. *)
let market_sim v =
  Market_sim.create
    { Market_sim.Config.default with
      seed = 1
    ; symbols = [ aapl.symbol ]
    ; initial_prices = [ aapl.symbol, aapl.reference_price ]
    ; noise_traders = 4
    ; vol_bps = 3.
    ; tick_interval = Time_ns.Span.of_int_ms 250
    ; max_orders_per_symbol = 4
    ; post_probability = 0.25
    ; cancel_probability = 0.15
    ; market_probability = 0.02
    ; max_offset_bps = 20.
    ; min_notional = 2_000.
    ; max_notional = 30_000.
    }
    (Venue.exchange v)
;;

type run =
  { h : H.t
  ; sim : Market_sim.t
  ; anchor : A.t
  ; mutable last_trade : string option
  ; mutable executions : int
  }

(* [seconds] of 250ms noise ticks, with the venue's once-a-second LULD tick. *)
let run r ~seconds =
  let v = H.venue r.h in
  for i = 1 to seconds * 4 do
    H.advance r.h (Time_ns.Span.of_int_ms 250);
    Venue.with_engine v ~symbol_id:1 ~f:(fun _ ->
      A.on_venue_tick
        r.anchor
        r.sim
        v
        ~symbol_id:1
        ~auction:(Phase.equal (Venue.phase v) Pre_market);
      Market_sim.tick r.sim);
    if i % 4 = 0 then Venue.tick v;
    List.iter (H.drain r.h) ~f:(fun line ->
      match String.lsplit2 line ~on:' ' with
      | Some ("feed:", rest) when String.is_prefix rest ~prefix:"Execution" ->
        r.executions <- r.executions + 1;
        r.last_trade <- List.last (String.split rest ~on:'@')
      | _ -> ())
  done
;;

let show r label =
  let v = H.venue r.h in
  let bands = Venue.luld_bands v ~symbol_id:1 in
  print_s
    [%message
      label
        ~phase:(Venue.phase v : Phase.t)
        ~walk:(Market_sim.reference_price r.sim aapl.symbol : Price.t option)
        ~engaged:(A.engaged r.anchor : bool)
        ~stats:(A.stats r.anchor : A.Stats.t)
        ~executions:(r.executions : int)
        ~last_trade:(r.last_trade : string option)
        (bands : Equities_core.Halts.Bands.t)]
;;

let%expect_test "waits through the pre-market auction, pulls from the open" =
  let h = H.create ~phase:Pre_market ~at:(9, 10) () in
  let v = H.venue h in
  let sim = market_sim v in
  let spec = A.Spec.of_fixed_flag "195=AAPL" |> Or_error.ok_exn in
  let anchor =
    A.create spec ~instrument:(Nyse_sim.Listing.instrument aapl) ~start:(Venue.now v)
  in
  let r = { h; sim; anchor; last_trade = None; executions = 0 } in
  run r ~seconds:60;
  show r "pre-market";
  [%expect
    {|
    (pre-market (phase Pre_market) (walk (189.51)) (engaged false)
     (stats ((pulls 0) (waits 240) (releases 0) (clamped 0))) (executions 36)
     (last_trade (189.4)) (bands ((lower 180.03) (upper 198.98))))
    |}];
  H.set_local_time h 9 30;
  Venue.set_phase v Core;
  ignore (H.drain h : string list);
  run r ~seconds:10;
  show r "core +10s";
  run r ~seconds:50;
  show r "core +60s";
  [%expect
    {|
    ("core +10s" (phase Core) (walk (195.02)) (engaged true)
     (stats ((pulls 40) (waits 240) (releases 0) (clamped 0))) (executions 55)
     (last_trade (195.23)) (bands ((lower 182.71) (upper 201.95))))
    ("core +60s" (phase Core) (walk (195.02)) (engaged true)
     (stats ((pulls 240) (waits 240) (releases 0) (clamped 0))) (executions 76)
     (last_trade (194.95)) (bands ((lower 185.33) (upper 204.83))))
    |}];
  H.set_local_time h 16 0;
  Venue.set_phase v Post_market;
  ignore (H.drain h : string list);
  run r ~seconds:30;
  show r "post-market";
  [%expect
    {|
    (post-market (phase Post_market) (walk (194.79)) (engaged false)
     (stats ((pulls 240) (waits 360) (releases 1) (clamped 0))) (executions 89)
     (last_trade (194.55)) (bands ((lower 185.33) (upper 204.83))))
    |}]
;;

let%expect_test "an anchor beyond the LULD band climbs with the bands instead of pausing" =
  let h = H.create ~phase:Core ~at:(10, 0) () in
  let v = H.venue h in
  let sim = market_sim v in
  let spec = A.Spec.of_fixed_flag "215=AAPL" |> Or_error.ok_exn in
  let anchor =
    A.create spec ~instrument:(Nyse_sim.Listing.instrument aapl) ~start:(Venue.now v)
  in
  let r = { h; sim; anchor; last_trade = None; executions = 0 } in
  for _ = 1 to 12 do
    run r ~seconds:10;
    show r (if Venue.halted v ~symbol_id:1 then "paused" else "trading")
  done;
  [%expect
    {|
    (trading (phase Core) (walk (198.36)) (engaged true)
     (stats ((pulls 40) (waits 0) (releases 0) (clamped 40))) (executions 8)
     (last_trade (197.94)) (bands ((lower 180.03) (upper 198.98))))
    (trading (phase Core) (walk (198.41)) (engaged true)
     (stats ((pulls 80) (waits 0) (releases 0) (clamped 80))) (executions 9)
     (last_trade (198.28)) (bands ((lower 180.03) (upper 198.98))))
    (trading (phase Core) (walk (198.32)) (engaged true)
     (stats ((pulls 120) (waits 0) (releases 0) (clamped 120))) (executions 12)
     (last_trade (198.24)) (bands ((lower 188.33) (upper 208.15))))
    (trading (phase Core) (walk (207.44)) (engaged true)
     (stats ((pulls 160) (waits 0) (releases 0) (clamped 160))) (executions 31)
     (last_trade (206.23)) (bands ((lower 188.33) (upper 208.15))))
    (trading (phase Core) (walk (207.49)) (engaged true)
     (stats ((pulls 200) (waits 0) (releases 0) (clamped 200))) (executions 33)
     (last_trade (207.38)) (bands ((lower 188.33) (upper 208.15))))
    (trading (phase Core) (walk (207.55)) (engaged true)
     (stats ((pulls 240) (waits 0) (releases 0) (clamped 240))) (executions 39)
     (last_trade (207.63)) (bands ((lower 197.25) (upper 218.01))))
    (trading (phase Core) (walk (215.04)) (engaged true)
     (stats ((pulls 280) (waits 0) (releases 0) (clamped 240))) (executions 55)
     (last_trade (214.82)) (bands ((lower 197.25) (upper 218.01))))
    (trading (phase Core) (walk (214.99)) (engaged true)
     (stats ((pulls 320) (waits 0) (releases 0) (clamped 240))) (executions 58)
     (last_trade (215.41)) (bands ((lower 197.25) (upper 218.01))))
    (trading (phase Core) (walk (215.07)) (engaged true)
     (stats ((pulls 360) (waits 0) (releases 0) (clamped 240))) (executions 60)
     (last_trade (214.9)) (bands ((lower 204.16) (upper 225.65))))
    (trading (phase Core) (walk (214.97)) (engaged true)
     (stats ((pulls 400) (waits 0) (releases 0) (clamped 240))) (executions 66)
     (last_trade (214.84)) (bands ((lower 204.16) (upper 225.65))))
    (trading (phase Core) (walk (215.03)) (engaged true)
     (stats ((pulls 440) (waits 0) (releases 0) (clamped 240))) (executions 68)
     (last_trade (214.9)) (bands ((lower 204.16) (upper 225.65))))
    (trading (phase Core) (walk (215.04)) (engaged true)
     (stats ((pulls 480) (waits 0) (releases 0) (clamped 240))) (executions 73)
     (last_trade (215.16)) (bands ((lower 204.16) (upper 225.65))))
    |}]
;;
