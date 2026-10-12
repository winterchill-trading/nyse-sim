open! Core
open! Async
module A = Nyse_sim.Anchoring
module Price = Exchange_core.Price
module Size = Exchange_core.Size

let instrument = Market_data.Instrument.of_string "binance:BTCUSDT"
let t0 = Time_ns.of_string_with_utc_offset "2026-10-12T14:00:00Z"
let ms n = Time_ns.add t0 (Time_ns.Span.of_int_ms n)

let trade ~at px : Market_data.Event.t =
  Trade
    { instrument
    ; exch_ts = ms at
    ; recv_ts = ms at
    ; price = Price.of_string px
    ; size = Size.of_string "0.1"
    ; side = Buy
    ; trade_id = Int.to_string at
    }
;;

let bbo ~at bid ask : Market_data.Event.t =
  Bbo
    { instrument
    ; exch_ts = ms at
    ; recv_ts = ms at
    ; bid_price = Price.of_string bid
    ; bid_size = Size.of_string "1"
    ; ask_price = Price.of_string ask
    ; ask_size = Size.of_string "1"
    }
;;

let aapl =
  Nyse_sim.Listing.instrument
    (Option.value_exn (Nyse_sim.Listing.find_by_id Nyse_sim.Listing.defaults 1))
;;

let%expect_test "a recorded crypto series, scaled onto AAPL" =
  let norm_dir = Filename_unix.temp_dir "norm" "" in
  let w = Market_data.Event_log.Writer.create ~dir:norm_dir () in
  List.iter
    [ trade ~at:0 "100000"
    ; bbo ~at:10 "99990" "100010" (* same mid: no point *)
    ; bbo ~at:50 "100100" "100110" (* within 100ms of the last point *)
    ; trade ~at:200 "100200" (* the book's mid wins over the trade *)
    ; bbo ~at:300 "100490" "100510"
    ; bbo ~at:1_000 "100490" "100510" (* unchanged *)
    ]
    ~f:(Market_data.Event_log.Writer.append w);
  let%bind () = Market_data.Event_log.Writer.close w in
  let date = Date.of_string "2026-10-12" in
  let%bind spec =
    A.Spec.of_scaled_flag "binance:BTCUSDT=AAPL,0.0019" ~norm_dir ~from:date ~to_:date
  in
  let spec = Or_error.ok_exn spec in
  (match spec.path with
   | Fixed _ -> ()
   | Series points ->
     Array.iter points ~f:(fun (ts, px) ->
       printf
         "+%dms %s\n"
         (Time_ns.Span.to_int_ms (Time_ns.diff ts t0))
         (Price.to_string px)));
  let scale = Price_anchor.Scale.create ~price_factor:spec.factor aapl in
  List.iter [ 0; 250; 400 ] ~f:(fun n ->
    let px = Price_anchor.Path.price_at spec.path ~elapsed:(Time_ns.Span.of_int_ms n) in
    printf
      "venue +%dms: %s\n"
      n
      (Price.to_string
         (Option.value_exn (Price_anchor.Scale.price scale px ~direction:Nearest))));
  [%expect
    {|
    +0ms 100000
    +200ms 100105
    +300ms 100500
    venue +0ms: 190
    venue +250ms: 190.2
    venue +400ms: 190.95
    |}];
  let%bind r =
    A.Spec.of_scaled_flag "binance:ETHUSDT=AAPL,0.05" ~norm_dir ~from:date ~to_:date
  in
  print_s [%sexp (Result.is_error r : bool)];
  let%bind r =
    A.Spec.of_scaled_flag "binance:BTCUSDT=AAPL,-1" ~norm_dir ~from:date ~to_:date
  in
  print_s [%sexp (r : A.Spec.t Or_error.t)];
  [%expect
    {|
    true
    (Error ("factor must be a positive number" binance:BTCUSDT=AAPL,-1))
    |}];
  return ()
;;
