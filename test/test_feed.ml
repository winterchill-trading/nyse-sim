open! Core
module Feed = Nyse_sim.Feed
module Xdp = Nyse_sim_wire.Xdp
module Price = Exchange_core.Price

let listings = List.take Nyse_sim.Listing.defaults 2
let now = Time_ns.of_string_with_utc_offset "2026-10-13 14:00:00.000000250Z"

let symbols i =
  List.find listings ~f:(fun l -> l.symbol_id = i)
  |> Option.map ~f:(fun (l : Nyse_sim.Listing.t) -> l.nyse_symbol, l.scale)
;;

let print_packet b =
  let p = Or_error.ok_exn (Xdp.Packet.decode b) in
  match p.msgs with
  | [] -> printf "seq=%d flag=%d heartbeat\n" p.seq_num p.delivery_flag
  | msgs ->
    List.iter msgs ~f:(fun m ->
      printf
        "seq=%d flag=%d %s\n"
        p.seq_num
        p.delivery_flag
        (Xdp.Msg.to_string_hum ~symbols m))
;;

let%expect_test "channel start, events, refresh and JSON" =
  let feed = Feed.create listings ~now:(fun () -> now) in
  let early = Feed.subscribe feed (Binary print_packet) in
  Feed.start feed;
  [%expect
    {|
    seq=0 flag=17 SymbolIndexMapping 1 AAPL scale=6 lot=100 prev_close=189.5
    seq=0 flag=20 SymbolIndexMapping 2 MSFT scale=6 lot=40 prev_close=420.1
    seq=1 flag=12 SequenceNumberReset product=115 channel=1
    seq=2 flag=11 SourceTimeReference 1791900000
    seq=3 flag=11 SymbolIndexMapping 1 AAPL scale=6 lot=100 prev_close=189.5
    seq=4 flag=11 SymbolIndexMapping 2 MSFT scale=6 lot=40 prev_close=420.1
    |}];
  Feed.publish
    feed
    (Status
       { symbol_id = 1
       ; status =
           { security_status = 'O'
           ; halt_condition = '~'
           ; ssr_state = '~'
           ; price_1 = None
           ; price_2 = None
           }
       });
  Feed.publish
    feed
    (Add
       { symbol_id = 1
       ; order_id = 5
       ; side = Buy
       ; px = Price.of_string "189.45"
       ; qty = 100
       });
  [%expect
    {|
    seq=5 flag=11 SecurityStatus AAPL status=O halt=~ ssr=~
    seq=6 flag=11 AddOrder AAPL/2 id=5 buy 100 @ 189.45
    |}];
  (* A late subscriber gets the reference data as a refresh, then the live stream. *)
  Feed.unsubscribe feed early;
  let json = ref [] in
  let late = Feed.subscribe feed (Binary print_packet) in
  let _ = Feed.subscribe feed (Json (fun s -> json := s :: !json)) in
  [%expect
    {|
    seq=6 flag=17 SymbolIndexMapping 1 AAPL scale=6 lot=100 prev_close=189.5
    seq=6 flag=18 SecurityStatus AAPL status=O halt=~ ssr=~
    seq=6 flag=20 SymbolIndexMapping 2 MSFT scale=6 lot=40 prev_close=420.1
    |}];
  Feed.publish
    feed
    (Execution
       { symbol_id = 1
       ; order_id = 5
       ; trade_id = 9
       ; px = Price.of_string "189.45"
       ; qty = 40
       ; auction = None
       });
  Feed.refresh feed ~subscriber:late ~symbol_index:1 ~resting:(fun ~symbol_id:_ ->
    [ 5, Buy, Price.of_string "189.45", 60 ]);
  Feed.second feed;
  Feed.second feed;
  [%expect
    {|
    seq=7 flag=11 OrderExecution AAPL/3 id=5 trade=9 40 @ 189.45
    seq=7 flag=17 SymbolIndexMapping 1 AAPL scale=6 lot=100 prev_close=189.5
    seq=7 flag=18 SecurityStatus AAPL status=O halt=~ ssr=~
    seq=7 flag=20 AddOrderRefresh AAPL/3 id=5 buy 60 @ 189.45
    seq=8 flag=11 SourceTimeReference 1791900000
    seq=9 flag=11 SourceTimeReference 1791900000
    seq=10 flag=1 heartbeat
    |}];
  List.iter (List.rev !json) ~f:print_endline;
  [%expect
    {|
    {"seq":6,"type":"symbol_index_mapping","symbol":"AAPL","scale":6,"lot_size":100,"prev_close":"189.5"}
    {"seq":6,"type":"security_status","symbol":"AAPL","status":"O","halt_condition":"~","ssr_state":"~","lower_band":null,"upper_band":null}
    {"seq":6,"type":"symbol_index_mapping","symbol":"MSFT","scale":6,"lot_size":40,"prev_close":"420.1"}
    {"seq":7,"type":"order_execution","symbol":"AAPL","symbol_seq":3,"source_time_ns":250,"order_id":5,"trade_id":9,"price":"189.45","volume":40,"trade_cond":""}
    {"seq":8,"type":"source_time_reference","source_time":1791900000}
    {"seq":9,"type":"source_time_reference","source_time":1791900000}
    |}]
;;
