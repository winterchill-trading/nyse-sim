open! Core
open Nyse_sim_wire
module Price = Exchange_core.Price

let common : Xdp.Common.t = { source_time_ns = 500; symbol_index = 1; symbol_seq_num = 7 }

let symbols = function
  | 1 -> Some ("AAPL", 6)
  | _ -> None
;;

let msgs : Xdp.Msg.t list =
  [ Sequence_number_reset
      { source_time = 1_791_000_000
      ; source_time_ns = 0
      ; product_id = 115
      ; channel_id = 1
      }
  ; Source_time_reference { id = 1; symbol_seq_num = 0; source_time = 1_791_000_001 }
  ; Symbol_index_mapping
      { symbol_index = 1
      ; symbol = "AAPL"
      ; market_id = 1
      ; system_id = 1
      ; exchange_code = 'N'
      ; price_scale_code = 6
      ; security_type = 'A'
      ; lot_size = 100
      ; prev_close_price = 189_500_000
      ; prev_close_volume = 0
      ; price_resolution = 1
      ; round_lot = 'Y'
      ; mpv = 10_000
      ; unit_of_trade = 100
      }
  ; Refresh_request
      { symbol_index = 0; source_id = "cli"; product_id = 115; channel_id = 1 }
  ; Security_status
      { source_time = 1
      ; source_time_ns = 2
      ; symbol_index = 1
      ; symbol_seq_num = 3
      ; security_status = '4'
      ; halt_condition = 'M'
      ; price_1 = 4
      ; price_2 = 5
      ; ssr_triggering_exchange = ' '
      ; ssr_triggering_volume = 0
      ; time = 6
      ; ssr_state = '~'
      ; market_state = 'O'
      ; session_state = ' '
      }
  ; Add_order
      { common
      ; order_id = 1_000_001
      ; price = 189_500_000
      ; volume = 100
      ; side = 'B'
      ; firm_id = "WCHL"
      }
  ; Modify_order
      { common
      ; order_id = 1_000_001
      ; price = 189_500_000
      ; volume = 50
      ; position_change = 1
      ; side = 'B'
      }
  ; Delete_order { common; order_id = 1_000_001 }
  ; Order_execution
      { common
      ; order_id = 1_000_001
      ; trade_id = 9
      ; price = 189_490_000
      ; volume = 40
      ; printable_flag = 1
      ; trade_cond = ""
      }
  ; Replace_order
      { common
      ; order_id = 1_000_001
      ; new_order_id = 1_000_002
      ; price = 189_510_000
      ; volume = 100
      ; side = 'S'
      }
  ; Imbalance
      { source_time = 1
      ; source_time_ns = 2
      ; symbol_index = 1
      ; symbol_seq_num = 3
      ; reference_price = 189_500_000
      ; paired_qty = 300
      ; total_imbalance_qty = 200
      ; market_imbalance_qty = 100
      ; auction_time = 930
      ; auction_type = 'M'
      ; imbalance_side = 'B'
      ; continuous_book_clearing_price = 0
      ; auction_interest_clearing_price = 0
      ; ssr_filing_price = 0
      ; indicative_match_price = 189_520_000
      ; upper_collar = 0
      ; lower_collar = 0
      ; auction_status = 0
      ; freeze_status = 0
      ; num_extensions = 0
      ; unpaired_qty = 0
      ; unpaired_side = ' '
      ; significant_imbalance = ' '
      }
  ; Add_order_refresh
      { common
      ; order_id = 5
      ; price = 189_000_000
      ; volume = 200
      ; side = 'S'
      ; firm_id = ""
      }
  ; Non_displayed_trade
      { common
      ; trade_id = 10
      ; price = 189_500_000
      ; volume = 300
      ; printable_flag = 1
      ; trade_cond = "O"
      }
  ]
;;

let%expect_test "every message round trips at its documented size" =
  List.iter msgs ~f:(fun m ->
    let b = Xdp.Msg.encode m in
    [%test_result: Xdp.Msg.t]
      (Or_error.ok_exn (Xdp.Msg.read b ~pos:0 ~len:(Bigstring.length b)))
      ~expect:m;
    printf
      "%3d %2d  %s\n"
      (Le.get_u16 b 2)
      (Le.get_u16 b 0)
      (Xdp.Msg.to_string_hum ~symbols m));
  [%expect
    {|
      1 14  SequenceNumberReset product=115 channel=1
      2 16  SourceTimeReference 1791000001
      3 44  SymbolIndexMapping 1 AAPL scale=6 lot=100 prev_close=189.5
     15 20  RefreshRequest 0
     34 46  SecurityStatus AAPL status=4 halt=M ssr=~
    100 39  AddOrder AAPL/7 id=1000001 buy 100 @ 189.5
    101 35  ModifyOrder AAPL/7 id=1000001 50 @ 189.5 lost-priority
    102 25  DeleteOrder AAPL/7 id=1000001
    103 42  OrderExecution AAPL/7 id=1000001 trade=9 40 @ 189.49
    104 42  ReplaceOrder AAPL/7 id=1000001 new=1000002 sell 100 @ 189.51
    105 73  Imbalance AAPL type=M ref=189.5 paired=300 imbalance=200 side=B indicative=189.52
    106 39  AddOrderRefresh AAPL/7 id=5 sell 200 @ 189
    110 33  NonDisplayedTrade AAPL/7 trade=10 300 @ 189.5
    |}]
;;

let%expect_test "Add Order bytes" =
  let b = Xdp.Msg.encode (List.nth_exn msgs 5) in
  print_endline (Le.hex b);
  [%expect
    {| 27 00 64 00 f4 01 00 00 01 00 00 00 07 00 00 00 41 42 0f 00 00 00 00 00 60 8a 4b 0b 64 00 00 00 42 57 43 48 4c 00 00 |}]
;;

let%expect_test "packet header and message skipping by MsgSize" =
  let p : Xdp.Packet.t =
    { delivery_flag = Xdp.Delivery_flag.original
    ; seq_num = 42
    ; send_time = 1_791_000_000
    ; send_time_ns = 123
    ; msgs = [ List.nth_exn msgs 5; List.nth_exn msgs 7 ]
    }
  in
  let b = Xdp.Packet.encode p in
  print_endline (Le.hex (Bigstring.sub b ~pos:0 ~len:16));
  printf "%d\n" (Xdp.Packet.frame_length b ~pos:0);
  [%expect {|
    50 00 0b 02 2a 00 00 00 c0 7d c0 6a 7b 00 00 00
    80
    |}];
  [%test_result: Xdp.Packet.t] (Or_error.ok_exn (Xdp.Packet.decode b)) ~expect:p;
  (* A heartbeat is a bare header. *)
  let hb = { p with delivery_flag = Xdp.Delivery_flag.heartbeat; msgs = [] } in
  print_endline (Le.hex (Xdp.Packet.encode hb));
  [%expect {| 10 00 01 00 2a 00 00 00 c0 7d c0 6a 7b 00 00 00 |}]
;;

let%expect_test "a message grown by trailing fields is still read" =
  let b = Xdp.Msg.encode (List.nth_exn msgs 7) in
  let grown = Bigstring.create 30 in
  Bigstring.memset grown ~pos:0 ~len:30 '\255';
  Bigstring.blit ~src:b ~src_pos:0 ~dst:grown ~dst_pos:0 ~len:25;
  Le.set_u16 grown 0 30;
  print_s [%sexp (Xdp.Msg.read grown ~pos:0 ~len:30 : Xdp.Msg.t Or_error.t)];
  [%expect
    {|
    (Ok
     (Delete_order
      (common ((source_time_ns 500) (symbol_index 1) (symbol_seq_num 7)))
      (order_id 1000001)))
    |}]
;;

let%expect_test "price scales" =
  List.iter [ "189.5"; "512.25"; "150000" ] ~f:(fun s ->
    let p = Price.of_string s in
    let scale = Xdp.scale_for_price p in
    let raw = Xdp.raw_of_price ~scale p in
    printf
      "%s scale=%d raw=%d back=%s\n"
      s
      scale
      raw
      (Price.to_string (Xdp.price_of_raw ~scale raw)));
  [%expect
    {|
    189.5 scale=6 raw=189500000 back=189.5
    512.25 scale=4 raw=5122500 back=512.25
    150000 scale=3 raw=150000000 back=150000
    |}]
;;
