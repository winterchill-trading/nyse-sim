open! Core
module H = Harness

let%expect_test "resting order, then a crossing IOC from another member" =
  let h = H.create () in
  H.order h ~cl_ord_id:1 ~side:`Buy ~qty:200 ~price:(Some "189.50");
  H.flush h;
  [%expect
    {|
    TRADER1: OrderAck type=1 sym=1 clordid=1 orderid=1 px=189.5 qty=200 leaves=200 reason=0
    feed: Add 1 id=1 B 200@189.5
    |}];
  H.order
    h
    ~user:"TRADER2"
    ~tif:2
    ~cl_ord_id:7
    ~side:`Sell
    ~qty:300
    ~price:(Some "189.00");
  H.flush h;
  [%expect
    {|
    TRADER2: OrderAck type=1 sym=1 clordid=7 orderid=2 px=189 qty=300 leaves=300 reason=0
    feed: Execution 1 id=1 trade=1 200@189.5
    TRADER2: Exec sym=1 clordid=7 orderid=2 deal=1 px=189.5 qty=200 cum=200 leaves=100 liq=R
    TRADER1: Exec sym=1 clordid=1 orderid=1 deal=1 px=189.5 qty=200 cum=200 leaves=0 liq=A
    TRADER2: ModCancelAck type=11 sym=1 orderid=2 ref=7 orig=7 qty=300 leaves=0 reason=11
    |}]
;;

let%expect_test "cancel, modify and cancel/replace" =
  let h = H.create () in
  H.order h ~cl_ord_id:1 ~side:`Buy ~qty:300 ~price:(Some "189.40");
  H.order h ~cl_ord_id:2 ~side:`Sell ~qty:100 ~price:(Some "190.00");
  H.flush h;
  [%expect
    {|
    TRADER1: OrderAck type=1 sym=1 clordid=1 orderid=1 px=189.4 qty=300 leaves=300 reason=0
    feed: Add 1 id=1 B 300@189.4
    TRADER1: OrderAck type=1 sym=1 clordid=2 orderid=2 px=190 qty=100 leaves=100 reason=0
    feed: Add 1 id=2 S 100@190
    |}];
  H.modify h ~cl_ord_id:3 ~orig:1 ~qty:200;
  H.flush h;
  [%expect
    {|
    feed: Modify 1 id=1 200@189.4 lost-priority
    TRADER1: ModCancelAck type=9 sym=1 orderid=1 ref=3 orig=1 qty=200 leaves=200 reason=0
    |}];
  H.order h ~cl_ord_id:4 ~orig:2 ~side:`Sell ~qty:100 ~price:(Some "189.90");
  H.flush h;
  [%expect
    {|
    TRADER1: OrderAck type=8 sym=1 clordid=4 orderid=4 px=189.9 qty=100 leaves=100 reason=0
    feed: Replace 1 id=2 new=4 S 100@189.9
    |}];
  H.cancel h ~cl_ord_id:5 ~orig:1;
  H.cancel h ~cl_ord_id:6 ~orig:1;
  H.flush h;
  [%expect
    {|
    feed: Delete 1 id=1
    TRADER1: ModCancelAck type=11 sym=1 orderid=1 ref=5 orig=1 qty=200 leaves=0 reason=17
    TRADER1: Reject type=3 sym=1 clordid=6 reason=8
    |}]
;;

let%expect_test "rejects" =
  let h = H.create () in
  H.order h ~cl_ord_id:1 ~side:`Buy ~qty:100 ~price:(Some "189.505");
  H.order h ~symbol_id:99 ~cl_ord_id:2 ~side:`Buy ~qty:100 ~price:(Some "1");
  H.order h ~cl_ord_id:3 ~side:`Buy ~qty:0 ~price:(Some "189");
  H.order h ~cl_ord_id:4 ~side:`Sell ~qty:100 ~price:(Some "190");
  H.order h ~cl_ord_id:4 ~side:`Sell ~qty:100 ~price:(Some "190");
  H.order h ~user:"TRADER2" ~alo:true ~cl_ord_id:5 ~side:`Buy ~qty:100 ~price:(Some "190");
  H.order h ~tif:3 ~cl_ord_id:6 ~side:`Buy ~qty:100 ~price:(Some "189");
  H.order h ~tif:2 ~cl_ord_id:7 ~side:`Sell ~qty:100 ~price:(Some "189");
  H.flush h;
  [%expect
    {|
    TRADER1: Reject type=1 sym=1 clordid=1 reason=2
    TRADER1: Reject type=1 sym=99 clordid=2 reason=1
    TRADER1: Reject type=1 sym=1 clordid=3 reason=3
    TRADER1: OrderAck type=1 sym=1 clordid=4 orderid=1 px=190 qty=100 leaves=100 reason=0
    feed: Add 1 id=1 S 100@190
    TRADER1: Reject type=1 sym=1 clordid=4 reason=7
    TRADER2: Reject type=1 sym=1 clordid=5 reason=9
    TRADER1: Reject type=1 sym=1 clordid=6 reason=14
    TRADER1: OrderAck type=1 sym=1 clordid=7 orderid=3 px=189 qty=100 leaves=100 reason=0
    TRADER1: ModCancelAck type=11 sym=1 orderid=3 ref=7 orig=7 qty=100 leaves=0 reason=11
    |}]
;;

let%expect_test "reserve order shows MaxFloor and refreshes from the reserve" =
  let h = H.create () in
  H.order h ~max_floor:100 ~cl_ord_id:1 ~side:`Sell ~qty:300 ~price:(Some "189.60");
  H.flush h;
  [%expect
    {|
    TRADER1: OrderAck type=1 sym=1 clordid=1 orderid=1 px=189.6 qty=300 leaves=300 reason=0
    feed: Add 1 id=1 S 100@189.6
    |}];
  H.order h ~user:"TRADER2" ~tif:2 ~cl_ord_id:1 ~side:`Buy ~qty:150 ~price:(Some "189.60");
  H.flush h;
  [%expect
    {|
    TRADER2: OrderAck type=1 sym=1 clordid=1 orderid=2 px=189.6 qty=150 leaves=150 reason=0
    feed: Execution 1 id=1 trade=1 100@189.6
    TRADER2: Exec sym=1 clordid=1 orderid=2 deal=1 px=189.6 qty=100 cum=100 leaves=50 liq=R
    TRADER1: Exec sym=1 clordid=1 orderid=1 deal=1 px=189.6 qty=100 cum=100 leaves=200 liq=A
    TRADER2: ModCancelAck type=11 sym=1 orderid=2 ref=1 orig=1 qty=150 leaves=0 reason=11
    feed: Add 1 id=1 S 100@189.6
    |}]
;;

let%expect_test "self-trade prevention" =
  let h = H.create () in
  H.order h ~cl_ord_id:1 ~side:`Sell ~qty:100 ~price:(Some "189.60");
  H.order h ~cl_ord_id:2 ~stp:2 ~side:`Buy ~qty:100 ~price:(Some "189.60");
  H.order h ~cl_ord_id:3 ~stp:3 ~side:`Buy ~qty:100 ~price:(Some "189.60");
  H.flush h;
  [%expect
    {|
    TRADER1: OrderAck type=1 sym=1 clordid=1 orderid=1 px=189.6 qty=100 leaves=100 reason=0
    feed: Add 1 id=1 S 100@189.6
    TRADER1: Reject type=1 sym=1 clordid=2 reason=10
    feed: Delete 1 id=1
    TRADER1: ModCancelAck type=11 sym=1 orderid=1 ref=1 orig=1 qty=100 leaves=0 reason=10
    TRADER1: OrderAck type=1 sym=1 clordid=3 orderid=2 px=189.6 qty=100 leaves=100 reason=0
    feed: Add 1 id=2 B 100@189.6
    |}];
  H.order h ~cl_ord_id:4 ~side:`Sell ~qty:300 ~price:(Some "189.70");
  H.order h ~cl_ord_id:5 ~stp:5 ~side:`Buy ~qty:100 ~price:(Some "189.70");
  H.flush h;
  [%expect
    {|
    TRADER1: OrderAck type=1 sym=1 clordid=4 orderid=3 px=189.7 qty=300 leaves=300 reason=0
    feed: Add 1 id=3 S 300@189.7
    feed: Modify 1 id=3 200@189.7 lost-priority
    TRADER1: ModCancelAck type=9 sym=1 orderid=3 ref=4 orig=4 qty=300 leaves=200 reason=10
    TRADER1: Reject type=1 sym=1 clordid=5 reason=10
    |}]
;;

let%expect_test "opening auction with imbalance" =
  let h = H.create ~phase:Pre_market () in
  H.set_local_time h 9 0;
  H.order h ~session:4 ~cl_ord_id:1 ~side:`Buy ~qty:300 ~price:(Some "189.00");
  H.order h ~tif:3 ~cl_ord_id:2 ~side:`Buy ~qty:200 ~price:(Some "190.00");
  H.order h ~user:"TRADER2" ~tif:3 ~cl_ord_id:1 ~side:`Sell ~qty:400 ~price:None;
  H.order h ~user:"TRADER2" ~tif:4 ~cl_ord_id:2 ~side:`Sell ~qty:100 ~price:None;
  H.flush h;
  [%expect
    {|
    TRADER1: OrderAck type=1 sym=1 clordid=1 orderid=1 px=189 qty=300 leaves=300 reason=0
    feed: Add 1 id=1 B 300@189
    TRADER1: OrderAck type=1 sym=1 clordid=2 orderid=1000000001 px=190 qty=200 leaves=200 reason=0
    TRADER2: OrderAck type=1 sym=1 clordid=1 orderid=1000000002 px=0 qty=400 leaves=400 reason=0
    TRADER2: OrderAck type=1 sym=1 clordid=2 orderid=1000000003 px=0 qty=100 leaves=100 reason=0
    |}];
  Nyse_sim.Venue.publish_imbalances (H.venue h) ~kind:Opening;
  Nyse_sim.Venue.publish_imbalances (H.venue h) ~kind:Opening;
  H.flush h;
  [%expect
    {| feed: Imbalance 1 930 ref=189.5 paired=200 imbalance=200 S indicative=189 |}];
  H.set_local_time h 9 30;
  Nyse_sim.Venue.set_phase (H.venue h) Core;
  H.flush h;
  [%expect
    {|
    TRADER1: Exec sym=1 clordid=2 orderid=1000000001 deal=2000000001 px=189 qty=200 cum=200 leaves=0 liq=O
    feed: NonDisplayedTrade 1 trade=2000000001 200@189
    feed: Execution 1 id=1 trade=2000000002 200@189 Opening
    TRADER1: Exec sym=1 clordid=1 orderid=1 deal=2000000002 px=189 qty=200 cum=200 leaves=100 liq=O
    TRADER2: Exec sym=1 clordid=1 orderid=1000000002 deal=2000000003 px=189 qty=400 cum=400 leaves=0 liq=O
    feed: Status 1 O halt=~ ssr=~
    feed: Status 2 O halt=~ ssr=~
    feed: Status 3 O halt=~ ssr=~
    feed: Status 4 O halt=~ ssr=~
    feed: Status 5 O halt=~ ssr=~
    feed: Status 6 O halt=~ ssr=~
    feed: Status 7 O halt=~ ssr=~
    feed: Status 8 O halt=~ ssr=~
    |}]
;;

let%expect_test "close: late-ineligible orders are deleted, then everything goes silently"
  =
  let h = H.create () in
  H.order h ~cl_ord_id:1 ~side:`Buy ~qty:200 ~price:(Some "189.00");
  H.order h ~session:5 ~cl_ord_id:2 ~side:`Buy ~qty:100 ~price:(Some "188.00");
  H.order h ~tif:4 ~cl_ord_id:3 ~side:`Sell ~qty:100 ~price:None;
  H.flush h;
  [%expect
    {|
    TRADER1: OrderAck type=1 sym=1 clordid=1 orderid=1 px=189 qty=200 leaves=200 reason=0
    feed: Add 1 id=1 B 200@189
    TRADER1: OrderAck type=1 sym=1 clordid=2 orderid=2 px=188 qty=100 leaves=100 reason=0
    feed: Add 1 id=2 B 100@188
    TRADER1: OrderAck type=1 sym=1 clordid=3 orderid=1000000001 px=0 qty=100 leaves=100 reason=0
    |}];
  H.set_local_time h 16 0;
  Nyse_sim.Venue.set_phase (H.venue h) Post_market;
  H.flush h;
  [%expect
    {|
    feed: Execution 1 id=1 trade=2000000001 100@189 Closing
    TRADER1: Exec sym=1 clordid=1 orderid=1 deal=2000000001 px=189 qty=100 cum=100 leaves=100 liq=C
    TRADER1: Exec sym=1 clordid=3 orderid=1000000001 deal=2000000002 px=189 qty=100 cum=100 leaves=0 liq=C
    feed: Delete 1 id=1
    TRADER1: ModCancelAck type=12 sym=1 orderid=1 ref=1 orig=1 qty=200 leaves=0 reason=15
    feed: Status 1 L halt=~ ssr=~
    feed: Status 2 L halt=~ ssr=~
    feed: Status 3 L halt=~ ssr=~
    feed: Status 4 L halt=~ ssr=~
    feed: Status 5 L halt=~ ssr=~
    feed: Status 6 L halt=~ ssr=~
    feed: Status 7 L halt=~ ssr=~
    feed: Status 8 L halt=~ ssr=~
    |}];
  H.set_local_time h 20 0;
  Nyse_sim.Venue.set_phase (H.venue h) Closed;
  H.flush h;
  [%expect
    {|
    TRADER1: ModCancelAck type=12 sym=1 orderid=2 ref=2 orig=2 qty=100 leaves=0 reason=15
    feed: Status 1 X halt=~ ssr=~
    feed: Status 2 X halt=~ ssr=~
    feed: Status 3 X halt=~ ssr=~
    feed: Status 4 X halt=~ ssr=~
    feed: Status 5 X halt=~ ssr=~
    feed: Status 6 X halt=~ ssr=~
    feed: Status 7 X halt=~ ssr=~
    feed: Status 8 X halt=~ ssr=~
    |}];
  H.order h ~cl_ord_id:4 ~side:`Buy ~qty:100 ~price:(Some "189.00");
  H.flush h;
  [%expect {| TRADER1: Reject type=1 sym=1 clordid=4 reason=4 |}];
  printf "%d live\n" (Nyse_sim.Venue.live_orders (H.venue h));
  [%expect {| 0 live |}]
;;

let%expect_test "LULD: re-pricing, limit state, pause and reopening" =
  let h = H.create () in
  let v = H.venue h in
  print_s [%sexp (Nyse_sim.Venue.luld_bands v ~symbol_id:1 : Equities_core.Halts.Bands.t)];
  [%expect {| ((lower 180.03) (upper 198.98)) |}];
  H.order h ~cl_ord_id:1 ~side:`Buy ~qty:100 ~price:(Some "250");
  H.order
    h
    ~cancel_instead_of_reprice:true
    ~cl_ord_id:2
    ~side:`Buy
    ~qty:100
    ~price:(Some "250");
  H.flush h;
  [%expect
    {|
    TRADER1: OrderAck type=1 sym=1 clordid=1 orderid=1 px=198.98 qty=100 leaves=100 reason=0
    feed: Add 1 id=1 B 100@198.98
    TRADER1: Reject type=1 sym=1 clordid=2 reason=18
    |}];
  H.advance h (Time_ns.Span.of_int_sec 16);
  Nyse_sim.Venue.tick v;
  H.order h ~cl_ord_id:3 ~side:`Buy ~qty:100 ~price:(Some "190");
  H.flush h;
  [%expect
    {|
    feed: Status 1 4 halt=M ssr=~ bands=180.03-198.98
    TRADER1: Reject type=1 sym=1 clordid=3 reason=5
    |}];
  H.order h ~user:"TRADER2" ~cl_ord_id:1 ~side:`Sell ~qty:50 ~price:(Some "198.00");
  H.flush h;
  [%expect {| TRADER2: Reject type=1 sym=1 clordid=1 reason=5 |}];
  H.advance h (Time_ns.Span.of_int_min 5);
  Nyse_sim.Venue.tick v;
  H.flush h;
  [%expect {| feed: Status 1 5 halt=~ ssr=~ bands=180.03-198.98 |}]
;;

let%expect_test "short sale restriction (Tier 2 test symbol, 10% band)" =
  let h = H.create () in
  H.order h ~symbol_id:8 ~cl_ord_id:1 ~side:`Buy ~qty:100 ~price:(Some "9.00");
  H.order
    h
    ~symbol_id:8
    ~user:"TRADER2"
    ~cl_ord_id:1
    ~side:`Sell
    ~qty:100
    ~price:(Some "9.00");
  H.order h ~symbol_id:8 ~cl_ord_id:2 ~side:`Buy ~qty:100 ~price:(Some "8.99");
  H.flush h;
  [%expect
    {|
    TRADER1: OrderAck type=1 sym=8 clordid=1 orderid=1 px=9 qty=100 leaves=100 reason=0
    feed: Add 8 id=1 B 100@9
    TRADER2: OrderAck type=1 sym=8 clordid=1 orderid=2 px=9 qty=100 leaves=100 reason=0
    feed: Execution 8 id=1 trade=1 100@9
    TRADER2: Exec sym=8 clordid=1 orderid=2 deal=1 px=9 qty=100 cum=100 leaves=0 liq=R
    TRADER1: Exec sym=8 clordid=1 orderid=1 deal=1 px=9 qty=100 cum=100 leaves=0 liq=A
    feed: Status 8 O halt=~ ssr=A
    TRADER1: OrderAck type=1 sym=8 clordid=2 orderid=3 px=8.99 qty=100 leaves=100 reason=0
    feed: Add 8 id=3 B 100@8.99
    |}];
  H.order
    h
    ~symbol_id:8
    ~user:"TRADER2"
    ~cl_ord_id:2
    ~side:`Short
    ~qty:100
    ~price:(Some "8.99");
  H.order
    h
    ~symbol_id:8
    ~user:"TRADER2"
    ~cl_ord_id:3
    ~side:`Short
    ~qty:100
    ~price:(Some "9.00");
  H.flush h;
  [%expect
    {|
    TRADER2: Reject type=1 sym=8 clordid=2 reason=13
    TRADER2: OrderAck type=1 sym=8 clordid=3 orderid=4 px=9 qty=100 leaves=100 reason=0
    feed: Add 8 id=4 S 100@9
    |}]
;;
