open! Core
module Pillar = Nyse_sim_wire.Pillar
module App = Nyse_sim_wire.Pillar_app
module Oi = Nyse_sim_wire.Order_instructions
module Stream_id = Nyse_sim_wire.Stream_id
module Price = Exchange_core.Price
open Nyse_sim

type env =
  { gateway : Gateway.t
  ; mutable wall : Time_ns.t
  ; mutable next_conn : int
  }

let listings = List.take Listing.defaults 2
let start = Time_ns.of_string_with_utc_offset "2026-10-13 14:00:00Z"

let env () =
  let gateway = Gateway.create listings ~clock:(fun () -> start) in
  let venue =
    Venue.create
      listings
      ~now:(fun () -> start)
      ~send_user:(Gateway.send_user gateway)
      ~publish:ignore
  in
  Venue.set_phase venue Core;
  Gateway.set_venue gateway venue;
  { gateway; wall = start; next_conn = 1 }
;;

let connect e name =
  let conn = e.next_conn in
  e.next_conn <- conn + 1;
  Session_handler.create
    ~wall:(fun () -> e.wall)
    e.gateway
    ~conn
    ~send:(fun m -> printf "%s< %s\n" name (Pillar.to_string_hum m))
    ~close:(fun ~reason -> printf "%s closed: %s\n" name reason)
;;

let stream kind = Stream_id.create kind ~user:1

let open_ ?(start_seq = 1) ?(end_seq = -1) h kind access =
  Session_handler.handle
    h
    (Open { stream_id = stream kind; start_seq; end_seq; access; mode = 0 })
;;

let tg h seq payload =
  Session_handler.handle
    h
    (Seq_msg { stream_id = stream Tg; seq; timestamp = 0; payload })
;;

let new_order ~cl_ord_id ~price : App.t =
  New_order
    { symbol_id = 1
    ; mpid = "WCHL"
    ; mmid = 0
    ; mp_sub_id = '\000'
    ; cl_ord_id
    ; orig_cl_ord_id = 0
    ; instructions = Oi.limit_day ~side:Oi.Side.buy
    ; price = Price.of_string price
    ; order_qty = 100
    ; min_qty = 0
    ; user_data = ""
    ; max_floor = None
    }
;;

let login h user =
  Session_handler.handle
    h
    (Login { username = user; password = "x"; mic = "XNYS"; version = "1.1" })
;;

let%expect_test "login, streams, orders, replay and cancel on disconnect" =
  let e = env () in
  let a = connect e "a" in
  Session_handler.handle
    a
    (Open { stream_id = stream Gt; start_seq = 1; end_seq = -1; access = 1; mode = 0 });
  login a "TRADER1";
  [%expect
    {|
    a< OpenResponse GT/1 status=18 (not logged in) access=0
    a< LoginResponse user=TRADER1 status=0 (ok)
    a< StreamAvail TG/1 next_seq=1 access=2
    a< StreamAvail GT/1 next_seq=1 access=1
    a< StreamAvail REF/1 next_seq=3 access=1
    |}];
  open_ a Ref Pillar.Access.read ~end_seq:3;
  open_ a Gt Pillar.Access.read;
  open_ a Tg Pillar.Access.read;
  open_ a Tg Pillar.Access.write;
  [%expect
    {|
    a< OpenResponse REF/1 status=0 (ok) access=1
    a< REF/1 #1 SymbolRef id=1 AAPL mic=XNAS lot=100
    a< REF/1 #2 SymbolRef id=2 MSFT mic=XNAS lot=40
    a< OpenResponse GT/1 status=0 (ok) access=1
    a< OpenResponse TG/1 status=54 (no stream permission) access=0
    a< OpenResponse TG/1 status=0 (ok) access=2
    |}];
  tg a 1 (new_order ~cl_ord_id:1 ~price:"189.00");
  tg a 3 (new_order ~cl_ord_id:2 ~price:"189.00");
  tg a 2 (new_order ~cl_ord_id:2 ~price:"189.01");
  [%expect
    {|
    a< GT/1 #1 OrderAck type=1 sym=1 clordid=1 orderid=1 px=189 qty=100 leaves=100 reason=0
    a< CloseResponse TG/1 status=82 (out of sequence)
    a< GT/1 #2 OrderAck type=1 sym=1 clordid=2 orderid=2 px=189.01 qty=100 leaves=100 reason=0
    |}];
  (* A second connection cannot take the same user. *)
  let b = connect e "b" in
  login b "TRADER1";
  [%expect {| b< LoginResponse user=TRADER1 status=27 (already logged in) |}];
  (* Heartbeats keep the session; silence closes it and cancels the orders. *)
  e.wall <- Time_ns.add e.wall (Time_ns.Span.of_int_sec 3);
  Session_handler.handle a Heartbeat;
  Session_handler.tick a;
  [%expect
    {|
    a< StreamAvail TG/1 next_seq=3 access=2
    a< StreamAvail GT/1 next_seq=3 access=1
    a< StreamAvail REF/1 next_seq=3 access=1
    |}];
  e.wall <- Time_ns.add e.wall (Time_ns.Span.of_int_sec 6);
  Session_handler.tick a;
  [%expect {| a closed: heartbeat timeout |}];
  (* Reconnect and replay the GT stream from 3. *)
  let c = connect e "c" in
  login c "TRADER1";
  open_ c Gt Pillar.Access.read ~start_seq:3;
  [%expect
    {|
    c< LoginResponse user=TRADER1 status=0 (ok)
    c< StreamAvail TG/1 next_seq=3 access=2
    c< StreamAvail GT/1 next_seq=5 access=1
    c< StreamAvail REF/1 next_seq=3 access=1
    c< OpenResponse GT/1 status=0 (ok) access=1
    c< GT/1 #3 ModCancelAck type=11 sym=1 orderid=1 ref=1 orig=1 qty=100 leaves=0 reason=16
    c< GT/1 #4 ModCancelAck type=11 sym=1 orderid=2 ref=2 orig=2 qty=100 leaves=0 reason=16
    |}]
;;

let%expect_test "login checks" =
  let e = env () in
  let a = connect e "a" in
  Session_handler.handle
    a
    (Login { username = "TRADER1"; password = ""; mic = "XNYS"; version = "1.0" });
  Session_handler.handle
    a
    (Login { username = ""; password = ""; mic = "XNYS"; version = "1.1" });
  e.wall <- Time_ns.add e.wall (Time_ns.Span.of_int_sec 6);
  Session_handler.tick a;
  [%expect
    {|
    a< LoginResponse user=TRADER1 status=81 (invalid protocol version)
    a< LoginResponse user= status=24 (invalid login)
    a closed: login timed out
    |}]
;;
