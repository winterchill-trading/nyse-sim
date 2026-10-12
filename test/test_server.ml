open! Core
open! Async
module Pillar = Nyse_sim_wire.Pillar
module Oi = Nyse_sim_wire.Order_instructions
module Xdp = Nyse_sim_wire.Xdp
module Price = Exchange_core.Price
open Nyse_sim

let new_order ~symbol_id ~cl_ord_id ~side ~price : Nyse_sim_wire.Pillar_app.t =
  New_order
    { symbol_id
    ; mpid = "WCHL"
    ; mmid = 0
    ; mp_sub_id = '\000'
    ; cl_ord_id
    ; orig_cl_ord_id = 0
    ; instructions = Oi.limit_day ~side
    ; price = Price.of_string price
    ; order_qty = 100
    ; min_qty = 0
    ; user_data = ""
    ; max_floor = None
    }
;;

let login server user =
  let%bind c =
    Pillar_client.connect ~host:"127.0.0.1" ~port:(Server.gateway_port server) ~user
    >>| Or_error.ok_exn
  in
  Pillar_client.open_gt c ~start_seq:1 ~end_seq:(-1);
  let%map () = Pillar_client.open_tg c >>| Or_error.ok_exn in
  c
;;

(* Prints the next [n] sequenced messages. *)
let rec print_gt c n =
  if n = 0
  then return ()
  else (
    match%bind Pillar_client.next c ~timeout:(Time_ns.Span.of_int_sec 5) with
    | None -> return ()
    | Some (Seq_msg _ as m) ->
      print_endline (Pillar.to_string_hum m);
      print_gt c (n - 1)
    | Some _ -> print_gt c n)
;;

let symbols = function
  | 1 -> Some ("AAPL", 6)
  | 2 -> Some ("MSFT", 6)
  | _ -> None
;;

(* Prints the next [n] feed messages other than time references. *)
let rec print_feed reader n =
  if n <= 0
  then return ()
  else (
    match%bind Framing.read_xdp reader with
    | None -> return ()
    | Some b ->
      let p = Or_error.ok_exn (Xdp.Packet.decode b) in
      let msgs =
        List.filter p.msgs ~f:(function
          | Source_time_reference _ -> false
          | _ -> true)
      in
      List.iter msgs ~f:(fun m ->
        printf "%2d %s\n" p.delivery_flag (Xdp.Msg.to_string_hum ~symbols m));
      print_feed reader (n - List.length msgs))
;;

let%expect_test "gateway and feed over TCP" =
  Log.Global.set_output [];
  let%bind server =
    Server.start
      { gateway_port = 0
      ; feed_port = 0
      ; http_port = 0
      ; listings = List.take Listing.defaults 2
      ; clock = Always_open
      ; noise = false
      ; seed = 0
      ; anchors = []
      ; anchor_half_life = Time_ns.Span.second
      }
  in
  let%bind _, feed_reader, feed_writer =
    Tcp.connect
      (Tcp.Where_to_connect.of_host_and_port
         { host = "127.0.0.1"; port = Server.feed_port server })
  in
  let%bind buyer = login server "T1" in
  List.iter (Pillar_client.greeting buyer) ~f:(fun m ->
    print_endline (Pillar.to_string_hum m));
  let symbol_id = Or_error.ok_exn (Pillar_client.symbol_id buyer "msft") in
  Pillar_client.send
    buyer
    (new_order ~symbol_id ~cl_ord_id:1 ~side:Oi.Side.buy ~price:"420.00");
  let%bind () = print_gt buyer 1 in
  let%bind seller = login server "T2" in
  Pillar_client.send
    seller
    (new_order ~symbol_id ~cl_ord_id:1 ~side:Oi.Side.sell ~price:"419.00");
  let%bind () = print_gt seller 2 in
  let%bind () = print_gt buyer 1 in
  [%expect
    {|
    LoginResponse user=T1 status=0 (ok)
    StreamAvail TG/1 next_seq=1 access=2
    StreamAvail GT/1 next_seq=1 access=1
    StreamAvail REF/1 next_seq=3 access=1
    GT/1 #1 OrderAck type=1 sym=2 clordid=1 orderid=1 px=420 qty=100 leaves=100 reason=0
    GT/2 #1 OrderAck type=1 sym=2 clordid=1 orderid=2 px=419 qty=100 leaves=100 reason=0
    GT/2 #2 Exec sym=2 clordid=1 orderid=2 deal=1 px=420 qty=100 cum=100 leaves=0 liq=R
    GT/1 #2 Exec sym=2 clordid=1 orderid=1 deal=1 px=420 qty=100 cum=100 leaves=0 liq=A
    |}];
  let%bind () = print_feed feed_reader 6 in
  [%expect
    {|
    17 SymbolIndexMapping 1 AAPL scale=6 lot=100 prev_close=189.5
    18 SecurityStatus AAPL status=O halt=~ ssr=~
    18 SymbolIndexMapping 2 MSFT scale=6 lot=40 prev_close=420.1
    20 SecurityStatus MSFT status=O halt=~ ssr=~
    11 AddOrder MSFT/2 id=1 buy 100 @ 420
    11 OrderExecution MSFT/3 id=1 trade=1 100 @ 420
    |}];
  let%bind () = Pillar_client.close buyer in
  let%bind () = Pillar_client.close seller in
  let%bind () = Writer.close feed_writer in
  Server.stop server
;;
