open! Core
open! Async
module Pillar = Nyse_sim_wire.Pillar
module App = Nyse_sim_wire.Pillar_app
module Oi = Nyse_sim_wire.Order_instructions
module Xdp = Nyse_sim_wire.Xdp
module Price = Exchange_core.Price
module Client = Nyse_sim.Pillar_client

let host =
  Command.Param.(
    flag "-host" (optional_with_default "127.0.0.1" string) ~doc:"HOST simulator host")
;;

let gateway_port =
  Command.Param.(flag "-port" (optional_with_default 4100 int) ~doc:"PORT gateway port")
;;

let feed_port =
  Command.Param.(flag "-feed-port" (optional_with_default 4101 int) ~doc:"PORT feed port")
;;

let user =
  Command.Param.(
    flag "-user" (optional_with_default "TRADER1" string) ~doc:"NAME Pillar username")
;;

let mpid = "WCHL"

let die e =
  Core.eprintf "%s\n%!" (Error.to_string_hum e);
  Stdlib.exit 1
;;

let print_pillar m = print_endline (Pillar.to_string_hum m)

let connect ~host ~port ~user =
  match%map Client.connect ~host ~port ~user with
  | Ok c -> c
  | Error e -> die e
;;

(* Prints GT messages until the stream has been quiet for [quiet]. *)
let drain ?(quiet = Time_ns.Span.of_int_ms 400) ?(first = Time_ns.Span.of_int_sec 3) c =
  let rec loop timeout =
    match%bind Client.next c ~timeout with
    | None -> return ()
    | Some m ->
      print_pillar m;
      loop quiet
  in
  loop first
;;

(* Logs in, opens GT for new messages and TG, turns cancel-on-disconnect off (or on), then
   runs [f]. *)
let with_session ~host ~port ~user ~cod f =
  let%bind c = connect ~host ~port ~user in
  Client.open_gt c ~start_seq:(Client.gt_next c) ~end_seq:(-1);
  let%bind () =
    match%map Client.open_tg c with
    | Ok () -> ()
    | Error e -> die e
  in
  (* The GT OpenResponse arrives first; skip it quietly. *)
  let%bind _ = Client.next c ~timeout:(Time_ns.Span.of_int_ms 200) in
  Client.send
    c
    (Session_config_request
       { username = user
       ; cancel_on_disconnect = cod
       ; throttle_preference = 0
       ; self_trade_prevention = 0
       ; order_priority_update = 0
       ; bold_designation = 0
       });
  let%bind () = f c in
  let%bind () = drain c in
  Client.close c
;;

let symbol_id_exn c symbol =
  match Client.symbol_id c symbol with
  | Ok id -> id
  | Error e -> die e
;;

let default_cl_ord_id () =
  Time_ns.to_int_ns_since_epoch (Time_ns.now ()) / 1_000_000 mod 1_000_000_000
;;

let cod =
  Command.Param.(flag "-cod" no_arg ~doc:" keep cancel on disconnect on (default off)")
;;

let logon =
  Command.async
    ~summary:"Log in, print the stream state, the symbol list and replay the GT stream"
    (let%map_open.Command host
     and port = gateway_port
     and user in
     fun () ->
       let%bind c = connect ~host ~port ~user in
       List.iter (Client.greeting c) ~f:print_pillar;
       List.iter (Client.symbols c) ~f:(fun s ->
         print_endline (Pillar.app_to_string_hum (Symbol_ref s)));
       let gt_next = Client.gt_next c in
       if gt_next > 1
       then (
         Client.open_gt c ~start_seq:1 ~end_seq:gt_next;
         let%bind () = drain c in
         Client.close c)
       else Client.close c)
;;

let side_arg =
  Command.Arg_type.of_alist_exn
    [ "buy", Oi.Side.buy
    ; "sell", Oi.Side.sell
    ; "short", Oi.Side.sell_short
    ; "short-exempt", Oi.Side.sell_short_exempt
    ]
;;

let tif_arg =
  Command.Arg_type.of_alist_exn
    [ "day", Oi.Tif.day; "ioc", Oi.Tif.ioc; "opg", Oi.Tif.opg; "close", Oi.Tif.on_close ]
;;

let order =
  Command.async
    ~summary:
      "Send a New Order Single (or a cancel/replace with -replace) and print the \
       responses"
    (let%map_open.Command host
     and port = gateway_port
     and user
     and cod
     and symbol = flag "-symbol" (required string) ~doc:"SYM symbol, e.g. AAPL"
     and side =
       flag "-side" (required side_arg) ~doc:"SIDE buy, sell, short or short-exempt"
     and qty = flag "-qty" (required int) ~doc:"N shares"
     and price =
       flag "-price" (optional string) ~doc:"PX limit price (omit for a market order)"
     and tif =
       flag
         "-tif"
         (optional_with_default Oi.Tif.day tif_arg)
         ~doc:"TIF day, ioc, opg or close"
     and alo = flag "-alo" no_arg ~doc:" add liquidity only (post only)"
     and max_floor =
       flag "-max-floor" (optional int) ~doc:"N reserve order: shares displayed"
     and session =
       flag
         "-session"
         (optional_with_default Oi.Trading_session.early_core_late int)
         ~doc:"N TradingSessionID (default 6: early, core and late)"
     and stp = flag "-stp" (optional_with_default 0 int) ~doc:"N SelfTradeType 0-5"
     and cl_ord_id =
       flag "-clordid" (optional int) ~doc:"N ClOrdID (default: derived from the time)"
     and replace =
       flag "-replace" (optional int) ~doc:"N cancel/replace the order with this ClOrdID"
     in
     fun () ->
       with_session ~host ~port ~user ~cod (fun c ->
         let cl_ord_id = Option.value_or_thunk cl_ord_id ~default:default_cl_ord_id in
         let instructions =
           { (Oi.limit_day ~side) with
             ord_type =
               (if Option.is_some price then Oi.Ord_type.limit else Oi.Ord_type.market)
           ; time_in_force = tif
           ; trading_session = session
           ; extended_exec_inst = (if alo then Oi.Extended_exec_inst.alo else 0)
           ; self_trade_type = stp
           }
         in
         Client.send
           c
           (New_order
              { symbol_id = symbol_id_exn c symbol
              ; mpid
              ; mmid = 0
              ; mp_sub_id = '\000'
              ; cl_ord_id
              ; orig_cl_ord_id = Option.value replace ~default:0
              ; instructions
              ; price = Option.value_map price ~default:Price.zero ~f:Price.of_string
              ; order_qty = qty
              ; min_qty = 0
              ; user_data = "cli"
              ; max_floor
              });
         return ()))
;;

let cancel =
  Command.async
    ~summary:"Cancel an order by its ClOrdID"
    (let%map_open.Command host
     and port = gateway_port
     and user
     and cod
     and id = flag "-id" (required int) ~doc:"N ClOrdID of the order to cancel"
     and symbol =
       flag
         "-symbol"
         (optional_with_default "AAPL" string)
         ~doc:"SYM symbol (default AAPL)"
     in
     fun () ->
       with_session ~host ~port ~user ~cod (fun c ->
         Client.send
           c
           (Cancel_request
              { symbol_id = symbol_id_exn c symbol
              ; mpid
              ; cl_ord_id = default_cl_ord_id ()
              ; orig_cl_ord_id = id
              });
         return ()))
;;

let modify =
  Command.async
    ~summary:"Reduce an order's quantity (Order Modify Request)"
    (let%map_open.Command host
     and port = gateway_port
     and user
     and cod
     and id = flag "-id" (required int) ~doc:"N ClOrdID of the order"
     and qty = flag "-qty" (required int) ~doc:"N new order quantity"
     and symbol =
       flag
         "-symbol"
         (optional_with_default "AAPL" string)
         ~doc:"SYM symbol (default AAPL)"
     in
     fun () ->
       with_session ~host ~port ~user ~cod (fun c ->
         Client.send
           c
           (Modify_request
              { symbol_id = symbol_id_exn c symbol
              ; mpid
              ; cl_ord_id = default_cl_ord_id ()
              ; orig_cl_ord_id = id
              ; order_qty = qty
              ; side = 0
              ; locate_reqd = false
              });
         return ()))
;;

(* Feed *)

let connect_feed ~host ~port =
  match%bind
    Monitor.try_with_or_error (fun () ->
      Tcp.connect (Tcp.Where_to_connect.of_host_and_port { host; port }))
  with
  | Ok (_, r, w) -> return (r, w)
  | Error e -> die e
;;

(* Reads packets until [stop] is determined or the stream ends, calling [f] on each. *)
let read_packets reader ~stop ~f =
  let rec loop () =
    match%bind
      choose
        [ choice stop (fun () -> `Stop)
        ; choice (Nyse_sim.Framing.read_xdp reader) (fun p -> `Packet p)
        ]
    with
    | `Stop | `Packet None -> return ()
    | `Packet (Some b) ->
      (match Xdp.Packet.decode b with
       | Ok p -> if f p then loop () else return ()
       | Error e ->
         Core.eprintf "bad packet: %s\n%!" (Error.to_string_hum e);
         loop ())
  in
  loop ()
;;

let symbols_table () = Int.Table.create ()

let learn tbl (m : Xdp.Msg.t) =
  match m with
  | Symbol_index_mapping { symbol_index; symbol; price_scale_code; _ } ->
    Hashtbl.set tbl ~key:symbol_index ~data:(symbol, price_scale_code)
  | _ -> ()
;;

let feed =
  Command.async
    ~summary:"Decode and print the XDP feed"
    (let%map_open.Command host
     and port = feed_port
     and count = flag "-count" (optional int) ~doc:"N stop after N messages"
     and seconds =
       flag
         "-seconds"
         (optional_with_default 5. float)
         ~doc:"S stop after S seconds (default 5)"
     and heartbeats =
       flag "-heartbeats" no_arg ~doc:" also print heartbeats and time references"
     and symbol =
       flag "-symbol" (optional string) ~doc:"SYM only this symbol's messages"
     in
     fun () ->
       let%bind reader, writer = connect_feed ~host ~port in
       let tbl = symbols_table () in
       let symbols i = Hashtbl.find tbl i in
       let seen = ref 0 in
       let wanted (m : Xdp.Msg.t) =
         match symbol, Xdp.Msg.symbol_index m with
         | None, _ -> true
         | Some _, None -> false
         | Some s, Some i ->
           Option.value_map (symbols i) ~default:false ~f:(fun (name, _) ->
             String.equal name (String.uppercase s))
       in
       let%bind () =
         read_packets
           reader
           ~stop:(after (Time_float.Span.of_sec seconds))
           ~f:(fun p ->
             if List.is_empty p.msgs && heartbeats then printf "%8d hb\n" p.seq_num;
             List.iter p.msgs ~f:(fun m ->
               learn tbl m;
               let quiet =
                 match m with
                 | Source_time_reference _ -> not heartbeats
                 | _ -> false
               in
               if (not quiet) && wanted m
               then (
                 incr seen;
                 printf
                   "%8d %2d %s\n"
                   p.seq_num
                   p.delivery_flag
                   (Xdp.Msg.to_string_hum ~symbols m)));
             Option.value_map count ~default:true ~f:(fun n -> !seen < n))
       in
       Writer.close writer)
;;

let book =
  Command.async
    ~summary:"Build a symbol's book from a feed refresh plus live updates and print it"
    (let%map_open.Command host
     and port = feed_port
     and symbol = flag "-symbol" (required string) ~doc:"SYM symbol"
     and seconds =
       flag
         "-seconds"
         (optional_with_default 1. float)
         ~doc:"S listen for S seconds (default 1)"
     and depth =
       flag "-depth" (optional_with_default 5 int) ~doc:"N levels per side (default 5)"
     in
     fun () ->
       let%bind reader, writer = connect_feed ~host ~port in
       let tbl = symbols_table () in
       let target = ref None in
       let orders : (char * int * int) Int.Table.t = Int.Table.create () in
       let name = String.uppercase symbol |> String.tr ~target:'.' ~replacement:' ' in
       let requested = ref false in
       let request index =
         if not !requested
         then (
           requested := true;
           Nyse_sim.Framing.write
             writer
             (Xdp.Packet.encode
                { delivery_flag = Xdp.Delivery_flag.original
                ; seq_num = 0
                ; send_time = 0
                ; send_time_ns = 0
                ; msgs =
                    [ Refresh_request
                        { symbol_index = index
                        ; source_id = "cli"
                        ; product_id = 115
                        ; channel_id = 1
                        }
                    ]
                }))
       in
       let apply (m : Xdp.Msg.t) =
         match m with
         | Add_order { order_id; price; volume; side; _ }
         | Add_order_refresh { order_id; price; volume; side; _ } ->
           Hashtbl.set orders ~key:order_id ~data:(side, price, volume)
         | Modify_order { order_id; price; volume; side; _ } ->
           Hashtbl.set orders ~key:order_id ~data:(side, price, volume)
         | Delete_order { order_id; _ } -> Hashtbl.remove orders order_id
         | Order_execution { order_id; volume; _ } ->
           Hashtbl.change orders order_id ~f:(function
             | Some (s, p, v) when v > volume -> Some (s, p, v - volume)
             | _ -> None)
         | Replace_order { order_id; new_order_id; price; volume; side; _ } ->
           Hashtbl.remove orders order_id;
           Hashtbl.set orders ~key:new_order_id ~data:(side, price, volume)
         | Security_status { security_status = 'X'; _ } -> Hashtbl.clear orders
         | _ -> ()
       in
       let%bind () =
         read_packets
           reader
           ~stop:(after (Time_float.Span.of_sec seconds))
           ~f:(fun p ->
             List.iter p.msgs ~f:(fun m ->
               learn tbl m;
               (match m with
                | Symbol_index_mapping { symbol_index; symbol = s; _ }
                  when String.equal s name && Option.is_none !target ->
                  target := Some symbol_index;
                  request symbol_index
                | _ -> ());
               match !target, Xdp.Msg.symbol_index m with
               | Some t, Some i when t = i -> apply m
               | _ -> ());
             true)
       in
       let%bind () = Writer.close writer in
       (match !target with
        | None -> printf "%s: not on the feed\n" symbol
        | Some index ->
          let scale = Option.value_map (Hashtbl.find tbl index) ~default:6 ~f:snd in
          let levels side =
            Hashtbl.data orders
            |> List.filter ~f:(fun (s, _, _) -> Char.equal s side)
            |> List.map ~f:(fun (_, p, v) -> p, v)
            |> Int.Map.of_alist_fold ~init:(0, 0) ~f:(fun (q, n) v -> q + v, n + 1)
            |> Map.to_alist
          in
          let px p = Price.to_string (Xdp.price_of_raw ~scale p) in
          let bids = List.rev (levels 'B') |> Fn.flip List.take depth
          and asks = levels 'S' |> Fn.flip List.take depth in
          printf "%s book (%d orders)\n" name (Hashtbl.length orders);
          List.iter (List.rev asks) ~f:(fun (p, (q, n)) ->
            printf "  ask %10s %8d  (%d)\n" (px p) q n);
          printf "  ---\n";
          List.iter bids ~f:(fun (p, (q, n)) ->
            printf "  bid %10s %8d  (%d)\n" (px p) q n));
       return ())
;;

let () =
  Command.group
    ~summary:"Client for nyse-sim: Pillar order entry and XDP feed"
    [ "logon", logon
    ; "order", order
    ; "cancel", cancel
    ; "modify", modify
    ; "feed", feed
    ; "book", book
    ]
  |> Command_unix.run
;;
