open! Core
open! Async
module Pillar = Nyse_sim_wire.Pillar
module Xdp = Nyse_sim_wire.Xdp
module Session = Equities_core.Session
module Phase = Session.Us_equities.Phase
module Market_sim = Exchange_core_engine.Market_sim

module Clock_mode = struct
  type t =
    | Real
    | Accelerated of
        { start : Time_ns.t
        ; speed : float
        }
    | Always_open
  [@@deriving sexp_of]
end

module Config = struct
  type t =
    { gateway_port : int
    ; feed_port : int
    ; http_port : int
    ; listings : Listing.t list
    ; clock : Clock_mode.t
    ; noise : bool
    ; seed : int
    }
  [@@deriving sexp_of]
end

type t =
  { config : Config.t
  ; venue : Venue.t
  ; feed : Feed.t
  ; gateway : Gateway.t
  ; gateway_server : (Socket.Address.Inet.t, int) Tcp.Server.t
  ; feed_server : (Socket.Address.Inet.t, int) Tcp.Server.t
  ; http_server : (Socket.Address.Inet.t, int) Cohttp_async.Server.t
  ; stopped : unit Ivar.t
  ; mutable next_conn : int
  }

let gateway_port t = Tcp.Server.listening_on t.gateway_server
let feed_port t = Tcp.Server.listening_on t.feed_server
let http_port t = Cohttp_async.Server.listening_on t.http_server
let venue t = t.venue

let clock (mode : Clock_mode.t) =
  match mode with
  | Real | Always_open -> Time_ns.now
  | Accelerated { start; speed } ->
    let wall0 = Time_ns.now () in
    fun () ->
      let elapsed = Time_ns.Span.to_sec (Time_ns.diff (Time_ns.now ()) wall0) in
      Time_ns.add start (Time_ns.Span.of_sec (elapsed *. speed))
;;

let phase_of (mode : Clock_mode.t) now : Phase.t =
  match mode with
  | Always_open -> Core
  | Real | Accelerated _ -> Session.Us_equities.phase_at now
;;

let local_minutes now =
  let _, since_midnight = Session.Zone.to_local New_york now in
  Time_ns.Span.to_int_sec since_midnight / 60
;;

(* Imbalance periods: 09:00 to the open, 15:50 to the close. *)
let imbalance_kind (phase : Phase.t) now : Equities_core.Auction.Kind.t option =
  let m = local_minutes now in
  match phase with
  | Pre_market when m >= 9 * 60 -> Some Opening
  | Core when m >= (15 * 60) + 50 -> Some Closing
  | _ -> None
;;

(* Gateway connections *)

let handle_gateway t ~wall reader writer =
  let conn = t.next_conn in
  t.next_conn <- conn + 1;
  Writer.set_raise_when_consumer_leaves writer false;
  let closed = Ivar.create () in
  upon (Writer.consumer_left writer) (fun () -> Ivar.fill_if_empty closed ());
  let send m = Framing.write writer (Pillar.encode m) in
  let handler =
    Session_handler.create ~wall t.gateway ~conn ~send ~close:(fun ~reason ->
      Log.Global.info "gateway %d: closing (%s)" conn reason;
      Ivar.fill_if_empty closed ())
  in
  Clock_ns.every
    ~stop:(Deferred.any [ Ivar.read closed; Ivar.read t.stopped ])
    Time_ns.Span.second
    (fun () -> Session_handler.tick handler);
  let rec loop () =
    if Ivar.is_full closed
    then return ()
    else (
      match%bind
        choose
          [ choice (Framing.read_pillar reader) (fun r -> `Read r)
          ; choice (Ivar.read closed) (fun () -> `Closed)
          ]
      with
      | `Closed | `Read None -> return ()
      | `Read (Some frame) ->
        (match Pillar.decode frame with
         | Ok m ->
           Log.Global.debug "gateway %d <- %s" conn (Pillar.to_string_hum m);
           Session_handler.handle handler m
         | Error e ->
           Log.Global.info_s [%message "gateway: bad frame" (conn : int) (e : Error.t)]);
        loop ())
  in
  let%bind () = loop () in
  let user = Session_handler.username handler in
  Session_handler.disconnected handler;
  Log.Global.info
    "gateway %d: disconnected%s"
    conn
    (Option.value_map user ~default:"" ~f:(sprintf " (%s)"));
  Writer.close writer
;;

(* Feed connections *)

let handle_feed t reader writer =
  Writer.set_raise_when_consumer_leaves writer false;
  let id = Feed.subscribe t.feed (Binary (Framing.write writer)) in
  let rec loop () =
    match%bind Framing.read_xdp reader with
    | None -> return ()
    | Some frame ->
      (match Xdp.Packet.decode frame with
       | Ok p ->
         List.iter p.msgs ~f:(function
           | Refresh_request { symbol_index; _ } ->
             Feed.refresh t.feed ~subscriber:id ~symbol_index ~resting:(fun ~symbol_id ->
               Venue.resting t.venue ~symbol_id)
           | _ -> ())
       | Error e -> Log.Global.info_s [%message "feed: bad packet" (e : Error.t)]);
      loop ()
  in
  let%bind () =
    Deferred.any
      [ loop ()
      ; Writer.close_started writer
      ; Writer.consumer_left writer
      ; Ivar.read t.stopped
      ]
  in
  Feed.unsubscribe t.feed id;
  Writer.close writer
;;

(* HTTP: the JSON feed on /ws *)

let handle_ws t reader writer =
  let ws = Websocket.create ~opcode:`Text ~role:Server reader writer in
  let incoming, outgoing = Websocket.pipes ws in
  let id =
    Feed.subscribe t.feed (Json (fun s -> Pipe.write_without_pushback_if_open outgoing s))
  in
  let%map () =
    Deferred.any
      [ Pipe.iter_without_pushback incoming ~f:ignore
      ; Pipe.closed outgoing
      ; Ivar.read t.stopped
      ]
  in
  Feed.unsubscribe t.feed id;
  Pipe.close outgoing
;;

let http_handler t ~body:_ _addr req =
  let path = Uri.path (Cohttp.Request.uri req) in
  let headers = Cohttp.Request.headers req in
  match path, Cohttp.Header.get headers "sec-websocket-key" with
  | "/ws", Some key ->
    let headers =
      Cohttp.Header.of_list
        [ "upgrade", "websocket"
        ; "connection", "Upgrade"
        ; ( "sec-websocket-accept"
          , Websocket.sec_websocket_accept_header_value ~sec_websocket_key:key )
        ]
    in
    return
      (`Expert
        ( Http.Response.make ~status:`Switching_protocols ~headers ()
        , fun reader writer -> handle_ws t reader writer ))
  | "/", _ | "/symbols", _ ->
    let symbols =
      List.map (Venue.listings t.venue) ~f:(fun (l : Listing.t) ->
        `Assoc
          [ "symbol_id", `Int l.symbol_id
          ; "symbol", `String l.nyse_symbol
          ; "reference_price", `String (Exchange_core.Price.to_string l.reference_price)
          ; "round_lot", `Int l.round_lot
          ])
    in
    let%map r =
      Cohttp_async.Server.respond_string
        ~headers:(Cohttp.Header.of_list [ "content-type", "application/json" ])
        (Yojson.Safe.to_string
           (`Assoc
             [ "venue", `String "nyse-sim"
             ; "phase", `String (Sexp.to_string [%sexp (Venue.phase t.venue : Phase.t)])
             ; "feed_seq", `Int (Feed.last_seq t.feed)
             ; "symbols", `List symbols
             ]))
    in
    `Response r
  | _ ->
    let%map r = Cohttp_async.Server.respond_string ~status:`Not_found "not found\n" in
    `Response r
;;

(* Timers *)

let run_session_clock t now =
  let last_second = ref Time_ns.epoch in
  Clock_ns.every ~stop:(Ivar.read t.stopped) (Time_ns.Span.of_int_ms 100) (fun () ->
    let n = now () in
    let phase = phase_of t.config.clock n in
    if not (Phase.equal phase (Venue.phase t.venue))
    then (
      Log.Global.info
        "session: %s at %s"
        (Sexp.to_string [%sexp (phase : Phase.t)])
        (Session.to_local_string New_york n);
      Venue.set_phase t.venue phase);
    let wall = Time_ns.now () in
    if Time_ns.Span.(Time_ns.diff wall !last_second >= Time_ns.Span.second)
    then (
      last_second := wall;
      Venue.tick t.venue;
      Option.iter (imbalance_kind phase n) ~f:(fun kind ->
        Venue.publish_imbalances t.venue ~kind);
      Feed.second t.feed))
;;

let run_noise t =
  List.iter t.config.listings ~f:(fun (l : Listing.t) ->
    let sim =
      Market_sim.create
        { Market_sim.Config.default with
          seed = t.config.seed + l.symbol_id
        ; symbols = [ l.symbol ]
        ; initial_prices = [ l.symbol, l.reference_price ]
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
        (Venue.exchange t.venue)
    in
    Clock_ns.every
      ~stop:(Ivar.read t.stopped)
      (Market_sim.config sim).tick_interval
      (fun () ->
         Venue.with_engine t.venue ~symbol_id:l.symbol_id ~f:(fun _ ->
           Market_sim.tick sim)))
;;

let start (config : Config.t) =
  let now = clock config.clock in
  let feed = Feed.create config.listings ~now in
  let gateway = Gateway.create config.listings ~clock:now in
  let venue =
    Venue.create
      config.listings
      ~now
      ~send_user:(Gateway.send_user gateway)
      ~publish:(Feed.publish feed)
  in
  Gateway.set_venue gateway venue;
  Feed.start feed;
  let t_ref = Set_once.create () in
  let get () = Set_once.get_exn t_ref in
  let tcp port f =
    Tcp.Server.create
      ~on_handler_error:`Ignore
      (Tcp.Where_to_listen.of_port port)
      (fun _addr reader writer -> f (get ()) reader writer)
  in
  let%bind gateway_server =
    tcp config.gateway_port (fun t reader writer ->
      handle_gateway t ~wall:Time_ns.now reader writer)
  in
  let%bind feed_server = tcp config.feed_port handle_feed in
  let%bind http_server =
    Cohttp_async.Server.create_expert
      ~on_handler_error:`Ignore
      (Tcp.Where_to_listen.of_port config.http_port)
      (fun ~body addr req -> http_handler (get ()) ~body addr req)
  in
  let t =
    { config
    ; venue
    ; feed
    ; gateway
    ; gateway_server
    ; feed_server
    ; http_server
    ; stopped = Ivar.create ()
    ; next_conn = 1
    }
  in
  Set_once.set_exn t_ref t;
  Venue.set_phase venue (phase_of config.clock (now ()));
  run_session_clock t now;
  if config.noise then run_noise t;
  return t
;;

let stop t =
  Ivar.fill_if_empty t.stopped ();
  let%bind () = Tcp.Server.close t.gateway_server in
  let%bind () = Tcp.Server.close t.feed_server in
  Cohttp_async.Server.close t.http_server
;;
