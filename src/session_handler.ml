open! Core
module Pillar = Nyse_sim_wire.Pillar
module App = Nyse_sim_wire.Pillar_app
module Stream_id = Nyse_sim_wire.Stream_id
module R = Nyse_sim_wire.Reason_code

type t =
  { gateway : Gateway.t
  ; conn : int
  ; wall : unit -> Time_ns.t
  ; send : Pillar.t -> unit
  ; close : reason:string -> unit
  ; connected_at : Time_ns.t
  ; mutable last_heard : Time_ns.t
  ; mutable user : Gateway.User.t option
  ; mutable tg_open : bool
  ; mutable read_open : Stream_id.Kind.t list
  ; mutable closed : bool
  }

let heartbeat_timeout = Time_ns.Span.of_int_sec 5

let create ?(wall = Time_ns.now) gateway ~conn ~send ~close =
  let now = wall () in
  { gateway
  ; conn
  ; wall
  ; send
  ; close
  ; connected_at = now
  ; last_heard = now
  ; user = None
  ; tg_open = false
  ; read_open = []
  ; closed = false
  }
;;

let username t = Option.map t.user ~f:Gateway.User.name

let stream_avail (u : Gateway.User.t) : Pillar.t list =
  [ Stream_avail
      { stream_id = Gateway.User.stream u Tg
      ; next_seq = Gateway.User.tg_next u
      ; access = Pillar.Access.write
      }
  ; Stream_avail
      { stream_id = Gateway.User.stream u Gt
      ; next_seq = Gateway.User.length u Gt + 1
      ; access = Pillar.Access.read
      }
  ; Stream_avail
      { stream_id = Gateway.User.stream u Ref
      ; next_seq = Gateway.User.length u Ref + 1
      ; access = Pillar.Access.read
      }
  ]
;;

let logout t =
  match t.user with
  | None -> ()
  | Some u ->
    t.user <- None;
    Gateway.detach t.gateway u ~conn:t.conn;
    if Gateway.User.cancel_on_disconnect u
    then
      Venue.cancel_all
        (Gateway.venue t.gateway)
        ~user:(Gateway.User.name u)
        ~reason:R.cancel_on_disconnect
;;

let shut t ~reason =
  if not t.closed
  then (
    t.closed <- true;
    logout t;
    t.close ~reason)
;;

let disconnected t =
  t.closed <- true;
  logout t
;;

let login t ~username ~version =
  let respond status = t.send (Login_response { username; status }) in
  if Option.is_some t.user
  then respond Pillar.Status.already_logged_in
  else if not (String.equal version Pillar.protocol_version)
  then respond Pillar.Status.invalid_protocol_version
  else if String.is_empty username
  then respond Pillar.Status.invalid_login
  else (
    let u = Gateway.user t.gateway username in
    match Gateway.attach t.gateway u ~conn:t.conn with
    | Error _ -> respond Pillar.Status.already_logged_in
    | Ok () ->
      t.user <- Some u;
      respond Pillar.Status.ok;
      List.iter (stream_avail u) ~f:t.send)
;;

let owns (u : Gateway.User.t) (s : Stream_id.t) =
  [%equal: Stream_id.t] s (Gateway.User.stream u s.kind)
;;

let open_stream t u ~(stream_id : Stream_id.t) ~start_seq ~end_seq ~access =
  let respond status =
    t.send
      (Open_response { stream_id; status; access = (if status = 0 then access else 0) })
  in
  if not (owns u stream_id)
  then respond Pillar.Status.invalid_stream
  else (
    match stream_id.kind with
    | Tg ->
      if access land Pillar.Access.write = 0
      then respond Pillar.Status.no_stream_permission
      else (
        t.tg_open <- true;
        respond Pillar.Status.ok)
    | Gt | Ref ->
      if access land Pillar.Access.read = 0
      then respond Pillar.Status.no_stream_permission
      else (
        respond Pillar.Status.ok;
        t.read_open <- stream_id.kind :: t.read_open;
        Gateway.open_read
          t.gateway
          u
          stream_id.kind
          ~start_seq:(Int.max 1 start_seq)
          ~end_seq
          ~deliver:t.send)
    | Other _ -> respond Pillar.Status.invalid_stream)
;;

let close_stream t u ~(stream_id : Stream_id.t) =
  if not (owns u stream_id)
  then t.send (Close_response { stream_id; status = Pillar.Status.invalid_stream })
  else (
    (match stream_id.kind with
     | Tg -> t.tg_open <- false
     | Gt | Ref ->
       Gateway.close_read t.gateway u stream_id.kind;
       t.read_open
       <- List.filter t.read_open ~f:(fun k ->
            not ([%equal: Stream_id.Kind.t] k stream_id.kind))
     | Other _ -> ());
    t.send (Close_response { stream_id; status = Pillar.Status.ok }))
;;

let application t u (payload : App.t) =
  let venue = Gateway.venue t.gateway in
  let user = Gateway.User.name u in
  match payload with
  | Session_config_request r ->
    Gateway.set_cancel_on_disconnect u r.cancel_on_disconnect;
    Venue.set_default_stp venue ~user r.self_trade_prevention;
    Gateway.send_ref
      t.gateway
      u
      (Session_config_ack
         { transact_time = Time_ns.to_int_ns_since_epoch (Venue.now venue)
         ; user_session_type = 1
         ; user_session_status = 0
         ; username = user
         ; mic = "XNYS"
         ; cancel_on_disconnect = r.cancel_on_disconnect
         ; throttle_preference = r.throttle_preference
         ; self_trade_prevention = r.self_trade_prevention
         ; order_priority_update = r.order_priority_update
         ; bold_designation = r.bold_designation
         })
  | New_order m -> Venue.new_order venue ~user m
  | Cancel_request m -> Venue.cancel venue ~user m
  | Modify_request m -> Venue.modify venue ~user m
  | Sequenced_filler -> ()
  | Symbol_ref _
  | Session_config_ack _
  | Order_ack _
  | Mod_cancel_ack _
  | Execution_report _
  | Reject _
  | Unknown _ ->
    Gateway.send_user
      t.gateway
      user
      (Reject
         { transact_time = Time_ns.to_int_ns_since_epoch (Venue.now venue)
         ; symbol_id = 0
         ; mpid = ""
         ; cl_ord_id = 0
         ; reason_code = R.unsupported
         ; reject_type = App.Reject_type.order
         ; user_data = ""
         })
;;

let handle t (m : Pillar.t) =
  t.last_heard <- t.wall ();
  match m, t.user with
  | Login { username; version; _ }, _ -> login t ~username ~version
  | Heartbeat, _ -> ()
  | (Open { stream_id; _ } | Close { stream_id }), None ->
    t.send (Open_response { stream_id; status = Pillar.Status.not_logged_in; access = 0 })
  | Seq_msg { stream_id; _ }, None ->
    t.send (Close_response { stream_id; status = Pillar.Status.not_logged_in })
  | Open { stream_id; start_seq; end_seq; access; _ }, Some u ->
    open_stream t u ~stream_id ~start_seq ~end_seq ~access
  | Close { stream_id }, Some u -> close_stream t u ~stream_id
  | Seq_msg { stream_id; seq; payload; _ }, Some u ->
    if not (owns u stream_id && [%equal: Stream_id.Kind.t] stream_id.kind Tg)
    then t.send (Close_response { stream_id; status = Pillar.Status.invalid_stream })
    else if not t.tg_open
    then t.send (Close_response { stream_id; status = Pillar.Status.stream_not_open })
    else (
      match Gateway.accept_tg u ~seq with
      | Error _ ->
        t.send (Close_response { stream_id; status = Pillar.Status.out_of_sequence })
      | Ok () -> application t u payload)
  | ( (Login_response _ | Stream_avail _ | Open_response _ | Close_response _ | Unknown _)
    , _ ) -> shut t ~reason:"invalid message"
;;

let tick t =
  if not t.closed
  then (
    let now = t.wall () in
    match t.user with
    | None ->
      if Time_ns.Span.(Time_ns.diff now t.connected_at > heartbeat_timeout)
      then shut t ~reason:"login timed out"
    | Some u ->
      if Time_ns.Span.(Time_ns.diff now t.last_heard > heartbeat_timeout)
      then shut t ~reason:"heartbeat timeout"
      else List.iter (stream_avail u) ~f:t.send)
;;
