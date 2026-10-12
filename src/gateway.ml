open! Core
module Pillar = Nyse_sim_wire.Pillar
module App = Nyse_sim_wire.Pillar_app
module Stream_id = Nyse_sim_wire.Stream_id

module Log_stream = struct
  (* A read stream: every message ever sent, and at most one live reader. *)
  type t =
    { stream_id : Stream_id.t
    ; msgs : Pillar.t Queue.t
    ; mutable reader : (Pillar.t -> unit) option
    }

  let create stream_id = { stream_id; msgs = Queue.create (); reader = None }

  let append t ~timestamp payload =
    let m =
      Pillar.Seq_msg
        { stream_id = t.stream_id; seq = Queue.length t.msgs + 1; timestamp; payload }
    in
    Queue.enqueue t.msgs m;
    Option.iter t.reader ~f:(fun f -> f m)
  ;;
end

module User = struct
  type t =
    { name : string
    ; index : int
    ; gt : Log_stream.t
    ; ref_ : Log_stream.t
    ; mutable tg_next : int
    ; mutable conn : int option
    ; mutable cancel_on_disconnect : bool
    }

  let name t = t.name
  let index t = t.index
  let stream t kind = Stream_id.create kind ~user:t.index

  let log t (kind : Stream_id.Kind.t) =
    match kind with
    | Gt -> Some t.gt
    | Ref -> Some t.ref_
    | Tg | Other _ -> None
  ;;

  let length t kind =
    Option.value_map (log t kind) ~default:0 ~f:(fun l -> Queue.length l.msgs)
  ;;

  let tg_next t = t.tg_next
  let cancel_on_disconnect t = t.cancel_on_disconnect
end

type t =
  { listings : Listing.t list
  ; clock : unit -> Time_ns.t
  ; users : User.t String.Table.t
  ; venue : Venue.t Set_once.t
  }

let create listings ~clock =
  { listings; clock; users = String.Table.create (); venue = Set_once.create () }
;;

let set_venue t v = Set_once.set_exn t.venue v
let venue t = Set_once.get_exn t.venue
let timestamp t = Time_ns.to_int_ns_since_epoch (t.clock ())
let send_ref t (u : User.t) m = Log_stream.append u.ref_ ~timestamp:(timestamp t) m

let user t name =
  match Hashtbl.find t.users name with
  | Some u -> u
  | None ->
    let index = Hashtbl.length t.users + 1 in
    let u : User.t =
      { name
      ; index
      ; gt = Log_stream.create (Stream_id.create Gt ~user:index)
      ; ref_ = Log_stream.create (Stream_id.create Ref ~user:index)
      ; tg_next = 1
      ; conn = None
      ; cancel_on_disconnect = true
      }
    in
    Hashtbl.set t.users ~key:name ~data:u;
    List.iter t.listings ~f:(fun (l : Listing.t) ->
      send_ref
        t
        u
        (Symbol_ref
           { transact_time = timestamp t
           ; symbol_id = l.symbol_id
           ; symbol = l.nyse_symbol
           ; listed_mic = l.listed_mic
           ; round_lot = l.round_lot
           ; adv_risk_range = 0
           ; mpv_class_id = 1
           ; test_symbol = l.test
           }));
    u
;;

let send_user t name m =
  let u = user t name in
  Log_stream.append u.gt ~timestamp:(timestamp t) m
;;

let attach _ (u : User.t) ~conn =
  match u.conn with
  | Some c when c <> conn -> Or_error.error_s [%message "already logged in" ~user:u.name]
  | _ ->
    u.conn <- Some conn;
    Ok ()
;;

let detach _ (u : User.t) ~conn =
  match u.conn with
  | Some c when c = conn ->
    u.conn <- None;
    u.gt.reader <- None;
    u.ref_.reader <- None
  | _ -> ()
;;

let open_read _ (u : User.t) kind ~start_seq ~end_seq ~deliver =
  match User.log u kind with
  | None -> ()
  | Some log ->
    let stop = if end_seq < 0 then Int.max_value else end_seq in
    Queue.iteri log.msgs ~f:(fun i m ->
      let seq = i + 1 in
      if seq >= start_seq && seq < stop then deliver m);
    log.reader <- (if end_seq < 0 then Some deliver else None)
;;

let close_read _ (u : User.t) kind =
  Option.iter (User.log u kind) ~f:(fun log -> log.reader <- None)
;;

let accept_tg (u : User.t) ~seq =
  if seq = u.tg_next
  then (
    u.tg_next <- seq + 1;
    Ok ())
  else Error u.tg_next
;;

let set_cancel_on_disconnect (u : User.t) b = u.cancel_on_disconnect <- b
