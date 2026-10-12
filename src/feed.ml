open! Core
module Price = Exchange_core.Price
module Side = Exchange_core.Side
module Xdp = Nyse_sim_wire.Xdp
module Auction = Equities_core.Auction

module Sink = struct
  type t =
    | Binary of (Bigstring.t -> unit)
    | Json of (string -> unit)
end

type t =
  { listings : Listing.t Int.Table.t
  ; now : unit -> Time_ns.t
  ; subscribers : Sink.t Int.Table.t
  ; symbol_seq : int Int.Table.t
  ; status : Market_event.Status.t Int.Table.t
  ; mutable seq : int
  ; mutable next_subscriber : int
  ; mutable sent_this_second : bool
  }

let system_id = 1
let market_id = 1
let product_id = 115
let channel_id = 1

let create listings ~now =
  { listings =
      Int.Table.of_alist_exn
        (List.map listings ~f:(fun (l : Listing.t) -> l.symbol_id, l))
  ; now
  ; subscribers = Int.Table.create ()
  ; symbol_seq = Int.Table.create ()
  ; status = Int.Table.create ()
  ; seq = 0
  ; next_subscriber = 0
  ; sent_this_second = false
  }
;;

let last_seq t = t.seq
let num_subscribers t = Hashtbl.length t.subscribers

let time t =
  let ns = Time_ns.to_int_ns_since_epoch (t.now ()) in
  ns / 1_000_000_000, ns mod 1_000_000_000
;;

let listing t symbol_id = Hashtbl.find_exn t.listings symbol_id
let raw t symbol_id p = Xdp.raw_of_price ~scale:(listing t symbol_id).scale p

let next_symbol_seq t symbol_id =
  let n = 1 + Option.value (Hashtbl.find t.symbol_seq symbol_id) ~default:0 in
  Hashtbl.set t.symbol_seq ~key:symbol_id ~data:n;
  n
;;

let side_char s = if Side.is_buy s then 'B' else 'S'

let trade_cond : Auction.Kind.t option -> string = function
  | None -> ""
  | Some Opening -> "O"
  | Some Closing -> "6"
  | Some Halt_reopening -> "5"
;;

let auction_type : Auction.Kind.t -> char = function
  | Opening -> 'M'
  | Closing -> 'C'
  | Halt_reopening -> 'H'
;;

let mapping (l : Listing.t) : Xdp.Msg.t =
  Symbol_index_mapping
    { symbol_index = l.symbol_id
    ; symbol = l.nyse_symbol
    ; market_id
    ; system_id
    ; exchange_code = 'N'
    ; price_scale_code = l.scale
    ; security_type = 'A'
    ; lot_size = l.round_lot
    ; prev_close_price = Xdp.raw_of_price ~scale:l.scale l.reference_price
    ; prev_close_volume = 0
    ; price_resolution = 1
    ; round_lot = 'Y'
    ; mpv = Xdp.raw_of_price ~scale:l.scale (Price.of_string "0.01")
    ; unit_of_trade = l.round_lot
    }
;;

let status_msg t ~symbol_id ~symbol_seq_num (s : Market_event.Status.t) : Xdp.Msg.t =
  let sec, ns = time t in
  let band = Option.value_map ~default:0 ~f:(raw t symbol_id) in
  Security_status
    { source_time = sec
    ; source_time_ns = ns
    ; symbol_index = symbol_id
    ; symbol_seq_num
    ; security_status = s.security_status
    ; halt_condition = s.halt_condition
    ; price_1 = band s.price_1
    ; price_2 = band s.price_2
    ; ssr_triggering_exchange = (if Char.equal s.ssr_state 'A' then 'N' else ' ')
    ; ssr_triggering_volume = 0
    ; time = sec
    ; ssr_state = s.ssr_state
    ; market_state = s.security_status
    ; session_state = ' '
    }
;;

let to_xdp t (e : Market_event.t) : Xdp.Msg.t =
  let symbol_id = Market_event.symbol_id e in
  let symbol_seq_num = next_symbol_seq t symbol_id in
  let sec, ns = time t in
  let common : Xdp.Common.t =
    { source_time_ns = ns; symbol_index = symbol_id; symbol_seq_num }
  in
  let raw = raw t symbol_id in
  match e with
  | Add { order_id; side; px; qty; _ } ->
    Add_order
      { common
      ; order_id
      ; price = raw px
      ; volume = qty
      ; side = side_char side
      ; firm_id = ""
      }
  | Modify { order_id; side; px; qty; lost_priority; _ } ->
    Modify_order
      { common
      ; order_id
      ; price = raw px
      ; volume = qty
      ; position_change = (if lost_priority then 1 else 0)
      ; side = side_char side
      }
  | Delete { order_id; _ } -> Delete_order { common; order_id }
  | Execution { order_id; trade_id; px; qty; auction; _ } ->
    Order_execution
      { common
      ; order_id
      ; trade_id = trade_id land 0xffff_ffff
      ; price = raw px
      ; volume = qty
      ; printable_flag = 1
      ; trade_cond = trade_cond auction
      }
  | Replace { order_id; new_order_id; side; px; qty; _ } ->
    Replace_order
      { common
      ; order_id
      ; new_order_id
      ; price = raw px
      ; volume = qty
      ; side = side_char side
      }
  | Non_displayed_trade { trade_id; px; qty; auction; _ } ->
    Non_displayed_trade
      { common
      ; trade_id = trade_id land 0xffff_ffff
      ; price = raw px
      ; volume = qty
      ; printable_flag = 1
      ; trade_cond = trade_cond auction
      }
  | Status { status; _ } ->
    Hashtbl.set t.status ~key:symbol_id ~data:status;
    status_msg t ~symbol_id ~symbol_seq_num status
  | Imbalance { imbalance = i; auction_time; _ } ->
    let side = Option.value_map i.imbalance_side ~default:' ' ~f:side_char in
    Imbalance
      { source_time = sec
      ; source_time_ns = ns
      ; symbol_index = symbol_id
      ; symbol_seq_num
      ; reference_price = raw i.reference_price
      ; paired_qty = i.paired_qty
      ; total_imbalance_qty = i.imbalance_qty
      ; market_imbalance_qty = i.market_imbalance_qty
      ; auction_time
      ; auction_type = auction_type i.kind
      ; imbalance_side = side
      ; continuous_book_clearing_price = 0
      ; auction_interest_clearing_price = 0
      ; ssr_filing_price = 0
      ; indicative_match_price = Option.value_map i.indicative_price ~default:0 ~f:raw
      ; upper_collar = 0
      ; lower_collar = 0
      ; auction_status = 0
      ; freeze_status = 0
      ; num_extensions = 0
      ; unpaired_qty = 0
      ; unpaired_side = ' '
      ; significant_imbalance = ' '
      }
;;

(* JSON *)

let msg_type_name : Xdp.Msg.t -> string = function
  | Sequence_number_reset _ -> "sequence_number_reset"
  | Source_time_reference _ -> "source_time_reference"
  | Symbol_index_mapping _ -> "symbol_index_mapping"
  | Refresh_request _ -> "refresh_request"
  | Security_status _ -> "security_status"
  | Add_order _ -> "add_order"
  | Modify_order _ -> "modify_order"
  | Delete_order _ -> "delete_order"
  | Order_execution _ -> "order_execution"
  | Replace_order _ -> "replace_order"
  | Imbalance _ -> "imbalance"
  | Add_order_refresh _ -> "add_order_refresh"
  | Non_displayed_trade _ -> "non_displayed_trade"
  | Unknown _ -> "unknown"
;;

let to_json t ~seq (m : Xdp.Msg.t) : Yojson.Safe.t =
  let symbol = Option.bind (Xdp.Msg.symbol_index m) ~f:(Hashtbl.find t.listings) in
  let px raw =
    match symbol with
    | Some l -> `String (Price.to_string (Xdp.price_of_raw ~scale:l.scale raw))
    | None -> `Int raw
  in
  let chr c = `String (String.of_char c) in
  let common (c : Xdp.Common.t) =
    [ "symbol_seq", `Int c.symbol_seq_num; "source_time_ns", `Int c.source_time_ns ]
  in
  let fields =
    match m with
    | Sequence_number_reset { source_time; _ } -> [ "source_time", `Int source_time ]
    | Source_time_reference { source_time; _ } -> [ "source_time", `Int source_time ]
    | Symbol_index_mapping m ->
      [ "scale", `Int m.price_scale_code
      ; "lot_size", `Int m.lot_size
      ; "prev_close", px m.prev_close_price
      ]
    | Refresh_request _ | Unknown _ -> []
    | Security_status m ->
      [ "status", chr m.security_status
      ; "halt_condition", chr m.halt_condition
      ; "ssr_state", chr m.ssr_state
      ; "lower_band", px m.price_1
      ; "upper_band", px m.price_2
      ]
    | Add_order { common = c; order_id; price; volume; side; _ }
    | Add_order_refresh { common = c; order_id; price; volume; side; _ } ->
      common c
      @ [ "order_id", `Int order_id
        ; "side", chr side
        ; "price", px price
        ; "volume", `Int volume
        ]
    | Modify_order { common = c; order_id; price; volume; side; position_change } ->
      common c
      @ [ "order_id", `Int order_id
        ; "side", chr side
        ; "price", px price
        ; "volume", `Int volume
        ; "lost_priority", `Bool (position_change = 1)
        ]
    | Delete_order { common = c; order_id } -> common c @ [ "order_id", `Int order_id ]
    | Order_execution { common = c; order_id; trade_id; price; volume; trade_cond; _ } ->
      common c
      @ [ "order_id", `Int order_id
        ; "trade_id", `Int trade_id
        ; "price", px price
        ; "volume", `Int volume
        ; "trade_cond", `String trade_cond
        ]
    | Replace_order { common = c; order_id; new_order_id; price; volume; side } ->
      common c
      @ [ "order_id", `Int order_id
        ; "new_order_id", `Int new_order_id
        ; "side", chr side
        ; "price", px price
        ; "volume", `Int volume
        ]
    | Non_displayed_trade { common = c; trade_id; price; volume; trade_cond; _ } ->
      common c
      @ [ "trade_id", `Int trade_id
        ; "price", px price
        ; "volume", `Int volume
        ; "trade_cond", `String trade_cond
        ]
    | Imbalance m ->
      [ "auction_type", chr m.auction_type
      ; "auction_time", `Int m.auction_time
      ; "reference_price", px m.reference_price
      ; "paired_qty", `Int m.paired_qty
      ; "imbalance_qty", `Int m.total_imbalance_qty
      ; "imbalance_side", chr m.imbalance_side
      ; "indicative_price", px m.indicative_match_price
      ]
  in
  `Assoc
    ([ "seq", `Int seq; "type", `String (msg_type_name m) ]
     @ (match symbol with
        | Some l -> [ "symbol", `String l.nyse_symbol ]
        | None -> [])
     @ fields)
;;

(* Sending *)

let packet t ~delivery_flag ~seq_num msgs =
  let sec, ns = time t in
  Xdp.Packet.encode { delivery_flag; seq_num; send_time = sec; send_time_ns = ns; msgs }
;;

let send_sequenced t ?(delivery_flag = Xdp.Delivery_flag.original) msg =
  t.seq <- t.seq + 1;
  t.sent_this_second <- true;
  let seq = t.seq in
  let bin = lazy (packet t ~delivery_flag ~seq_num:seq [ msg ]) in
  let json = lazy (Yojson.Safe.to_string (to_json t ~seq msg)) in
  Hashtbl.iter t.subscribers ~f:(function
    | Binary f -> f (Lazy.force bin)
    | Json f -> f (Lazy.force json))
;;

let publish t e = send_sequenced t (to_xdp t e)

let source_time_reference t : Xdp.Msg.t =
  Source_time_reference { id = system_id; symbol_seq_num = 0; source_time = fst (time t) }
;;

let start t =
  let sec, ns = time t in
  t.seq <- 0;
  send_sequenced
    t
    ~delivery_flag:Xdp.Delivery_flag.sequence_reset
    (Sequence_number_reset
       { source_time = sec; source_time_ns = ns; product_id; channel_id });
  send_sequenced t (source_time_reference t);
  Hashtbl.data t.listings
  |> List.sort ~compare:(fun (a : Listing.t) b -> Int.compare a.symbol_id b.symbol_id)
  |> List.iter ~f:(fun l -> send_sequenced t (mapping l))
;;

let second t =
  let idle = not t.sent_this_second in
  send_sequenced t (source_time_reference t);
  t.sent_this_second <- false;
  if idle
  then (
    let hb =
      lazy (packet t ~delivery_flag:Xdp.Delivery_flag.heartbeat ~seq_num:(t.seq + 1) [])
    in
    Hashtbl.iter t.subscribers ~f:(function
      | Binary f -> f (Lazy.force hb)
      | Json _ -> ()))
;;

(* Sends unsequenced refresh messages to one sink, flagged start/part/end. *)
let send_refresh t (sink : Sink.t) msgs =
  let n = List.length msgs in
  List.iteri msgs ~f:(fun i msg ->
    let delivery_flag =
      if n = 1
      then Xdp.Delivery_flag.refresh_single
      else if i = 0
      then Xdp.Delivery_flag.refresh_start
      else if i = n - 1
      then Xdp.Delivery_flag.refresh_end
      else Xdp.Delivery_flag.refresh_part
    in
    match sink with
    | Binary f -> f (packet t ~delivery_flag ~seq_num:t.seq [ msg ])
    | Json f -> f (Yojson.Safe.to_string (to_json t ~seq:t.seq msg)))
;;

let sorted_listings t ~symbol_index =
  Hashtbl.data t.listings
  |> List.filter ~f:(fun (l : Listing.t) ->
    symbol_index = 0 || l.symbol_id = symbol_index)
  |> List.sort ~compare:(fun (a : Listing.t) b -> Int.compare a.symbol_id b.symbol_id)
;;

let reference_msgs t ~symbol_index =
  sorted_listings t ~symbol_index
  |> List.concat_map ~f:(fun l ->
    let symbol_seq_num =
      Option.value (Hashtbl.find t.symbol_seq l.symbol_id) ~default:0
    in
    mapping l
    :: Option.to_list
         (Option.map (Hashtbl.find t.status l.symbol_id) ~f:(fun s ->
            status_msg t ~symbol_id:l.symbol_id ~symbol_seq_num s)))
;;

let subscribe t sink =
  let id = t.next_subscriber in
  t.next_subscriber <- id + 1;
  send_refresh t sink (reference_msgs t ~symbol_index:0);
  Hashtbl.set t.subscribers ~key:id ~data:sink;
  id
;;

let unsubscribe t id = Hashtbl.remove t.subscribers id

let refresh t ~subscriber ~symbol_index ~resting =
  match Hashtbl.find t.subscribers subscriber with
  | None -> ()
  | Some sink ->
    let msgs =
      sorted_listings t ~symbol_index
      |> List.concat_map ~f:(fun l ->
        let symbol_seq_num =
          Option.value (Hashtbl.find t.symbol_seq l.symbol_id) ~default:0
        in
        let sec_ns = snd (time t) in
        mapping l
        :: (Option.to_list
              (Option.map (Hashtbl.find t.status l.symbol_id) ~f:(fun s ->
                 status_msg t ~symbol_id:l.symbol_id ~symbol_seq_num s))
            @ List.map
                (resting ~symbol_id:l.symbol_id)
                ~f:(fun (order_id, side, px, qty) : Xdp.Msg.t ->
                  Add_order_refresh
                    { common =
                        { source_time_ns = sec_ns
                        ; symbol_index = l.symbol_id
                        ; symbol_seq_num
                        }
                    ; order_id
                    ; price = raw t l.symbol_id px
                    ; volume = qty
                    ; side = side_char side
                    ; firm_id = ""
                    })))
    in
    send_refresh t sink msgs
;;
