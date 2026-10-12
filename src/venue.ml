open! Core
module Price = Exchange_core.Price
module Size = Exchange_core.Size
module Side = Exchange_core.Side
module Account = Exchange_core.Account
module Order_id = Exchange_core.Order_id
module Exchange = Exchange_core_engine.Exchange
module Event = Exchange_core_engine.Event
module Auction = Equities_core.Auction
module Halts = Equities_core.Halts
module Session = Equities_core.Session
module Ot = Equities_core.Order_types
module Phase = Session.Us_equities.Phase
module App = Nyse_sim_wire.Pillar_app
module Oi = Nyse_sim_wire.Order_instructions
module R = Nyse_sim_wire.Reason_code

module Order = struct
  (* A member's order as the gateway sees it. *)
  type t =
    { public_id : int
    ; user : string
    ; mpid : string
    ; listing : Listing.t
    ; cl_ord_id : int
    ; instructions : Oi.t
    ; mutable side : Ot.Side.t
    ; px : Price.t option (** working limit price; [None] for market orders *)
    ; mutable qty : int
    ; mutable cum_qty : int
    ; mutable leaves : int (** open shares, displayed and in reserve *)
    ; mutable hidden : int (** reserve shares not yet on the book *)
    ; max_floor : int option
    ; trading_session : int
    ; user_data : string
    ; auction_kind : Auction.Order_kind.t option (** set for OPG and on-close orders *)
    ; mutable engine_id : int option (** [None] while held for an auction *)
    ; mutable pending : (int * int * int) option
    (** an in-flight cancel: [(ref_cl_ord_id, reason, ack_type)] *)
    }

  let key ~user cl_ord_id = sprintf "%s/%d" user cl_ord_id
  let core_side t = Ot.Side.to_core t.side
end

module Sym = struct
  type t =
    { listing : Listing.t
    ; luld : Halts.Luld.t
    ; mutable luld_reference : Price.t
    ; mutable luld_reference_at : Time_ns.t
    ; mutable last_trade : Price.t option
    ; mutable ssr : Ot.Ssr.t
    ; mutable halted : bool
    ; mutable status : Market_event.Status.t
    ; mutable auction_orders : Order.t list (** arrival order *)
    ; mutable last_imbalance : Auction.Imbalance.t option
    }
end

type t =
  { exchange : Exchange.t
  ; listings : Listing.t list
  ; now : unit -> Time_ns.t
  ; send_user : string -> App.t -> unit
  ; publish : Market_event.t -> unit
  ; initial_balance : Exchange_core.Usd.t
  ; syms : Sym.t Int.Table.t
  ; orders : Order.t Int.Table.t (** live gateway orders by public id *)
  ; by_engine : Order.t Int.Table.t
  ; by_cl_ord_id : Order.t String.Table.t
  ; public_of_engine : int Int.Table.t (** re-placed orders of any account *)
  ; on_book : Int.Hash_set.t (** engine ids published as resting *)
  ; suppress_add : Int.Hash_set.t
  ; suppress_delete : Int.Hash_set.t
  ; default_stp : int String.Table.t
  ; users : String.Hash_set.t
  ; mutable phase : Phase.t
  ; mutable replacing : (int * bool) option
  (** while a cancel/replace is entered: the old public id, and whether it was shown *)
  ; mutable next_auction_order_id : int
  ; mutable next_auction_trade_id : int
  }

let listings t = t.listings
let exchange t = t.exchange
let now t = t.now ()
let phase t = t.phase
let now_ns t = Time_ns.to_int_ns_since_epoch (t.now ())
let live_orders t = Hashtbl.length t.orders
let sym t symbol_id = Hashtbl.find_exn t.syms symbol_id
let oid_int oid = Order_id.to_int_exn oid
let public_id t eid = Hashtbl.find t.public_of_engine eid |> Option.value ~default:eid
let ny_date t = fst (Session.Zone.to_local New_york (now t))

let luld_bands t ~symbol_id =
  let s = sym t symbol_id in
  Halts.luld_bands s.listing.tier ~reference:s.luld_reference ~at:(now t)
;;

let status t ~symbol_id = (sym t symbol_id).status
let halted t ~symbol_id = (sym t symbol_id).halted

let step_clock t =
  let n = now_ns t in
  if n > Exchange.now t.exchange then Exchange.step t.exchange ~now:n
;;

let ensure_user t user =
  if not (Hash_set.mem t.users user)
  then (
    Hash_set.add t.users user;
    Exchange.deposit t.exchange (Account.of_string user) t.initial_balance)
;;

(* Status *)

let phase_status : Phase.t -> char = function
  | Closed -> 'X'
  | Pre_market -> 'E'
  | Core -> 'O'
  | Post_market -> 'L'
;;

let set_status t (s : Sym.t) status =
  if not ([%equal: Market_event.Status.t] s.status status)
  then (
    s.status <- status;
    t.publish (Status { symbol_id = s.listing.symbol_id; status }))
;;

let with_bands t (s : Sym.t) (st : Market_event.Status.t) =
  let b = luld_bands t ~symbol_id:s.listing.symbol_id in
  { st with price_1 = Some b.lower; price_2 = Some b.upper }
;;

(* Messages to members *)

let order_ack t (o : Order.t) ~ack_type ~orig_cl_ord_id ~reason =
  let px = Option.value o.px ~default:Price.zero in
  t.send_user
    o.user
    (Order_ack
       { transact_time = now_ns t
       ; symbol_id = o.listing.symbol_id
       ; mpid = o.mpid
       ; mmid = 0
       ; mp_sub_id = '\000'
       ; cl_ord_id = o.cl_ord_id
       ; orig_cl_ord_id
       ; instructions = o.instructions
       ; price = px
       ; order_qty = o.qty
       ; min_qty = 0
       ; order_id = o.public_id
       ; leaves_qty = o.leaves
       ; working_price = px
       ; working_away = false
       ; pre_liquidity_indicator = ""
       ; reason_code = reason
       ; ack_type
       ; flow_indicator = 0
       ; user_data = o.user_data
       })
;;

let side_code : Ot.Side.t -> int = function
  | Buy -> Oi.Side.buy
  | Sell -> Oi.Side.sell
  | Sell_short -> Oi.Side.sell_short
  | Sell_short_exempt -> Oi.Side.sell_short_exempt
;;

let mod_cancel_ack t (o : Order.t) ~ref_cl_ord_id ~ack_type ~reason =
  t.send_user
    o.user
    (Mod_cancel_ack
       { transact_time = now_ns t
       ; symbol_id = o.listing.symbol_id
       ; mpid = o.mpid
       ; order_id = o.public_id
       ; ref_cl_ord_id
       ; orig_cl_ord_id = o.cl_ord_id
       ; price = Option.value o.px ~default:Price.zero
       ; order_qty = o.qty
       ; leaves_qty = o.leaves
       ; side = side_code o.side
       ; locate_reqd = false
       ; reason_code = reason
       ; ack_type
       ; flow_indicator = 0
       ; user_data = o.user_data
       })
;;

let reject t ~user ~mpid ~symbol_id ~cl_ord_id ~user_data ~reject_type reason =
  t.send_user
    user
    (Reject
       { transact_time = now_ns t
       ; symbol_id
       ; mpid
       ; cl_ord_id
       ; reason_code = reason
       ; reject_type
       ; user_data
       })
;;

let exec_report t (o : Order.t) ~px ~qty ~deal_id ~liquidity =
  o.cum_qty <- o.cum_qty + qty;
  o.leaves <- o.leaves - qty;
  t.send_user
    o.user
    (Execution_report
       { transact_time = now_ns t
       ; symbol_id = o.listing.symbol_id
       ; mpid = o.mpid
       ; order_id = o.public_id
       ; cl_ord_id = o.cl_ord_id
       ; deal_id
       ; last_px = px
       ; leaves_qty = o.leaves
       ; cum_qty = o.cum_qty
       ; last_qty = qty
       ; liquidity_indicator = liquidity
       ; execution_details = 0
       ; locate_reqd = false
       ; participant_type = 0
       ; reason_code = 0
       ; user_data = o.user_data
       })
;;

(* Order tables *)

let register t (o : Order.t) =
  Hashtbl.set t.orders ~key:o.public_id ~data:o;
  Hashtbl.set t.by_cl_ord_id ~key:(Order.key ~user:o.user o.cl_ord_id) ~data:o;
  Option.iter o.engine_id ~f:(fun eid ->
    Hashtbl.set t.by_engine ~key:eid ~data:o;
    if eid <> o.public_id then Hashtbl.set t.public_of_engine ~key:eid ~data:o.public_id)
;;

let finish t (o : Order.t) =
  Hashtbl.remove t.orders o.public_id;
  let key = Order.key ~user:o.user o.cl_ord_id in
  (match Hashtbl.find t.by_cl_ord_id key with
   | Some o' when o'.public_id = o.public_id -> Hashtbl.remove t.by_cl_ord_id key
   | _ -> ());
  Option.iter o.engine_id ~f:(fun eid ->
    Hashtbl.remove t.by_engine eid;
    Hashtbl.remove t.public_of_engine eid);
  o.engine_id <- None;
  let s = sym t o.listing.symbol_id in
  s.auction_orders
  <- List.filter s.auction_orders ~f:(fun (a : Order.t) -> a.public_id <> o.public_id)
;;

let find_order t ~user cl_ord_id = Hashtbl.find t.by_cl_ord_id (Order.key ~user cl_ord_id)

(* Engine plumbing *)

let reject_reason : Exchange_core.Reject_reason.t -> int = function
  | Unknown_symbol -> R.unknown_symbol
  | Unsupported _ | Reduce_only_would_increase -> R.unsupported
  | Invalid_size | Below_min_value _ -> R.invalid_quantity
  | Invalid_price -> R.invalid_price
  | Duplicate_client_order_id -> R.duplicate_cl_ord_id
  | Insufficient_margin -> R.insufficient_buying_power
  | No_liquidity -> R.ioc_unfilled
  | Self_trade -> R.self_trade
  | Post_only_would_cross _ -> R.would_cross
  | Order_not_found -> R.unknown_order
;;

let cancel_reason : Event.Cancel_reason.t -> int = function
  | User | Modified -> R.user_cancel
  | Ioc_unfilled -> R.ioc_unfilled
  | Self_trade -> R.self_trade
  | Liquidated -> R.insufficient_buying_power
;;

let place_limit t ~account ~symbol ~side ~px ~qty ~(tif : Exchange_core.Tif.t) ~cloid =
  let request =
    Exchange_core.Order_request.limit
      ~tif
      ?client_order_id:cloid
      symbol
      side
      ~px
      ~size:(Size.of_int qty)
  in
  Exchange.place_with_order_id t.exchange account request
;;

let replenish t (o : Order.t) =
  (* A reserve order's displayed slice is gone: show the next slice. *)
  match o.max_floor, o.px with
  | Some floor, Some px when o.hidden > 0 ->
    let slice = Int.min floor o.hidden in
    o.hidden <- o.hidden - slice;
    Option.iter o.engine_id ~f:(fun eid ->
      Hashtbl.remove t.by_engine eid;
      Hashtbl.remove t.public_of_engine eid);
    let eid, status =
      place_limit
        t
        ~account:(Account.of_string o.user)
        ~symbol:o.listing.symbol
        ~side:(Order.core_side o)
        ~px
        ~qty:slice
        ~tif:Gtc
        ~cloid:None
    in
    (match status with
     | Rejected _ ->
       o.engine_id <- None;
       o.leaves <- 0;
       mod_cancel_ack
         t
         o
         ~ref_cl_ord_id:o.cl_ord_id
         ~ack_type:App.Ack_type.canceled
         ~reason:R.insufficient_buying_power;
       finish t o
     | Resting _ | Filled _ ->
       o.engine_id <- Some (oid_int eid);
       register t o)
  | _ -> ()
;;

let on_trade t (s : Sym.t) px =
  s.last_trade <- Some px;
  let was_active = Ot.Ssr.is_active s.ssr ~date:(ny_date t) in
  s.ssr
  <- Ot.Ssr.on_trade
       s.ssr
       ~date:(ny_date t)
       ~prev_close:s.listing.reference_price
       ~price:px;
  if (not was_active) && Ot.Ssr.is_active s.ssr ~date:(ny_date t)
  then set_status t s { s.status with ssr_state = 'A' }
;;

let handle_event t (event : Event.t) =
  match event with
  | Order_accepted { order; _ } ->
    let eid = oid_int order.order_id in
    Hash_set.add t.on_book eid;
    let qty = Size.to_units order.size / Size.units_per_one in
    if Hash_set.mem t.suppress_add eid
    then Hash_set.remove t.suppress_add eid
    else (
      match Listing.find_by_symbol t.listings order.symbol, t.replacing with
      | None, _ -> ()
      | Some l, Some (old, true) when Hashtbl.mem t.by_engine eid ->
        t.replacing <- None;
        t.publish
          (Replace
             { symbol_id = l.symbol_id
             ; order_id = old
             ; new_order_id = public_id t eid
             ; side = order.side
             ; px = order.px
             ; qty
             })
      | Some l, _ ->
        t.publish
          (Add
             { symbol_id = l.symbol_id
             ; order_id = public_id t eid
             ; side = order.side
             ; px = order.px
             ; qty
             }))
  | Fill { fill; _ } ->
    (match Listing.find_by_symbol t.listings fill.symbol with
     | None -> ()
     | Some l ->
       let qty = Size.to_units fill.sz / Size.units_per_one
       and trade_id = Exchange_core.Trade_id.to_int fill.tid
       and maker = oid_int fill.maker_oid
       and taker = oid_int fill.taker_oid in
       t.publish
         (Execution
            { symbol_id = l.symbol_id
            ; order_id = public_id t maker
            ; trade_id
            ; px = fill.px
            ; qty
            ; auction = None
            });
       List.iter
         [ taker, R.Liquidity.removed; maker, R.Liquidity.added ]
         ~f:(fun (eid, liquidity) ->
           Option.iter (Hashtbl.find t.by_engine eid) ~f:(fun o ->
             exec_report t o ~px:fill.px ~qty ~deal_id:trade_id ~liquidity));
       on_trade t (sym t l.symbol_id) fill.px)
  | Order_filled { order; _ } ->
    let eid = oid_int order.order_id in
    Hash_set.remove t.on_book eid;
    Option.iter (Hashtbl.find t.by_engine eid) ~f:(fun o ->
      if o.hidden > 0 then replenish t o else if o.leaves <= 0 then finish t o);
    Hashtbl.remove t.public_of_engine eid
  | Order_cancelled { order; reason; _ } ->
    let eid = oid_int order.order_id in
    let was_on_book = Hash_set.mem t.on_book eid in
    Hash_set.remove t.on_book eid;
    let suppressed = Hash_set.mem t.suppress_delete eid in
    Hash_set.remove t.suppress_delete eid;
    if was_on_book && not suppressed
    then (
      match Listing.find_by_symbol t.listings order.symbol with
      | None -> ()
      | Some l ->
        t.publish (Delete { symbol_id = l.symbol_id; order_id = public_id t eid }));
    (match Hashtbl.find t.by_engine eid with
     | Some o when not suppressed ->
       let ref_cl_ord_id, reason, ack_type =
         Option.value
           o.pending
           ~default:(o.cl_ord_id, cancel_reason reason, App.Ack_type.canceled)
       in
       o.leaves <- 0;
       o.hidden <- 0;
       mod_cancel_ack t o ~ref_cl_ord_id ~ack_type ~reason;
       finish t o
     | _ -> ());
    Hashtbl.remove t.public_of_engine eid
  | Order_rejected _ | Liquidation _ | Funding _ -> ()
;;

let rec process t =
  match Exchange.drain_events t.exchange with
  | [] -> ()
  | events ->
    List.iter events ~f:(handle_event t);
    process t
;;

(* LULD *)

let best t (l : Listing.t) =
  match Exchange.book t.exchange l.symbol with
  | None -> None, None
  | Some book -> Order_book.best_bid book, Order_book.best_ask book
;;

let check_luld t (s : Sym.t) =
  let bands = luld_bands t ~symbol_id:s.listing.symbol_id in
  let best_bid, best_offer = best t s.listing in
  Halts.Luld.on_quote s.luld ~now:(now t) ~bands ~best_bid ~best_offer;
  match Halts.Luld.state s.luld with
  | Paused _ when not s.halted ->
    s.halted <- true;
    set_status
      t
      s
      (with_bands t s { s.status with security_status = '4'; halt_condition = 'M' })
  | _ -> ()
;;

let after_mutation t =
  process t;
  Hashtbl.iter t.syms ~f:(check_luld t)
;;

(* Re-placing a resting engine order under a new engine id, keeping its public id. *)
let re_place t ~account ~(l : Listing.t) ~eid ~side ~px ~qty ~cloid =
  let public = public_id t eid in
  Hash_set.add t.suppress_delete eid;
  ignore
    (Exchange.cancel t.exchange account ~symbol:l.symbol (Order_id.of_int eid)
     : Exchange_core.Cancel_status.t);
  if qty <= 0
  then None
  else (
    let neid, status =
      place_limit t ~account ~symbol:l.symbol ~side ~px ~qty ~tif:Gtc ~cloid
    in
    match status with
    | Rejected _ -> None
    | Resting _ | Filled _ ->
      let neid = oid_int neid in
      Hash_set.add t.suppress_add neid;
      Hashtbl.set t.public_of_engine ~key:neid ~data:public;
      Some neid)
;;

let cloid (o : Order.t) =
  Some (Exchange_core.Client_order_id.of_string (Int.to_string o.cl_ord_id))
;;

(* Re-places a member's book order with [display] shares shown. *)
let re_place_order t (o : Order.t) ~display =
  match o.engine_id, o.px with
  | Some eid, Some px ->
    Hashtbl.remove t.by_engine eid;
    (match
       re_place
         t
         ~account:(Account.of_string o.user)
         ~l:o.listing
         ~eid
         ~side:(Order.core_side o)
         ~px
         ~qty:display
         ~cloid:(cloid o)
     with
     | Some neid ->
       o.engine_id <- Some neid;
       register t o;
       true
     | None ->
       o.engine_id <- None;
       false)
  | _ -> false
;;

(* Order intake *)

module Spec = struct
  type t =
    { order : Ot.t
    ; trading_session : int
    ; cancel_instead_of_reprice : bool
    }
end

let max_order_qty = 5_000_000

let decode_spec t ~user (m : App.New_order.t) (l : Listing.t) : (Spec.t, int) Result.t =
  let open Result.Let_syntax in
  let i = m.instructions in
  let%bind side =
    match i.side with
    | 1 -> Ok Ot.Side.Buy
    | 2 -> Ok Sell
    | 3 -> Ok Sell_short
    | 4 -> Ok Sell_short_exempt
    | _ -> Error R.unsupported
  in
  let%bind limit =
    match i.ord_type with
    | 1 -> Ok None
    | 2 ->
      if Price.is_positive m.price
         && Equities_core.Tick_size.is_valid_equity_price m.price
      then Ok (Some m.price)
      else Error R.invalid_price
    | _ -> Error R.unsupported
  in
  let%bind tif =
    match i.time_in_force with
    | 1 -> Ok Ot.Tif.Day
    | 2 -> Ok Ioc
    | 3 -> Ok Opg
    | 4 -> Ok Clo
    | _ -> Error R.unsupported
  in
  let%bind () =
    if m.order_qty <= 0 || m.order_qty > max_order_qty
    then Error R.invalid_quantity
    else Ok ()
  in
  let%bind () =
    if i.exec_inst <> 0
       || m.min_qty > 0
       || i.interest_type <> 0
       || i.special_ord_type <> 0
    then Error R.unsupported
    else Ok ()
  in
  let stp_code =
    if i.self_trade_type = 0
    then Hashtbl.find t.default_stp user |> Option.value ~default:Oi.Self_trade.none
    else i.self_trade_type
  in
  let%bind stp =
    match stp_code with
    | 0 | 1 -> Ok Ot.Stp.Allow
    | 2 -> Ok Cancel_newest
    | 3 -> Ok Cancel_oldest
    | 4 -> Ok Cancel_both
    | 5 -> Ok Decrement_and_cancel
    | _ -> Error R.unsupported
  in
  let display =
    match m.max_floor with
    | Some n when n > 0 && n < m.order_qty -> Ot.Display.Reserve { display_qty = n }
    | _ -> Visible
  in
  let order : Ot.t =
    { side
    ; qty = m.order_qty
    ; limit
    ; tif
    ; display
    ; iso = false
    ; post_only = i.extended_exec_inst = Oi.Extended_exec_inst.alo
    ; min_qty = None
    ; stp
    }
  in
  let%map () =
    match Ot.validate ~round_lot:l.round_lot order with
    | Ok () -> Ok ()
    | Error _ -> Error R.unsupported
  in
  { Spec.order
  ; trading_session = i.trading_session
  ; cancel_instead_of_reprice = i.cancel_instead_of_reprice <> 0
  }
;;

(* Whether the session accepts the order, and how. *)
let session_check t (spec : Spec.t)
  : ([ `Auction of Auction.Order_kind.t | `Book ], int) Result.t
  =
  let o = spec.order in
  match t.phase, o.tif with
  | Closed, _ -> Error R.session_closed
  | Pre_market, Opg -> Ok (`Auction (Option.value_exn (Ot.auction_kind o)))
  | (Pre_market | Core), Clo -> Ok (`Auction (Option.value_exn (Ot.auction_kind o)))
  | (Core | Post_market), Opg | Post_market, Clo -> Error R.auction_not_accepted
  | Pre_market, _ when Option.is_none o.limit -> Error R.session_closed
  | Post_market, _ when Option.is_none o.limit -> Error R.session_closed
  | Post_market, _ when not (Oi.Trading_session.includes_late spec.trading_session) ->
    Error R.session_closed
  | (Pre_market | Core | Post_market), _ -> Ok `Book
;;

(* LULD: limit prices beyond the band are re-priced to it (or cancelled); market orders
   become IOC limits at the band. *)
let luld_price t (l : Listing.t) (spec : Spec.t) : (Ot.t, int) Result.t =
  let o = spec.order in
  let b = luld_bands t ~symbol_id:l.symbol_id in
  let side = Ot.Side.to_core o.side in
  let band : Price.t = if Side.is_buy side then b.upper else b.lower in
  let beyond p = if Side.is_buy side then Price.(p > band) else Price.(p < band) in
  match o.limit with
  | None -> Ok { o with limit = Some band; tif = Ioc }
  | Some p when beyond p ->
    if spec.cancel_instead_of_reprice
    then Error R.luld
    else Ok { o with limit = Some band }
  | Some _ -> Ok o
;;

let crosses (o : Order.t) ~side ~limit =
  match limit, o.px with
  | None, _ -> true
  | Some _, None -> false
  | Some p, Some rp -> if Side.is_buy side then Price.(p >= rp) else Price.(p <= rp)
;;

(* The member's own resting orders an incoming order would trade with, best first. *)
let own_crossing t ~user ~(l : Listing.t) ~side ~limit =
  Hashtbl.data t.orders
  |> List.filter ~f:(fun (o : Order.t) ->
    String.equal o.user user
    && o.listing.symbol_id = l.symbol_id
    && Option.is_some o.engine_id
    && Side.equal (Order.core_side o) (Side.flip side)
    && crosses o ~side ~limit)
  |> List.sort ~compare:(fun (a : Order.t) (b : Order.t) ->
    let by_px = Option.compare Price.compare a.px b.px in
    let by_px = if Side.is_buy side then by_px else -by_px in
    if by_px <> 0 then by_px else Int.compare a.public_id b.public_id)
;;

let cancel_resting
  ?(ack_type = App.Ack_type.canceled)
  ?ref_cl_ord_id
  t
  (o : Order.t)
  ~reason
  =
  match o.engine_id with
  | None -> ()
  | Some eid ->
    o.pending <- Some (Option.value ref_cl_ord_id ~default:o.cl_ord_id, reason, ack_type);
    ignore
      (Exchange.cancel
         t.exchange
         (Account.of_string o.user)
         ~symbol:o.listing.symbol
         (Order_id.of_int eid)
       : Exchange_core.Cancel_status.t);
    process t
;;

(* Applies self-trade prevention; returns the quantity left to enter, or a reject. *)
let self_trade t ~user ~(l : Listing.t) (o : Ot.t) : (int, int) Result.t =
  let side = Ot.Side.to_core o.side in
  let own = own_crossing t ~user ~l ~side ~limit:o.limit in
  if List.is_empty own
  then Ok o.qty
  else (
    match o.stp with
    | Allow -> Ok o.qty
    | Cancel_newest -> Error R.self_trade
    | Cancel_oldest ->
      List.iter own ~f:(cancel_resting t ~reason:R.self_trade);
      Ok o.qty
    | Cancel_both ->
      List.iter own ~f:(cancel_resting t ~reason:R.self_trade);
      Error R.self_trade
    | Decrement_and_cancel ->
      let remaining =
        List.fold own ~init:o.qty ~f:(fun remaining (r : Order.t) ->
          if remaining = 0
          then 0
          else (
            let m = Int.min remaining r.leaves in
            if m >= r.leaves
            then cancel_resting t r ~reason:R.self_trade
            else (
              r.leaves <- r.leaves - m;
              let display =
                Option.value_map r.max_floor ~default:r.leaves ~f:(Int.min r.leaves)
              in
              r.hidden <- r.leaves - display;
              if re_place_order t r ~display
              then (
                t.publish
                  (Modify
                     { symbol_id = l.symbol_id
                     ; order_id = r.public_id
                     ; side = Order.core_side r
                     ; px = Option.value_exn r.px
                     ; qty = display
                     ; lost_priority = true
                     });
                mod_cancel_ack
                  t
                  r
                  ~ref_cl_ord_id:r.cl_ord_id
                  ~ack_type:App.Ack_type.modified
                  ~reason:R.self_trade));
            remaining - m))
      in
      process t;
      if remaining = 0 then Error R.self_trade else Ok remaining)
;;

let ssr_check t (l : Listing.t) (o : Ot.t) =
  let s = sym t l.symbol_id in
  match o.side with
  | Sell_short ->
    let date = ny_date t in
    if not (Ot.Ssr.is_active s.ssr ~date)
    then Ok ()
    else (
      match o.limit, fst (best t l) with
      | Some price, Some national_best_bid ->
        if Ot.Ssr.allows s.ssr ~date ~side:o.side ~price ~national_best_bid
        then Ok ()
        else Error R.short_sale_price_test
      | None, _ -> Error R.short_sale_price_test
      | Some _, None -> Ok ())
  | Buy | Sell | Sell_short_exempt -> Ok ()
;;

let next_auction_order_id t =
  t.next_auction_order_id <- t.next_auction_order_id + 1;
  t.next_auction_order_id
;;

(* Enters a validated order: into the auction book or the engine. [ack] sends the
   acknowledgement once the public id is known. *)
let enter t ~user ~(m : App.New_order.t) ~(l : Listing.t) ~(spec : Spec.t) ~ack
  : (Order.t, int) Result.t
  =
  let open Result.Let_syntax in
  let s = sym t l.symbol_id in
  let%bind route = session_check t spec in
  let%bind () = if s.halted then Error R.halted else Ok () in
  let%bind () = ssr_check t l spec.order in
  let make ~public_id ~engine_id ~(order : Ot.t) ~auction_kind ~shown : Order.t =
    { public_id
    ; user
    ; mpid = m.mpid
    ; listing = l
    ; cl_ord_id = m.cl_ord_id
    ; instructions = m.instructions
    ; side = order.side
    ; px = order.limit
    ; qty = m.order_qty
    ; cum_qty = 0
    ; leaves = m.order_qty
    ; hidden = order.qty - shown
    ; max_floor =
        (match order.display with
         | Reserve { display_qty } -> Some display_qty
         | Visible | Hidden -> None)
    ; trading_session = spec.trading_session
    ; user_data = m.user_data
    ; auction_kind
    ; engine_id
    ; pending = None
    }
  in
  match route with
  | `Auction kind ->
    let o =
      make
        ~public_id:(next_auction_order_id t)
        ~engine_id:None
        ~order:spec.order
        ~auction_kind:(Some kind)
        ~shown:spec.order.qty
    in
    register t o;
    s.auction_orders <- s.auction_orders @ [ o ];
    ack o;
    Ok o
  | `Book ->
    let%bind order = luld_price t l spec in
    let%bind qty = self_trade t ~user ~l order in
    let order = { order with qty } in
    let shown =
      match order.display with
      | Reserve { display_qty } -> Int.min display_qty qty
      | Visible | Hidden -> qty
    in
    let%bind request =
      Ot.to_order_request
        ~client_order_id:
          (Exchange_core.Client_order_id.of_string (Int.to_string m.cl_ord_id))
        { order with qty = shown; display = Visible }
        ~symbol:l.symbol
      |> Result.map_error ~f:(fun _ -> R.unsupported)
    in
    let eid, status =
      Exchange.place_with_order_id t.exchange (Account.of_string user) request
    in
    let eid = oid_int eid in
    (match status with
     | Rejected No_liquidity ->
       (* An IOC that found nothing: acknowledged, then cancelled. *)
       let o = make ~public_id:eid ~engine_id:None ~order ~auction_kind:None ~shown in
       ack o;
       o.leaves <- 0;
       mod_cancel_ack
         t
         o
         ~ref_cl_ord_id:o.cl_ord_id
         ~ack_type:App.Ack_type.canceled
         ~reason:R.ioc_unfilled;
       Ok o
     | Rejected reason -> Error (reject_reason reason)
     | Resting _ | Filled _ ->
       let o =
         make ~public_id:eid ~engine_id:(Some eid) ~order ~auction_kind:None ~shown
       in
       (* Quantity removed by decrement-and-cancel STP is neither open nor executed. *)
       o.leaves <- qty;
       register t o;
       ack o;
       after_mutation t;
       Ok o)
;;

let set_default_stp t ~user stp = Hashtbl.set t.default_stp ~key:user ~data:stp

let order_reject t ~user (m : App.New_order.t) reason =
  reject
    t
    ~user
    ~mpid:m.mpid
    ~symbol_id:m.symbol_id
    ~cl_ord_id:m.cl_ord_id
    ~user_data:m.user_data
    ~reject_type:App.Reject_type.order
    reason
;;

let replace t ~user (m : App.New_order.t) (l : Listing.t) =
  match find_order t ~user m.orig_cl_ord_id with
  | None -> order_reject t ~user m R.unknown_order
  | Some old when old.listing.symbol_id <> l.symbol_id ->
    order_reject t ~user m R.unknown_symbol
  | Some old ->
    (match decode_spec t ~user m l with
     | Error reason -> order_reject t ~user m reason
     | Ok spec ->
       let was_on_book =
         Option.value_map old.engine_id ~default:false ~f:(Hash_set.mem t.on_book)
       in
       (* Take the original off the book without publishing a delete: the feed shows
          Replace Order once the new order rests. *)
       Option.iter old.engine_id ~f:(fun eid ->
         Hashtbl.remove t.by_engine eid;
         Hash_set.add t.suppress_delete eid;
         ignore
           (Exchange.cancel
              t.exchange
              (Account.of_string user)
              ~symbol:l.symbol
              (Order_id.of_int eid)
            : Exchange_core.Cancel_status.t));
       finish t old;
       process t;
       t.replacing <- Some (old.public_id, was_on_book);
       let ack (o : Order.t) =
         order_ack
           t
           o
           ~ack_type:App.Ack_type.replaced
           ~orig_cl_ord_id:m.orig_cl_ord_id
           ~reason:R.none
       in
       let result = enter t ~user ~m ~l ~spec ~ack in
       let unpublished = Option.is_some t.replacing in
       t.replacing <- None;
       if was_on_book && unpublished
       then t.publish (Delete { symbol_id = l.symbol_id; order_id = old.public_id });
       (match result with
        | Ok _ -> ()
        | Error reason ->
          (* The original is gone and the replacement was refused. *)
          old.leaves <- 0;
          mod_cancel_ack
            t
            old
            ~ref_cl_ord_id:m.cl_ord_id
            ~ack_type:App.Ack_type.canceled
            ~reason;
          order_reject t ~user m reason))
;;

let new_order t ~user (m : App.New_order.t) =
  step_clock t;
  process t;
  ensure_user t user;
  match Listing.find_by_id t.listings m.symbol_id with
  | None -> order_reject t ~user m R.unknown_symbol
  | Some l ->
    if m.orig_cl_ord_id <> 0
    then replace t ~user m l
    else if Option.is_some (find_order t ~user m.cl_ord_id)
    then order_reject t ~user m R.duplicate_cl_ord_id
    else (
      let ack o =
        order_ack t o ~ack_type:App.Ack_type.new_order ~orig_cl_ord_id:0 ~reason:R.none
      in
      match
        Result.bind (decode_spec t ~user m l) ~f:(fun spec ->
          enter t ~user ~m ~l ~spec ~ack)
      with
      | Ok _ -> ()
      | Error reason -> order_reject t ~user m reason)
;;

let cancel t ~user (m : App.Cancel_request.t) =
  step_clock t;
  process t;
  let target = if m.orig_cl_ord_id = 0 then m.cl_ord_id else m.orig_cl_ord_id in
  match find_order t ~user target with
  | Some o when o.listing.symbol_id = m.symbol_id || m.symbol_id = 0 ->
    (match o.engine_id with
     | None ->
       o.leaves <- 0;
       mod_cancel_ack
         t
         o
         ~ref_cl_ord_id:m.cl_ord_id
         ~ack_type:App.Ack_type.canceled
         ~reason:R.user_cancel;
       finish t o
     | Some _ -> cancel_resting t o ~ref_cl_ord_id:m.cl_ord_id ~reason:R.user_cancel);
    after_mutation t
  | Some _ | None ->
    reject
      t
      ~user
      ~mpid:m.mpid
      ~symbol_id:m.symbol_id
      ~cl_ord_id:m.cl_ord_id
      ~user_data:""
      ~reject_type:App.Reject_type.cancel
      R.unknown_order
;;

let is_sell_side : Ot.Side.t -> bool = function
  | Buy -> false
  | Sell | Sell_short | Sell_short_exempt -> true
;;

let modify t ~user (m : App.Modify_request.t) =
  step_clock t;
  process t;
  let reject_modify reason =
    reject
      t
      ~user
      ~mpid:m.mpid
      ~symbol_id:m.symbol_id
      ~cl_ord_id:m.cl_ord_id
      ~user_data:""
      ~reject_type:App.Reject_type.modify
      reason
  in
  match find_order t ~user m.orig_cl_ord_id with
  | None -> reject_modify R.unknown_order
  | Some o ->
    let new_side : Ot.Side.t option =
      match m.side with
      | 2 -> Some Sell
      | 3 -> Some Sell_short
      | 4 -> Some Sell_short_exempt
      | 0 -> Some o.side
      | 1 -> Some Buy
      | _ -> None
    in
    (match new_side with
     | None -> reject_modify R.unsupported
     | Some side
       when (not (Ot.Side.equal side o.side))
            && not (is_sell_side side && is_sell_side o.side) ->
       reject_modify R.unsupported
     | Some side ->
       let new_qty = if m.order_qty = 0 then o.qty else m.order_qty in
       if new_qty > o.qty || new_qty <= o.cum_qty
       then reject_modify R.invalid_quantity
       else (
         o.side <- side;
         let leaves = new_qty - o.cum_qty in
         let reduced = leaves < o.leaves in
         o.qty <- new_qty;
         o.leaves <- leaves;
         let display = Option.value_map o.max_floor ~default:leaves ~f:(Int.min leaves) in
         o.hidden <- leaves - display;
         let ok =
           match o.engine_id with
           | None -> true
           | Some _ when not reduced -> true
           | Some _ ->
             let ok = re_place_order t o ~display in
             if ok
             then
               t.publish
                 (Modify
                    { symbol_id = o.listing.symbol_id
                    ; order_id = o.public_id
                    ; side = Order.core_side o
                    ; px = Option.value_exn o.px
                    ; qty = display
                    ; lost_priority = true
                    });
             ok
         in
         if ok
         then
           mod_cancel_ack
             t
             o
             ~ref_cl_ord_id:m.cl_ord_id
             ~ack_type:App.Ack_type.modified
             ~reason:R.none
         else (
           o.leaves <- 0;
           mod_cancel_ack
             t
             o
             ~ref_cl_ord_id:m.cl_ord_id
             ~ack_type:App.Ack_type.canceled
             ~reason:R.insufficient_buying_power;
           finish t o);
         after_mutation t))
;;

let cancel_all t ~user ~reason =
  step_clock t;
  process t;
  Hashtbl.data t.orders
  |> List.filter ~f:(fun (o : Order.t) -> String.equal o.user user)
  |> List.sort ~compare:(fun (a : Order.t) b -> Int.compare a.public_id b.public_id)
  |> List.iter ~f:(fun (o : Order.t) ->
    match o.engine_id with
    | Some _ -> cancel_resting t o ~reason
    | None ->
      o.leaves <- 0;
      mod_cancel_ack
        t
        o
        ~ref_cl_ord_id:o.cl_ord_id
        ~ack_type:App.Ack_type.canceled
        ~reason;
      finish t o);
  after_mutation t
;;

(* Auctions *)

let reference_price (s : Sym.t) =
  Option.value s.last_trade ~default:s.listing.reference_price
;;

type participant =
  | Book of Order_book.Resting_order.t
  | Held of Order.t

let participants t (s : Sym.t) ~kind =
  let book =
    match Exchange.book t.exchange s.listing.symbol with
    | None -> []
    | Some book -> Order_book.orders book
  in
  let book =
    List.map book ~f:(fun (r : Order_book.Resting_order.t) ->
      ( { Auction.Order.id = oid_int r.oid
        ; side = r.side
        ; qty = Size.to_units r.remaining / Size.units_per_one
        ; kind = Book
        ; limit = Some r.px
        }
      , Book r ))
  in
  let held =
    List.filter_map s.auction_orders ~f:(fun (o : Order.t) ->
      match o.auction_kind with
      | Some k when Auction.Order_kind.participates k kind && o.leaves > 0 ->
        Some
          ( { Auction.Order.id = o.public_id
            ; side = Order.core_side o
            ; qty = o.leaves
            ; kind = k
            ; limit = (if Auction.Order_kind.has_limit k then o.px else None)
            }
          , Held o )
      | _ -> None)
  in
  book @ held
;;

let liquidity_of_kind : Auction.Kind.t -> string = function
  | Opening -> R.Liquidity.opening_auction
  | Closing -> R.Liquidity.closing_auction
  | Halt_reopening -> R.Liquidity.reopening_auction
;;

let next_auction_trade_id t =
  t.next_auction_trade_id <- t.next_auction_trade_id + 1;
  t.next_auction_trade_id
;;

let run_auction t (s : Sym.t) ~kind =
  let ps = participants t s ~kind in
  let by_id = Int.Table.of_alist_exn (List.map ps ~f:(fun (a, p) -> a.id, p)) in
  (match Auction.uncross ~kind ~reference:(reference_price s) (List.map ps ~f:fst) with
   | None -> ()
   | Some (result, fills) ->
     let symbol_id = s.listing.symbol_id in
     List.iter fills ~f:(fun (f : Auction.Fill.t) ->
       let trade_id = next_auction_trade_id t in
       match Hashtbl.find_exn by_id f.id with
       | Book r ->
         let eid = oid_int r.oid in
         t.publish
           (Execution
              { symbol_id
              ; order_id = public_id t eid
              ; trade_id
              ; px = f.price
              ; qty = f.qty
              ; auction = Some kind
              });
         let member = Hashtbl.find t.by_engine eid in
         Option.iter member ~f:(fun o ->
           exec_report
             t
             o
             ~px:f.price
             ~qty:f.qty
             ~deal_id:trade_id
             ~liquidity:(liquidity_of_kind kind));
         let remaining = (Size.to_units r.remaining / Size.units_per_one) - f.qty in
         (match member with
          | Some o ->
            if remaining > 0
            then ignore (re_place_order t o ~display:remaining : bool)
            else if o.hidden > 0
            then (
              ignore (re_place_order t o ~display:0 : bool);
              replenish t o)
            else (
              Hash_set.add t.suppress_delete eid;
              ignore
                (Exchange.cancel t.exchange r.owner ~symbol:s.listing.symbol r.oid
                 : Exchange_core.Cancel_status.t);
              finish t o)
          | None ->
            ignore
              (re_place
                 t
                 ~account:r.owner
                 ~l:s.listing
                 ~eid
                 ~side:r.side
                 ~px:r.px
                 ~qty:remaining
                 ~cloid:r.cloid
               : int option))
       | Held o ->
         exec_report
           t
           o
           ~px:f.price
           ~qty:f.qty
           ~deal_id:trade_id
           ~liquidity:(liquidity_of_kind kind);
         if Side.is_buy f.side
         then
           t.publish
             (Non_displayed_trade
                { symbol_id; trade_id; px = f.price; qty = f.qty; auction = Some kind });
         if o.leaves <= 0 then finish t o);
     on_trade t s result.price);
  (* Auction-only orders that did not execute are cancelled. *)
  List.iter s.auction_orders ~f:(fun (o : Order.t) ->
    match o.auction_kind with
    | Some k when Auction.Order_kind.participates k kind ->
      o.leaves <- 0;
      mod_cancel_ack
        t
        o
        ~ref_cl_ord_id:o.cl_ord_id
        ~ack_type:App.Ack_type.canceled
        ~reason:R.done_for_day;
      finish t o
    | _ -> ());
  s.last_imbalance <- None;
  process t
;;

let auction_time : Auction.Kind.t -> int = function
  | Opening -> 930
  | Closing -> 1600
  | Halt_reopening -> 0
;;

let publish_imbalances t ~kind =
  step_clock t;
  process t;
  Hashtbl.iter t.syms ~f:(fun s ->
    let ps = participants t s ~kind |> List.map ~f:fst in
    if List.exists ps ~f:(fun (o : Auction.Order.t) ->
         not (Auction.Order_kind.equal o.kind Book))
    then (
      let imbalance = Auction.imbalance ~kind ~reference:(reference_price s) ps in
      if not ([%equal: Auction.Imbalance.t option] (Some imbalance) s.last_imbalance)
      then (
        s.last_imbalance <- Some imbalance;
        t.publish
          (Imbalance
             { symbol_id = s.listing.symbol_id
             ; imbalance
             ; auction_time = auction_time kind
             }))))
;;

(* Sessions *)

let remove_ineligible t ~eligible =
  Hashtbl.data t.orders
  |> List.filter ~f:(fun (o : Order.t) ->
    Option.is_some o.engine_id && not (eligible o.trading_session))
  |> List.sort ~compare:(fun (a : Order.t) b -> Int.compare a.public_id b.public_id)
  |> List.iter ~f:(fun (o : Order.t) ->
    cancel_resting t o ~ack_type:App.Ack_type.done_for_day ~reason:R.done_for_day)
;;

let close_silently t =
  (* At the close Security Status X implies every order is gone; no deletes. *)
  List.iter t.listings ~f:(fun l ->
    match Exchange.book t.exchange l.symbol with
    | None -> ()
    | Some book ->
      List.iter (Order_book.orders book) ~f:(fun (r : Order_book.Resting_order.t) ->
        let eid = oid_int r.oid in
        Hash_set.add t.suppress_delete eid;
        (match Hashtbl.find t.by_engine eid with
         | Some o ->
           o.pending <- Some (o.cl_ord_id, R.done_for_day, App.Ack_type.done_for_day);
           Hash_set.remove t.suppress_delete eid;
           Hash_set.remove t.on_book eid
         | None -> ());
        ignore
          (Exchange.cancel t.exchange r.owner ~symbol:l.symbol r.oid
           : Exchange_core.Cancel_status.t)));
  process t;
  Hashtbl.data t.orders
  |> List.iter ~f:(fun (o : Order.t) ->
    o.leaves <- 0;
    mod_cancel_ack
      t
      o
      ~ref_cl_ord_id:o.cl_ord_id
      ~ack_type:App.Ack_type.done_for_day
      ~reason:R.done_for_day;
    finish t o)
;;

let set_phase t phase =
  step_clock t;
  process t;
  let old = t.phase in
  if not (Phase.equal old phase)
  then (
    t.phase <- phase;
    let each f = List.iter t.listings ~f:(fun l -> f (sym t l.symbol_id)) in
    (match old, phase with
     | Pre_market, Core ->
       each (run_auction t ~kind:Opening);
       remove_ineligible t ~eligible:Oi.Trading_session.includes_core
     | Core, Post_market ->
       each (run_auction t ~kind:Closing);
       remove_ineligible t ~eligible:Oi.Trading_session.includes_late
     | _, Closed -> close_silently t
     | _ -> ());
    each (fun s ->
      set_status
        t
        s
        { s.status with security_status = (if s.halted then '4' else phase_status phase) });
    after_mutation t)
;;

let luld_reference_interval = Time_ns.Span.of_int_sec 30

let tick t =
  step_clock t;
  process t;
  let now = now t in
  Hashtbl.iter t.syms ~f:(fun s ->
    Halts.Luld.tick s.luld ~now;
    (match Halts.Luld.state s.luld with
     | Normal when s.halted ->
       s.halted <- false;
       run_auction t s ~kind:Halt_reopening;
       set_status
         t
         s
         (with_bands t s { s.status with security_status = '5'; halt_condition = '~' })
     | _ -> ());
    (* LULD reference price: the last sale, refreshed when it moved 1% or more. *)
    match s.last_trade with
    | Some last
      when Time_ns.Span.(Time_ns.diff now s.luld_reference_at >= luld_reference_interval)
           && Float.(
                abs (Price.to_float last -. Price.to_float s.luld_reference)
                >= 0.01 *. Price.to_float s.luld_reference) ->
      s.luld_reference <- last;
      s.luld_reference_at <- now
    | _ -> ());
  after_mutation t
;;

let with_engine t ~symbol_id ~f =
  step_clock t;
  process t;
  let s = sym t symbol_id in
  if (not s.halted) && not (Phase.equal t.phase Closed)
  then (
    f t.exchange;
    after_mutation t)
;;

let resting t ~symbol_id =
  let s = sym t symbol_id in
  match Exchange.book t.exchange s.listing.symbol with
  | None -> []
  | Some book ->
    Order_book.orders book
    |> List.map ~f:(fun (r : Order_book.Resting_order.t) ->
      ( public_id t (oid_int r.oid)
      , r.side
      , r.px
      , Size.to_units r.remaining / Size.units_per_one ))
;;

let create
  ?(initial_balance = Exchange_core.Usd.of_int 1_000_000_000)
  listings
  ~now
  ~send_user
  ~publish
  =
  let config =
    { Exchange.Config.default with
      instruments = List.map listings ~f:Listing.instrument
    ; default_leverage = 4
    ; funding_interval_ns = Int.max_value / 2
    ; default_funding_rate = 0.
    ; min_order_value = Exchange_core.Usd.zero
    ; market_slippage_bps = 500
    }
  in
  let exchange = Exchange.create ~config () in
  let start = now () in
  Exchange.step exchange ~now:(Time_ns.to_int_ns_since_epoch start);
  let syms =
    List.map listings ~f:(fun (l : Listing.t) ->
      ( l.symbol_id
      , { Sym.listing = l
        ; luld = Halts.Luld.create ()
        ; luld_reference = l.reference_price
        ; luld_reference_at = start
        ; last_trade = None
        ; ssr = Ot.Ssr.inactive
        ; halted = false
        ; status =
            { security_status = 'P'
            ; halt_condition = '~'
            ; ssr_state = '~'
            ; price_1 = None
            ; price_2 = None
            }
        ; auction_orders = []
        ; last_imbalance = None
        } ))
    |> Int.Table.of_alist_exn
  in
  { exchange
  ; listings
  ; now
  ; send_user
  ; publish
  ; initial_balance
  ; syms
  ; orders = Int.Table.create ()
  ; by_engine = Int.Table.create ()
  ; by_cl_ord_id = String.Table.create ()
  ; public_of_engine = Int.Table.create ()
  ; on_book = Int.Hash_set.create ()
  ; suppress_add = Int.Hash_set.create ()
  ; suppress_delete = Int.Hash_set.create ()
  ; default_stp = String.Table.create ()
  ; users = String.Hash_set.create ()
  ; phase = Closed
  ; replacing = None
  ; next_auction_order_id = 1_000_000_000
  ; next_auction_trade_id = 2_000_000_000
  }
;;
