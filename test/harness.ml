open! Core
module Price = Exchange_core.Price
module Venue = Nyse_sim.Venue
module App = Nyse_sim_wire.Pillar_app
module Oi = Nyse_sim_wire.Order_instructions
module Session = Equities_core.Session

type t =
  { mutable now : Time_ns.t
  ; venue : Venue.t Set_once.t
  ; out : string Queue.t
  }

let venue t = Set_once.get_exn t.venue
let date = Date.of_string "2026-10-13"
let local h m = Session.Zone.of_local New_york date (Session.hm h m)

let event_to_string (e : Nyse_sim.Market_event.t) =
  let side s = if Exchange_core.Side.is_buy s then "B" else "S" in
  let px = Price.to_string in
  match e with
  | Add { symbol_id; order_id; side = s; px = p; qty } ->
    sprintf "Add %d id=%d %s %d@%s" symbol_id order_id (side s) qty (px p)
  | Modify { symbol_id; order_id; px = p; qty; lost_priority; _ } ->
    sprintf
      "Modify %d id=%d %d@%s%s"
      symbol_id
      order_id
      qty
      (px p)
      (if lost_priority then " lost-priority" else "")
  | Delete { symbol_id; order_id } -> sprintf "Delete %d id=%d" symbol_id order_id
  | Execution { symbol_id; order_id; trade_id; px = p; qty; auction } ->
    sprintf
      "Execution %d id=%d trade=%d %d@%s%s"
      symbol_id
      order_id
      trade_id
      qty
      (px p)
      (Option.value_map auction ~default:"" ~f:(fun k ->
         " " ^ Sexp.to_string [%sexp (k : Equities_core.Auction.Kind.t)]))
  | Replace { symbol_id; order_id; new_order_id; side = s; px = p; qty } ->
    sprintf
      "Replace %d id=%d new=%d %s %d@%s"
      symbol_id
      order_id
      new_order_id
      (side s)
      qty
      (px p)
  | Non_displayed_trade { symbol_id; trade_id; px = p; qty; _ } ->
    sprintf "NonDisplayedTrade %d trade=%d %d@%s" symbol_id trade_id qty (px p)
  | Status { symbol_id; status } ->
    sprintf
      "Status %d %c halt=%c ssr=%c%s"
      symbol_id
      status.security_status
      status.halt_condition
      status.ssr_state
      (match status.price_1, status.price_2 with
       | Some a, Some b -> sprintf " bands=%s-%s" (px a) (px b)
       | _ -> "")
  | Imbalance { symbol_id; imbalance = i; auction_time } ->
    sprintf
      "Imbalance %d %d ref=%s paired=%d imbalance=%d%s indicative=%s"
      symbol_id
      auction_time
      (px i.reference_price)
      i.paired_qty
      i.imbalance_qty
      (Option.value_map i.imbalance_side ~default:"" ~f:(fun s -> " " ^ side s))
      (Option.value_map i.indicative_price ~default:"-" ~f:px)
;;

let create ?(phase = Session.Us_equities.Phase.Core) () =
  let t = { now = local 10 0; venue = Set_once.create (); out = Queue.create () } in
  let v =
    Venue.create
      Nyse_sim.Listing.defaults
      ~now:(fun () -> t.now)
      ~send_user:(fun user m ->
        Queue.enqueue
          t.out
          (sprintf "%s: %s" user (Nyse_sim_wire.Pillar.app_to_string_hum m)))
      ~publish:(fun e -> Queue.enqueue t.out ("feed: " ^ event_to_string e))
  in
  Set_once.set_exn t.venue v;
  Venue.set_phase v phase;
  Queue.clear t.out;
  t
;;

let advance t span = t.now <- Time_ns.add t.now span
let set_local_time t h m = t.now <- local h m

let order
  ?(user = "TRADER1")
  ?(symbol_id = 1)
  ?(tif = Oi.Tif.day)
  ?(alo = false)
  ?max_floor
  ?(stp = 0)
  ?(orig = 0)
  ?(session = Oi.Trading_session.core)
  ?(cancel_instead_of_reprice = false)
  t
  ~cl_ord_id
  ~side
  ~qty
  ~price
  =
  let side =
    match side with
    | `Buy -> Oi.Side.buy
    | `Sell -> Oi.Side.sell
    | `Short -> Oi.Side.sell_short
  in
  let instructions =
    { (Oi.limit_day ~side) with
      time_in_force = tif
    ; ord_type = (if Option.is_some price then Oi.Ord_type.limit else Oi.Ord_type.market)
    ; extended_exec_inst = (if alo then Oi.Extended_exec_inst.alo else 0)
    ; self_trade_type = stp
    ; trading_session = session
    ; cancel_instead_of_reprice = (if cancel_instead_of_reprice then 1 else 0)
    }
  in
  Venue.new_order
    (venue t)
    ~user
    { symbol_id
    ; mpid = "WCHL"
    ; mmid = 0
    ; mp_sub_id = '\000'
    ; cl_ord_id
    ; orig_cl_ord_id = orig
    ; instructions
    ; price = Option.value_map price ~default:Price.zero ~f:Price.of_string
    ; order_qty = qty
    ; min_qty = 0
    ; user_data = ""
    ; max_floor
    }
;;

let cancel ?(user = "TRADER1") ?(symbol_id = 1) t ~cl_ord_id ~orig =
  Venue.cancel
    (venue t)
    ~user
    { symbol_id; mpid = "WCHL"; cl_ord_id; orig_cl_ord_id = orig }
;;

let modify ?(user = "TRADER1") t ~cl_ord_id ~orig ~qty =
  Venue.modify
    (venue t)
    ~user
    { symbol_id = 1
    ; mpid = "WCHL"
    ; cl_ord_id
    ; orig_cl_ord_id = orig
    ; order_qty = qty
    ; side = 0
    ; locate_reqd = false
    }
;;

let flush t =
  Queue.iter t.out ~f:print_endline;
  Queue.clear t.out
;;
