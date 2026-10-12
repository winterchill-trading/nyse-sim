(** The simulated NYSE matching venue: Pillar order semantics on top of the venue-neutral
    {!Exchange_core_engine.Exchange}.

    The venue validates gateway orders with {!Equities_core.Order_types} (tick, quantity,
    order type combinations), enforces the trading session ({!Equities_core.Session}),
    LULD bands and pauses and the short sale restriction ({!Equities_core.Halts},
    {!Equities_core.Order_types.Ssr}), keeps opening and closing auction interest aside
    and uncrosses it with {!Equities_core.Auction} on session transitions.

    It reports to members through [send_user] (Pillar GT payloads: acks, unsolicited
    cancels, execution reports, rejects) and to the market through [publish]
    ({!Market_event.t}, turned into XDP by {!Feed}).

    Ids: the public order id (gateway OrderID = XDP OrderID) is the engine order id the
    order first got. The engine has no in-place quantity reduction, so a modify, a partial
    auction execution or a reserve refresh re-places the order in the engine under a new
    engine id; the public id stays, the feed shows Modify Order with [PositionChange 1]. A
    cancel/replace takes a new public id and is published as Replace Order.

    All functions run synchronously; the caller serialises them. *)

open! Core
module Price := Exchange_core.Price
module Phase := Equities_core.Session.Us_equities.Phase

type t

(** [now] is the venue clock (wall clock or accelerated); it must not go backwards. *)
val create
  :  ?initial_balance:Exchange_core.Usd.t
  -> Listing.t list
  -> now:(unit -> Time_ns.t)
  -> send_user:(string -> Nyse_sim_wire.Pillar_app.t -> unit)
  -> publish:(Market_event.t -> unit)
  -> t

val listings : t -> Listing.t list
val exchange : t -> Exchange_core_engine.Exchange.t
val now : t -> Time_ns.t
val phase : t -> Phase.t

(** {2 Gateway requests} *)

(** The session-level self-trade prevention default for a user ([SelfTradeType 0]). *)
val set_default_stp : t -> user:string -> int -> unit

val new_order : t -> user:string -> Nyse_sim_wire.Pillar_app.New_order.t -> unit
val cancel : t -> user:string -> Nyse_sim_wire.Pillar_app.Cancel_request.t -> unit
val modify : t -> user:string -> Nyse_sim_wire.Pillar_app.Modify_request.t -> unit

(** Cancels every live order of the user (cancel on disconnect). *)
val cancel_all : t -> user:string -> reason:int -> unit

(** {2 Market} *)

(** Runs [f] on the engine (e.g. a noise trader tick) and publishes what it did. Skipped
    while the symbol is halted or the market is closed. *)
val with_engine
  :  t
  -> symbol_id:int
  -> f:(Exchange_core_engine.Exchange.t -> unit)
  -> unit

(** Moves to a session phase: runs the opening auction (pre-market to core), the closing
    auction (core to post-market), removes orders not eligible for the new session, and
    publishes Security Status. *)
val set_phase : t -> Phase.t -> unit

(** Publishes the auction imbalance of every symbol whose indicative figures changed,
    during the opening ([Opening]) or closing ([Closing]) imbalance period. *)
val publish_imbalances : t -> kind:Equities_core.Auction.Kind.t -> unit

(** Advances LULD timers: ends pauses (reopening auction and Security Status [5]) and
    recalculates bands. Call about once a second. *)
val tick : t -> unit

(** {2 Views} *)

(** Displayed resting orders of a symbol in priority order (bids then asks):
    [(order_id, side, price, qty)]. *)
val resting : t -> symbol_id:int -> (int * Exchange_core.Side.t * Price.t * int) list

(** The current Security Status of a symbol. *)
val status : t -> symbol_id:int -> Market_event.Status.t

(** Whether the symbol is in a LULD trading pause. *)
val halted : t -> symbol_id:int -> bool

val luld_bands : t -> symbol_id:int -> Equities_core.Halts.Bands.t

(** Number of live gateway orders, for tests. *)
val live_orders : t -> int
