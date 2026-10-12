(** Ties a listed symbol's noise traders to a reference price: the TradFi side of the
    price-anchor design (specs/design/price-anchor.md, "TradFi simulators").

    NYSE has no free real-time source, so there is no depth to shadow and no trade flow to
    replay. The noise traders stay the book's only liquidity; their random walk becomes
    mean-reverting around a reference price through
    {!Exchange_core_engine.Market_sim.set_reference} with a half-life. The reference is a
    series file ([time price] lines, {!Price_anchor.Source.parse_series_line}), a fixed
    price with an optional daily drift, or a recorded crypto series scaled by a factor.

    The anchor only pulls while the symbol trades continuously: in the core session,
    outside the auction imbalance periods and outside LULD pauses. Otherwise the venue
    owns the price; the anchor hands the symbol back to the free walk
    ({!Exchange_core_engine.Market_sim.clear_reference}) and resumes at the next open
    tick. LULD reference prices keep coming from the venue's own trades. Shadow levels and
    trade replay are off. *)

open! Core
module Price := Exchange_core.Price
module Phase := Equities_core.Session.Us_equities.Phase

module Reference : sig
  type t =
    | Series of (Time_ns.t * Price.t) array
    (** points in time order, at least one; the first point is applied when the anchor
        starts and the rest follow at the same distances in venue time *)
    | Fixed of
        { price : Price.t
        ; drift_bps_per_day : float (** compounded per day of venue time *)
        }
  [@@deriving sexp_of]
end

module Spec : sig
  type t =
    { symbol : string (** the listing, either spelling ([BRK.B] or [BRK B]) *)
    ; reference : Reference.t
    ; factor : float (** simulated price = reference price * factor *)
    }
  [@@deriving sexp_of]

  (** Points of a series file's contents; see {!Price_anchor.Source.of_series}. *)
  val parse_series : string -> (Time_ns.t * Price.t) array Or_error.t

  (** [-anchor-series FILE=SYM]: reads [FILE]. *)
  val of_series_flag : string -> t Or_error.t

  (** [-anchor-fixed PRICE[,DRIFT_BPS_PER_DAY]=SYM]. *)
  val of_fixed_flag : string -> t Or_error.t

  (** A reference series from recorded data: the mid of the reference book (L2 or BBO),
      else the last trade price, at most one point per [min_interval] (default 100ms) of
      [recv_ts] and only when the price changed. A gap drops the book. *)
  val points_of_source
    :  ?min_interval:Time_ns.Span.t
    -> Price_anchor.Source.t
    -> (Time_ns.t * Price.t) array Async.Deferred.t

  (** [-anchor-scaled VENUE:SYMBOL=SYM,FACTOR]: the recorded crypto instrument's
      normalized event logs under [norm_dir] for the UTC days [[from, to_]]
      ({!Price_anchor.Source.of_norm}), through {!points_of_source}, scaled by [FACTOR].
      Raw minute files would need the venue's normalizer, which lives in the venue's
      client repository; rebuild [norm/] there first ([rebuild-norm]). *)
  val of_scaled_flag
    :  string
    -> norm_dir:string
    -> from:Date.t
    -> to_:Date.t
    -> t Or_error.t Async.Deferred.t
end

(** Whether the anchor pulls: core session, no imbalance period ([auction]), not halted. *)
val active : phase:Phase.t -> halted:bool -> auction:bool -> bool

type t

(** [start] is the venue time the anchor starts at. [half_life] (default 1s) is the
    mean-reversion half-life. Raises if [factor] is not positive. *)
val create
  :  ?half_life:Time_ns.Span.t
  -> Spec.t
  -> instrument:Exchange_core.Instrument.t
  -> start:Time_ns.t
  -> t

val spec : t -> Spec.t

(** The scaled reference price at a venue time, rounded to the instrument's tick. *)
val price_at : t -> Time_ns.t -> Price.t option

(** Called on each noise tick before [Market_sim.tick]: pulls the symbol's walk towards
    {!price_at} when [active], and releases it to the free walk on the first tick that is
    not. *)
val on_tick
  :  t
  -> Exchange_core_engine.Market_sim.t
  -> active:bool
  -> now:Time_ns.t
  -> unit

(** {!on_tick} with the venue's phase, pause state and clock, for the noise tick of
    [symbol_id] (inside [Venue.with_engine]). [auction] is whether an imbalance period is
    running. *)
val on_venue_tick
  :  t
  -> Exchange_core_engine.Market_sim.t
  -> Venue.t
  -> symbol_id:int
  -> auction:bool
  -> unit

(** Whether the walk is currently pulled towards the anchor. *)
val engaged : t -> bool

module Stats : sig
  type t =
    { pulls : int (** ticks that pulled towards the anchor *)
    ; waits : int (** ticks the anchor waited (not active) *)
    ; releases : int (** hand-backs to the free walk *)
    }
  [@@deriving sexp_of]
end

val stats : t -> Stats.t
