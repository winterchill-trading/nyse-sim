(** Ties a listed symbol's noise traders to a reference price: the TradFi side of the
    price-anchor design (specs/design/price-anchor.md, "TradFi simulators"), as a thin
    wrapper over {!Price_anchor.Anchor}.

    NYSE has no free real-time source, so there is no depth to shadow and no trade flow to
    replay. The noise traders stay the book's only liquidity; their random walk becomes
    mean-reverting around a reference {!Price_anchor.Path.t} pulled on the venue's clock
    ([half_life], [step_clock:false], [levels:0], [mark_interval] zero). The path is a
    series file, a fixed price with an optional daily drift, or a recorded crypto series
    scaled by a factor.

    The anchor only moves the price while the symbol trades continuously ({!active}): in
    the core session, outside the auction imbalance periods and outside LULD pauses.
    Otherwise the venue owns the price; the anchor hands the symbol back to the free walk
    and, when it may engage again, restarts the walk from the venue's last trade (the
    auction price). The target is kept inside the LULD bands
    ({!Price_anchor.Clamp.within_bands}, 30 bps margin); the bands keep coming from the
    venue's own trades. *)

open! Core
module Price := Exchange_core.Price
module Phase := Equities_core.Session.Us_equities.Phase

module Spec : sig
  type t =
    { symbol : string (** the listing, either spelling ([BRK.B] or [BRK B]) *)
    ; path : Price_anchor.Path.t
    ; factor : float (** simulated price = reference price * factor *)
    ; recorded : Market_data.Instrument.t option
    (** the recorded instrument of [-anchor-scaled]; [None] for a series or a fixed price *)
    }
  [@@deriving sexp_of]

  (** [-anchor-series FILE=SYM]: reads [FILE] ({!Price_anchor.Source.of_series}). *)
  val of_series_flag : string -> t Or_error.t Async.Deferred.t

  (** [-anchor-fixed PRICE[,DRIFT_BPS_PER_DAY]=SYM]
      ({!Price_anchor.Path.fixed_of_string}). *)
  val of_fixed_flag : string -> t Or_error.t

  (** [-anchor-scaled VENUE:SYMBOL=SYM,FACTOR]: the recorded crypto instrument's
      normalized event logs under [norm_dir] for the UTC days [[from, to_]]
      ({!Price_anchor.Source.of_norm}), at most one point per 100 ms
      ({!Price_anchor.Source.to_path}), scaled by [FACTOR]. Raw minute files would need
      the venue's normalizer, which lives in the venue's client repository; rebuild
      [norm/] there first ([rebuild-norm]). *)
  val of_scaled_flag
    :  string
    -> norm_dir:string
    -> from:Date.t
    -> to_:Date.t
    -> t Or_error.t Async.Deferred.t
end

(** Whether the anchor may move the price: core session, no imbalance period ([auction]),
    not halted. *)
val active : phase:Phase.t -> halted:bool -> auction:bool -> bool

type t

(** Anchors [symbol_id]'s walk in [market_sim] (which keeps [noise_traders]). [half_life]
    (default 1s) is the mean-reversion half-life; [auction] says whether an imbalance
    period is running. The path starts at the venue's current time. Raises if
    [spec.factor] is not positive. *)
val create
  :  ?half_life:Time_ns.Span.t
  -> ?log:(Sexp.t -> unit)
  -> Spec.t
  -> Venue.t
  -> symbol_id:int
  -> market_sim:Exchange_core_engine.Market_sim.t
  -> noise_traders:int
  -> auction:(unit -> bool)
  -> t

val spec : t -> Spec.t
val anchor : t -> Price_anchor.Anchor.t

(** The scaled reference price at the last {!on_venue_tick}, rounded to the tick, before
    the LULD clamp. *)
val price : t -> Price.t option

(** Called on each noise tick inside [Venue.with_engine], before [Market_sim.tick]. *)
val on_venue_tick : t -> unit

(** Whether the walk is currently pulled towards the anchor. *)
val engaged : t -> bool

module Stats : sig
  type t =
    { pulls : int (** reference changes set on the walk ([marks_set]) *)
    ; waits : int (** ticks the anchor waited (not {!active}) *)
    ; releases : int (** hand-backs to the free walk ([pauses]) *)
    ; resumes : int (** restarts from the venue's last trade after a release *)
    ; clamped : int (** pulls whose target was held inside the LULD bands *)
    }
  [@@deriving sexp_of]
end

val stats : t -> Stats.t
