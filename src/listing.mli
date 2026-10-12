(** The equities the simulator lists, with the reference data both protocols publish
    (Pillar symbol reference data, XDP Symbol Index Mapping). *)

open! Core
module Price := Exchange_core.Price

type t =
  { symbol_id : int (** Pillar SymbolID and XDP SymbolIndex *)
  ; symbol : Exchange_core.Symbol.t (** engine symbol, compact form, e.g. ["BRK.B"] *)
  ; nyse_symbol : string (** NYSE symbology, e.g. ["BRK B"] *)
  ; listed_mic : string (** primary listing market *)
  ; reference_price : Price.t (** previous close: starting price and LULD reference *)
  ; round_lot : int
  ; tier : Equities_core.Halts.Tier.t
  ; scale : int (** XDP price scale code *)
  ; test : bool
  }
[@@deriving sexp_of]

val create
  :  ?listed_mic:string
  -> ?tier:Equities_core.Halts.Tier.t
  -> symbol_id:int
  -> string
  -> reference_price:Price.t
  -> t

(** AAPL, MSFT, IBM, JPM, KO, XOM, BRK B and the test symbol ZVZZT, with plausible
    previous closes. *)
val defaults : t list

(** The defaults whose symbol is listed (either spelling), in the order given. *)
val select : string list -> t list Or_error.t

(** An engine instrument per listing: whole shares, one-cent tick, NYSE-like fees. *)
val instrument : t -> Exchange_core.Instrument.t

val find_by_id : t list -> int -> t option
val find_by_symbol : t list -> Exchange_core.Symbol.t -> t option
