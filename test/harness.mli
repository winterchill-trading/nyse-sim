(** A venue on a hand-driven clock that prints what it sends to members and the market. *)

open! Core
module Price := Exchange_core.Price

type t

(** Starts at 2026-10-13 10:00 New York time, in [phase] (default core). *)
val create : ?phase:Equities_core.Session.Us_equities.Phase.t -> unit -> t

val venue : t -> Nyse_sim.Venue.t
val advance : t -> Time_ns.Span.t -> unit

(** Moves the clock to a New York local time on the same day. *)
val set_local_time : t -> int -> int -> unit

(** Sends a New Order Single. [side] is [`Buy], [`Sell] or [`Short]; [price] [None] is a
    market order. *)
val order
  :  ?user:string
  -> ?symbol_id:int
  -> ?tif:int
  -> ?alo:bool
  -> ?max_floor:int
  -> ?stp:int
  -> ?orig:int
  -> ?session:int
  -> ?cancel_instead_of_reprice:bool
  -> t
  -> cl_ord_id:int
  -> side:[ `Buy | `Sell | `Short ]
  -> qty:int
  -> price:string option
  -> unit

val cancel : ?user:string -> ?symbol_id:int -> t -> cl_ord_id:int -> orig:int -> unit
val modify : ?user:string -> t -> cl_ord_id:int -> orig:int -> qty:int -> unit

(** Prints and forgets everything sent since the last call: member messages prefixed by
    the user, market events prefixed by [feed]. *)
val flush : t -> unit
