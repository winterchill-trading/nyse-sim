(** Pillar [BitfieldOrderInstructions], a little-endian [u64] of small integer fields.
    Values are kept raw so that every bit pattern round-trips; the constant modules name
    the values the simulator understands. Bits 0-11 are reserved and written as zero. *)

open! Core

type t =
  { sub_id_indicator : int (** bit 12, width 1 *)
  ; special_ord_type : int (** 13, 4 *)
  ; locate_reqd : int (** 17, 1 *)
  ; retail_indicator : int (** 18, 1 *)
  ; attributed_quote : int (** 19, 3 *)
  ; order_capacity : int (** 22, 3 *)
  ; interest_type : int (** 25, 3 *)
  ; trading_session : int (** 28, 3 *)
  ; time_in_force : int (** 31, 3 *)
  ; proactive_if_locked : int (** 34, 3 *)
  ; self_trade_type : int (** 37, 3 *)
  ; cancel_instead_of_reprice : int (** 40, 4 *)
  ; routing_inst : int (** 44, 4 *)
  ; extended_exec_inst : int (** 48, 4 *)
  ; exec_inst : int (** 52, 4 *)
  ; ord_type : int (** 56, 4 *)
  ; side : int (** 60, 4 *)
  }
[@@deriving sexp, compare, equal]

(** All fields zero. *)
val empty : t

(** A limit day order on [side] for the core session, agency capacity, non-routable. *)
val limit_day : side:int -> t

val to_int64 : t -> Int64.t

(** Raises nothing: any [u64] decodes, reserved bits are ignored. *)
val of_int64 : Int64.t -> t

module Side : sig
  val buy : int
  val sell : int
  val sell_short : int
  val sell_short_exempt : int
end

module Ord_type : sig
  val market : int
  val limit : int
end

module Tif : sig
  val day : int
  val ioc : int

  (** at the opening: MOO/LOO *)
  val opg : int

  (** MOC/LOC *)
  val on_close : int
end

module Trading_session : sig
  val overnight : int
  val early : int
  val core : int
  val late : int
  val early_core : int
  val core_late : int
  val early_core_late : int
  val all : int

  (** Whether the code includes the early (pre-market), core or late session. *)
  val includes_early : int -> bool

  val includes_core : int -> bool
  val includes_late : int -> bool
end

module Self_trade : sig
  val session_default : int
  val none : int
  val cancel_newest : int
  val cancel_oldest : int
  val cancel_both : int
  val cancel_decrement : int
end

module Extended_exec_inst : sig
  val alo : int
end
