(** Reason codes and liquidity indicators used by the simulator in rejects, unsolicited
    cancels and execution reports.

    NYSE publishes its values in a spreadsheet that the spec summary does not reproduce,
    so these numbers are the simulator's own; only their meaning follows Pillar. *)

open! Core

val none : int
val unknown_symbol : int
val invalid_price : int
val invalid_quantity : int

(** not accepted in the current trading session *)
val session_closed : int

val halted : int
val unsupported : int
val duplicate_cl_ord_id : int
val unknown_order : int

(** ALO order would take liquidity *)
val would_cross : int

val self_trade : int
val ioc_unfilled : int
val insufficient_buying_power : int

(** Reg SHO Rule 201 restriction *)
val short_sale_price_test : int

(** OPG/on-close order outside its window *)
val auction_not_accepted : int

val done_for_day : int
val cancel_on_disconnect : int
val user_cancel : int

(** cancelled instead of re-priced to the LULD band *)
val luld : int

val not_logged_in : int

(** A short name, e.g. ["invalid price"], or ["reason N"]. *)
val to_string : int -> string

module Liquidity : sig
  (** ["A"] *)
  val added : string

  (** ["R"] *)
  val removed : string

  (** ["O"] *)
  val opening_auction : string

  (** ["C"] *)
  val closing_auction : string

  (** ["H"] *)
  val reopening_auction : string
end
