(** Per-user Pillar streams, shared by that user's connections over time.

    Each user (Pillar username) owns three streams: TG (its orders, written by the
    member), GT (acks, cancels, executions, rejects) and REF (symbol reference data and
    session configuration acks). GT and REF are logs kept for the life of the simulator,
    so a reconnecting member can replay them from any sequence number. Only one connection
    may be logged in as a user at a time. *)

open! Core
module Pillar := Nyse_sim_wire.Pillar
module App := Nyse_sim_wire.Pillar_app
module Stream_id := Nyse_sim_wire.Stream_id

module User : sig
  type t

  val name : t -> string
  val index : t -> int
  val stream : t -> Stream_id.Kind.t -> Stream_id.t

  (** Messages in a read stream so far ([GT] or [REF]); the next one gets this plus 1. *)
  val length : t -> Stream_id.Kind.t -> int

  (** The next TG sequence number expected from the member. *)
  val tg_next : t -> int

  val cancel_on_disconnect : t -> bool
end

type t

(** [clock] stamps sequenced messages (the venue clock). *)
val create : Listing.t list -> clock:(unit -> Time_ns.t) -> t

(** The venue that receives members' orders; set once, before any login. *)
val set_venue : t -> Venue.t -> unit

val venue : t -> Venue.t

(** Finds or creates the user; a new user's REF stream starts with one Symbol Reference
    Data message per listing. *)
val user : t -> string -> User.t

(** Appends to the user's GT stream and delivers to its live subscriber, if any. Used as
    the venue's [send_user]. *)
val send_user : t -> string -> App.t -> unit

(** Appends to the REF stream. *)
val send_ref : t -> User.t -> App.t -> unit

(** {2 Connections} *)

(** Marks the user as logged in on a connection; [deliver] receives sequenced messages of
    the read streams it opens. [Error] if another connection holds the user. *)
val attach : t -> User.t -> conn:int -> unit Or_error.t

val detach : t -> User.t -> conn:int -> unit

(** Replays a read stream from [start_seq] to [end_seq] (exclusive, [-1] for no end)
    through [deliver], and keeps delivering new messages when [end_seq] is [-1]. *)
val open_read
  :  t
  -> User.t
  -> Stream_id.Kind.t
  -> start_seq:int
  -> end_seq:int
  -> deliver:(Pillar.t -> unit)
  -> unit

val close_read : t -> User.t -> Stream_id.Kind.t -> unit

(** Accepts the next TG sequence number, or returns the expected one. *)
val accept_tg : User.t -> seq:int -> (unit, int) Result.t

val set_cancel_on_disconnect : User.t -> bool -> unit
