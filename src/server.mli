(** The simulator process: the venue, the Pillar gateway (TCP), the XDP feed (TCP) and a
    JSON websocket mirror of the feed (HTTP [/ws]), driven by a session clock and,
    optionally, noise traders. *)

open! Core
open! Async

module Clock_mode : sig
  type t =
    | Real (** New York wall clock: closed at night and at weekends *)
    | Accelerated of
        { start : Time_ns.t
        ; speed : float (** venue seconds per real second *)
        }
    | Always_open (** wall clock time, but always in the core session *)
  [@@deriving sexp_of]
end

module Config : sig
  type t =
    { gateway_port : int
    ; feed_port : int
    ; http_port : int
    ; listings : Listing.t list
    ; clock : Clock_mode.t
    ; noise : bool
    ; seed : int
    }
  [@@deriving sexp_of]
end

type t

(** Binds the three ports (0 picks free ones) and starts the clock, feed and noise. *)
val start : Config.t -> t Deferred.t

val gateway_port : t -> int
val feed_port : t -> int
val http_port : t -> int
val venue : t -> Venue.t

(** Closes the listeners and stops the timers. *)
val stop : t -> unit Deferred.t
