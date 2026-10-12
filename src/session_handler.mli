(** One Pillar Gateway TCP connection: the Pillar Stream Protocol state machine.

    - Login (username any non-empty name, any password, version ["1.1"]) answered by
      LoginResponse and one StreamAvail per stream (TG write, GT and REF read).
    - Open/OpenResponse with replay of GT and REF from [start_seq]; Close/CloseResponse.
    - TG SeqMsgs must arrive in sequence; their payloads go to the {!Venue}: Session
      Configuration Request (acknowledged on REF), New Order, Cancel, Modify.
    - Every second the gateway sends StreamAvail per stream (its heartbeat). A connection
      silent for 5 s, or not logged in within 5 s, is closed; the gateway then cancels the
      user's orders unless the session configuration turned cancel on disconnect off.

    Timeouts use [wall] (real time); message timestamps use the venue clock. *)

open! Core
module Pillar := Nyse_sim_wire.Pillar

type t

val create
  :  ?wall:(unit -> Time_ns.t)
  -> Gateway.t
  -> conn:int
  -> send:(Pillar.t -> unit)
  -> close:(reason:string -> unit)
  -> t

val handle : t -> Pillar.t -> unit

(** Once a second: StreamAvail heartbeats and timeout checks. *)
val tick : t -> unit

(** The TCP connection went away: log out, cancel on disconnect. *)
val disconnected : t -> unit

val username : t -> string option
val heartbeat_timeout : Time_ns.Span.t
