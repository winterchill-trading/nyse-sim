(** A minimal Pillar Gateway client over TCP, for the CLI and end-to-end tests: logs in,
    learns the symbols from the REF stream, opens GT and TG, sends sequenced orders and
    heartbeats once a second. *)

open! Core
open! Async
module Pillar := Nyse_sim_wire.Pillar
module App := Nyse_sim_wire.Pillar_app

type t

(** Connects and logs in. On success the LoginResponse and StreamAvail messages are in
    {!greeting} and the REF stream has been read up to its current end. *)
val connect : host:string -> port:int -> user:string -> t Or_error.t Deferred.t

val greeting : t -> Pillar.t list

(** Symbol Reference Data from the REF stream, by SymbolID. *)
val symbols : t -> App.Symbol_ref.t list

(** The SymbolID of a ticker in either spelling (["BRK.B"] or ["BRK B"]). *)
val symbol_id : t -> string -> int Or_error.t

(** The next GT sequence number advertised at login. *)
val gt_next : t -> int

(** Opens GT for reading from [start_seq]; [end_seq] [-1] keeps it live. *)
val open_gt : t -> start_seq:int -> end_seq:int -> unit

(** Opens TG for writing; waits for the OpenResponse. *)
val open_tg : t -> unit Or_error.t Deferred.t

(** Sends an application message on TG with the next sequence number. *)
val send : t -> App.t -> unit

(** The next message from the gateway other than StreamAvail heartbeats, or [None] after
    [timeout] or at end of stream. *)
val next : t -> timeout:Time_ns.Span.t -> Pillar.t option Deferred.t

val close : t -> unit Deferred.t
