(** Reading whole Pillar messages and XDP packets from an Async [Reader]. Both are framed
    by their own length field: Pillar by [length:u16] at offset 2, XDP packets by
    [PktSize:u16] at offset 0. *)

open! Core
open! Async

(** The next Pillar message as raw bytes, [None] at end of file. *)
val read_pillar : Reader.t -> Bigstring.t option Deferred.t

(** The next XDP packet as raw bytes, [None] at end of file. *)
val read_xdp : Reader.t -> Bigstring.t option Deferred.t

val write : Writer.t -> Bigstring.t -> unit
