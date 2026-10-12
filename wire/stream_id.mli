(** Pillar [StreamId], 8 bytes: [sess:u32] (bits 24-31 environment id, 0-23 session
    number) then [value:u32] (bits 24-31 stream type, 8-23 user id, 0-7 sub id). *)

open! Core

module Kind : sig
  type t =
    | Tg (** trader to gateway, written by the member (type 15) *)
    | Gt (** gateway to trader (13) *)
    | Ref (** reference data (33) *)
    | Other of int
  [@@deriving sexp, compare, equal]

  val to_int : t -> int
  val of_int : int -> t
end

type t =
  { env : int
  ; session : int
  ; kind : Kind.t
  ; user : int
  ; sub : int
  }
[@@deriving sexp, compare, equal]

val create : ?env:int -> ?session:int -> ?sub:int -> Kind.t -> user:int -> t

(** Byte length on the wire. *)
val length : int

val read : Bigstring.t -> int -> t
val write : Bigstring.t -> int -> t -> unit
