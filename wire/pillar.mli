(** Pillar Stream Protocol: the session layer of the Pillar Gateway over TCP. Messages are
    framed by their own [MsgHeader] ([type:u16], [length:u16]); a TCP reader reads 4
    bytes, then [length - 4] more ({!frame_length}).

    Sequence numbers are [u64] on the wire. They are OCaml [int]s here; [-1] encodes
    [0xffffffffffffffff], the "no end" value of {!Open.end_seq}. *)

open! Core

module Status : sig
  val ok : int
  val not_logged_in : int
  val invalid_login : int
  val already_logged_in : int
  val heartbeat_timeout : int
  val login_timed_out : int
  val invalid_message : int
  val no_stream_permission : int
  val invalid_protocol_version : int
  val out_of_sequence : int
  val invalid_stream : int
  val stream_not_open : int

  (** Name of a status code, e.g. ["ok"], or ["status N"] for unknown ones. *)
  val to_string : int -> string
end

module Access : sig
  (** bit 0 *)
  val read : int

  (** bit 1 *)
  val write : int

  (** bit 2 *)
  val throttle_reject : int
end

(** The protocol version this gateway speaks, ["1.1"]. *)
val protocol_version : string

type t =
  | Login of
      { username : string
      ; password : string
      ; mic : string
      ; version : string
      }
  | Login_response of
      { username : string
      ; status : int
      }
  | Stream_avail of
      { stream_id : Stream_id.t
      ; next_seq : int
      ; access : int
      }
  | Heartbeat
  | Open of
      { stream_id : Stream_id.t
      ; start_seq : int
      ; end_seq : int (** exclusive; [-1] for no end *)
      ; access : int
      ; mode : int (** bit 0 lossy *)
      }
  | Open_response of
      { stream_id : Stream_id.t
      ; status : int
      ; access : int
      }
  | Close of { stream_id : Stream_id.t }
  | Close_response of
      { stream_id : Stream_id.t
      ; status : int
      }
  | Seq_msg of
      { stream_id : Stream_id.t
      ; seq : int
      ; timestamp : int (** ns since the epoch *)
      ; payload : Pillar_app.t
      }
  | Unknown of
      { msg_type : int
      ; body : string
      }
[@@deriving sexp, compare, equal]

val msg_type : t -> int
val length : t -> int
val encode : t -> Bigstring.t

(** The total length announced by a 4-byte message header. *)
val frame_length : Bigstring.t -> pos:int -> int

val read : Bigstring.t -> pos:int -> len:int -> t Or_error.t
val decode : Bigstring.t -> t Or_error.t

(** One line per message, for the CLI and logs. *)
val to_string_hum : t -> string
