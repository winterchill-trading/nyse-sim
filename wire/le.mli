(** Little-endian field access at fixed offsets, the building block of both protocols.

    Integers wider than 62 bits do not fit an OCaml [int]: [u64] reads and writes the low
    63 bits, so [-1] stands for [0xffffffffffffffff] (the "no end" sequence number of the
    Pillar Open message). [u64_bits] keeps all 64 bits for bitfields. *)

open! Core

val get_u8 : Bigstring.t -> int -> int
val set_u8 : Bigstring.t -> int -> int -> unit
val get_u16 : Bigstring.t -> int -> int
val set_u16 : Bigstring.t -> int -> int -> unit
val get_u32 : Bigstring.t -> int -> int
val set_u32 : Bigstring.t -> int -> int -> unit
val get_u64 : Bigstring.t -> int -> int
val set_u64 : Bigstring.t -> int -> int -> unit
val get_u64_bits : Bigstring.t -> int -> Int64.t
val set_u64_bits : Bigstring.t -> int -> Int64.t -> unit

(** A signed 32-bit integer (XDP prices may be negative in some messages). *)
val get_i32 : Bigstring.t -> int -> int

(** One ASCII character; NUL reads as ['\000']. *)
val get_char : Bigstring.t -> int -> char

val set_char : Bigstring.t -> int -> char -> unit

(** [char(n)]: ASCII right padded with spaces. Reading strips trailing spaces and NULs. *)
val get_text : Bigstring.t -> int -> len:int -> string

val set_text : Bigstring.t -> int -> len:int -> string -> unit

(** [zchar(n)]: ASCII right padded with NULs. Reading strips trailing NULs and spaces. *)
val get_ztext : Bigstring.t -> int -> len:int -> string

val set_ztext : Bigstring.t -> int -> len:int -> string -> unit

(** Space-separated hex bytes, e.g. ["01 02 4c 00"], for tests and debugging. *)
val hex : Bigstring.t -> string
