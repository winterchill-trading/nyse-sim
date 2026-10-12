(** The XDP Integrated Feed publisher: sequences venue {!Market_event}s into XDP messages
    on one channel and fans them out to subscribers, one message per packet.

    - The channel starts with a Sequence Number Reset (its own packet, SeqNum 1), then a
      Source Time Reference and one Symbol Index Mapping per symbol.
    - Source Time Reference once a second, and a heartbeat packet (header only,
      DeliveryFlag 1) to binary subscribers when the channel was idle that second.
    - A subscriber joining later first gets a refresh (Symbol Index Mapping and Security
      Status per symbol, DeliveryFlag 17 to 20, SeqNum = the last sequenced message) and
      then the live packets. {!refresh} answers a Refresh Request (15) with Add Order
      Refresh (106) for every displayed resting order.
    - JSON subscribers receive the same sequenced messages as one JSON object per
      websocket message (see {!to_json}); no heartbeats. *)

open! Core
module Price := Exchange_core.Price
module Xdp := Nyse_sim_wire.Xdp

type t

module Sink : sig
  type t =
    | Binary of (Bigstring.t -> unit) (** one encoded packet per call *)
    | Json of (string -> unit)
end

val create : Listing.t list -> now:(unit -> Time_ns.t) -> t

(** Publishes the channel start (sequence reset, time reference, symbol mappings). *)
val start : t -> unit

val publish : t -> Market_event.t -> unit

(** The once-a-second Source Time Reference and heartbeats. *)
val second : t -> unit

(** Adds a subscriber and sends it the start-of-stream refresh. Returns its id. *)
val subscribe : t -> Sink.t -> int

val unsubscribe : t -> int -> unit
val num_subscribers : t -> int

(** Answers a Refresh Request for [symbol_index] (0: every symbol) to one subscriber.
    [resting] lists a symbol's displayed orders as [(order_id, side, price, qty)]. *)
val refresh
  :  t
  -> subscriber:int
  -> symbol_index:int
  -> resting:(symbol_id:int -> (int * Exchange_core.Side.t * Price.t * int) list)
  -> unit

(** The last sequence number used on the channel. *)
val last_seq : t -> int

(** The XDP message for an event (with its symbol sequence number taken). *)
val to_xdp : t -> Market_event.t -> Xdp.Msg.t

(** JSON rendering of a sequenced message: [{"seq", "type", "symbol", ...}] with prices as
    decimal strings. *)
val to_json : t -> seq:int -> Xdp.Msg.t -> Yojson.Safe.t
