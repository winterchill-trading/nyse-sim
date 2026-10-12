(** NYSE Pillar Integrated Feed (XDP) packets and messages, little endian.

    A packet is a 16-byte header followed by [NumberMsgs] messages; each message starts
    with [MsgSize:u16] and [MsgType:u16], and readers skip by [MsgSize]. Over TCP (the
    simulator's stand-in for multicast) packets are sent back to back and framed by their
    own [PktSize].

    Prices are [u32] numerators: [price = value / 10^PriceScaleCode] with the scale from
    the symbol's {!Msg.Symbol_index_mapping}. Offsets follow
    [equities-core/docs/nyse-xdp-integrated-feed.md]; undocumented bytes are zero. The
    layouts of Add Order Refresh (106) and Refresh Request (15) are not in that summary:
    106 is taken to be the Add Order layout with type 106, and 15 is this simulator's own
    ([SymbolIndex:u32] at 4, 0 for every symbol; [SourceID char(10)] at 8; [ProductID] at
    18; [ChannelID] at 19; 20 bytes). *)

open! Core
module Price := Exchange_core.Price

module Delivery_flag : sig
  (** 1 *)
  val heartbeat : int

  (** 10 *)
  val failover : int

  (** 11 *)
  val original : int

  (** 12 *)
  val sequence_reset : int

  (** 17: first packet of a refresh *)
  val refresh_start : int

  (** 18 *)
  val refresh_part : int

  (** 19: a refresh that fits one packet *)
  val refresh_single : int

  (** 20 *)
  val refresh_end : int
end

(** [value / 10^scale] as a price. *)
val price_of_raw : scale:int -> int -> Price.t

(** The [u32] numerator of a price at a scale, truncated. *)
val raw_of_price : scale:int -> Price.t -> int

(** The NYSE price scale code for a symbol by its last close: 6 under $500, 4 from $500, 3
    from $100,000. *)
val scale_for_price : Price.t -> int

(** Fields shared by the order and trade messages (100-110): [SourceTimeNS] at 4,
    [SymbolIndex] at 8, [SymbolSeqNum] at 12. *)
module Common : sig
  type t =
    { source_time_ns : int
    ; symbol_index : int
    ; symbol_seq_num : int
    }
  [@@deriving sexp, compare, equal]
end

module Msg : sig
  type t =
    | Sequence_number_reset of
        { source_time : int
        ; source_time_ns : int
        ; product_id : int
        ; channel_id : int
        } (** type 1, 14 bytes *)
    | Source_time_reference of
        { id : int
        ; symbol_seq_num : int
        ; source_time : int (** seconds since the epoch *)
        } (** type 2, 16 bytes *)
    | Symbol_index_mapping of
        { symbol_index : int
        ; symbol : string
        ; market_id : int
        ; system_id : int
        ; exchange_code : char
        ; price_scale_code : int
        ; security_type : char
        ; lot_size : int
        ; prev_close_price : int
        ; prev_close_volume : int
        ; price_resolution : int
        ; round_lot : char
        ; mpv : int (** minimum price variation, at the symbol's scale *)
        ; unit_of_trade : int
        } (** type 3, 44 bytes *)
    | Refresh_request of
        { symbol_index : int
        ; source_id : string
        ; product_id : int
        ; channel_id : int
        } (** type 15, client to server *)
    | Security_status of
        { source_time : int
        ; source_time_ns : int
        ; symbol_index : int
        ; symbol_seq_num : int
        ; security_status : char
        ; halt_condition : char
        ; price_1 : int
        ; price_2 : int
        ; ssr_triggering_exchange : char
        ; ssr_triggering_volume : int
        ; time : int
        ; ssr_state : char
        ; market_state : char
        ; session_state : char
        } (** type 34, 46 bytes *)
    | Add_order of
        { common : Common.t
        ; order_id : int
        ; price : int
        ; volume : int
        ; side : char (** ['B'] or ['S'] *)
        ; firm_id : string
        } (** type 100, 39 bytes *)
    | Modify_order of
        { common : Common.t
        ; order_id : int
        ; price : int
        ; volume : int
        ; position_change : int (** 1: the order lost its queue position *)
        ; side : char
        } (** type 101, 35 bytes *)
    | Delete_order of
        { common : Common.t
        ; order_id : int
        } (** type 102, 25 bytes *)
    | Order_execution of
        { common : Common.t
        ; order_id : int
        ; trade_id : int
        ; price : int
        ; volume : int (** executed shares *)
        ; printable_flag : int
        ; trade_cond : string (** TradeCond1-4 *)
        } (** type 103, 42 bytes *)
    | Replace_order of
        { common : Common.t
        ; order_id : int
        ; new_order_id : int
        ; price : int
        ; volume : int
        ; side : char
        } (** type 104, 42 bytes *)
    | Imbalance of
        { source_time : int
        ; source_time_ns : int
        ; symbol_index : int
        ; symbol_seq_num : int
        ; reference_price : int
        ; paired_qty : int
        ; total_imbalance_qty : int
        ; market_imbalance_qty : int
        ; auction_time : int (** HHMM *)
        ; auction_type : char
        (** ['O'] early open, ['M'] core open, ['H'] reopening, ['C'] closing *)
        ; imbalance_side : char (** ['B'], ['S'] or [' '] *)
        ; continuous_book_clearing_price : int
        ; auction_interest_clearing_price : int
        ; ssr_filing_price : int
        ; indicative_match_price : int
        ; upper_collar : int
        ; lower_collar : int
        ; auction_status : int
        ; freeze_status : int
        ; num_extensions : int
        ; unpaired_qty : int
        ; unpaired_side : char
        ; significant_imbalance : char
        } (** type 105, 73 bytes *)
    | Add_order_refresh of
        { common : Common.t
        ; order_id : int
        ; price : int
        ; volume : int
        ; side : char
        ; firm_id : string
        } (** type 106 *)
    | Non_displayed_trade of
        { common : Common.t
        ; trade_id : int
        ; price : int
        ; volume : int
        ; printable_flag : int
        ; trade_cond : string
        } (** type 110, 33 bytes *)
    | Unknown of
        { msg_type : int
        ; body : string
        }
  [@@deriving sexp, compare, equal]

  val msg_type : t -> int
  val size : t -> int

  (** The symbol index the message is about, if any. *)
  val symbol_index : t -> int option

  val encode : t -> Bigstring.t
  val write : t -> Bigstring.t -> pos:int -> unit

  (** Reads the message at [pos], which must fit in [len] bytes. *)
  val read : Bigstring.t -> pos:int -> len:int -> t Or_error.t

  (** One line per message. [symbols] gives a symbol index's name and price scale. *)
  val to_string_hum : ?symbols:(int -> (string * int) option) -> t -> string
end

module Packet : sig
  type t =
    { delivery_flag : int
    ; seq_num : int (** of the first message *)
    ; send_time : int (** seconds *)
    ; send_time_ns : int
    ; msgs : Msg.t list
    }
  [@@deriving sexp, compare, equal]

  val header_length : int

  (** Maximum packet size on the real feed. *)
  val max_size : int

  val size : t -> int
  val encode : t -> Bigstring.t

  (** The [PktSize] of the packet starting at [pos] (2 bytes are enough). *)
  val frame_length : Bigstring.t -> pos:int -> int

  val read : Bigstring.t -> pos:int -> len:int -> t Or_error.t
  val decode : Bigstring.t -> t Or_error.t
end
