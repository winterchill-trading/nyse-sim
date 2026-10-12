(** Pillar Gateway application messages, carried as the payload of a sequenced message
    ([SeqMsg]). Every message starts with a [MsgHeader]: [type:u16] at 0 and [length:u16]
    at 2, the length covering header, body and add-ons.

    Prices are Pillar [Price] values (scale 8), which are exactly the units of
    {!Exchange_core.Price}. Offsets follow the NYSE Pillar Gateway Binary Protocol
    Specification as summarised in [equities-core/docs/nyse-pillar-gateway.md]; bytes the
    summary does not describe are written as zero and ignored on read. *)

open! Core
module Price := Exchange_core.Price

module Symbol_ref : sig
  (** Equities Symbol Reference Data, [0x0232], 56 bytes, REF stream. *)
  type t =
    { transact_time : int (** ns since the epoch *)
    ; symbol_id : int
    ; symbol : string (** NYSE symbology, e.g. ["BRK B"] *)
    ; listed_mic : string
    ; round_lot : int
    ; adv_risk_range : int
    ; mpv_class_id : int
    ; test_symbol : bool
    }
  [@@deriving sexp, compare, equal]
end

module Session_config_request : sig
  (** Session Configuration Request, [0x0220], 74 bytes, TG stream. *)
  type t =
    { username : string
    ; cancel_on_disconnect : bool
    ; throttle_preference : int
    ; self_trade_prevention : int
    (** session default STP, {!Order_instructions.Self_trade} *)
    ; order_priority_update : int
    ; bold_designation : int
    }
  [@@deriving sexp, compare, equal]
end

module Session_config_ack : sig
  (** Session Configuration Ack, [0x0221], 98 bytes, REF stream. The settings after
      [cancel_on_disconnect] (offsets 35 to 38) are this simulator's reading of "the rest
      echoes the request"; the spec summary does not give them. *)
  type t =
    { transact_time : int
    ; user_session_type : int
    ; user_session_status : int (** 0 accepted *)
    ; username : string
    ; mic : string
    ; cancel_on_disconnect : bool
    ; throttle_preference : int
    ; self_trade_prevention : int
    ; order_priority_update : int
    ; bold_designation : int
    }
  [@@deriving sexp, compare, equal]
end

module New_order : sig
  (** New Order Single and Cancel/Replace, [0x0240], 65 bytes plus an optional
      OptionalOrderAddOn [0x0241] (41 bytes) carrying [MaxFloor] at add-on offset 9. A
      non-zero [orig_cl_ord_id] makes it a cancel/replace. *)
  type t =
    { symbol_id : int
    ; mpid : string
    ; mmid : int
    ; mp_sub_id : char
    ; cl_ord_id : int
    ; orig_cl_ord_id : int
    ; instructions : Order_instructions.t
    ; price : Price.t
    ; order_qty : int
    ; min_qty : int
    ; user_data : string
    ; max_floor : int option (** reserve order display size, from the add-on *)
    }
  [@@deriving sexp, compare, equal]
end

module Cancel_request : sig
  (** Order Cancel Request, [0x0280], 28 bytes. [orig_cl_ord_id] names the order. *)
  type t =
    { symbol_id : int
    ; mpid : string
    ; cl_ord_id : int
    ; orig_cl_ord_id : int
    }
  [@@deriving sexp, compare, equal]
end

module Modify_request : sig
  (** Order Modify Request, [0x0270], 34 bytes: reduce the quantity (keeping priority) or
      change between the sell sides. *)
  type t =
    { symbol_id : int
    ; mpid : string
    ; cl_ord_id : int
    ; orig_cl_ord_id : int
    ; order_qty : int
    ; side : int
    ; locate_reqd : bool
    }
  [@@deriving sexp, compare, equal]
end

module Ack_type : sig
  (** 1 *)
  val new_order : int

  (** 5 *)
  val pending_cancel : int

  (** 6 *)
  val pending_replace : int

  (** 7 *)
  val pending_modify : int

  (** 8 *)
  val replaced : int

  (** 9 *)
  val modified : int

  (** 11 *)
  val canceled : int

  (** 12 *)
  val done_for_day : int
end

module Order_ack : sig
  (** Order and Cancel/Replace Ack, [0x0260], 102 bytes, GT stream. *)
  type t =
    { transact_time : int
    ; symbol_id : int
    ; mpid : string
    ; mmid : int
    ; mp_sub_id : char
    ; cl_ord_id : int
    ; orig_cl_ord_id : int
    ; instructions : Order_instructions.t
    ; price : Price.t
    ; order_qty : int
    ; min_qty : int
    ; order_id : int (** equals the XDP order id *)
    ; leaves_qty : int
    ; working_price : Price.t
    ; working_away : bool
    ; pre_liquidity_indicator : string
    ; reason_code : int
    ; ack_type : int
    ; flow_indicator : int
    ; user_data : string
    }
  [@@deriving sexp, compare, equal]
end

module Mod_cancel_ack : sig
  (** Modify/Cancel Request Ack and unsolicited out (UROUT), [0x0271], 74 bytes. *)
  type t =
    { transact_time : int
    ; symbol_id : int
    ; mpid : string
    ; order_id : int
    ; ref_cl_ord_id : int (** the request's ClOrdID; the order's own for an UROUT *)
    ; orig_cl_ord_id : int
    ; price : Price.t
    ; order_qty : int
    ; leaves_qty : int
    ; side : int
    ; locate_reqd : bool
    ; reason_code : int
    ; ack_type : int
    ; flow_indicator : int
    ; user_data : string
    }
  [@@deriving sexp, compare, equal]
end

module Execution_report : sig
  (** Execution Report, [0x0290], 84 bytes. [deal_id] is shared by both sides; its low 32
      bits are the XDP trade id. *)
  type t =
    { transact_time : int
    ; symbol_id : int
    ; mpid : string
    ; order_id : int
    ; cl_ord_id : int
    ; deal_id : int
    ; last_px : Price.t
    ; leaves_qty : int
    ; cum_qty : int
    ; last_qty : int
    ; liquidity_indicator : string
    ; execution_details : int
    ; locate_reqd : bool
    ; participant_type : int
    ; reason_code : int
    ; user_data : string
    }
  [@@deriving sexp, compare, equal]
end

module Reject_type : sig
  (** 1, new order or cancel/replace *)
  val order : int

  (** 2 *)
  val modify : int

  (** 3 *)
  val cancel : int
end

module Reject : sig
  (** Application Layer Reject, [0x0263], 43 bytes. *)
  type t =
    { transact_time : int
    ; symbol_id : int
    ; mpid : string
    ; cl_ord_id : int
    ; reason_code : int
    ; reject_type : int
    ; user_data : string
    }
  [@@deriving sexp, compare, equal]
end

type t =
  | Symbol_ref of Symbol_ref.t
  | Session_config_request of Session_config_request.t
  | Session_config_ack of Session_config_ack.t
  | New_order of New_order.t
  | Cancel_request of Cancel_request.t
  | Modify_request of Modify_request.t
  | Order_ack of Order_ack.t
  | Mod_cancel_ack of Mod_cancel_ack.t
  | Execution_report of Execution_report.t
  | Reject of Reject.t
  | Sequenced_filler (** [0x0282], 4 bytes *)
  | Unknown of
      { msg_type : int
      ; body : string (** the whole message, header included *)
      }
[@@deriving sexp, compare, equal]

val msg_type : t -> int

(** Encoded length, header included. *)
val length : t -> int

val encode : t -> Bigstring.t

(** Encodes at [pos] of a buffer with at least [length t] bytes left. *)
val write : t -> Bigstring.t -> pos:int -> unit

(** Decodes the message at [pos]; its header length must fit in [len] bytes. A known type
    shorter than its fixed layout is an error. *)
val read : Bigstring.t -> pos:int -> len:int -> t Or_error.t

val decode : Bigstring.t -> t Or_error.t
