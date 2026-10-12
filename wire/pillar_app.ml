open! Core
module Price = Exchange_core.Price

let price b pos = Price.of_units (Le.get_u64 b pos)
let set_price b pos p = Le.set_u64 b pos (Price.to_units p)
let bool b pos = Le.get_u8 b pos <> 0
let set_bool b pos v = Le.set_u8 b pos (if v then 1 else 0)

let header b ~msg_type ~length =
  Le.set_u16 b 0 msg_type;
  Le.set_u16 b 2 length
;;

module Symbol_ref = struct
  type t =
    { transact_time : int
    ; symbol_id : int
    ; symbol : string
    ; listed_mic : string
    ; round_lot : int
    ; adv_risk_range : int
    ; mpv_class_id : int
    ; test_symbol : bool
    }
  [@@deriving sexp, compare, equal]

  let msg_type = 0x0232
  let length = 56

  let write t b =
    Le.set_u64 b 4 t.transact_time;
    Le.set_u32 b 12 t.symbol_id;
    Le.set_text b 16 ~len:24 t.symbol;
    Le.set_text b 40 ~len:4 t.listed_mic;
    Le.set_u8 b 44 t.round_lot;
    Le.set_u8 b 45 t.adv_risk_range;
    Le.set_u16 b 53 t.mpv_class_id;
    set_bool b 55 t.test_symbol
  ;;

  let read b =
    { transact_time = Le.get_u64 b 4
    ; symbol_id = Le.get_u32 b 12
    ; symbol = Le.get_text b 16 ~len:24
    ; listed_mic = Le.get_text b 40 ~len:4
    ; round_lot = Le.get_u8 b 44
    ; adv_risk_range = Le.get_u8 b 45
    ; mpv_class_id = Le.get_u16 b 53
    ; test_symbol = bool b 55
    }
  ;;
end

module Session_config_request = struct
  type t =
    { username : string
    ; cancel_on_disconnect : bool
    ; throttle_preference : int
    ; self_trade_prevention : int
    ; order_priority_update : int
    ; bold_designation : int
    }
  [@@deriving sexp, compare, equal]

  let msg_type = 0x0220
  let length = 74

  let write t b =
    Le.set_text b 4 ~len:16 t.username;
    set_bool b 20 t.cancel_on_disconnect;
    Le.set_u8 b 21 t.throttle_preference;
    Le.set_u8 b 22 t.self_trade_prevention;
    Le.set_u8 b 23 t.order_priority_update;
    Le.set_u8 b 24 t.bold_designation
  ;;

  let read b =
    { username = Le.get_text b 4 ~len:16
    ; cancel_on_disconnect = bool b 20
    ; throttle_preference = Le.get_u8 b 21
    ; self_trade_prevention = Le.get_u8 b 22
    ; order_priority_update = Le.get_u8 b 23
    ; bold_designation = Le.get_u8 b 24
    }
  ;;
end

module Session_config_ack = struct
  type t =
    { transact_time : int
    ; user_session_type : int
    ; user_session_status : int
    ; username : string
    ; mic : string
    ; cancel_on_disconnect : bool
    ; throttle_preference : int
    ; self_trade_prevention : int
    ; order_priority_update : int
    ; bold_designation : int
    }
  [@@deriving sexp, compare, equal]

  let msg_type = 0x0221
  let length = 98

  let write t b =
    Le.set_u64 b 4 t.transact_time;
    Le.set_u8 b 12 t.user_session_type;
    Le.set_u8 b 13 t.user_session_status;
    Le.set_text b 14 ~len:16 t.username;
    Le.set_text b 30 ~len:4 t.mic;
    set_bool b 34 t.cancel_on_disconnect;
    Le.set_u8 b 35 t.throttle_preference;
    Le.set_u8 b 36 t.self_trade_prevention;
    Le.set_u8 b 37 t.order_priority_update;
    Le.set_u8 b 38 t.bold_designation
  ;;

  let read b =
    { transact_time = Le.get_u64 b 4
    ; user_session_type = Le.get_u8 b 12
    ; user_session_status = Le.get_u8 b 13
    ; username = Le.get_text b 14 ~len:16
    ; mic = Le.get_text b 30 ~len:4
    ; cancel_on_disconnect = bool b 34
    ; throttle_preference = Le.get_u8 b 35
    ; self_trade_prevention = Le.get_u8 b 36
    ; order_priority_update = Le.get_u8 b 37
    ; bold_designation = Le.get_u8 b 38
    }
  ;;
end

module New_order = struct
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
    ; max_floor : int option
    }
  [@@deriving sexp, compare, equal]

  let msg_type = 0x0240
  let base_length = 65
  let add_on_type = 0x0241
  let add_on_length = 41

  let length t =
    if Option.is_some t.max_floor then base_length + add_on_length else base_length
  ;;

  let write t b =
    Le.set_u32 b 4 t.symbol_id;
    Le.set_ztext b 8 ~len:4 t.mpid;
    Le.set_u32 b 12 t.mmid;
    Le.set_char b 16 t.mp_sub_id;
    Le.set_u64 b 17 t.cl_ord_id;
    Le.set_u64 b 25 t.orig_cl_ord_id;
    Le.set_u64_bits b 33 (Order_instructions.to_int64 t.instructions);
    set_price b 41 t.price;
    Le.set_u32 b 49 t.order_qty;
    Le.set_u32 b 53 t.min_qty;
    Le.set_ztext b 57 ~len:8 t.user_data;
    Option.iter t.max_floor ~f:(fun max_floor ->
      let a = base_length in
      Le.set_u16 b a add_on_type;
      Le.set_u16 b (a + 2) add_on_length;
      Le.set_u32 b (a + 9) max_floor)
  ;;

  let read b =
    let len = Bigstring.length b in
    let max_floor =
      (* Add-ons follow the fixed body; each starts with its own type and length. *)
      let rec scan pos =
        if pos + 4 > len
        then None
        else (
          let typ = Le.get_u16 b pos
          and alen = Le.get_u16 b (pos + 2) in
          if alen < 4 || pos + alen > len
          then None
          else if typ = add_on_type && alen >= 13
          then Some (Le.get_u32 b (pos + 9))
          else scan (pos + alen))
      in
      scan base_length
    in
    { symbol_id = Le.get_u32 b 4
    ; mpid = Le.get_ztext b 8 ~len:4
    ; mmid = Le.get_u32 b 12
    ; mp_sub_id = Le.get_char b 16
    ; cl_ord_id = Le.get_u64 b 17
    ; orig_cl_ord_id = Le.get_u64 b 25
    ; instructions = Order_instructions.of_int64 (Le.get_u64_bits b 33)
    ; price = price b 41
    ; order_qty = Le.get_u32 b 49
    ; min_qty = Le.get_u32 b 53
    ; user_data = Le.get_ztext b 57 ~len:8
    ; max_floor
    }
  ;;
end

module Cancel_request = struct
  type t =
    { symbol_id : int
    ; mpid : string
    ; cl_ord_id : int
    ; orig_cl_ord_id : int
    }
  [@@deriving sexp, compare, equal]

  let msg_type = 0x0280
  let length = 28

  let write t b =
    Le.set_u32 b 4 t.symbol_id;
    Le.set_ztext b 8 ~len:4 t.mpid;
    Le.set_u64 b 12 t.cl_ord_id;
    Le.set_u64 b 20 t.orig_cl_ord_id
  ;;

  let read b =
    { symbol_id = Le.get_u32 b 4
    ; mpid = Le.get_ztext b 8 ~len:4
    ; cl_ord_id = Le.get_u64 b 12
    ; orig_cl_ord_id = Le.get_u64 b 20
    }
  ;;
end

module Modify_request = struct
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

  let msg_type = 0x0270
  let length = 34

  let write t b =
    Le.set_u32 b 4 t.symbol_id;
    Le.set_ztext b 8 ~len:4 t.mpid;
    Le.set_u64 b 12 t.cl_ord_id;
    Le.set_u64 b 20 t.orig_cl_ord_id;
    Le.set_u32 b 28 t.order_qty;
    Le.set_u8 b 32 t.side;
    set_bool b 33 t.locate_reqd
  ;;

  let read b =
    { symbol_id = Le.get_u32 b 4
    ; mpid = Le.get_ztext b 8 ~len:4
    ; cl_ord_id = Le.get_u64 b 12
    ; orig_cl_ord_id = Le.get_u64 b 20
    ; order_qty = Le.get_u32 b 28
    ; side = Le.get_u8 b 32
    ; locate_reqd = bool b 33
    }
  ;;
end

module Ack_type = struct
  let new_order = 1
  let pending_cancel = 5
  let pending_replace = 6
  let pending_modify = 7
  let replaced = 8
  let modified = 9
  let canceled = 11
  let done_for_day = 12
end

module Order_ack = struct
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
    ; order_id : int
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

  let msg_type = 0x0260
  let length = 102

  let write t b =
    Le.set_u64 b 4 t.transact_time;
    Le.set_u32 b 12 t.symbol_id;
    Le.set_ztext b 16 ~len:4 t.mpid;
    Le.set_u32 b 20 t.mmid;
    Le.set_char b 24 t.mp_sub_id;
    Le.set_u64 b 25 t.cl_ord_id;
    Le.set_u64 b 33 t.orig_cl_ord_id;
    Le.set_u64_bits b 41 (Order_instructions.to_int64 t.instructions);
    set_price b 49 t.price;
    Le.set_u32 b 57 t.order_qty;
    Le.set_u32 b 61 t.min_qty;
    Le.set_u64 b 65 t.order_id;
    Le.set_u32 b 73 t.leaves_qty;
    set_price b 77 t.working_price;
    set_bool b 85 t.working_away;
    Le.set_ztext b 86 ~len:4 t.pre_liquidity_indicator;
    Le.set_u16 b 90 t.reason_code;
    Le.set_u8 b 92 t.ack_type;
    Le.set_u8 b 93 t.flow_indicator;
    Le.set_ztext b 94 ~len:8 t.user_data
  ;;

  let read b =
    { transact_time = Le.get_u64 b 4
    ; symbol_id = Le.get_u32 b 12
    ; mpid = Le.get_ztext b 16 ~len:4
    ; mmid = Le.get_u32 b 20
    ; mp_sub_id = Le.get_char b 24
    ; cl_ord_id = Le.get_u64 b 25
    ; orig_cl_ord_id = Le.get_u64 b 33
    ; instructions = Order_instructions.of_int64 (Le.get_u64_bits b 41)
    ; price = price b 49
    ; order_qty = Le.get_u32 b 57
    ; min_qty = Le.get_u32 b 61
    ; order_id = Le.get_u64 b 65
    ; leaves_qty = Le.get_u32 b 73
    ; working_price = price b 77
    ; working_away = bool b 85
    ; pre_liquidity_indicator = Le.get_ztext b 86 ~len:4
    ; reason_code = Le.get_u16 b 90
    ; ack_type = Le.get_u8 b 92
    ; flow_indicator = Le.get_u8 b 93
    ; user_data = Le.get_ztext b 94 ~len:8
    }
  ;;
end

module Mod_cancel_ack = struct
  type t =
    { transact_time : int
    ; symbol_id : int
    ; mpid : string
    ; order_id : int
    ; ref_cl_ord_id : int
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

  let msg_type = 0x0271
  let length = 74

  let write t b =
    Le.set_u64 b 4 t.transact_time;
    Le.set_u32 b 12 t.symbol_id;
    Le.set_ztext b 16 ~len:4 t.mpid;
    Le.set_u64 b 20 t.order_id;
    Le.set_u64 b 28 t.ref_cl_ord_id;
    Le.set_u64 b 36 t.orig_cl_ord_id;
    set_price b 44 t.price;
    Le.set_u32 b 52 t.order_qty;
    Le.set_u32 b 56 t.leaves_qty;
    Le.set_u8 b 60 t.side;
    set_bool b 61 t.locate_reqd;
    Le.set_u16 b 62 t.reason_code;
    Le.set_u8 b 64 t.ack_type;
    Le.set_u8 b 65 t.flow_indicator;
    Le.set_ztext b 66 ~len:8 t.user_data
  ;;

  let read b =
    { transact_time = Le.get_u64 b 4
    ; symbol_id = Le.get_u32 b 12
    ; mpid = Le.get_ztext b 16 ~len:4
    ; order_id = Le.get_u64 b 20
    ; ref_cl_ord_id = Le.get_u64 b 28
    ; orig_cl_ord_id = Le.get_u64 b 36
    ; price = price b 44
    ; order_qty = Le.get_u32 b 52
    ; leaves_qty = Le.get_u32 b 56
    ; side = Le.get_u8 b 60
    ; locate_reqd = bool b 61
    ; reason_code = Le.get_u16 b 62
    ; ack_type = Le.get_u8 b 64
    ; flow_indicator = Le.get_u8 b 65
    ; user_data = Le.get_ztext b 66 ~len:8
    }
  ;;
end

module Execution_report = struct
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

  let msg_type = 0x0290
  let length = 84

  let write t b =
    Le.set_u64 b 4 t.transact_time;
    Le.set_u32 b 12 t.symbol_id;
    Le.set_ztext b 16 ~len:4 t.mpid;
    Le.set_u64 b 20 t.order_id;
    Le.set_u64 b 28 t.cl_ord_id;
    Le.set_u64 b 36 t.deal_id;
    set_price b 44 t.last_px;
    Le.set_u32 b 52 t.leaves_qty;
    Le.set_u32 b 56 t.cum_qty;
    Le.set_u32 b 60 t.last_qty;
    Le.set_ztext b 64 ~len:4 t.liquidity_indicator;
    Le.set_u8 b 68 t.execution_details;
    set_bool b 72 t.locate_reqd;
    Le.set_u8 b 73 t.participant_type;
    Le.set_u16 b 74 t.reason_code;
    Le.set_ztext b 76 ~len:8 t.user_data
  ;;

  let read b =
    { transact_time = Le.get_u64 b 4
    ; symbol_id = Le.get_u32 b 12
    ; mpid = Le.get_ztext b 16 ~len:4
    ; order_id = Le.get_u64 b 20
    ; cl_ord_id = Le.get_u64 b 28
    ; deal_id = Le.get_u64 b 36
    ; last_px = price b 44
    ; leaves_qty = Le.get_u32 b 52
    ; cum_qty = Le.get_u32 b 56
    ; last_qty = Le.get_u32 b 60
    ; liquidity_indicator = Le.get_ztext b 64 ~len:4
    ; execution_details = Le.get_u8 b 68
    ; locate_reqd = bool b 72
    ; participant_type = Le.get_u8 b 73
    ; reason_code = Le.get_u16 b 74
    ; user_data = Le.get_ztext b 76 ~len:8
    }
  ;;
end

module Reject_type = struct
  let order = 1
  let modify = 2
  let cancel = 3
end

module Reject = struct
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

  let msg_type = 0x0263
  let length = 43

  let write t b =
    Le.set_u64 b 4 t.transact_time;
    Le.set_u32 b 12 t.symbol_id;
    Le.set_ztext b 16 ~len:4 t.mpid;
    Le.set_u64 b 20 t.cl_ord_id;
    Le.set_u16 b 28 t.reason_code;
    Le.set_u8 b 30 t.reject_type;
    Le.set_ztext b 31 ~len:8 t.user_data
  ;;

  let read b =
    { transact_time = Le.get_u64 b 4
    ; symbol_id = Le.get_u32 b 12
    ; mpid = Le.get_ztext b 16 ~len:4
    ; cl_ord_id = Le.get_u64 b 20
    ; reason_code = Le.get_u16 b 28
    ; reject_type = Le.get_u8 b 30
    ; user_data = Le.get_ztext b 31 ~len:8
    }
  ;;
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
  | Sequenced_filler
  | Unknown of
      { msg_type : int
      ; body : string
      }
[@@deriving sexp, compare, equal]

let sequenced_filler_type = 0x0282

let msg_type = function
  | Symbol_ref _ -> Symbol_ref.msg_type
  | Session_config_request _ -> Session_config_request.msg_type
  | Session_config_ack _ -> Session_config_ack.msg_type
  | New_order _ -> New_order.msg_type
  | Cancel_request _ -> Cancel_request.msg_type
  | Modify_request _ -> Modify_request.msg_type
  | Order_ack _ -> Order_ack.msg_type
  | Mod_cancel_ack _ -> Mod_cancel_ack.msg_type
  | Execution_report _ -> Execution_report.msg_type
  | Reject _ -> Reject.msg_type
  | Sequenced_filler -> sequenced_filler_type
  | Unknown { msg_type; _ } -> msg_type
;;

let length = function
  | Symbol_ref _ -> Symbol_ref.length
  | Session_config_request _ -> Session_config_request.length
  | Session_config_ack _ -> Session_config_ack.length
  | New_order m -> New_order.length m
  | Cancel_request _ -> Cancel_request.length
  | Modify_request _ -> Modify_request.length
  | Order_ack _ -> Order_ack.length
  | Mod_cancel_ack _ -> Mod_cancel_ack.length
  | Execution_report _ -> Execution_report.length
  | Reject _ -> Reject.length
  | Sequenced_filler -> 4
  | Unknown { body; _ } -> String.length body
;;

let write t buf ~pos =
  let length = length t in
  let b = Bigstring.sub_shared buf ~pos ~len:length in
  Bigstring.memset b ~pos:0 ~len:length '\000';
  (match t with
   | Symbol_ref m -> Symbol_ref.write m b
   | Session_config_request m -> Session_config_request.write m b
   | Session_config_ack m -> Session_config_ack.write m b
   | New_order m -> New_order.write m b
   | Cancel_request m -> Cancel_request.write m b
   | Modify_request m -> Modify_request.write m b
   | Order_ack m -> Order_ack.write m b
   | Mod_cancel_ack m -> Mod_cancel_ack.write m b
   | Execution_report m -> Execution_report.write m b
   | Reject m -> Reject.write m b
   | Sequenced_filler -> ()
   | Unknown { body; _ } ->
     Bigstring.From_string.blit ~src:body ~src_pos:0 ~dst:b ~dst_pos:0 ~len:length);
  header b ~msg_type:(msg_type t) ~length
;;

let encode t =
  let b = Bigstring.create (length t) in
  write t b ~pos:0;
  b
;;

let read buf ~pos ~len =
  if len < 4
  then Or_error.error_s [%message "Pillar message shorter than its header" (len : int)]
  else (
    let msg_type = Le.get_u16 buf pos
    and length = Le.get_u16 buf (pos + 2) in
    if length < 4 || length > len
    then
      Or_error.error_s
        [%message "bad Pillar message length" (msg_type : int) (length : int) (len : int)]
    else (
      let b = Bigstring.sub_shared buf ~pos ~len:length in
      let fixed n f =
        if length < n
        then
          Or_error.error_s
            [%message
              "Pillar message too short" (msg_type : int) (length : int) ~min:(n : int)]
        else Ok (f b)
      in
      match msg_type with
      | 0x0232 -> fixed Symbol_ref.length (fun b -> Symbol_ref (Symbol_ref.read b))
      | 0x0220 ->
        fixed Session_config_request.length (fun b ->
          Session_config_request (Session_config_request.read b))
      | 0x0221 ->
        fixed Session_config_ack.length (fun b ->
          Session_config_ack (Session_config_ack.read b))
      | 0x0240 -> fixed New_order.base_length (fun b -> New_order (New_order.read b))
      | 0x0280 ->
        fixed Cancel_request.length (fun b -> Cancel_request (Cancel_request.read b))
      | 0x0270 ->
        fixed Modify_request.length (fun b -> Modify_request (Modify_request.read b))
      | 0x0260 -> fixed Order_ack.length (fun b -> Order_ack (Order_ack.read b))
      | 0x0271 ->
        fixed Mod_cancel_ack.length (fun b -> Mod_cancel_ack (Mod_cancel_ack.read b))
      | 0x0290 ->
        fixed Execution_report.length (fun b ->
          Execution_report (Execution_report.read b))
      | 0x0263 -> fixed Reject.length (fun b -> Reject (Reject.read b))
      | 0x0282 -> Ok Sequenced_filler
      | msg_type -> Ok (Unknown { msg_type; body = Bigstring.to_string b })))
;;

let decode b = read b ~pos:0 ~len:(Bigstring.length b)
