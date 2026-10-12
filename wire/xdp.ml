open! Core
module Price = Exchange_core.Price

module Delivery_flag = struct
  let heartbeat = 1
  let failover = 10
  let original = 11
  let sequence_reset = 12
  let refresh_start = 17
  let refresh_part = 18
  let refresh_single = 19
  let refresh_end = 20
end

let pow10 n = Int.pow 10 n
let price_of_raw ~scale raw = Price.of_units (raw * pow10 (8 - scale))
let raw_of_price ~scale p = Price.to_units p / pow10 (8 - scale)

let scale_for_price p =
  if Price.(p >= of_int 100_000) then 3 else if Price.(p >= of_int 500) then 4 else 6
;;

module Common = struct
  type t =
    { source_time_ns : int
    ; symbol_index : int
    ; symbol_seq_num : int
    }
  [@@deriving sexp, compare, equal]

  let write b t =
    Le.set_u32 b 4 t.source_time_ns;
    Le.set_u32 b 8 t.symbol_index;
    Le.set_u32 b 12 t.symbol_seq_num
  ;;

  let read b =
    { source_time_ns = Le.get_u32 b 4
    ; symbol_index = Le.get_u32 b 8
    ; symbol_seq_num = Le.get_u32 b 12
    }
  ;;
end

module Msg = struct
  type t =
    | Sequence_number_reset of
        { source_time : int
        ; source_time_ns : int
        ; product_id : int
        ; channel_id : int
        }
    | Source_time_reference of
        { id : int
        ; symbol_seq_num : int
        ; source_time : int
        }
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
        ; mpv : int
        ; unit_of_trade : int
        }
    | Refresh_request of
        { symbol_index : int
        ; source_id : string
        ; product_id : int
        ; channel_id : int
        }
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
        }
    | Add_order of
        { common : Common.t
        ; order_id : int
        ; price : int
        ; volume : int
        ; side : char
        ; firm_id : string
        }
    | Modify_order of
        { common : Common.t
        ; order_id : int
        ; price : int
        ; volume : int
        ; position_change : int
        ; side : char
        }
    | Delete_order of
        { common : Common.t
        ; order_id : int
        }
    | Order_execution of
        { common : Common.t
        ; order_id : int
        ; trade_id : int
        ; price : int
        ; volume : int
        ; printable_flag : int
        ; trade_cond : string
        }
    | Replace_order of
        { common : Common.t
        ; order_id : int
        ; new_order_id : int
        ; price : int
        ; volume : int
        ; side : char
        }
    | Imbalance of
        { source_time : int
        ; source_time_ns : int
        ; symbol_index : int
        ; symbol_seq_num : int
        ; reference_price : int
        ; paired_qty : int
        ; total_imbalance_qty : int
        ; market_imbalance_qty : int
        ; auction_time : int
        ; auction_type : char
        ; imbalance_side : char
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
        }
    | Add_order_refresh of
        { common : Common.t
        ; order_id : int
        ; price : int
        ; volume : int
        ; side : char
        ; firm_id : string
        }
    | Non_displayed_trade of
        { common : Common.t
        ; trade_id : int
        ; price : int
        ; volume : int
        ; printable_flag : int
        ; trade_cond : string
        }
    | Unknown of
        { msg_type : int
        ; body : string
        }
  [@@deriving sexp, compare, equal]

  let msg_type = function
    | Sequence_number_reset _ -> 1
    | Source_time_reference _ -> 2
    | Symbol_index_mapping _ -> 3
    | Refresh_request _ -> 15
    | Security_status _ -> 34
    | Add_order _ -> 100
    | Modify_order _ -> 101
    | Delete_order _ -> 102
    | Order_execution _ -> 103
    | Replace_order _ -> 104
    | Imbalance _ -> 105
    | Add_order_refresh _ -> 106
    | Non_displayed_trade _ -> 110
    | Unknown { msg_type; _ } -> msg_type
  ;;

  let fixed_size = function
    | 1 -> Some 14
    | 2 -> Some 16
    | 3 -> Some 44
    | 15 -> Some 20
    | 34 -> Some 46
    | 100 | 106 -> Some 39
    | 101 -> Some 35
    | 102 -> Some 25
    | 103 | 104 -> Some 42
    | 105 -> Some 73
    | 110 -> Some 33
    | _ -> None
  ;;

  let size = function
    | Unknown { body; _ } -> String.length body
    | t -> Option.value_exn (fixed_size (msg_type t))
  ;;

  let symbol_index = function
    | Sequence_number_reset _ | Source_time_reference _ | Unknown _ -> None
    | Symbol_index_mapping { symbol_index; _ }
    | Refresh_request { symbol_index; _ }
    | Security_status { symbol_index; _ }
    | Imbalance { symbol_index; _ } -> Some symbol_index
    | Add_order { common; _ }
    | Modify_order { common; _ }
    | Delete_order { common; _ }
    | Order_execution { common; _ }
    | Replace_order { common; _ }
    | Add_order_refresh { common; _ }
    | Non_displayed_trade { common; _ } -> Some common.symbol_index
  ;;

  let write t buf ~pos =
    let size = size t in
    let b = Bigstring.sub_shared buf ~pos ~len:size in
    Bigstring.memset b ~pos:0 ~len:size '\000';
    let add common ~order_id ~price ~volume ~side ~firm_id =
      Common.write b common;
      Le.set_u64 b 16 order_id;
      Le.set_u32 b 24 price;
      Le.set_u32 b 28 volume;
      Le.set_char b 32 side;
      Le.set_ztext b 33 ~len:5 firm_id
    in
    (match t with
     | Sequence_number_reset { source_time; source_time_ns; product_id; channel_id } ->
       Le.set_u32 b 4 source_time;
       Le.set_u32 b 8 source_time_ns;
       Le.set_u8 b 12 product_id;
       Le.set_u8 b 13 channel_id
     | Source_time_reference { id; symbol_seq_num; source_time } ->
       Le.set_u32 b 4 id;
       Le.set_u32 b 8 symbol_seq_num;
       Le.set_u32 b 12 source_time
     | Symbol_index_mapping m ->
       Le.set_u32 b 4 m.symbol_index;
       Le.set_ztext b 8 ~len:11 m.symbol;
       Le.set_u16 b 20 m.market_id;
       Le.set_u8 b 22 m.system_id;
       Le.set_char b 23 m.exchange_code;
       Le.set_u8 b 24 m.price_scale_code;
       Le.set_char b 25 m.security_type;
       Le.set_u16 b 26 m.lot_size;
       Le.set_u32 b 28 m.prev_close_price;
       Le.set_u32 b 32 m.prev_close_volume;
       Le.set_u8 b 36 m.price_resolution;
       Le.set_char b 37 m.round_lot;
       Le.set_u16 b 38 m.mpv;
       Le.set_u16 b 40 m.unit_of_trade
     | Refresh_request { symbol_index; source_id; product_id; channel_id } ->
       Le.set_u32 b 4 symbol_index;
       Le.set_ztext b 8 ~len:10 source_id;
       Le.set_u8 b 18 product_id;
       Le.set_u8 b 19 channel_id
     | Security_status m ->
       Le.set_u32 b 4 m.source_time;
       Le.set_u32 b 8 m.source_time_ns;
       Le.set_u32 b 12 m.symbol_index;
       Le.set_u32 b 16 m.symbol_seq_num;
       Le.set_char b 20 m.security_status;
       Le.set_char b 21 m.halt_condition;
       Le.set_u32 b 26 m.price_1;
       Le.set_u32 b 30 m.price_2;
       Le.set_char b 34 m.ssr_triggering_exchange;
       Le.set_u32 b 35 m.ssr_triggering_volume;
       Le.set_u32 b 39 m.time;
       Le.set_char b 43 m.ssr_state;
       Le.set_char b 44 m.market_state;
       Le.set_char b 45 m.session_state
     | Add_order { common; order_id; price; volume; side; firm_id }
     | Add_order_refresh { common; order_id; price; volume; side; firm_id } ->
       add common ~order_id ~price ~volume ~side ~firm_id
     | Modify_order { common; order_id; price; volume; position_change; side } ->
       Common.write b common;
       Le.set_u64 b 16 order_id;
       Le.set_u32 b 24 price;
       Le.set_u32 b 28 volume;
       Le.set_u8 b 32 position_change;
       Le.set_char b 33 side
     | Delete_order { common; order_id } ->
       Common.write b common;
       Le.set_u64 b 16 order_id
     | Order_execution
         { common; order_id; trade_id; price; volume; printable_flag; trade_cond } ->
       Common.write b common;
       Le.set_u64 b 16 order_id;
       Le.set_u32 b 24 trade_id;
       Le.set_u32 b 28 price;
       Le.set_u32 b 32 volume;
       Le.set_u8 b 36 printable_flag;
       Le.set_text b 38 ~len:4 trade_cond
     | Replace_order { common; order_id; new_order_id; price; volume; side } ->
       Common.write b common;
       Le.set_u64 b 16 order_id;
       Le.set_u64 b 24 new_order_id;
       Le.set_u32 b 32 price;
       Le.set_u32 b 36 volume;
       Le.set_char b 40 side
     | Imbalance m ->
       Le.set_u32 b 4 m.source_time;
       Le.set_u32 b 8 m.source_time_ns;
       Le.set_u32 b 12 m.symbol_index;
       Le.set_u32 b 16 m.symbol_seq_num;
       Le.set_u32 b 20 m.reference_price;
       Le.set_u32 b 24 m.paired_qty;
       Le.set_u32 b 28 m.total_imbalance_qty;
       Le.set_u32 b 32 m.market_imbalance_qty;
       Le.set_u16 b 36 m.auction_time;
       Le.set_char b 38 m.auction_type;
       Le.set_char b 39 m.imbalance_side;
       Le.set_u32 b 40 m.continuous_book_clearing_price;
       Le.set_u32 b 44 m.auction_interest_clearing_price;
       Le.set_u32 b 48 m.ssr_filing_price;
       Le.set_u32 b 52 m.indicative_match_price;
       Le.set_u32 b 56 m.upper_collar;
       Le.set_u32 b 60 m.lower_collar;
       Le.set_u8 b 64 m.auction_status;
       Le.set_u8 b 65 m.freeze_status;
       Le.set_u8 b 66 m.num_extensions;
       Le.set_u32 b 67 m.unpaired_qty;
       Le.set_char b 71 m.unpaired_side;
       Le.set_char b 72 m.significant_imbalance
     | Non_displayed_trade { common; trade_id; price; volume; printable_flag; trade_cond }
       ->
       Common.write b common;
       Le.set_u32 b 16 trade_id;
       Le.set_u32 b 20 price;
       Le.set_u32 b 24 volume;
       Le.set_u8 b 28 printable_flag;
       Le.set_text b 29 ~len:4 trade_cond
     | Unknown { body; _ } ->
       Bigstring.From_string.blit ~src:body ~src_pos:0 ~dst:b ~dst_pos:0 ~len:size);
    Le.set_u16 b 0 size;
    Le.set_u16 b 2 (msg_type t)
  ;;

  let encode t =
    let b = Bigstring.create (size t) in
    write t b ~pos:0;
    b
  ;;

  let read buf ~pos ~len =
    if len < 4
    then Or_error.error_s [%message "short XDP message" (len : int)]
    else (
      let size = Le.get_u16 buf pos
      and msg_type = Le.get_u16 buf (pos + 2) in
      if size < 4 || size > len
      then Or_error.error_s [%message "bad XDP MsgSize" (size : int) (len : int)]
      else (
        let b = Bigstring.sub_shared buf ~pos ~len:size in
        match fixed_size msg_type with
        | Some min when size < min ->
          Or_error.error_s
            [%message "XDP message too short" (msg_type : int) (size : int) (min : int)]
        | None -> Ok (Unknown { msg_type; body = Bigstring.to_string b })
        | Some _ ->
          let u8 = Le.get_u8 b
          and u16 = Le.get_u16 b
          and u32 = Le.get_u32 b
          and c = Le.get_char b in
          let common () = Common.read b in
          Ok
            (match msg_type with
             | 1 ->
               Sequence_number_reset
                 { source_time = u32 4
                 ; source_time_ns = u32 8
                 ; product_id = u8 12
                 ; channel_id = u8 13
                 }
             | 2 ->
               Source_time_reference
                 { id = u32 4; symbol_seq_num = u32 8; source_time = u32 12 }
             | 3 ->
               Symbol_index_mapping
                 { symbol_index = u32 4
                 ; symbol = Le.get_ztext b 8 ~len:11
                 ; market_id = u16 20
                 ; system_id = u8 22
                 ; exchange_code = c 23
                 ; price_scale_code = u8 24
                 ; security_type = c 25
                 ; lot_size = u16 26
                 ; prev_close_price = u32 28
                 ; prev_close_volume = u32 32
                 ; price_resolution = u8 36
                 ; round_lot = c 37
                 ; mpv = u16 38
                 ; unit_of_trade = u16 40
                 }
             | 15 ->
               Refresh_request
                 { symbol_index = u32 4
                 ; source_id = Le.get_ztext b 8 ~len:10
                 ; product_id = u8 18
                 ; channel_id = u8 19
                 }
             | 34 ->
               Security_status
                 { source_time = u32 4
                 ; source_time_ns = u32 8
                 ; symbol_index = u32 12
                 ; symbol_seq_num = u32 16
                 ; security_status = c 20
                 ; halt_condition = c 21
                 ; price_1 = u32 26
                 ; price_2 = u32 30
                 ; ssr_triggering_exchange = c 34
                 ; ssr_triggering_volume = u32 35
                 ; time = u32 39
                 ; ssr_state = c 43
                 ; market_state = c 44
                 ; session_state = c 45
                 }
             | 100 ->
               Add_order
                 { common = common ()
                 ; order_id = Le.get_u64 b 16
                 ; price = u32 24
                 ; volume = u32 28
                 ; side = c 32
                 ; firm_id = Le.get_ztext b 33 ~len:5
                 }
             | 106 ->
               Add_order_refresh
                 { common = common ()
                 ; order_id = Le.get_u64 b 16
                 ; price = u32 24
                 ; volume = u32 28
                 ; side = c 32
                 ; firm_id = Le.get_ztext b 33 ~len:5
                 }
             | 101 ->
               Modify_order
                 { common = common ()
                 ; order_id = Le.get_u64 b 16
                 ; price = u32 24
                 ; volume = u32 28
                 ; position_change = u8 32
                 ; side = c 33
                 }
             | 102 -> Delete_order { common = common (); order_id = Le.get_u64 b 16 }
             | 103 ->
               Order_execution
                 { common = common ()
                 ; order_id = Le.get_u64 b 16
                 ; trade_id = u32 24
                 ; price = u32 28
                 ; volume = u32 32
                 ; printable_flag = u8 36
                 ; trade_cond = Le.get_text b 38 ~len:4
                 }
             | 104 ->
               Replace_order
                 { common = common ()
                 ; order_id = Le.get_u64 b 16
                 ; new_order_id = Le.get_u64 b 24
                 ; price = u32 32
                 ; volume = u32 36
                 ; side = c 40
                 }
             | 105 ->
               Imbalance
                 { source_time = u32 4
                 ; source_time_ns = u32 8
                 ; symbol_index = u32 12
                 ; symbol_seq_num = u32 16
                 ; reference_price = u32 20
                 ; paired_qty = u32 24
                 ; total_imbalance_qty = u32 28
                 ; market_imbalance_qty = u32 32
                 ; auction_time = u16 36
                 ; auction_type = c 38
                 ; imbalance_side = c 39
                 ; continuous_book_clearing_price = u32 40
                 ; auction_interest_clearing_price = u32 44
                 ; ssr_filing_price = u32 48
                 ; indicative_match_price = u32 52
                 ; upper_collar = u32 56
                 ; lower_collar = u32 60
                 ; auction_status = u8 64
                 ; freeze_status = u8 65
                 ; num_extensions = u8 66
                 ; unpaired_qty = u32 67
                 ; unpaired_side = c 71
                 ; significant_imbalance = c 72
                 }
             | 110 ->
               Non_displayed_trade
                 { common = common ()
                 ; trade_id = u32 16
                 ; price = u32 20
                 ; volume = u32 24
                 ; printable_flag = u8 28
                 ; trade_cond = Le.get_text b 29 ~len:4
                 }
             | _ -> assert false)))
  ;;

  let to_string_hum ?(symbols = fun _ -> None) t =
    let name i =
      match symbols i with
      | Some (s, _) -> s
      | None -> sprintf "#%d" i
    in
    let px i raw =
      match symbols i with
      | Some (_, scale) -> Price.to_string (price_of_raw ~scale raw)
      | None -> sprintf "%d/10^?" raw
    in
    let side c = if Char.equal c 'B' then "buy" else "sell" in
    let cs (common : Common.t) = name common.symbol_index, common.symbol_seq_num in
    match t with
    | Sequence_number_reset { product_id; channel_id; _ } ->
      sprintf "SequenceNumberReset product=%d channel=%d" product_id channel_id
    | Source_time_reference { source_time; _ } ->
      sprintf "SourceTimeReference %d" source_time
    | Symbol_index_mapping m ->
      sprintf
        "SymbolIndexMapping %d %s scale=%d lot=%d prev_close=%s"
        m.symbol_index
        m.symbol
        m.price_scale_code
        m.lot_size
        (Price.to_string (price_of_raw ~scale:m.price_scale_code m.prev_close_price))
    | Refresh_request { symbol_index; _ } -> sprintf "RefreshRequest %d" symbol_index
    | Security_status m ->
      sprintf
        "SecurityStatus %s status=%c halt=%c ssr=%c"
        (name m.symbol_index)
        m.security_status
        m.halt_condition
        m.ssr_state
    | Add_order { common; order_id; price; volume; side = s; _ } ->
      let n, seq = cs common in
      sprintf
        "AddOrder %s/%d id=%d %s %d @ %s"
        n
        seq
        order_id
        (side s)
        volume
        (px common.symbol_index price)
    | Add_order_refresh { common; order_id; price; volume; side = s; _ } ->
      let n, seq = cs common in
      sprintf
        "AddOrderRefresh %s/%d id=%d %s %d @ %s"
        n
        seq
        order_id
        (side s)
        volume
        (px common.symbol_index price)
    | Modify_order { common; order_id; price; volume; position_change; _ } ->
      let n, seq = cs common in
      sprintf
        "ModifyOrder %s/%d id=%d %d @ %s%s"
        n
        seq
        order_id
        volume
        (px common.symbol_index price)
        (if position_change = 1 then " lost-priority" else "")
    | Delete_order { common; order_id } ->
      let n, seq = cs common in
      sprintf "DeleteOrder %s/%d id=%d" n seq order_id
    | Order_execution { common; order_id; trade_id; price; volume; _ } ->
      let n, seq = cs common in
      sprintf
        "OrderExecution %s/%d id=%d trade=%d %d @ %s"
        n
        seq
        order_id
        trade_id
        volume
        (px common.symbol_index price)
    | Replace_order { common; order_id; new_order_id; price; volume; side = s } ->
      let n, seq = cs common in
      sprintf
        "ReplaceOrder %s/%d id=%d new=%d %s %d @ %s"
        n
        seq
        order_id
        new_order_id
        (side s)
        volume
        (px common.symbol_index price)
    | Imbalance m ->
      sprintf
        "Imbalance %s type=%c ref=%s paired=%d imbalance=%d side=%c indicative=%s"
        (name m.symbol_index)
        m.auction_type
        (px m.symbol_index m.reference_price)
        m.paired_qty
        m.total_imbalance_qty
        m.imbalance_side
        (px m.symbol_index m.indicative_match_price)
    | Non_displayed_trade { common; trade_id; price; volume; _ } ->
      let n, seq = cs common in
      sprintf
        "NonDisplayedTrade %s/%d trade=%d %d @ %s"
        n
        seq
        trade_id
        volume
        (px common.symbol_index price)
    | Unknown { msg_type; body } ->
      sprintf "Unknown type=%d size=%d" msg_type (String.length body)
  ;;
end

module Packet = struct
  type t =
    { delivery_flag : int
    ; seq_num : int
    ; send_time : int
    ; send_time_ns : int
    ; msgs : Msg.t list
    }
  [@@deriving sexp, compare, equal]

  let header_length = 16
  let max_size = 1400
  let size t = header_length + List.sum (module Int) t.msgs ~f:Msg.size

  let encode t =
    let size = size t in
    let b = Bigstring.create size in
    Le.set_u16 b 0 size;
    Le.set_u8 b 2 t.delivery_flag;
    Le.set_u8 b 3 (List.length t.msgs);
    Le.set_u32 b 4 t.seq_num;
    Le.set_u32 b 8 t.send_time;
    Le.set_u32 b 12 t.send_time_ns;
    ignore
      (List.fold t.msgs ~init:header_length ~f:(fun pos m ->
         Msg.write m b ~pos;
         pos + Msg.size m)
       : int);
    b
  ;;

  let frame_length b ~pos = Le.get_u16 b pos

  let read buf ~pos ~len =
    let open Or_error.Let_syntax in
    let%bind () =
      if len >= header_length
      then Ok ()
      else Or_error.error_s [%message "short XDP packet" (len : int)]
    in
    let size = Le.get_u16 buf pos in
    let%bind () =
      if size >= header_length && size <= len
      then Ok ()
      else Or_error.error_s [%message "bad XDP PktSize" (size : int) (len : int)]
    in
    let n = Le.get_u8 buf (pos + 3) in
    let rec msgs i at acc =
      if i = n
      then Ok (List.rev acc)
      else (
        let%bind m = Msg.read buf ~pos:at ~len:(pos + size - at) in
        msgs (i + 1) (at + Le.get_u16 buf at) (m :: acc))
    in
    let%map msgs = msgs 0 (pos + header_length) [] in
    { delivery_flag = Le.get_u8 buf (pos + 2)
    ; seq_num = Le.get_u32 buf (pos + 4)
    ; send_time = Le.get_u32 buf (pos + 8)
    ; send_time_ns = Le.get_u32 buf (pos + 12)
    ; msgs
    }
  ;;

  let decode b = read b ~pos:0 ~len:(Bigstring.length b)
end
