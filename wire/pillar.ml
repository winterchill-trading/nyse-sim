open! Core

module Status = struct
  let ok = 0
  let not_logged_in = 18
  let invalid_login = 24
  let already_logged_in = 27
  let heartbeat_timeout = 28
  let login_timed_out = 29
  let invalid_message = 33
  let no_stream_permission = 54
  let invalid_protocol_version = 81
  let out_of_sequence = 82
  let invalid_stream = 84
  let stream_not_open = 85

  let names =
    [ 0, "ok"
    ; 18, "not logged in"
    ; 24, "invalid login"
    ; 27, "already logged in"
    ; 28, "heartbeat timeout"
    ; 29, "login timed out"
    ; 33, "invalid message"
    ; 54, "no stream permission"
    ; 81, "invalid protocol version"
    ; 82, "out of sequence"
    ; 84, "invalid stream"
    ; 85, "stream not open"
    ; 86, "invalid timestamp"
    ; 89, "denial of service"
    ; 90, "write permission revoked"
    ]
  ;;

  let to_string n =
    List.Assoc.find names n ~equal:Int.equal
    |> Option.value ~default:(sprintf "status %d" n)
  ;;
end

module Access = struct
  let read = 1
  let write = 2
  let throttle_reject = 4
end

let protocol_version = "1.1"

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
      ; end_seq : int
      ; access : int
      ; mode : int
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
      ; timestamp : int
      ; payload : Pillar_app.t
      }
  | Unknown of
      { msg_type : int
      ; body : string
      }
[@@deriving sexp, compare, equal]

let msg_type = function
  | Login _ -> 0x0201
  | Login_response _ -> 0x0202
  | Stream_avail _ -> 0x0203
  | Heartbeat -> 0x0204
  | Open _ -> 0x0205
  | Open_response _ -> 0x0206
  | Close _ -> 0x0207
  | Close_response _ -> 0x0208
  | Seq_msg _ -> 0x0905
  | Unknown { msg_type; _ } -> msg_type
;;

let seq_msg_header = 32

let length = function
  | Login _ -> 76
  | Login_response _ -> 21
  | Stream_avail _ -> 21
  | Heartbeat -> 4
  | Open _ -> 30
  | Open_response _ -> 14
  | Close _ -> 12
  | Close_response _ -> 13
  | Seq_msg { payload; _ } -> seq_msg_header + Pillar_app.length payload
  | Unknown { body; _ } -> String.length body
;;

let encode t =
  let length = length t in
  let b = Bigstring.init length ~f:(fun _ -> '\000') in
  (match t with
   | Login { username; password; mic; version } ->
     Le.set_text b 4 ~len:16 username;
     Le.set_text b 20 ~len:32 password;
     Le.set_text b 52 ~len:4 mic;
     Le.set_text b 56 ~len:20 version
   | Login_response { username; status } ->
     Le.set_text b 4 ~len:16 username;
     Le.set_u8 b 20 status
   | Stream_avail { stream_id; next_seq; access } ->
     Stream_id.write b 4 stream_id;
     Le.set_u64 b 12 next_seq;
     Le.set_u8 b 20 access
   | Heartbeat -> ()
   | Open { stream_id; start_seq; end_seq; access; mode } ->
     Stream_id.write b 4 stream_id;
     Le.set_u64 b 12 start_seq;
     Le.set_u64 b 20 end_seq;
     Le.set_u8 b 28 access;
     Le.set_u8 b 29 mode
   | Open_response { stream_id; status; access } ->
     Stream_id.write b 4 stream_id;
     Le.set_u8 b 12 status;
     Le.set_u8 b 13 access
   | Close { stream_id } -> Stream_id.write b 4 stream_id
   | Close_response { stream_id; status } ->
     Stream_id.write b 4 stream_id;
     Le.set_u8 b 12 status
   | Seq_msg { stream_id; seq; timestamp; payload } ->
     Stream_id.write b 4 stream_id;
     Le.set_u64 b 12 seq;
     Le.set_u32 b 20 0;
     Le.set_u64 b 24 timestamp;
     Pillar_app.write payload b ~pos:seq_msg_header
   | Unknown { body; _ } ->
     Bigstring.From_string.blit ~src:body ~src_pos:0 ~dst:b ~dst_pos:0 ~len:length);
  Le.set_u16 b 0 (msg_type t);
  Le.set_u16 b 2 length;
  b
;;

let frame_length b ~pos = Le.get_u16 b (pos + 2)

let read buf ~pos ~len =
  let open Or_error.Let_syntax in
  let%bind () =
    if len >= 4
    then Ok ()
    else Or_error.error_s [%message "short Pillar frame" (len : int)]
  in
  let msg_type = Le.get_u16 buf pos
  and length = Le.get_u16 buf (pos + 2) in
  let%bind () =
    if length >= 4 && length <= len
    then Ok ()
    else Or_error.error_s [%message "bad Pillar frame length" (length : int) (len : int)]
  in
  let b = Bigstring.sub_shared buf ~pos ~len:length in
  let need n f =
    if length >= n
    then f ()
    else
      Or_error.error_s
        [%message
          "Pillar message too short" (msg_type : int) (length : int) ~min:(n : int)]
  in
  match msg_type with
  | 0x0201 ->
    need 76 (fun () ->
      Ok
        (Login
           { username = Le.get_text b 4 ~len:16
           ; password = Le.get_text b 20 ~len:32
           ; mic = Le.get_text b 52 ~len:4
           ; version = Le.get_text b 56 ~len:20
           }))
  | 0x0202 ->
    need 21 (fun () ->
      Ok (Login_response { username = Le.get_text b 4 ~len:16; status = Le.get_u8 b 20 }))
  | 0x0203 ->
    need 21 (fun () ->
      Ok
        (Stream_avail
           { stream_id = Stream_id.read b 4
           ; next_seq = Le.get_u64 b 12
           ; access = Le.get_u8 b 20
           }))
  | 0x0204 -> Ok Heartbeat
  | 0x0205 ->
    need 30 (fun () ->
      Ok
        (Open
           { stream_id = Stream_id.read b 4
           ; start_seq = Le.get_u64 b 12
           ; end_seq = Le.get_u64 b 20
           ; access = Le.get_u8 b 28
           ; mode = Le.get_u8 b 29
           }))
  | 0x0206 ->
    need 14 (fun () ->
      Ok
        (Open_response
           { stream_id = Stream_id.read b 4
           ; status = Le.get_u8 b 12
           ; access = Le.get_u8 b 13
           }))
  | 0x0207 -> need 12 (fun () -> Ok (Close { stream_id = Stream_id.read b 4 }))
  | 0x0208 ->
    need 13 (fun () ->
      Ok (Close_response { stream_id = Stream_id.read b 4; status = Le.get_u8 b 12 }))
  | 0x0905 ->
    need (seq_msg_header + 4) (fun () ->
      let%map payload =
        Pillar_app.read b ~pos:seq_msg_header ~len:(length - seq_msg_header)
      in
      Seq_msg
        { stream_id = Stream_id.read b 4
        ; seq = Le.get_u64 b 12
        ; timestamp = Le.get_u64 b 24
        ; payload
        })
  | msg_type -> Ok (Unknown { msg_type; body = Bigstring.to_string b })
;;

let decode b = read b ~pos:0 ~len:(Bigstring.length b)

let stream (s : Stream_id.t) =
  match s.kind with
  | Tg -> sprintf "TG/%d" s.user
  | Gt -> sprintf "GT/%d" s.user
  | Ref -> sprintf "REF/%d" s.user
  | Other n -> sprintf "%d/%d" n s.user
;;

let price p = Exchange_core.Price.to_string p

let app_to_string_hum (m : Pillar_app.t) =
  match m with
  | Symbol_ref r ->
    sprintf
      "SymbolRef id=%d %s mic=%s lot=%d%s"
      r.symbol_id
      r.symbol
      r.listed_mic
      r.round_lot
      (if r.test_symbol then " test" else "")
  | Session_config_request r ->
    sprintf
      "SessionConfig user=%s cod=%b stp=%d"
      r.username
      r.cancel_on_disconnect
      r.self_trade_prevention
  | Session_config_ack r ->
    sprintf
      "SessionConfigAck user=%s status=%d cod=%b"
      r.username
      r.user_session_status
      r.cancel_on_disconnect
  | New_order r ->
    sprintf
      "NewOrder sym=%d clordid=%d orig=%d side=%d type=%d tif=%d px=%s qty=%d%s"
      r.symbol_id
      r.cl_ord_id
      r.orig_cl_ord_id
      r.instructions.side
      r.instructions.ord_type
      r.instructions.time_in_force
      (price r.price)
      r.order_qty
      (Option.value_map r.max_floor ~default:"" ~f:(sprintf " maxfloor=%d"))
  | Cancel_request r ->
    sprintf "Cancel sym=%d clordid=%d orig=%d" r.symbol_id r.cl_ord_id r.orig_cl_ord_id
  | Modify_request r ->
    sprintf
      "Modify sym=%d clordid=%d orig=%d qty=%d"
      r.symbol_id
      r.cl_ord_id
      r.orig_cl_ord_id
      r.order_qty
  | Order_ack r ->
    sprintf
      "OrderAck type=%d sym=%d clordid=%d orderid=%d px=%s qty=%d leaves=%d reason=%d"
      r.ack_type
      r.symbol_id
      r.cl_ord_id
      r.order_id
      (price r.price)
      r.order_qty
      r.leaves_qty
      r.reason_code
  | Mod_cancel_ack r ->
    sprintf
      "ModCancelAck type=%d sym=%d orderid=%d ref=%d orig=%d qty=%d leaves=%d reason=%d"
      r.ack_type
      r.symbol_id
      r.order_id
      r.ref_cl_ord_id
      r.orig_cl_ord_id
      r.order_qty
      r.leaves_qty
      r.reason_code
  | Execution_report r ->
    sprintf
      "Exec sym=%d clordid=%d orderid=%d deal=%d px=%s qty=%d cum=%d leaves=%d liq=%s"
      r.symbol_id
      r.cl_ord_id
      r.order_id
      r.deal_id
      (price r.last_px)
      r.last_qty
      r.cum_qty
      r.leaves_qty
      r.liquidity_indicator
  | Reject r ->
    sprintf
      "Reject type=%d sym=%d clordid=%d reason=%d"
      r.reject_type
      r.symbol_id
      r.cl_ord_id
      r.reason_code
  | Sequenced_filler -> "SequencedFiller"
  | Unknown { msg_type; body } ->
    sprintf "Unknown type=0x%04x len=%d" msg_type (String.length body)
;;

let to_string_hum = function
  | Login { username; mic; version; _ } ->
    sprintf "Login user=%s mic=%s version=%s" username mic version
  | Login_response { username; status } ->
    sprintf
      "LoginResponse user=%s status=%d (%s)"
      username
      status
      (Status.to_string status)
  | Stream_avail { stream_id; next_seq; access } ->
    sprintf "StreamAvail %s next_seq=%d access=%d" (stream stream_id) next_seq access
  | Heartbeat -> "Heartbeat"
  | Open { stream_id; start_seq; end_seq; access; _ } ->
    sprintf
      "Open %s start=%d end=%d access=%d"
      (stream stream_id)
      start_seq
      end_seq
      access
  | Open_response { stream_id; status; access } ->
    sprintf
      "OpenResponse %s status=%d (%s) access=%d"
      (stream stream_id)
      status
      (Status.to_string status)
      access
  | Close { stream_id } -> sprintf "Close %s" (stream stream_id)
  | Close_response { stream_id; status } ->
    sprintf
      "CloseResponse %s status=%d (%s)"
      (stream stream_id)
      status
      (Status.to_string status)
  | Seq_msg { stream_id; seq; payload; _ } ->
    sprintf "%s #%d %s" (stream stream_id) seq (app_to_string_hum payload)
  | Unknown { msg_type; body } ->
    sprintf "Unknown type=0x%04x len=%d" msg_type (String.length body)
;;
