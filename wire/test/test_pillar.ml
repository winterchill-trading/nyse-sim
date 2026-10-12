open! Core
open Nyse_sim_wire
module Price = Exchange_core.Price

(* Prints the bytes 16 to a line with their offset. *)
let dump b =
  let n = Bigstring.length b in
  let rec go pos =
    if pos < n
    then (
      let len = Int.min 16 (n - pos) in
      printf "%3d: %s\n" pos (Le.hex (Bigstring.sub b ~pos ~len));
      go (pos + 16))
  in
  printf "len %d\n" n;
  go 0
;;

let round_trip t =
  let b = Pillar.encode t in
  assert (Pillar.length t = Bigstring.length b);
  [%test_result: Pillar.t] (Or_error.ok_exn (Pillar.decode b)) ~expect:t
;;

let gt = Stream_id.create Gt ~user:7
let tg = Stream_id.create Tg ~user:7

let%expect_test "Login: 76 bytes, text fields space padded" =
  let t =
    Pillar.Login { username = "TRADER1"; password = "pw"; mic = "XNYS"; version = "1.1" }
  in
  dump (Pillar.encode t);
  [%expect
    {|
    len 76
      0: 01 02 4c 00 54 52 41 44 45 52 31 20 20 20 20 20
     16: 20 20 20 20 70 77 20 20 20 20 20 20 20 20 20 20
     32: 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20
     48: 20 20 20 20 58 4e 59 53 31 2e 31 20 20 20 20 20
     64: 20 20 20 20 20 20 20 20 20 20 20 20
    |}];
  round_trip t
;;

let%expect_test "StreamId and Open with the no-end sequence" =
  let t =
    Pillar.Open
      { stream_id = gt
      ; start_seq = 1
      ; end_seq = -1
      ; access = Pillar.Access.read
      ; mode = 0
      }
  in
  dump (Pillar.encode t);
  [%expect
    {|
    len 30
      0: 05 02 1e 00 01 00 00 01 00 07 00 0d 01 00 00 00
     16: 00 00 00 00 ff ff ff ff ff ff ff ff 01 00
    |}];
  round_trip t
;;

let new_order ?max_floor () : Pillar_app.New_order.t =
  { symbol_id = 1
  ; mpid = "WCHL"
  ; mmid = 0
  ; mp_sub_id = '\000'
  ; cl_ord_id = 42
  ; orig_cl_ord_id = 0
  ; instructions = Order_instructions.limit_day ~side:Order_instructions.Side.buy
  ; price = Price.of_string "1.23"
  ; order_qty = 100
  ; min_qty = 0
  ; user_data = "abc"
  ; max_floor
  }
;;

let%expect_test "New Order Single inside a SeqMsg: Price scale 8 at offset 41" =
  let payload = Pillar_app.New_order (new_order ()) in
  dump (Pillar_app.encode payload);
  [%expect
    {|
    len 65
      0: 40 02 41 00 01 00 00 00 57 43 48 4c 00 00 00 00
     16: 00 2a 00 00 00 00 00 00 00 00 00 00 00 00 00 00
     32: 00 00 00 40 a0 00 10 00 12 c0 d4 54 07 00 00 00
     48: 00 64 00 00 00 00 00 00 00 61 62 63 00 00 00 00
     64: 00
    |}];
  (* 123000000 = 0x0754d4c0 *)
  let t = Pillar.Seq_msg { stream_id = tg; seq = 1; timestamp = 5; payload } in
  printf "%d\n" (Pillar.length t);
  [%expect {| 97 |}];
  round_trip t
;;

let%expect_test "OptionalOrderAddOn carries MaxFloor and extends the length" =
  let payload = Pillar_app.New_order (new_order ~max_floor:200 ()) in
  let b = Pillar_app.encode payload in
  printf "%d %s\n" (Le.get_u16 b 2) (Le.hex (Bigstring.sub b ~pos:65 ~len:13));
  [%expect {| 106 41 02 29 00 00 00 00 00 00 c8 00 00 00 |}];
  round_trip (Seq_msg { stream_id = tg; seq = 2; timestamp = 0; payload })
;;

let%expect_test "gateway messages round trip at their fixed lengths" =
  let msgs : Pillar_app.t list =
    [ Symbol_ref
        { transact_time = 1
        ; symbol_id = 3
        ; symbol = "BRK B"
        ; listed_mic = "XNYS"
        ; round_lot = 100
        ; adv_risk_range = 0
        ; mpv_class_id = 1
        ; test_symbol = false
        }
    ; Session_config_request
        { username = "TRADER1"
        ; cancel_on_disconnect = true
        ; throttle_preference = 0
        ; self_trade_prevention = 2
        ; order_priority_update = 0
        ; bold_designation = 0
        }
    ; Session_config_ack
        { transact_time = 9
        ; user_session_type = 1
        ; user_session_status = 0
        ; username = "TRADER1"
        ; mic = "XNYS"
        ; cancel_on_disconnect = true
        ; throttle_preference = 0
        ; self_trade_prevention = 2
        ; order_priority_update = 0
        ; bold_designation = 0
        }
    ; Cancel_request { symbol_id = 1; mpid = "WCHL"; cl_ord_id = 43; orig_cl_ord_id = 42 }
    ; Modify_request
        { symbol_id = 1
        ; mpid = "WCHL"
        ; cl_ord_id = 44
        ; orig_cl_ord_id = 42
        ; order_qty = 50
        ; side = 1
        ; locate_reqd = false
        }
    ; Order_ack
        { transact_time = 10
        ; symbol_id = 1
        ; mpid = "WCHL"
        ; mmid = 0
        ; mp_sub_id = ' '
        ; cl_ord_id = 42
        ; orig_cl_ord_id = 0
        ; instructions = Order_instructions.limit_day ~side:2
        ; price = Price.of_string "189.5"
        ; order_qty = 100
        ; min_qty = 0
        ; order_id = 1_000_001
        ; leaves_qty = 100
        ; working_price = Price.of_string "189.5"
        ; working_away = false
        ; pre_liquidity_indicator = ""
        ; reason_code = 0
        ; ack_type = Pillar_app.Ack_type.new_order
        ; flow_indicator = 0
        ; user_data = "u1"
        }
    ; Mod_cancel_ack
        { transact_time = 11
        ; symbol_id = 1
        ; mpid = "WCHL"
        ; order_id = 1_000_001
        ; ref_cl_ord_id = 43
        ; orig_cl_ord_id = 42
        ; price = Price.of_string "189.5"
        ; order_qty = 100
        ; leaves_qty = 0
        ; side = 2
        ; locate_reqd = false
        ; reason_code = 0
        ; ack_type = Pillar_app.Ack_type.canceled
        ; flow_indicator = 0
        ; user_data = "u1"
        }
    ; Execution_report
        { transact_time = 12
        ; symbol_id = 1
        ; mpid = "WCHL"
        ; order_id = 1_000_001
        ; cl_ord_id = 42
        ; deal_id = 77
        ; last_px = Price.of_string "189.49"
        ; leaves_qty = 60
        ; cum_qty = 40
        ; last_qty = 40
        ; liquidity_indicator = "A"
        ; execution_details = 0
        ; locate_reqd = false
        ; participant_type = 0
        ; reason_code = 0
        ; user_data = "u1"
        }
    ; Reject
        { transact_time = 13
        ; symbol_id = 1
        ; mpid = "WCHL"
        ; cl_ord_id = 45
        ; reason_code = 7
        ; reject_type = Pillar_app.Reject_type.order
        ; user_data = ""
        }
    ; Sequenced_filler
    ]
  in
  List.iter msgs ~f:(fun payload ->
    let b = Pillar_app.encode payload in
    printf "0x%04x %d\n" (Le.get_u16 b 0) (Le.get_u16 b 2);
    round_trip (Seq_msg { stream_id = gt; seq = 3; timestamp = 4; payload }));
  [%expect
    {|
    0x0232 56
    0x0220 74
    0x0221 98
    0x0280 28
    0x0270 34
    0x0260 102
    0x0271 74
    0x0290 84
    0x0263 43
    0x0282 4
    |}]
;;

let%expect_test "execution report field offsets" =
  let b =
    Pillar_app.encode
      (Execution_report
         { transact_time = 0
         ; symbol_id = 0
         ; mpid = ""
         ; order_id = 0x11
         ; cl_ord_id = 0x22
         ; deal_id = 0x33
         ; last_px = Price.of_units 0x44
         ; leaves_qty = 0x55
         ; cum_qty = 0x66
         ; last_qty = 0x77
         ; liquidity_indicator = "R"
         ; execution_details = 0
         ; locate_reqd = false
         ; participant_type = 0
         ; reason_code = 0x99
         ; user_data = ""
         })
  in
  List.iter [ 20; 28; 36; 44; 52; 56; 60; 64; 74 ] ~f:(fun off ->
    printf "%d:%02x " off (Le.get_u8 b off));
  [%expect {| 20:11 28:22 36:33 44:44 52:55 56:66 60:77 64:52 74:99 |}]
;;

let%expect_test "session messages" =
  List.iter
    [ Pillar.Login_response { username = "TRADER1"; status = Pillar.Status.ok }
    ; Stream_avail { stream_id = gt; next_seq = 12; access = 1 }
    ; Heartbeat
    ; Open_response { stream_id = gt; status = 0; access = 1 }
    ; Close { stream_id = gt }
    ; Close_response { stream_id = gt; status = 0 }
    ]
    ~f:(fun t ->
      round_trip t;
      print_endline (Pillar.to_string_hum t));
  [%expect
    {|
    LoginResponse user=TRADER1 status=0 (ok)
    StreamAvail GT/7 next_seq=12 access=1
    Heartbeat
    OpenResponse GT/7 status=0 (ok) access=1
    Close GT/7
    CloseResponse GT/7 status=0 (ok)
    |}]
;;

let%expect_test "truncated and unknown messages" =
  let b = Pillar.encode Heartbeat in
  print_s [%sexp (Pillar.read b ~pos:0 ~len:3 : Pillar.t Or_error.t)];
  let short = Bigstring.of_string "\x05\x02\x08\x00\x00\x00\x00\x00" in
  print_s [%sexp (Pillar.decode short : Pillar.t Or_error.t)];
  let unknown = Bigstring.of_string "\x99\x09\x05\x00\x01" in
  print_s [%sexp (Pillar.decode unknown : Pillar.t Or_error.t)];
  [%expect
    {|
    (Error ("short Pillar frame" (len 3)))
    (Error ("Pillar message too short" (msg_type 517) (length 8) (min 30)))
    (Ok (Unknown (msg_type 2457) (body "\153\t\005\000\001")))
    |}]
;;
