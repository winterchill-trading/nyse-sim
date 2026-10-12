open! Core

type t =
  { sub_id_indicator : int
  ; special_ord_type : int
  ; locate_reqd : int
  ; retail_indicator : int
  ; attributed_quote : int
  ; order_capacity : int
  ; interest_type : int
  ; trading_session : int
  ; time_in_force : int
  ; proactive_if_locked : int
  ; self_trade_type : int
  ; cancel_instead_of_reprice : int
  ; routing_inst : int
  ; extended_exec_inst : int
  ; exec_inst : int
  ; ord_type : int
  ; side : int
  }
[@@deriving sexp, compare, equal]

let empty =
  { sub_id_indicator = 0
  ; special_ord_type = 0
  ; locate_reqd = 0
  ; retail_indicator = 0
  ; attributed_quote = 0
  ; order_capacity = 0
  ; interest_type = 0
  ; trading_session = 0
  ; time_in_force = 0
  ; proactive_if_locked = 0
  ; self_trade_type = 0
  ; cancel_instead_of_reprice = 0
  ; routing_inst = 0
  ; extended_exec_inst = 0
  ; exec_inst = 0
  ; ord_type = 0
  ; side = 0
  }
;;

let limit_day ~side =
  { empty with
    order_capacity = 1
  ; trading_session = 2
  ; time_in_force = 1
  ; routing_inst = 1
  ; ord_type = 2
  ; side
  }
;;

(* (bit, width) of every field, in record order. *)
let layout =
  [ 12, 1
  ; 13, 4
  ; 17, 1
  ; 18, 1
  ; 19, 3
  ; 22, 3
  ; 25, 3
  ; 28, 3
  ; 31, 3
  ; 34, 3
  ; 37, 3
  ; 40, 4
  ; 44, 4
  ; 48, 4
  ; 52, 4
  ; 56, 4
  ; 60, 4
  ]
;;

let to_list t =
  [ t.sub_id_indicator
  ; t.special_ord_type
  ; t.locate_reqd
  ; t.retail_indicator
  ; t.attributed_quote
  ; t.order_capacity
  ; t.interest_type
  ; t.trading_session
  ; t.time_in_force
  ; t.proactive_if_locked
  ; t.self_trade_type
  ; t.cancel_instead_of_reprice
  ; t.routing_inst
  ; t.extended_exec_inst
  ; t.exec_inst
  ; t.ord_type
  ; t.side
  ]
;;

let to_int64 t =
  List.fold2_exn layout (to_list t) ~init:0L ~f:(fun acc (bit, width) v ->
    let mask = Int64.(shift_left 1L width - 1L) in
    Int64.(acc lor shift_left (of_int v land mask) bit))
;;

let of_int64 bits =
  let field (bit, width) =
    Int64.(to_int_exn (shift_right_logical bits bit land (shift_left 1L width - 1L)))
  in
  match List.map layout ~f:field with
  | [ sub_id_indicator
    ; special_ord_type
    ; locate_reqd
    ; retail_indicator
    ; attributed_quote
    ; order_capacity
    ; interest_type
    ; trading_session
    ; time_in_force
    ; proactive_if_locked
    ; self_trade_type
    ; cancel_instead_of_reprice
    ; routing_inst
    ; extended_exec_inst
    ; exec_inst
    ; ord_type
    ; side
    ] ->
    { sub_id_indicator
    ; special_ord_type
    ; locate_reqd
    ; retail_indicator
    ; attributed_quote
    ; order_capacity
    ; interest_type
    ; trading_session
    ; time_in_force
    ; proactive_if_locked
    ; self_trade_type
    ; cancel_instead_of_reprice
    ; routing_inst
    ; extended_exec_inst
    ; exec_inst
    ; ord_type
    ; side
    }
  | _ -> assert false
;;

module Side = struct
  let buy = 1
  let sell = 2
  let sell_short = 3
  let sell_short_exempt = 4
end

module Ord_type = struct
  let market = 1
  let limit = 2
end

module Tif = struct
  let day = 1
  let ioc = 2
  let opg = 3
  let on_close = 4
end

module Trading_session = struct
  let overnight = 0
  let early = 1
  let core = 2
  let late = 3
  let early_core = 4
  let core_late = 5
  let early_core_late = 6
  let all = 7
  let includes_early n = List.mem [ 1; 4; 6; 7 ] n ~equal:Int.equal
  let includes_core n = List.mem [ 2; 4; 5; 6; 7 ] n ~equal:Int.equal
  let includes_late n = List.mem [ 3; 5; 6; 7 ] n ~equal:Int.equal
end

module Self_trade = struct
  let session_default = 0
  let none = 1
  let cancel_newest = 2
  let cancel_oldest = 3
  let cancel_both = 4
  let cancel_decrement = 5
end

module Extended_exec_inst = struct
  let alo = 1
end
