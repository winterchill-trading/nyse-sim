open! Core

let table =
  [ 0, "none"
  ; 1, "unknown symbol"
  ; 2, "invalid price"
  ; 3, "invalid quantity"
  ; 4, "session closed"
  ; 5, "halted"
  ; 6, "unsupported"
  ; 7, "duplicate ClOrdID"
  ; 8, "unknown order"
  ; 9, "would cross"
  ; 10, "self trade"
  ; 11, "IOC unfilled"
  ; 12, "insufficient buying power"
  ; 13, "short sale price test"
  ; 14, "auction order not accepted"
  ; 15, "done for day"
  ; 16, "cancel on disconnect"
  ; 17, "user cancel"
  ; 18, "LULD"
  ; 19, "not logged in"
  ]
;;

let none = 0
let unknown_symbol = 1
let invalid_price = 2
let invalid_quantity = 3
let session_closed = 4
let halted = 5
let unsupported = 6
let duplicate_cl_ord_id = 7
let unknown_order = 8
let would_cross = 9
let self_trade = 10
let ioc_unfilled = 11
let insufficient_buying_power = 12
let short_sale_price_test = 13
let auction_not_accepted = 14
let done_for_day = 15
let cancel_on_disconnect = 16
let user_cancel = 17
let luld = 18
let not_logged_in = 19

let to_string n =
  List.Assoc.find table n ~equal:Int.equal
  |> Option.value ~default:(sprintf "reason %d" n)
;;

module Liquidity = struct
  let added = "A"
  let removed = "R"
  let opening_auction = "O"
  let closing_auction = "C"
  let reopening_auction = "H"
end
