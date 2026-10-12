open! Core
module Price = Exchange_core.Price
module Side = Exchange_core.Side

module Status = struct
  type t =
    { security_status : char
    ; halt_condition : char
    ; ssr_state : char
    ; price_1 : Price.t option
    ; price_2 : Price.t option
    }
  [@@deriving sexp_of, compare, equal]
end

type t =
  | Add of
      { symbol_id : int
      ; order_id : int
      ; side : Side.t
      ; px : Price.t
      ; qty : int
      }
  | Modify of
      { symbol_id : int
      ; order_id : int
      ; side : Side.t
      ; px : Price.t
      ; qty : int
      ; lost_priority : bool
      }
  | Delete of
      { symbol_id : int
      ; order_id : int
      }
  | Execution of
      { symbol_id : int
      ; order_id : int
      ; trade_id : int
      ; px : Price.t
      ; qty : int
      ; auction : Equities_core.Auction.Kind.t option
      }
  | Replace of
      { symbol_id : int
      ; order_id : int
      ; new_order_id : int
      ; side : Side.t
      ; px : Price.t
      ; qty : int
      }
  | Non_displayed_trade of
      { symbol_id : int
      ; trade_id : int
      ; px : Price.t
      ; qty : int
      ; auction : Equities_core.Auction.Kind.t option
      }
  | Status of
      { symbol_id : int
      ; status : Status.t
      }
  | Imbalance of
      { symbol_id : int
      ; imbalance : Equities_core.Auction.Imbalance.t
      ; auction_time : int
      }
[@@deriving sexp_of, compare, equal]

let symbol_id = function
  | Add { symbol_id; _ }
  | Modify { symbol_id; _ }
  | Delete { symbol_id; _ }
  | Execution { symbol_id; _ }
  | Replace { symbol_id; _ }
  | Non_displayed_trade { symbol_id; _ }
  | Status { symbol_id; _ }
  | Imbalance { symbol_id; _ } -> symbol_id
;;
