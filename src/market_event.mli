(** What the venue tells the market data feed, in matching-engine order. Order ids are the
    public order ids (equal to the gateway OrderID); quantities are whole shares. *)

open! Core
module Price := Exchange_core.Price
module Side := Exchange_core.Side

module Status : sig
  type t =
    { security_status : char
    (** XDP Security Status: ['P'] pre-opening, ['E'] early session, ['O'] core, ['L']
        late, ['X'] closed, ['4'] halt, ['5'] resume *)
    ; halt_condition : char (** ['~'] none, ['M'] LULD pause *)
    ; ssr_state : char (** ['~'] none, ['A'] activated, ['C'] continued *)
    ; price_1 : Price.t option (** LULD lower band *)
    ; price_2 : Price.t option (** LULD upper band *)
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
      ; auction_time : int (** HHMM, New York time *)
      }
[@@deriving sexp_of, compare, equal]

val symbol_id : t -> int
