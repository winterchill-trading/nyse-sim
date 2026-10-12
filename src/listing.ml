open! Core
module Price = Exchange_core.Price
module Symbol = Equities_core.Symbol

type t =
  { symbol_id : int
  ; symbol : Exchange_core.Symbol.t
  ; nyse_symbol : string
  ; listed_mic : string
  ; reference_price : Price.t
  ; round_lot : int
  ; tier : Equities_core.Halts.Tier.t
  ; scale : int
  ; test : bool
  }
[@@deriving sexp_of]

let create
  ?(listed_mic = "XNYS")
  ?(tier = Equities_core.Halts.Tier.Tier1)
  ~symbol_id
  name
  ~reference_price
  =
  let equity = Symbol.Equity.of_string name |> Or_error.ok_exn in
  { symbol_id
  ; symbol = Exchange_core.Symbol.of_string (Symbol.Equity.to_compact equity)
  ; nyse_symbol = Symbol.Equity.to_nyse equity
  ; listed_mic
  ; reference_price
  ; round_lot = Equities_core.Lot.round_lot_for_price reference_price
  ; tier
  ; scale = Nyse_sim_wire.Xdp.scale_for_price reference_price
  ; test = Symbol.Equity.is_test equity
  }
;;

let defaults =
  let p = Price.of_string in
  [ create ~symbol_id:1 "AAPL" ~listed_mic:"XNAS" ~reference_price:(p "189.50")
  ; create ~symbol_id:2 "MSFT" ~listed_mic:"XNAS" ~reference_price:(p "420.10")
  ; create ~symbol_id:3 "IBM" ~reference_price:(p "250.30")
  ; create ~symbol_id:4 "JPM" ~reference_price:(p "245.80")
  ; create ~symbol_id:5 "KO" ~reference_price:(p "70.15")
  ; create ~symbol_id:6 "XOM" ~reference_price:(p "115.40")
  ; create ~symbol_id:7 "BRK B" ~reference_price:(p "470.25")
  ; create ~symbol_id:8 "ZVZZT" ~tier:Tier2 ~reference_price:(p "10.00")
  ]
;;

let compact name =
  Symbol.Equity.of_string name |> Or_error.map ~f:Symbol.Equity.to_compact
;;

let select names =
  let open Or_error.Let_syntax in
  List.map names ~f:(fun name ->
    let%bind c = compact (String.uppercase (String.strip name)) in
    match
      List.find defaults ~f:(fun l ->
        String.equal (Exchange_core.Symbol.to_string l.symbol) c)
    with
    | Some l -> Ok l
    | None -> Or_error.error_s [%message "unknown symbol" (name : string)])
  |> Or_error.all
;;

let instrument t =
  Exchange_core.Instrument.create
    t.symbol
    ~size_decimals:0
    ~price_tick:(Equities_core.Tick_size.equity t.reference_price)
    ~max_leverage:4
    ~fees:
      (Equities_core.Fees.to_engine Equities_core.Fees.nyse_like ~price:t.reference_price)
;;

let find_by_id ts id = List.find ts ~f:(fun t -> t.symbol_id = id)

let find_by_symbol ts symbol =
  List.find ts ~f:(fun t -> Exchange_core.Symbol.equal t.symbol symbol)
;;
