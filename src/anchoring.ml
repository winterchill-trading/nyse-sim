open! Core
module Price = Exchange_core.Price
module Phase = Equities_core.Session.Us_equities.Phase
module Market_sim = Exchange_core_engine.Market_sim

module Reference = struct
  type t =
    | Series of (Time_ns.t * Price.t) array
    | Fixed of
        { price : Price.t
        ; drift_bps_per_day : float
        }
  [@@deriving sexp_of]
end

module Spec = struct
  type t =
    { symbol : string
    ; reference : Reference.t
    ; factor : float
    }
  [@@deriving sexp_of]

  let parse_series contents =
    let open Or_error.Let_syntax in
    let%bind points =
      String.split_lines contents
      |> List.filter_mapi ~f:(fun i line ->
        let line = String.strip line in
        if String.is_empty line || String.is_prefix line ~prefix:"#"
        then None
        else
          Some
            (Price_anchor.Source.parse_series_line line
             |> Or_error.tag_s ~tag:[%message "series" ~line:(i + 1 : int)]))
      |> Or_error.all
    in
    let points = Array.of_list points in
    if Array.is_empty points
    then Or_error.error_string "series: no points"
    else if Array.existsi points ~f:(fun i (ts, _) ->
              i > 0 && Time_ns.( < ) ts (fst points.(i - 1)))
    then Or_error.error_string "series: points out of time order"
    else return points
  ;;

  (* "LEFT=SYM", split at the last '=' *)
  let split_target flag =
    match String.rsplit2 flag ~on:'=' with
    | Some (left, sym) when not (String.is_empty left || String.is_empty sym) ->
      Ok (left, sym)
    | _ -> Or_error.error_s [%message "expected ...=SYMBOL" flag]
  ;;

  let of_series_flag flag =
    let open Or_error.Let_syntax in
    let%bind file, symbol = split_target flag in
    let%bind contents = Or_error.try_with (fun () -> In_channel.read_all file) in
    let%map points = parse_series contents |> Or_error.tag ~tag:file in
    { symbol; reference = Series points; factor = 1. }
  ;;

  let of_fixed_flag flag =
    let open Or_error.Let_syntax in
    let%bind left, symbol = split_target flag in
    let%bind price, drift_bps_per_day =
      Or_error.try_with (fun () ->
        match String.split left ~on:',' with
        | [ p ] -> Price.of_string p, 0.
        | [ p; d ] -> Price.of_string p, Float.of_string d
        | _ -> failwith "expected PRICE[,DRIFT_BPS_PER_DAY]")
      |> Or_error.tag ~tag:flag
    in
    if Price.( <= ) price Price.zero
    then Or_error.error_s [%message "anchor price must be positive" flag]
    else Ok { symbol; reference = Fixed { price; drift_bps_per_day }; factor = 1. }
  ;;
end

let active ~(phase : Phase.t) ~halted ~auction =
  Phase.equal phase Core && (not halted) && not auction
;;

module Stats = struct
  type t =
    { pulls : int
    ; waits : int
    ; releases : int
    }
  [@@deriving sexp_of]
end

type t =
  { spec : Spec.t
  ; scale : Price_anchor.Scale.t
  ; symbol : Exchange_core.Symbol.t
  ; half_life : Time_ns.Span.t
  ; start : Time_ns.t
  ; mutable next : int (** series: index of the first point not yet reached *)
  ; mutable engaged : bool
  ; mutable pulls : int
  ; mutable waits : int
  ; mutable releases : int
  }

let create ?(half_life = Time_ns.Span.second) (spec : Spec.t) ~instrument ~start =
  { spec
  ; scale = Price_anchor.Scale.create ~price_factor:spec.factor instrument
  ; symbol = instrument.Exchange_core.Instrument.symbol
  ; half_life
  ; start
  ; next = 0
  ; engaged = false
  ; pulls = 0
  ; waits = 0
  ; releases = 0
  }
;;

let spec t = t.spec

let raw_price_at t now =
  match t.spec.reference with
  | Fixed { price; drift_bps_per_day } ->
    if Float.equal drift_bps_per_day 0.
    then price
    else (
      let days = Time_ns.Span.to_day (Time_ns.diff now t.start) in
      (Price.of_float [@alert "-lossy"])
        (Price.to_float price *. ((1. +. (drift_bps_per_day /. 10_000.)) **. days)))
  | Series points ->
    let first = fst points.(0) in
    let elapsed = Time_ns.diff now t.start in
    let reached i = Time_ns.Span.( <= ) (Time_ns.diff (fst points.(i)) first) elapsed in
    (* Venue time only moves forward; restart the scan if it did not. *)
    if t.next > 0 && not (reached (t.next - 1)) then t.next <- 0;
    while t.next < Array.length points && reached t.next do
      t.next <- t.next + 1
    done;
    snd points.(Int.max 0 (t.next - 1))
;;

let price_at t now =
  Price_anchor.Scale.price t.scale (raw_price_at t now) ~direction:Nearest
;;

let on_tick t sim ~active ~now =
  if active
  then (
    match price_at t now with
    | None -> ()
    | Some px ->
      Market_sim.set_reference ~half_life:t.half_life sim t.symbol px;
      t.engaged <- true;
      t.pulls <- t.pulls + 1)
  else (
    t.waits <- t.waits + 1;
    if t.engaged
    then (
      Market_sim.clear_reference sim t.symbol;
      t.engaged <- false;
      t.releases <- t.releases + 1))
;;

let engaged t = t.engaged
let stats t : Stats.t = { pulls = t.pulls; waits = t.waits; releases = t.releases }
