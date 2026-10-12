open! Core
module Price = Exchange_core.Price
module Phase = Equities_core.Session.Us_equities.Phase
module Anchor = Price_anchor.Anchor
module Path = Price_anchor.Path
module Source = Price_anchor.Source

module Spec = struct
  type t =
    { symbol : string
    ; path : Path.t
    ; factor : float
    ; recorded : Market_data.Instrument.t option
    }
  [@@deriving sexp_of]

  (* "LEFT=SYM", split at the last '=' *)
  let split_target flag =
    match String.rsplit2 flag ~on:'=' with
    | Some (left, sym) when not (String.is_empty left || String.is_empty sym) ->
      Ok (left, sym)
    | _ -> Or_error.error_s [%message "expected ...=SYMBOL" flag]
  ;;

  let of_series_flag flag =
    let open Async in
    match split_target flag with
    | Error _ as e -> return e
    | Ok (file, symbol) ->
      (* [Source.of_series] refuses a file whose points go back in time. *)
      let%map path =
        Source.of_series file >>=? Source.to_path >>| Or_error.tag ~tag:file
      in
      Or_error.map path ~f:(fun path -> { symbol; path; factor = 1.; recorded = None })
  ;;

  let of_fixed_flag flag =
    let open Or_error.Let_syntax in
    let%bind left, symbol = split_target flag in
    let%map path = Path.fixed_of_string left |> Or_error.tag ~tag:flag in
    { symbol; path; factor = 1.; recorded = None }
  ;;

  let of_scaled_flag flag ~norm_dir ~from ~to_ =
    let open Async in
    match
      let open Or_error.Let_syntax in
      let%bind reference, right = split_target flag in
      let%bind symbol, factor =
        match String.lsplit2 right ~on:',' with
        | Some (symbol, factor) ->
          (match Float.of_string_opt factor with
           | Some f when Float.is_finite f && Float.( > ) f 0. -> Ok (symbol, f)
           | _ -> Or_error.error_s [%message "factor must be a positive number" flag])
        | None -> Or_error.error_s [%message "expected VENUE:SYMBOL=SYM,FACTOR" flag]
      in
      let%map instrument =
        Or_error.try_with (fun () -> Market_data.Instrument.of_string reference)
        |> Or_error.tag ~tag:flag
      in
      instrument, symbol, factor
    with
    | Error _ as e -> return e
    | Ok (instrument, symbol, factor) ->
      let%map path =
        Source.of_norm ~dir:norm_dir instrument ~from ~to_
        >>=? Source.to_path ~min_interval:(Time_ns.Span.of_int_ms 100)
      in
      (match path with
       | Error e ->
         Or_error.error_s
           [%message "no reference prices in the recording" flag (e : Error.t)]
       | Ok path -> Ok { symbol; path; factor; recorded = Some instrument })
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
    ; resumes : int
    ; clamped : int
    }
  [@@deriving sexp_of]
end

type t =
  { spec : Spec.t
  ; anchor : Anchor.t
  ; venue : Venue.t
  }

let create
  ?(half_life = Time_ns.Span.second)
  ?log
  (spec : Spec.t)
  venue
  ~symbol_id
  ~market_sim
  ~noise_traders
  ~auction
  =
  if not (Float.is_finite spec.factor && Float.( > ) spec.factor 0.)
  then raise_s [%message "anchor factor must be positive" (spec : Spec.t)];
  let listing =
    Option.value_exn
      ~message:"anchor: symbol is not listed"
      (Listing.find_by_id (Venue.listings venue) symbol_id)
  in
  let instrument = Listing.instrument listing in
  let sim_symbol = instrument.symbol in
  let reference =
    match spec.recorded with
    | Some r -> r
    | None ->
      Market_data.Instrument.create Nyse (Exchange_core.Symbol.to_string sim_symbol)
  in
  let anchor =
    Anchor.create
      ~price_factor:spec.factor
      ~levels:0
      ~mark_interval:Time_ns.Span.zero
      ~noise_traders
      ~half_life
      ~may_engage:(fun () ->
        active
          ~phase:(Venue.phase venue)
          ~halted:(Venue.halted venue ~symbol_id)
          ~auction:(auction ()))
      ~last_trade:(fun () -> Venue.last_trade venue ~symbol_id)
      ~step_clock:false
      ~clamp:(fun px ->
        let bands = Venue.luld_bands venue ~symbol_id in
        Price_anchor.Clamp.within_bands
          instrument
          ~lower:bands.lower
          ~upper:bands.upper
          px)
      ~path:spec.path
      ?log
      ~market_sim
      ~exchange:(Venue.exchange venue)
      ~reference
      ~sim_symbol
      ()
  in
  { spec; anchor; venue }
;;

let spec t = t.spec
let anchor t = t.anchor
let price t = Anchor.reference_price t.anchor

let on_venue_tick t =
  Anchor.step t.anchor ~now:(Exchange_core_engine.Exchange.now (Venue.exchange t.venue))
;;

let engaged t =
  (not (Anchor.is_paused t.anchor)) && Option.is_some (Anchor.last_set t.anchor)
;;

let stats t : Stats.t =
  let s = Anchor.stats t.anchor in
  { pulls = s.marks_set
  ; waits = s.waits
  ; releases = s.pauses
  ; resumes = s.resumes
  ; clamped = s.clamped
  }
;;
