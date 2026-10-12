open! Core
open! Async
open Nyse_sim

let parse_clock ~clock ~start ~speed : Server.Clock_mode.t =
  match clock with
  | "real" -> Real
  | "always-open" -> Always_open
  | "accelerated" ->
    let date =
      Equities_core.Session.Us_equities.next_trading_day
        (Date.today ~zone:Time_float.Zone.utc)
    in
    let h, m =
      match String.split start ~on:':' with
      | [ h; m ] -> Int.of_string h, Int.of_string m
      | _ -> failwithf "-start-time %s: expected HH:MM" start ()
    in
    Accelerated
      { start =
          Equities_core.Session.Zone.of_local New_york date (Equities_core.Session.hm h m)
      ; speed
      }
  | other -> failwithf "-clock %s: expected real, accelerated or always-open" other ()
;;

let command =
  Command.async
    ~summary:"NYSE Pillar gateway and XDP integrated feed simulator"
    ~readme:(fun () ->
      "Serves Pillar Gateway binary order entry on -gateway-port, the XDP Integrated \
       Feed (packets length-framed by PktSize over TCP) on -feed-port, and the same feed \
       as JSON on ws://localhost:<http-port>/ws. The book is driven by noise traders \
       (-noise); there is no real market data. -anchor-series and -anchor-fixed make a \
       symbol's noise traders mean-revert towards a reference price during continuous \
       core trading; auctions and LULD pauses keep their own price.")
    (let%map_open.Command gateway_port =
       flag "-gateway-port" (optional_with_default 4100 int) ~doc:"PORT Pillar gateway"
     and feed_port =
       flag "-feed-port" (optional_with_default 4101 int) ~doc:"PORT XDP feed over TCP"
     and http_port =
       flag
         "-http-port"
         (optional_with_default 4102 int)
         ~doc:"PORT HTTP and JSON websocket"
     and symbols =
       flag
         "-symbols"
         (optional (Arg_type.comma_separated string))
         ~doc:"SYM,... symbols to list (default: AAPL,MSFT,IBM,JPM,KO,XOM,BRK.B,ZVZZT)"
     and noise = flag "-noise" no_arg ~doc:" run noise traders so the book moves"
     and seed = flag "-seed" (optional_with_default 0 int) ~doc:"N noise trader seed"
     and clock =
       flag
         "-clock"
         (optional_with_default "always-open" string)
         ~doc:"MODE real, accelerated or always-open (default)"
     and start =
       flag
         "-start-time"
         (optional_with_default "09:25" string)
         ~doc:"HH:MM New York time the accelerated clock starts at (next trading day)"
     and speed =
       flag
         "-speed"
         (optional_with_default 60. float)
         ~doc:"X accelerated clock speed, venue seconds per real second (default 60)"
     and anchor_series =
       flag
         "-anchor-series"
         (listed string)
         ~doc:
           "FILE=SYM anchor SYM's noise traders to a series of 'time price' lines \
            (repeatable)"
     and anchor_fixed =
       flag
         "-anchor-fixed"
         (listed string)
         ~doc:
           "PRICE[,DRIFT_BPS_PER_DAY]=SYM anchor SYM's noise traders to a fixed price \
            with an optional daily drift (repeatable)"
     and anchor_scaled =
       flag
         "-anchor-scaled"
         (listed string)
         ~doc:
           "VENUE:SYMBOL=SYM,FACTOR anchor SYM to a recorded crypto instrument's prices \
            times FACTOR, from -anchor-norm (repeatable)"
     and anchor_norm =
       flag
         "-anchor-norm"
         (optional string)
         ~doc:"DIR normalized event logs for -anchor-scaled (the recorder's norm/)"
     and anchor_from =
       flag "-anchor-from" (optional date) ~doc:"DATE first UTC day for -anchor-scaled"
     and anchor_to =
       flag
         "-anchor-to"
         (optional date)
         ~doc:"DATE last UTC day for -anchor-scaled (default -anchor-from)"
     and anchor_half_life_ms =
       flag
         "-anchor-half-life-ms"
         (optional_with_default 1000 int)
         ~doc:"MS mean-reversion half-life towards the anchor (default 1000)"
     and log_level =
       flag
         "-log-level"
         (optional_with_default `Info Log.Level.arg)
         ~doc:"LEVEL debug, info, warn or error"
     in
     fun () ->
       Log.Global.set_output [ Log.Output.stderr () ];
       Log.Global.set_level log_level;
       let listings =
         match symbols with
         | None -> Listing.defaults
         | Some s -> Listing.select s |> Or_error.ok_exn
       in
       let clock = parse_clock ~clock ~start ~speed in
       let anchors =
         List.map anchor_series ~f:Anchoring.Spec.of_series_flag
         @ List.map anchor_fixed ~f:Anchoring.Spec.of_fixed_flag
         |> Or_error.all
         |> Or_error.ok_exn
       in
       let%bind scaled =
         match anchor_scaled with
         | [] -> return []
         | flags ->
           let norm_dir, from =
             match anchor_norm, anchor_from with
             | Some d, Some f -> d, f
             | _ -> failwith "-anchor-scaled needs -anchor-norm and -anchor-from"
           in
           let to_ = Option.value anchor_to ~default:from in
           Deferred.List.map ~how:`Sequential flags ~f:(fun flag ->
             Anchoring.Spec.of_scaled_flag flag ~norm_dir ~from ~to_)
           >>| Or_error.all
           >>| Or_error.ok_exn
       in
       let anchors = anchors @ scaled in
       if anchor_half_life_ms <= 0 then failwith "-anchor-half-life-ms must be positive";
       let%bind server =
         Server.start
           { gateway_port
           ; feed_port
           ; http_port
           ; listings
           ; clock
           ; noise
           ; seed
           ; anchors
           ; anchor_half_life = Time_ns.Span.of_int_ms anchor_half_life_ms
           }
       in
       List.iter anchors ~f:(fun (a : Anchoring.Spec.t) ->
         Log.Global.info
           "anchor %s: %s, factor %g"
           a.symbol
           (match a.reference with
            | Fixed { price; drift_bps_per_day } ->
              sprintf
                "fixed %s, drift %g bps/day"
                (Exchange_core.Price.to_string price)
                drift_bps_per_day
            | Series points -> sprintf "series of %d points" (Array.length points))
           a.factor);
       Log.Global.info
         "nyse-sim: gateway %d, feed %d, http %d (/ws), clock %s, %d symbols%s"
         (Server.gateway_port server)
         (Server.feed_port server)
         (Server.http_port server)
         (Sexp.to_string [%sexp (clock : Server.Clock_mode.t)])
         (List.length listings)
         (if noise then ", noise traders on" else "");
       Deferred.never ())
;;

let () = Command_unix.run command
