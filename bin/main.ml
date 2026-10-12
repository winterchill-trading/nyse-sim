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
       (-noise); there is no real market data.")
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
       let%bind server =
         Server.start { gateway_port; feed_port; http_port; listings; clock; noise; seed }
       in
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
