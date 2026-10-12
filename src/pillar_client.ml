open! Core
open! Async
module Pillar = Nyse_sim_wire.Pillar
module App = Nyse_sim_wire.Pillar_app
module Stream_id = Nyse_sim_wire.Stream_id

type t =
  { reader : Reader.t
  ; writer : Writer.t
  ; incoming : Pillar.t Pipe.Reader.t
  ; greeting : Pillar.t list
  ; mutable symbols : App.Symbol_ref.t list
  ; mutable tg_seq : int
  ; avail : (Stream_id.t * int) list
  }

let greeting t = t.greeting
let symbols t = t.symbols
let write t m = Framing.write t.writer (Pillar.encode m)

let stream t (kind : Stream_id.Kind.t) =
  List.find_map_exn t.avail ~f:(fun ((s : Stream_id.t), next) ->
    Option.some_if ([%equal: Stream_id.Kind.t] s.kind kind) (s, next))
;;

let gt_next t = snd (stream t Gt)

let read_raw incoming ~timeout =
  match%map Clock_ns.with_timeout timeout (Pipe.values_available incoming) with
  | `Timeout | `Result `Eof -> None
  | `Result `Ok ->
    (match Pipe.read_now incoming with
     | `Ok m -> Some m
     | `Eof | `Nothing_available -> None)
;;

let next t ~timeout =
  let deadline = Time_ns.add (Time_ns.now ()) timeout in
  let rec loop () =
    let timeout = Time_ns.diff deadline (Time_ns.now ()) in
    if Time_ns.Span.(timeout <= zero)
    then return None
    else (
      match%bind read_raw t.incoming ~timeout with
      | Some (Stream_avail _) -> loop ()
      | r -> return r)
  in
  loop ()
;;

let normalise name =
  Equities_core.Symbol.Equity.of_string (String.uppercase name)
  |> Or_error.map ~f:Equities_core.Symbol.Equity.to_nyse
;;

let symbol_id t name =
  let open Or_error.Let_syntax in
  let%bind nyse = normalise name in
  match List.find t.symbols ~f:(fun s -> String.equal s.symbol nyse) with
  | Some s -> Ok s.symbol_id
  | None -> Or_error.error_s [%message "unknown symbol" (name : string)]
;;

let open_gt t ~start_seq ~end_seq =
  write
    t
    (Open
       { stream_id = fst (stream t Gt)
       ; start_seq
       ; end_seq
       ; access = Pillar.Access.read
       ; mode = 0
       })
;;

let open_tg t =
  let stream_id, next = stream t Tg in
  t.tg_seq <- next;
  write
    t
    (Open
       { stream_id
       ; start_seq = next
       ; end_seq = -1
       ; access = Pillar.Access.write
       ; mode = 0
       });
  let rec wait () =
    match%bind read_raw t.incoming ~timeout:(Time_ns.Span.of_int_sec 5) with
    | None -> return (Or_error.error_string "no OpenResponse for TG")
    | Some (Open_response { stream_id = s; status; _ })
      when [%equal: Stream_id.t] s stream_id ->
      return
        (if status = Pillar.Status.ok
         then Ok ()
         else
           Or_error.error_s
             [%message "TG open refused" ~status:(Pillar.Status.to_string status)])
    | Some _ -> wait ()
  in
  wait ()
;;

let send t payload =
  let stream_id, _ = stream t Tg in
  let seq = t.tg_seq in
  t.tg_seq <- seq + 1;
  write
    t
    (Seq_msg
       { stream_id
       ; seq
       ; timestamp = Time_ns.to_int_ns_since_epoch (Time_ns.now ())
       ; payload
       })
;;

let connect ~host ~port ~user =
  match%bind
    Monitor.try_with_or_error (fun () ->
      Tcp.connect (Tcp.Where_to_connect.of_host_and_port { host; port }))
  with
  | Error e -> return (Error e)
  | Ok (_, reader, writer) ->
    let incoming, push = Pipe.create () in
    don't_wait_for
      (let rec loop () =
         match%bind Framing.read_pillar reader with
         | None ->
           Pipe.close push;
           return ()
         | Some frame ->
           (match Pillar.decode frame with
            | Ok m -> Pipe.write_without_pushback_if_open push m
            | Error _ -> ());
           loop ()
       in
       loop ());
    Framing.write
      writer
      (Pillar.encode
         (Login
            { username = user
            ; password = ""
            ; mic = "XNYS"
            ; version = Pillar.protocol_version
            }));
    let timeout = Time_ns.Span.of_int_sec 5 in
    (match%bind read_raw incoming ~timeout with
     | Some (Login_response { status; _ } as response) when status = Pillar.Status.ok ->
       let%bind avail =
         Deferred.List.init ~how:`Sequential 3 ~f:(fun _ ->
           match%map read_raw incoming ~timeout with
           | Some (Stream_avail { stream_id; next_seq; _ } as m) ->
             Some (m, (stream_id, next_seq))
           | _ -> None)
       in
       (match Option.all avail with
        | None -> return (Or_error.error_string "missing StreamAvail after login")
        | Some avail ->
          let t =
            { reader
            ; writer
            ; incoming
            ; greeting = response :: List.map avail ~f:fst
            ; symbols = []
            ; tg_seq = 1
            ; avail = List.map avail ~f:snd
            }
          in
          Clock_ns.every
            ~stop:(Writer.close_started writer)
            Time_ns.Span.second
            (fun () -> write t Heartbeat);
          (* Read the REF stream to its current end for the symbol list. *)
          let ref_stream, ref_next = stream t Ref in
          write
            t
            (Open
               { stream_id = ref_stream
               ; start_seq = 1
               ; end_seq = ref_next
               ; access = Pillar.Access.read
               ; mode = 0
               });
          let rec collect n =
            if n = 0
            then return ()
            else (
              match%bind read_raw incoming ~timeout with
              | None -> return ()
              | Some (Seq_msg { payload = Symbol_ref s; _ }) ->
                t.symbols <- t.symbols @ [ s ];
                collect (n - 1)
              | Some (Seq_msg _) -> collect (n - 1)
              | Some _ -> collect n)
          in
          let%map () = collect (ref_next - 1) in
          Ok t)
     | Some (Login_response { status; _ }) ->
       let%map () = Writer.close writer in
       Or_error.error_s
         [%message "login refused" ~status:(Pillar.Status.to_string status)]
     | _ ->
       let%map () = Writer.close writer in
       Or_error.error_string "no LoginResponse")
;;

let close t =
  let%bind () = Writer.close t.writer in
  Reader.close t.reader
;;
