open! Core
module A = Nyse_sim.Anchoring
module Price = Exchange_core.Price

let aapl =
  Nyse_sim.Listing.instrument
    (Option.value_exn (Nyse_sim.Listing.find_by_id Nyse_sim.Listing.defaults 1))
;;

let start = Time_ns.of_string_with_utc_offset "2026-10-13T14:00:00Z"
let at sec = Time_ns.add start (Time_ns.Span.of_sec sec)

let show t secs =
  List.iter secs ~f:(fun s ->
    printf "%5.1fs %s\n" s (Option.value_map (A.price_at t (at s)) ~default:"-" ~f:Price.to_string))
;;

let%expect_test "series flag: points follow at their distances from the anchor start" =
  let file = Filename_unix.temp_file "series" ".txt" in
  Out_channel.write_all
    file
    ~data:
      "# ES proxy\n\
       2026-10-12T13:30:00Z 190.004\n\n\
       2026-10-12T13:30:01Z 191\n\
       1791811805000000000 192.5\n";
  let spec = A.Spec.of_series_flag (file ^ "=AAPL") |> Or_error.ok_exn in
  print_s [%sexp (spec.symbol : string), (spec.factor : float)];
  let t = A.create spec ~instrument:aapl ~start in
  show t [ 0.; 0.9; 1.; 4.; 5.; 60.; 2. ];
  [%expect {|
    (AAPL 1)
      0.0s 190
      0.9s 190
      1.0s 191
      4.0s 191
      5.0s 192.5
     60.0s 192.5
      2.0s 191
    |}]
;;

let%expect_test "fixed flag with a daily drift" =
  let spec = A.Spec.of_fixed_flag "200,100=AAPL" |> Or_error.ok_exn in
  let t = A.create spec ~instrument:aapl ~start in
  show t [ 0.; 43_200.; 86_400.; 2. *. 86_400. ];
  [%expect {|
      0.0s 200
    43200.0s 201
    86400.0s 202
    172800.0s 204.02
    |}];
  let spec = A.Spec.of_fixed_flag "10.5=ZVZZT" |> Or_error.ok_exn in
  print_s [%sexp (spec : A.Spec.t)];
  [%expect {|
    ((symbol ZVZZT) (reference (Fixed (price 10.5) (drift_bps_per_day 0)))
     (factor 1))
    |}]
;;

let%expect_test "bad flags" =
  List.iter
    [ "200"; "=AAPL"; "abc=AAPL"; "-1=AAPL"; "1,2,3=AAPL" ]
    ~f:(fun f -> print_s [%sexp (A.Spec.of_fixed_flag f : A.Spec.t Or_error.t)]);
  print_s
    [%sexp
      (A.Spec.parse_series "2026-10-12T13:30:01Z 1\n2026-10-12T13:30:00Z 2\n"
       : (Time_ns.t * Price.t) array Or_error.t)];
  print_s [%sexp (A.Spec.parse_series "# nothing\n" : (Time_ns.t * Price.t) array Or_error.t)];
  print_s [%sexp (A.Spec.parse_series "x y\n" : (Time_ns.t * Price.t) array Or_error.t)];
  [%expect {|
    (Error ("expected ...=SYMBOL" 200))
    (Error ("expected ...=SYMBOL" =AAPL))
    (Error (abc=AAPL ("Fixed_decimal.of_string: invalid decimal" abc)))
    (Error ("anchor price must be positive" -1=AAPL))
    (Error (1,2,3=AAPL (Failure "expected PRICE[,DRIFT_BPS_PER_DAY]")))
    (Error "series: points out of time order")
    (Error "series: no points")
    (Error
     ((series (line 1))
      (("series line" (line "x y"))
       (core/src/time_ns.ml.To_and_of_string.Time_ns_of_string x
        (Failure "no spaces or T found")))))
    |}]
;;

let%expect_test "active only in continuous core trading" =
  List.iter Equities_core.Session.Us_equities.Phase.all ~f:(fun phase ->
    List.iter [ false, false; true, false; false, true ] ~f:(fun (halted, auction) ->
      if A.active ~phase ~halted ~auction
      then
        print_s
          [%message
            "active"
              (phase : Equities_core.Session.Us_equities.Phase.t)
              (halted : bool)
              (auction : bool)]));
  [%expect {| (active (phase Core) (halted false) (auction false)) |}]
;;
