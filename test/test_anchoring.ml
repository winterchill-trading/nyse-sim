open! Core
open! Async
module A = Nyse_sim.Anchoring
module Path = Price_anchor.Path
module Price = Exchange_core.Price

(* Reference prices, before the factor and the tick ([Anchor] scales and rounds them). *)
let show (spec : A.Spec.t) secs =
  List.iter secs ~f:(fun s ->
    printf
      "%5.1fs %s\n"
      s
      (Price.to_string (Path.price_at spec.path ~elapsed:(Time_ns.Span.of_sec s))))
;;

let write data =
  let file = Filename_unix.temp_file "series" ".txt" in
  Out_channel.write_all file ~data;
  file
;;

let%expect_test "series flag: points follow at their distances from the anchor start" =
  let file =
    write
      "# ES proxy\n\
       2026-10-12T13:30:00Z 190.004\n\n\
       2026-10-12T13:30:01Z 191\n\
       1791811805000000000 192.5\n"
  in
  let%bind spec = A.Spec.of_series_flag (file ^ "=AAPL") in
  let spec = Or_error.ok_exn spec in
  print_s [%sexp (spec.symbol : string), (spec.factor : float)];
  show spec [ 0.; 0.9; 1.; 4.; 5.; 60.; 2. ];
  [%expect
    {|
    (AAPL 1)
      0.0s 190.004
      0.9s 190.004
      1.0s 191
      4.0s 191
      5.0s 192.5
     60.0s 192.5
      2.0s 191
    |}];
  return ()
;;

let%expect_test "fixed flag with a daily drift" =
  let spec = A.Spec.of_fixed_flag "200,100=AAPL" |> Or_error.ok_exn in
  show spec [ 0.; 43_200.; 86_400.; 2. *. 86_400. ];
  [%expect
    {|
      0.0s 200
    43200.0s 200.99751242
    86400.0s 202
    172800.0s 204.02
    |}];
  let spec = A.Spec.of_fixed_flag "10.5=ZVZZT" |> Or_error.ok_exn in
  print_s [%sexp (spec : A.Spec.t)];
  [%expect
    {|
    ((symbol ZVZZT) (path (Fixed (price 10.5) (drift_bps_per_day 0))) (factor 1)
     (recorded ()))
    |}];
  return ()
;;

let%expect_test "bad flags" =
  List.iter [ "200"; "=AAPL"; "abc=AAPL"; "-1=AAPL"; "1,2,3=AAPL" ] ~f:(fun f ->
    print_s [%sexp (A.Spec.of_fixed_flag f : A.Spec.t Or_error.t)]);
  [%expect
    {|
    (Error ("expected ...=SYMBOL" 200))
    (Error ("expected ...=SYMBOL" =AAPL))
    (Error
     (abc=AAPL
      (("fixed reference" abc) ("Fixed_decimal.of_string: invalid decimal" abc))))
    (Error
     (-1=AAPL
      (("fixed reference" -1) ("reference price must be positive" (px -1)))))
    (Error
     (1,2,3=AAPL
      (("fixed reference" 1,2,3) "expected PRICE[,DRIFT_BPS_PER_DAY]")))
    |}];
  let%bind () =
    Deferred.List.iter
      ~how:`Sequential
      [ write "2026-10-12T13:30:01Z 1\n2026-10-12T13:30:00Z 2\n"
      ; write "# nothing\n"
      ; write "x y\n"
      ; "/nonexistent/series.txt"
      ]
      ~f:(fun file ->
        let%map r = A.Spec.of_series_flag (file ^ "=AAPL") in
        (* The file name is a temporary one. *)
        match r with
        | Ok _ -> print_endline "ok"
        | Error e ->
          print_s
            (Sexp.of_string
               (String.substr_replace_all
                  (Error.to_string_mach e)
                  ~pattern:file
                  ~with_:"FILE")))
  in
  [%expect
    {|
    (FILE
     (("series file" (path FILE))
      ("series line goes back in time" (line_number 2)
       (line "2026-10-12T13:30:00Z 2") (previous "2026-10-12 13:30:01Z"))))
    (FILE ("no reference price in the source" "empty reference series"))
    (FILE
     (("series file" (path FILE))
      (("series line" (line "x y"))
       (core/src/time_ns.ml.To_and_of_string.Time_ns_of_string x
        (Failure "no spaces or T found")))))
    (FILE
     (("series file" (path FILE))
      (Unix.Unix_error "No such file or directory" open
       "((filename FILE) (mode (O_RDONLY O_CLOEXEC)) (perm 0o0))")))
    |}];
  return ()
;;

let%expect_test "active only in continuous core trading" =
  List.iter Equities_core.Session.Us_equities.Phase.all ~f:(fun phase ->
    List.iter
      [ false, false; true, false; false, true ]
      ~f:(fun (halted, auction) ->
        if A.active ~phase ~halted ~auction
        then
          print_s
            [%message
              "active"
                (phase : Equities_core.Session.Us_equities.Phase.t)
                (halted : bool)
                (auction : bool)]));
  [%expect {| (active (phase Core) (halted false) (auction false)) |}];
  return ()
;;
