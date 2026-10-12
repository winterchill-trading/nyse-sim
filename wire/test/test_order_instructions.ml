open! Core
open Nyse_sim_wire

let%expect_test "limit day buy: field positions" =
  let t = Order_instructions.limit_day ~side:Order_instructions.Side.buy in
  let bits = Order_instructions.to_int64 t in
  printf "0x%016Lx\n" bits;
  [%expect {| 0x12001000a0400000 |}];
  (* side 1 at bit 60, ord_type 2 at 56, routing 1 at 44, tif 1 at 31, session 2 at 28,
     capacity 1 at 22 *)
  print_s [%sexp (Order_instructions.of_int64 bits : Order_instructions.t)];
  [%expect
    {|
    ((sub_id_indicator 0) (special_ord_type 0) (locate_reqd 0)
     (retail_indicator 0) (attributed_quote 0) (order_capacity 1)
     (interest_type 0) (trading_session 2) (time_in_force 1)
     (proactive_if_locked 0) (self_trade_type 0) (cancel_instead_of_reprice 0)
     (routing_inst 1) (extended_exec_inst 0) (exec_inst 0) (ord_type 2) (side 1))
    |}]
;;

let%expect_test "the top nibble (side) survives 64-bit encoding" =
  let t =
    { (Order_instructions.limit_day ~side:7) with
      time_in_force = 7 (* straddles bit 32 *)
    }
  in
  let bits = Order_instructions.to_int64 t in
  printf "0x%016Lx\n" bits;
  [%expect {| 0x72001003a0400000 |}];
  assert ([%equal: Order_instructions.t] t (Order_instructions.of_int64 bits))
;;

let%test_unit "round trip" =
  let gen =
    let open Quickcheck.Generator.Let_syntax in
    let w n = Int.gen_incl 0 ((1 lsl n) - 1) in
    let%bind a = w 1
    and b = w 4
    and c = w 1
    and d = w 1
    and e = w 3
    and f = w 3
    and g = w 3
    and h = w 3
    and i = w 3 in
    let%map j = w 3
    and k = w 3
    and l = w 4
    and m = w 4
    and n = w 4
    and o = w 4
    and p = w 4
    and q = w 4 in
    { Order_instructions.sub_id_indicator = a
    ; special_ord_type = b
    ; locate_reqd = c
    ; retail_indicator = d
    ; attributed_quote = e
    ; order_capacity = f
    ; interest_type = g
    ; trading_session = h
    ; time_in_force = i
    ; proactive_if_locked = j
    ; self_trade_type = k
    ; cancel_instead_of_reprice = l
    ; routing_inst = m
    ; extended_exec_inst = n
    ; exec_inst = o
    ; ord_type = p
    ; side = q
    }
  in
  Quickcheck.test gen ~sexp_of:Order_instructions.sexp_of_t ~f:(fun t ->
    [%test_result: Order_instructions.t]
      (Order_instructions.of_int64 (Order_instructions.to_int64 t))
      ~expect:t)
;;
