open! Core

let get_u8 b pos = Bigstring.get_uint8 b ~pos
let set_u8 b pos v = Bigstring.set_uint8_exn b ~pos v
let get_u16 b pos = Bigstring.get_uint16_le b ~pos
let set_u16 b pos v = Bigstring.set_uint16_le_exn b ~pos v
let get_u32 b pos = Bigstring.get_uint32_le b ~pos
let set_u32 b pos v = Bigstring.set_uint32_le_exn b ~pos v
let get_i32 b pos = Bigstring.get_int32_le b ~pos
let get_u64 b pos = Bigstring.get_int64_le_trunc b ~pos
let set_u64 b pos v = Bigstring.set_int64_le b ~pos v

let get_u64_bits b pos =
  let lo = Int64.of_int (get_u32 b pos)
  and hi = Int64.of_int (get_u32 b (pos + 4)) in
  Int64.(lo lor shift_left hi 32)
;;

let set_u64_bits b pos v =
  set_u32 b pos (Int64.to_int_exn Int64.(v land 0xffff_ffffL));
  set_u32 b (pos + 4) (Int64.to_int_exn Int64.(shift_right_logical v 32))
;;

let get_char b pos = Bigstring.get b pos
let set_char b pos c = Bigstring.set b pos c

let strip_padding s =
  String.rstrip s ~drop:(function
    | ' ' | '\000' -> true
    | _ -> false)
;;

let get_text b pos ~len = strip_padding (Bigstring.To_string.sub b ~pos ~len)

let set_padded b pos ~len ~pad s =
  for i = 0 to len - 1 do
    Bigstring.set b (pos + i) (if i < String.length s then s.[i] else pad)
  done
;;

let set_text b pos ~len s = set_padded b pos ~len ~pad:' ' s
let get_ztext = get_text
let set_ztext b pos ~len s = set_padded b pos ~len ~pad:'\000' s

let hex b =
  List.init (Bigstring.length b) ~f:(fun i -> sprintf "%02x" (get_u8 b i))
  |> String.concat ~sep:" "
;;
