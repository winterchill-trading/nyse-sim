open! Core
open! Async

let read_exactly reader n =
  let buf = Bytes.create n in
  match%map Reader.really_read reader buf with
  | `Ok -> Some buf
  | `Eof _ -> None
;;

(* Reads [prefix] bytes, derives the total length from them, then reads the rest. *)
let read_frame reader ~prefix ~length =
  match%bind read_exactly reader prefix with
  | None -> return None
  | Some head ->
    let total = length head in
    if total < prefix
    then return None
    else (
      match%map read_exactly reader (total - prefix) with
      | None -> None
      | Some rest ->
        let b = Bigstring.create total in
        Bigstring.From_bytes.blit ~src:head ~src_pos:0 ~dst:b ~dst_pos:0 ~len:prefix;
        Bigstring.From_bytes.blit
          ~src:rest
          ~src_pos:0
          ~dst:b
          ~dst_pos:prefix
          ~len:(total - prefix);
        Some b)
;;

let u16 b pos =
  Char.to_int (Bytes.get b pos) lor (Char.to_int (Bytes.get b (pos + 1)) lsl 8)
;;

let read_pillar reader = read_frame reader ~prefix:4 ~length:(fun h -> u16 h 2)
let read_xdp reader = read_frame reader ~prefix:2 ~length:(fun h -> u16 h 0)
let write writer b = if not (Writer.is_closed writer) then Writer.write_bigstring writer b
