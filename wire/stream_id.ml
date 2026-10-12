open! Core

module Kind = struct
  type t =
    | Tg
    | Gt
    | Ref
    | Other of int
  [@@deriving sexp, compare, equal]

  let to_int = function
    | Tg -> 15
    | Gt -> 13
    | Ref -> 33
    | Other n -> n
  ;;

  let of_int = function
    | 15 -> Tg
    | 13 -> Gt
    | 33 -> Ref
    | n -> Other n
  ;;
end

type t =
  { env : int
  ; session : int
  ; kind : Kind.t
  ; user : int
  ; sub : int
  }
[@@deriving sexp, compare, equal]

let create ?(env = 1) ?(session = 1) ?(sub = 0) kind ~user =
  { env; session; kind; user; sub }
;;

let length = 8

let read b pos =
  let sess = Le.get_u32 b pos
  and value = Le.get_u32 b (pos + 4) in
  { env = sess lsr 24
  ; session = sess land 0xff_ffff
  ; kind = Kind.of_int (value lsr 24)
  ; user = (value lsr 8) land 0xffff
  ; sub = value land 0xff
  }
;;

let write b pos t =
  Le.set_u32 b pos ((t.env lsl 24) lor (t.session land 0xff_ffff));
  Le.set_u32
    b
    (pos + 4)
    ((Kind.to_int t.kind lsl 24) lor ((t.user land 0xffff) lsl 8) lor (t.sub land 0xff))
;;
