(* dom -- type-safe DOM diffing with batched streaming *)

#include "share/atspre_staload.hats"

#use array as A
#use wasm.bats-packages.dev/bridge as B

#pub stadef DOM_BUF_CAP = 262144


#pub absvtype state(l:addr)

#pub absvtype stream(l:addr)

#pub fun init
  (): [l:agz] state(l)

#pub fun fini
  {l:agz}
  (s: state(l)): void

#pub fun begin_batch
  {l:agz}
  (s: state(l)): [l2:agz] stream(l2)

#pub fun end_batch
  {l:agz}
  (s: stream(l)): [l2:agz] state(l2)

#pub fun create_element
  {l:agz}{tl:pos | tl + 10 <= DOM_BUF_CAP; tl < 256}
  (s: stream(l), node_id: int, parent_id: int,
   tag: $A.text(tl), tag_len: int tl): stream(l)

#pub fun set_text
  {l:agz}{lb:agz}{tl:nat | tl + 7 <= DOM_BUF_CAP; tl < 65536}
  (s: stream(l), node_id: int,
   text: !$A.borrow(byte, lb, tl), text_len: int tl): stream(l)

#pub fun set_attr
  {l:agz}{lb:agz}{nl:pos | nl < 256}{vl:nat | nl + vl + 8 <= DOM_BUF_CAP; vl < 65536}
  (s: stream(l), node_id: int,
   attr_name: $A.text(nl), name_len: int nl,
   value: !$A.borrow(byte, lb, vl), value_len: int vl): stream(l)

#pub fun set_style
  {l:agz}{lb:agz}{vl:nat | vl + 13 <= DOM_BUF_CAP; vl < 65536}
  (s: stream(l), node_id: int,
   value: !$A.borrow(byte, lb, vl), value_len: int vl): stream(l)

#pub fun remove_children
  {l:agz}
  (s: stream(l), node_id: int): stream(l)

#pub fun remove_child
  {l:agz}
  (s: stream(l), node_id: int): stream(l)

#pub fun set_safe_text
  {l:agz}{tl:nat | tl + 7 <= DOM_BUF_CAP; tl < 65536}
  (s: stream(l), node_id: int,
   text: $A.text(tl), text_len: int tl): stream(l)

#pub fun set_attr_safe
  {l:agz}{nl:pos | nl < 256}{vl:nat | nl + vl + 8 <= DOM_BUF_CAP; vl < 65536}
  (s: stream(l), node_id: int,
   attr_name: $A.text(nl), name_len: int nl,
   value: $A.text(vl), value_len: int vl): stream(l)

#pub fun set_image_src
  {l:agz}{ld:agz}{n:pos}{lm:agz}{m:pos}
  (s: stream(l), node_id: int,
   data: !$A.borrow(byte, ld, n), data_len: int(n),
   mime: !$A.borrow(byte, lm, m), mime_len: int(m)): stream(l)

local

datavtype stream_vt(l:addr) =
  | {l:agz} stream_mk(l) of ($A.arr(byte, l, DOM_BUF_CAP), int)

assume state(l) = $A.arr(byte, l, 1)
assume stream(l) = stream_vt(l)

in

extern fun _ward_dom_flush_raw
  (buf: ptr, len: int): void = "mac#ward_dom_flush"

fun _flush_arr{l:agz}
  (buf: !$A.arr(byte, l, DOM_BUF_CAP), len: int): void =
  _ward_dom_flush_raw(
    $UNSAFE begin $UNSAFE.castvwtp1{ptr}(buf) end,
    len)

implement init() = $A.alloc<byte>(1)

implement fini{l}(s) = $A.free<byte>(s)

implement begin_batch{l}(s) = let
  val () = $A.free<byte>(s)
  val buf = $A.alloc<byte>(262144)
in stream_mk(buf, 0) end

implement end_batch{l}(s) = let
  val+ ~stream_mk(buf, c) = s
  val () = if c > 0 then _flush_arr(buf, c)
  val () = $A.free<byte>(buf)
in $A.alloc<byte>(1) end

fun _auto_flush
  {l:agz}{needed:pos | needed <= DOM_BUF_CAP}
  (s: !stream_vt(l), needed: int needed)
  : [c:nat | c + needed <= DOM_BUF_CAP] int(c) = let
  val+ @stream_mk(buf, cursor) = s
  val c0 = cursor
  val c1 = g1ofg0(c0)
in
  if c1 + needed > 262144 then let
    val () = _flush_arr(buf, c0)
    val () = cursor := 0
    prval () = fold@(s)
  in 0 end
  else if c1 >= 0 then let
    prval () = fold@(s)
  in c1 end
  else let
    val () = _flush_arr(buf, c0)
    val () = cursor := 0
    prval () = fold@(s)
  in 0 end
end

implement create_element{l}{tl}(s, node_id, parent_id, tag, tag_len) = let
  val op_size = 10 + tag_len
  val c = _auto_flush(s, op_size)
  val+ @stream_mk(buf, cursor) = s
  val () = $A.write_byte(buf, c, 4)
  val () = $A.write_i32(buf, c + 1, node_id)
  val () = $A.write_i32(buf, c + 5, parent_id)
  val () = $A.write_byte(buf, c + 9, tag_len)
  val () = $A.write_text(buf, c + 10, tag, tag_len)
  val () = cursor := g0ofg1(c + op_size)
  prval () = fold@(s)
in s end

implement set_text{l}{lb}{tl}(s, node_id, text, text_len) = let
  val op_size = 7 + text_len
  val c = _auto_flush(s, op_size)
  val+ @stream_mk(buf, cursor) = s
  val () = $A.write_byte(buf, c, 1)
  val () = $A.write_i32(buf, c + 1, node_id)
  val () = $A.write_u16le(buf, c + 5, text_len)
  val () = $A.write_borrow(buf, c + 7, text, text_len)
  val () = cursor := g0ofg1(c + op_size)
  prval () = fold@(s)
in s end

implement set_attr{l}{lb}{nl}{vl}(s, node_id, attr_name, name_len, value, value_len) = let
  val op_size = 6 + name_len + 2 + value_len
  val c = _auto_flush(s, op_size)
  val+ @stream_mk(buf, cursor) = s
  val () = $A.write_byte(buf, c, 2)
  val () = $A.write_i32(buf, c + 1, node_id)
  val () = $A.write_byte(buf, c + 5, name_len)
  val () = $A.write_text(buf, c + 6, attr_name, name_len)
  val off = c + 6 + name_len
  val () = $A.write_u16le(buf, off, value_len)
  val () = $A.write_borrow(buf, off + 2, value, value_len)
  val () = cursor := g0ofg1(c + op_size)
  prval () = fold@(s)
in s end

implement set_style{l}{lb}{vl}(s, node_id, value, value_len) = let
  val op_size = 13 + value_len
  val c = _auto_flush(s, op_size)
  val+ @stream_mk(buf, cursor) = s
  val () = $A.write_byte(buf, c, 2)
  val () = $A.write_i32(buf, c + 1, node_id)
  val () = $A.write_byte(buf, c + 5, 5)
  val () = $A.write_byte(buf, c + 6, 115)
  val () = $A.write_byte(buf, c + 7, 116)
  val () = $A.write_byte(buf, c + 8, 121)
  val () = $A.write_byte(buf, c + 9, 108)
  val () = $A.write_byte(buf, c + 10, 101)
  val () = $A.write_u16le(buf, c + 11, value_len)
  val () = $A.write_borrow(buf, c + 13, value, value_len)
  val () = cursor := g0ofg1(c + op_size)
  prval () = fold@(s)
in s end

implement remove_children{l}(s, node_id) = let
  val c = _auto_flush{l}{5}(s, 5)
  val+ @stream_mk(buf, cursor) = s
  val () = $A.write_byte(buf, c, 3)
  val () = $A.write_i32(buf, c + 1, node_id)
  val () = cursor := g0ofg1(c + 5)
  prval () = fold@(s)
in s end

implement remove_child{l}(s, node_id) = let
  val c = _auto_flush{l}{5}(s, 5)
  val+ @stream_mk(buf, cursor) = s
  val () = $A.write_byte(buf, c, 5)
  val () = $A.write_i32(buf, c + 1, node_id)
  val () = cursor := g0ofg1(c + 5)
  prval () = fold@(s)
in s end

implement set_safe_text{l}{tl}(s, node_id, text, text_len) = let
  val op_size = 7 + text_len
  val c = _auto_flush(s, op_size)
  val+ @stream_mk(buf, cursor) = s
  val () = $A.write_byte(buf, c, 1)
  val () = $A.write_i32(buf, c + 1, node_id)
  val () = $A.write_u16le(buf, c + 5, text_len)
  val () = $A.write_text(buf, c + 7, text, text_len)
  val () = cursor := g0ofg1(c + op_size)
  prval () = fold@(s)
in s end

implement set_attr_safe{l}{nl}{vl}(s, node_id, attr_name, name_len, value, value_len) = let
  val op_size = 6 + name_len + 2 + value_len
  val c = _auto_flush(s, op_size)
  val+ @stream_mk(buf, cursor) = s
  val () = $A.write_byte(buf, c, 2)
  val () = $A.write_i32(buf, c + 1, node_id)
  val () = $A.write_byte(buf, c + 5, name_len)
  val () = $A.write_text(buf, c + 6, attr_name, name_len)
  val off = c + 6 + name_len
  val () = $A.write_u16le(buf, off, value_len)
  val () = $A.write_text(buf, off + 2, value, value_len)
  val () = cursor := g0ofg1(c + op_size)
  prval () = fold@(s)
in s end

implement set_image_src{l}{ld}{n}{lm}{m}(s, node_id, data, data_len, mime, mime_len) = let
  val+ @stream_mk(buf, cursor) = s
  val c0 = cursor
  val () = if c0 > 0 then _flush_arr(buf, c0)
  val () = cursor := 0
  prval () = fold@(s)
in
  $B.set_image_src(node_id, data, data_len, mime, mime_len);
  s
end

end (* local *)
