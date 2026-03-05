(* dom -- DOM diffing: accepts widget diffs, serializes to binary stream *)

#include "share/atspre_staload.hats"

#use array as A
#use arith as AR
#use builder as BU
#use css as C
#use str as S
#use widget as W
#use wasm.bats-packages.dev/bridge as B
staload "wasm.bats-packages.dev/bridge/src/dom.bats"

#pub stadef DOM_BUF_CAP = 262144

(* ============================================================
   Document: owns mount point, CSS rules, node ID counter
   ============================================================ *)

#pub datavtype document(l:addr) =
  | {l:agz}{nm:pos | nm < 256} doc_mk(l) of (
      $A.arr(byte, l, 262144),
      int,
      int,
      $A.text(nm),
      int nm
    )

vtypedef doc_vt(l:addr) = document(l)

(* ============================================================
   Public API
   ============================================================ *)

#pub fun create_document
  {nt:pos | nt < 256}{ni:pos | ni < 256}
  (mount_tag: $A.text(nt), tag_len: int nt,
   mount_id: $A.text(ni), id_len: int ni): [l:agz] document(l)

#pub fun apply
  {l:agz}
  (doc: !document(l), d: $W.diff): void

#pub fun destroy
  {l:agz}
  (doc: document(l)): void

(* ============================================================
   Canvas API — emit canvas opcodes into the diff buffer
   ============================================================ *)

#pub fun canvas_fill_rect
  {l:agz}{li:agz}{ni:pos | ni < 65536}
  (doc: !document(l), node_id: !$A.borrow(byte, li, ni), id_len: int ni,
   x: int, y: int, w: int, h: int): void

#pub fun canvas_stroke_rect
  {l:agz}{li:agz}{ni:pos | ni < 65536}
  (doc: !document(l), node_id: !$A.borrow(byte, li, ni), id_len: int ni,
   x: int, y: int, w: int, h: int): void

#pub fun canvas_clear_rect
  {l:agz}{li:agz}{ni:pos | ni < 65536}
  (doc: !document(l), node_id: !$A.borrow(byte, li, ni), id_len: int ni,
   x: int, y: int, w: int, h: int): void

#pub fun canvas_begin_path
  {l:agz}{li:agz}{ni:pos | ni < 65536}
  (doc: !document(l), node_id: !$A.borrow(byte, li, ni), id_len: int ni): void

#pub fun canvas_move_to
  {l:agz}{li:agz}{ni:pos | ni < 65536}
  (doc: !document(l), node_id: !$A.borrow(byte, li, ni), id_len: int ni,
   x: int, y: int): void

#pub fun canvas_line_to
  {l:agz}{li:agz}{ni:pos | ni < 65536}
  (doc: !document(l), node_id: !$A.borrow(byte, li, ni), id_len: int ni,
   x: int, y: int): void

#pub fun canvas_arc
  {l:agz}{li:agz}{ni:pos | ni < 65536}
  (doc: !document(l), node_id: !$A.borrow(byte, li, ni), id_len: int ni,
   cx: int, cy: int, r: int,
   start1000: int, end1000: int, ccw: int): void

#pub fun canvas_close_path
  {l:agz}{li:agz}{ni:pos | ni < 65536}
  (doc: !document(l), node_id: !$A.borrow(byte, li, ni), id_len: int ni): void

#pub fun canvas_fill
  {l:agz}{li:agz}{ni:pos | ni < 65536}
  (doc: !document(l), node_id: !$A.borrow(byte, li, ni), id_len: int ni): void

#pub fun canvas_stroke
  {l:agz}{li:agz}{ni:pos | ni < 65536}
  (doc: !document(l), node_id: !$A.borrow(byte, li, ni), id_len: int ni): void

#pub fun canvas_fill_color
  {l:agz}{li:agz}{ni:pos | ni < 65536}
  (doc: !document(l), node_id: !$A.borrow(byte, li, ni), id_len: int ni,
   r: int, g: int, b: int, a: int): void

#pub fun canvas_stroke_color
  {l:agz}{li:agz}{ni:pos | ni < 65536}
  (doc: !document(l), node_id: !$A.borrow(byte, li, ni), id_len: int ni,
   r: int, g: int, b: int, a: int): void

#pub fun canvas_line_width
  {l:agz}{li:agz}{ni:pos | ni < 65536}
  (doc: !document(l), node_id: !$A.borrow(byte, li, ni), id_len: int ni,
   w100: int): void

#pub fun canvas_fill_text
  {l:agz}{li:agz}{ni:pos | ni < 65536}{tl:pos | tl < 65536}
  (doc: !document(l), node_id: !$A.borrow(byte, li, ni), id_len: int ni,
   x: int, y: int,
   text: $A.text(tl), text_len: int tl): void

#pub fun canvas_stroke_text
  {l:agz}{li:agz}{ni:pos | ni < 65536}{tl:pos | tl < 65536}
  (doc: !document(l), node_id: !$A.borrow(byte, li, ni), id_len: int ni,
   x: int, y: int,
   text: $A.text(tl), text_len: int tl): void

#pub fun canvas_set_font
  {l:agz}{li:agz}{ni:pos | ni < 65536}{fl:pos | fl < 65536}
  (doc: !document(l), node_id: !$A.borrow(byte, li, ni), id_len: int ni,
   font: $A.text(fl), font_len: int fl): void

#pub fun canvas_save
  {l:agz}{li:agz}{ni:pos | ni < 65536}
  (doc: !document(l), node_id: !$A.borrow(byte, li, ni), id_len: int ni): void

#pub fun canvas_restore
  {l:agz}{li:agz}{ni:pos | ni < 65536}
  (doc: !document(l), node_id: !$A.borrow(byte, li, ni), id_len: int ni): void

#pub fun canvas_translate
  {l:agz}{li:agz}{ni:pos | ni < 65536}
  (doc: !document(l), node_id: !$A.borrow(byte, li, ni), id_len: int ni,
   x: int, y: int): void

#pub fun canvas_rotate
  {l:agz}{li:agz}{ni:pos | ni < 65536}
  (doc: !document(l), node_id: !$A.borrow(byte, li, ni), id_len: int ni,
   angle1000: int): void

#pub fun canvas_scale
  {l:agz}{li:agz}{ni:pos | ni < 65536}
  (doc: !document(l), node_id: !$A.borrow(byte, li, ni), id_len: int ni,
   sx1000: int, sy1000: int): void

(* ============================================================
   Text constants: attribute and tag names (compile-time verified)
   ============================================================ *)

fn _txt_hidden(): $A.text(6) =
  let var c = @[char][6]('h', 'i', 'd', 'd', 'e', 'n') in $S.text_of_chars(c, 6) end
fn _txt_class(): $A.text(5) =
  let var c = @[char][5]('c', 'l', 'a', 's', 's') in $S.text_of_chars(c, 5) end
fn _txt_tabindex(): $A.text(8) =
  let var c = @[char][8]('t', 'a', 'b', 'i', 'n', 'd', 'e', 'x') in $S.text_of_chars(c, 8) end
fn _txt_title(): $A.text(5) =
  let var c = @[char][5]('t', 'i', 't', 'l', 'e') in $S.text_of_chars(c, 5) end
fn _txt_id(): $A.text(2) =
  let var c = @[char][2]('i', 'd') in $S.text_of_chars(c, 2) end
fn _txt_style(): $A.text(5) =
  let var c = @[char][5]('s', 't', 'y', 'l', 'e') in $S.text_of_chars(c, 5) end

fn _tag_div(): $A.text(3) =
  let var c = @[char][3]('d', 'i', 'v') in $S.text_of_chars(c, 3) end
fn _tag_span(): $A.text(4) =
  let var c = @[char][4]('s', 'p', 'a', 'n') in $S.text_of_chars(c, 4) end
fn _tag_p(): $A.text(1) =
  let var c = @[char][1]('p') in $S.text_of_chars(c, 1) end
fn _tag_br(): $A.text(2) =
  let var c = @[char][2]('b', 'r') in $S.text_of_chars(c, 2) end
fn _tag_hr(): $A.text(2) =
  let var c = @[char][2]('h', 'r') in $S.text_of_chars(c, 2) end
fn _tag_ul(): $A.text(2) =
  let var c = @[char][2]('u', 'l') in $S.text_of_chars(c, 2) end
fn _tag_li(): $A.text(2) =
  let var c = @[char][2]('l', 'i') in $S.text_of_chars(c, 2) end
fn _tag_a(): $A.text(1) =
  let var c = @[char][1]('a') in $S.text_of_chars(c, 1) end
fn _tag_img(): $A.text(3) =
  let var c = @[char][3]('i', 'm', 'g') in $S.text_of_chars(c, 3) end
fn _tag_input(): $A.text(5) =
  let var c = @[char][5]('i', 'n', 'p', 'u', 't') in $S.text_of_chars(c, 5) end

fn _tag_default(): $A.text(3) = _tag_div()

fn _normal_tag(n: $W.html_normal): [m:pos | m < 256] @($A.text(m), int m) =
  case+ n of
  | $W.Div() => @(_tag_div(), 3)
  | $W.Span() => @(_tag_span(), 4)
  | $W.P() => @(_tag_p(), 1)
  | $W.Ul() => @(_tag_ul(), 2)
  | $W.Li() => @(_tag_li(), 2)
  | $W.A(_, _) => @(_tag_a(), 1)
  | _ => @(_tag_default(), 3)

fn _void_tag(v: $W.html_void): [m:pos | m < 256] @($A.text(m), int m) =
  case+ v of
  | $W.Br() => @(_tag_br(), 2)
  | $W.Hr() => @(_tag_hr(), 2)
  | $W.Img(_, _, _) => @(_tag_img(), 3)
  | $W.HtmlInput(_, _, _) => @(_tag_input(), 5)
  | _ => @(_tag_default(), 3)

(* ============================================================
   Internal: binary stream protocol
   ============================================================ *)

local

in

fn _flush_arr{l:agz}{m:nat | m <= DOM_BUF_CAP}
  (buf: !$A.arr(byte, l, DOM_BUF_CAP), len: int m): void =
  dom_flush(buf, len)

(* Refined auto_flush for canvas ops with compile-time sizes *)
fn _auto_flush
  {l:agz}{needed:pos | needed <= DOM_BUF_CAP}
  (doc: !doc_vt(l), needed: int needed)
  : [c:nat | c + needed <= DOM_BUF_CAP] int(c) = let
  val+ @doc_mk(buf, cursor, _, _, _) = doc
  val c0 = cursor
  val c1 = $AR.checked_idx(c0, 262144)
in
  if c1 + needed > 262144 then let
    val () = _flush_arr(buf, $AR.checked_idx(c0, 262144))
    val () = cursor := 0
    prval () = fold@(doc)
  in 0 end
  else if c1 >= 0 then let
    prval () = fold@(doc)
  in c1 end
  else let
    val () = _flush_arr(buf, $AR.checked_idx(c0, 262144))
    val () = cursor := 0
    prval () = fold@(doc)
  in 0 end
end

(* Dynamic auto_flush for DOM ops with runtime-computed sizes *)
fn _auto_flush_dyn{l:agz}
  (doc: !doc_vt(l), needed: int): int = let
  val+ @doc_mk(buf, cursor, _, _, _) = doc
  val c0 = cursor
in
  if c0 + needed > 262144 then let
    val () = if c0 > 0 then _flush_arr(buf, $AR.checked_idx(c0, 262144))
    val () = cursor := 0
    prval () = fold@(doc)
  in 0 end
  else let
    prval () = fold@(doc)
  in c0 end
end

fn _next_id{l:agz}(doc: !doc_vt(l)): int = let
  val+ @doc_mk(_, _, nid, _, _) = doc
  val id = nid
  val () = nid := nid + 1
  prval () = fold@(doc)
in id end

(* ---- Node ID helpers ----
   Bridge JS wire format: [u16le str_len][string bytes]
   Root (id <= 0): uses mount_id
   Generated (id > 0): "b" + decimal digits *)

fn _digit_count(n: int): int =
  if n < 10 then 1
  else if n < 100 then 2
  else if n < 1000 then 3
  else if n < 10000 then 4
  else 5

fn _nid_str_len(node_id: int, mid_len: int): int =
  if node_id <= 0 then mid_len
  else 1 + _digit_count(node_id)

fun _write_digits_loop{l:agz}{fuel:nat} .<fuel>.
  (buf: !$A.arr(byte, l, 262144), off: int, n: int, pos: int, fuel: int fuel): void =
  if fuel <= 0 then ()
  else if pos < 0 then ()
  else let
    val d = n mod 10
    val () = $A.write_byte(buf, $AR.checked_idx(off + pos, 262144), d + 48)
  in _write_digits_loop(buf, off, n / 10, pos - 1, fuel - 1) end

fn _write_nid{l:agz}{nm:pos | nm < 256}
  (buf: !$A.arr(byte, l, 262144), off: int,
   node_id: int, mid: $A.text(nm), mid_len: int nm): int =
  if node_id <= 0 then let
    val () = $A.write_u16le(buf, $AR.checked_idx(off, 262144), mid_len)
    val () = $A.write_text(buf, $AR.checked_idx(off + 2, 262144), mid, mid_len)
  in off + 2 + mid_len end
  else let
    val dc = _digit_count(node_id)
    val slen = 1 + dc
    val () = $A.write_u16le(buf, $AR.checked_idx(off, 262144), slen)
    val () = $A.write_byte(buf, $AR.checked_idx(off + 2, 262144), 98)
    val () = _write_digits_loop(buf, off + 3, node_id, dc - 1, $AR.checked_nat(dc + 1))
  in off + 2 + slen end

(* ---- DOM opcodes with string node IDs ---- *)

(* Opcode 4: create_element
   Wire: [4][nid:str][pid:str][tag_len:u8][tag_bytes] *)
fn _emit_create_element
  {l:agz}{tl:pos | tl < 256}
  (doc: !doc_vt(l), node_id: int, parent_id: int,
   tag: $A.text(tl), tag_len: int tl): void = let
  val+ @doc_mk(_, _, _, mid, midl) = doc
  val nslen = _nid_str_len(node_id, midl)
  val pslen = _nid_str_len(parent_id, midl)
  val op_size = 1 + (2 + nslen) + (2 + pslen) + 1 + tag_len
  prval () = fold@(doc)
  val c = _auto_flush_dyn(doc, op_size)
  val+ @doc_mk(buf, cursor, _, mid2, midl2) = doc
  val () = $A.write_byte(buf, $AR.checked_idx(c, 262144), 4)
  val off1 = _write_nid(buf, c + 1, node_id, mid2, midl2)
  val off2 = _write_nid(buf, off1, parent_id, mid2, midl2)
  val () = $A.write_byte(buf, $AR.checked_idx(off2, 262144), tag_len)
  val () = $A.write_text(buf, $AR.checked_idx(off2 + 1, 262144), tag, tag_len)
  val () = cursor := c + op_size
  prval () = fold@(doc)
in end

(* Opcode 1: set_text with $A.text
   Wire: [1][nid:str][text_len:u16le][text_bytes] *)
fn _emit_set_text
  {l:agz}{tl:pos | tl < 65536}
  (doc: !doc_vt(l), node_id: int,
   text: $A.text(tl), text_len: int tl): void = let
  val+ @doc_mk(_, _, _, mid, midl) = doc
  val nslen = _nid_str_len(node_id, midl)
  val op_size = 1 + (2 + nslen) + 2 + text_len
  prval () = fold@(doc)
  val c = _auto_flush_dyn(doc, op_size)
  val+ @doc_mk(buf, cursor, _, mid2, midl2) = doc
  val () = $A.write_byte(buf, $AR.checked_idx(c, 262144), 1)
  val off1 = _write_nid(buf, c + 1, node_id, mid2, midl2)
  val () = $A.write_u16le(buf, $AR.checked_idx(off1, 262144), text_len)
  val () = $A.write_text(buf, $AR.checked_idx(off1 + 2, 262144), text, text_len)
  val () = cursor := c + op_size
  prval () = fold@(doc)
in end

(* Opcode 2: set_attr
   Wire: [2][nid:str][name_len:u8][name_bytes][val_len:u16le][val_bytes] *)
fn _emit_set_attr
  {l:agz}{nl:pos | nl < 256}{vl:pos | vl < 65536}
  (doc: !doc_vt(l), node_id: int,
   attr_name: $A.text(nl), name_len: int nl,
   attr_value: $A.text(vl), value_len: int vl): void = let
  val+ @doc_mk(_, _, _, mid, midl) = doc
  val nslen = _nid_str_len(node_id, midl)
  val op_size = 1 + (2 + nslen) + 1 + name_len + 2 + value_len
  prval () = fold@(doc)
  val c = _auto_flush_dyn(doc, op_size)
  val+ @doc_mk(buf, cursor, _, mid2, midl2) = doc
  val () = $A.write_byte(buf, $AR.checked_idx(c, 262144), 2)
  val off1 = _write_nid(buf, c + 1, node_id, mid2, midl2)
  val () = $A.write_byte(buf, $AR.checked_idx(off1, 262144), name_len)
  val () = $A.write_text(buf, $AR.checked_idx(off1 + 1, 262144), attr_name, name_len)
  val off2 = off1 + 1 + name_len
  val () = $A.write_u16le(buf, $AR.checked_idx(off2, 262144), value_len)
  val () = $A.write_text(buf, $AR.checked_idx(off2 + 2, 262144), attr_value, value_len)
  val () = cursor := c + op_size
  prval () = fold@(doc)
in end

(* Opcode 2: set_attr with empty value (boolean attr) *)
fn _emit_set_attr_empty
  {l:agz}{nl:pos | nl < 256}
  (doc: !doc_vt(l), node_id: int,
   attr_name: $A.text(nl), name_len: int nl): void = let
  val+ @doc_mk(_, _, _, mid, midl) = doc
  val nslen = _nid_str_len(node_id, midl)
  val op_size = 1 + (2 + nslen) + 1 + name_len + 2
  prval () = fold@(doc)
  val c = _auto_flush_dyn(doc, op_size)
  val+ @doc_mk(buf, cursor, _, mid2, midl2) = doc
  val () = $A.write_byte(buf, $AR.checked_idx(c, 262144), 2)
  val off1 = _write_nid(buf, c + 1, node_id, mid2, midl2)
  val () = $A.write_byte(buf, $AR.checked_idx(off1, 262144), name_len)
  val () = $A.write_text(buf, $AR.checked_idx(off1 + 1, 262144), attr_name, name_len)
  val off2 = off1 + 1 + name_len
  val () = $A.write_u16le(buf, $AR.checked_idx(off2, 262144), 0)
  val () = cursor := c + op_size
  prval () = fold@(doc)
in end

(* Opcode 3: remove_children
   Wire: [3][nid:str] *)
fn _emit_remove_children
  {l:agz}
  (doc: !doc_vt(l), node_id: int): void = let
  val+ @doc_mk(_, _, _, mid, midl) = doc
  val nslen = _nid_str_len(node_id, midl)
  val op_size = 1 + 2 + nslen
  prval () = fold@(doc)
  val c = _auto_flush_dyn(doc, op_size)
  val+ @doc_mk(buf, cursor, _, mid2, midl2) = doc
  val () = $A.write_byte(buf, $AR.checked_idx(c, 262144), 3)
  val _ = _write_nid(buf, c + 1, node_id, mid2, midl2)
  val () = cursor := c + op_size
  prval () = fold@(doc)
in end

(* Opcode 5: remove_child
   Wire: [5][nid:str] *)
fn _emit_remove_child
  {l:agz}
  (doc: !doc_vt(l), node_id: int): void = let
  val+ @doc_mk(_, _, _, mid, midl) = doc
  val nslen = _nid_str_len(node_id, midl)
  val op_size = 1 + 2 + nslen
  prval () = fold@(doc)
  val c = _auto_flush_dyn(doc, op_size)
  val+ @doc_mk(buf, cursor, _, mid2, midl2) = doc
  val () = $A.write_byte(buf, $AR.checked_idx(c, 262144), 5)
  val _ = _write_nid(buf, c + 1, node_id, mid2, midl2)
  val () = cursor := c + op_size
  prval () = fold@(doc)
in end

fn _flush{l:agz}(doc: !doc_vt(l)): void = let
  val+ @doc_mk(buf, cursor, _, _, _) = doc
  val c = cursor
  val () = if c > 0 then _flush_arr(buf, $AR.checked_idx(c, 262144))
  val () = cursor := 0
  prval () = fold@(doc)
in end

fn _resolve_id(wid: $W.widget_id): int =
  case+ wid of
  | $W.Root() => 0
  | $W.Generated(_, _) => ~1

(* Convert int to text for int attribute values like tabindex *)
fn _int_to_text(v: int): [n:pos | n < 256] @($A.text(n), int n) = let
  val b = $A.text_build(1)
  val b = $A.text_putc(b, 0, 48)
in @($A.text_done(b), 1) end

(* ---- Text node emission from string ---- *)

fun _write_str_bytes{l:agz}{sn:nat}{i:nat | i <= sn}{fuel:nat} .<fuel>.
  (buf: !$A.arr(byte, l, 262144), off: int,
   s: string sn, slen: int sn, i: int i, fuel: int fuel): void =
  if fuel <= 0 then ()
  else if i >= slen then ()
  else let
    val c = char2int0(string_get_at(s, i))
    val () = $A.write_byte(buf, $AR.checked_idx(off + i, 262144), c)
  in _write_str_bytes(buf, off, s, slen, i + 1, fuel - 1) end

fn _emit_text_str{l:agz}
  (doc: !doc_vt(l), node_id: int, s: string): void = let
  val slen = g1u2i(string1_length(s))
in
  if slen <= 0 then ()
  else if slen >= 65536 then ()
  else let
    val+ @doc_mk(_, _, _, mid, midl) = doc
    val nslen = _nid_str_len(node_id, midl)
    val op_size = 1 + (2 + nslen) + 2 + slen
    prval () = fold@(doc)
    val c = _auto_flush_dyn(doc, op_size)
    val+ @doc_mk(buf, cursor, _, mid2, midl2) = doc
    val () = $A.write_byte(buf, $AR.checked_idx(c, 262144), 1)
    val off1 = _write_nid(buf, c + 1, node_id, mid2, midl2)
    val () = $A.write_u16le(buf, $AR.checked_idx(off1, 262144), slen)
    val () = _write_str_bytes(buf, off1 + 2, s, slen, 0, $AR.checked_nat(slen + 1))
    val () = cursor := c + op_size
    prval () = fold@(doc)
  in end
end

(* Emit a widget *)
fn _emit_widget
  {l:agz}
  (doc: !doc_vt(l), parent_id: int, w: $W.widget): void =
  case+ w of
  | $W.Text(s) => _emit_text_str(doc, parent_id, s)
  | $W.Element(wid, top, hidden, cls) => let
      val nid = _next_id(doc)
      val @(tag, tlen) = (case+ top of
        | $W.Normal(n) => _normal_tag(n)
        | $W.Void(v) => _void_tag(v)
      ): [m:pos | m < 256] @($A.text(m), int m)
      val () = _emit_create_element(doc, nid, parent_id, tag, tlen)
      val () = (if hidden > 0 then _emit_set_attr_empty(doc, nid, _txt_hidden(), 6) else ())
    in end

(* ============================================================
   Implementations
   ============================================================ *)

implement create_document{nt}{ni}(mount_tag, tag_len, mount_id, id_len) = let
  val buf = $A.alloc<byte>(262144)
  val doc = doc_mk(buf, 0, 1, mount_id, id_len)
  val () = _emit_create_element(doc, 0, ~1, mount_tag, tag_len)
  val () = _emit_set_attr(doc, 0, _txt_id(), 2, mount_id, id_len)
  val () = _flush(doc)
in doc end

implement apply{l}(doc, d) = let
  val () = (case+ d of
  | $W.RemoveAllChildren(wid) =>
      _emit_remove_children(doc, _resolve_id(wid))
  | $W.AddChild(parent_wid, child) =>
      _emit_widget(doc, _resolve_id(parent_wid), child)
  | $W.RemoveChild(_, child_wid) =>
      _emit_remove_child(doc, _resolve_id(child_wid))
  | $W.SetHidden(wid, h) => let
      val nid = _resolve_id(wid)
    in
      if h > 0 then _emit_set_attr_empty(doc, nid, _txt_hidden(), 6)
      else ()
    end
  | $W.SetClass(wid, cls) => ()
  )
in _flush(doc) end

implement destroy{l}(doc) = let
  val+ ~doc_mk(buf, _, _, _, _) = doc
in $A.free<byte>(buf) end

(* ============================================================
   Canvas implementations — opcodes 64-84
   Wire format: [opcode:1][id_len:u16le:2][id_bytes:ni][...params...]
   ============================================================ *)

fn _write_canvas_id
  {l:agz}{cap:pos}{li:agz}{ni:pos | ni < 65536}
  {c:nat | c + 3 + ni <= cap}
  {v:nat | v < 256}
  (buf: !$A.arr(byte, l, cap), c: int c,
   opc: int v,
   node_id: !$A.borrow(byte, li, ni), id_len: int ni): int(c + 3 + ni) = let
  val () = $A.write_byte(buf, c, opc)
  val () = $A.write_u16le(buf, c + 1, id_len)
  val () = $A.write_borrow(buf, c + 3, node_id, id_len)
in c + 3 + id_len end

fn _emit_canvas_str_op
  {l:agz}{li:agz}{ni:pos | ni < 65536}
  {v:nat | v < 256}
  (doc: !doc_vt(l), opc: int v,
   node_id: !$A.borrow(byte, li, ni), id_len: int ni): void = let
  val op_size = 3 + id_len
  val c = _auto_flush(doc, op_size)
  val+ @doc_mk(buf, cursor, _, _, _) = doc
  val _ = _write_canvas_id(buf, c, opc, node_id, id_len)
  val () = cursor := g0ofg1(c + op_size)
  prval () = fold@(doc)
in end

fn _emit_canvas_str_op_i32
  {l:agz}{li:agz}{ni:pos | ni < 65536}
  {v:nat | v < 256}
  (doc: !doc_vt(l), opc: int v,
   node_id: !$A.borrow(byte, li, ni), id_len: int ni,
   v0: int): void = let
  val op_size = 7 + id_len
  val c = _auto_flush(doc, op_size)
  val+ @doc_mk(buf, cursor, _, _, _) = doc
  val off = _write_canvas_id(buf, c, opc, node_id, id_len)
  val () = $A.write_i32(buf, off, v0)
  val () = cursor := g0ofg1(c + op_size)
  prval () = fold@(doc)
in end

fn _emit_canvas_str_op_2i32
  {l:agz}{li:agz}{ni:pos | ni < 65536}
  {v:nat | v < 256}
  (doc: !doc_vt(l), opc: int v,
   node_id: !$A.borrow(byte, li, ni), id_len: int ni,
   v0: int, v1: int): void = let
  val op_size = 11 + id_len
  val c = _auto_flush(doc, op_size)
  val+ @doc_mk(buf, cursor, _, _, _) = doc
  val off = _write_canvas_id(buf, c, opc, node_id, id_len)
  val () = $A.write_i32(buf, off, v0)
  val () = $A.write_i32(buf, off + 4, v1)
  val () = cursor := g0ofg1(c + op_size)
  prval () = fold@(doc)
in end

fn _emit_canvas_str_op_4i32
  {l:agz}{li:agz}{ni:pos | ni < 65536}
  {v:nat | v < 256}
  (doc: !doc_vt(l), opc: int v,
   node_id: !$A.borrow(byte, li, ni), id_len: int ni,
   v0: int, v1: int, v2: int, v3: int): void = let
  val op_size = 19 + id_len
  val c = _auto_flush(doc, op_size)
  val+ @doc_mk(buf, cursor, _, _, _) = doc
  val off = _write_canvas_id(buf, c, opc, node_id, id_len)
  val () = $A.write_i32(buf, off, v0)
  val () = $A.write_i32(buf, off + 4, v1)
  val () = $A.write_i32(buf, off + 8, v2)
  val () = $A.write_i32(buf, off + 12, v3)
  val () = cursor := g0ofg1(c + op_size)
  prval () = fold@(doc)
in end

implement canvas_fill_rect{l}{li}{ni}(doc, node_id, id_len, x, y, w, h) =
  _emit_canvas_str_op_4i32(doc, 64, node_id, id_len, x, y, w, h)

implement canvas_stroke_rect{l}{li}{ni}(doc, node_id, id_len, x, y, w, h) =
  _emit_canvas_str_op_4i32(doc, 65, node_id, id_len, x, y, w, h)

implement canvas_clear_rect{l}{li}{ni}(doc, node_id, id_len, x, y, w, h) =
  _emit_canvas_str_op_4i32(doc, 66, node_id, id_len, x, y, w, h)

implement canvas_begin_path{l}{li}{ni}(doc, node_id, id_len) =
  _emit_canvas_str_op(doc, 67, node_id, id_len)

implement canvas_move_to{l}{li}{ni}(doc, node_id, id_len, x, y) =
  _emit_canvas_str_op_2i32(doc, 68, node_id, id_len, x, y)

implement canvas_line_to{l}{li}{ni}(doc, node_id, id_len, x, y) =
  _emit_canvas_str_op_2i32(doc, 69, node_id, id_len, x, y)

implement canvas_arc{l}{li}{ni}(doc, node_id, id_len, cx, cy, r, start1000, end1000, ccw) = let
  val op_size = 24 + id_len
  val c = _auto_flush(doc, op_size)
  val+ @doc_mk(buf, cursor, _, _, _) = doc
  val off = _write_canvas_id(buf, c, 70, node_id, id_len)
  val () = $A.write_i32(buf, off, cx)
  val () = $A.write_i32(buf, off + 4, cy)
  val () = $A.write_i32(buf, off + 8, r)
  val () = $A.write_i32(buf, off + 12, start1000)
  val () = $A.write_i32(buf, off + 16, end1000)
  val () = $A.write_byte(buf, off + 20, $AR.checked_byte(if ccw > 0 then 1 else 0))
  val () = cursor := g0ofg1(c + op_size)
  prval () = fold@(doc)
in end

implement canvas_close_path{l}{li}{ni}(doc, node_id, id_len) =
  _emit_canvas_str_op(doc, 71, node_id, id_len)

implement canvas_fill{l}{li}{ni}(doc, node_id, id_len) =
  _emit_canvas_str_op(doc, 72, node_id, id_len)

implement canvas_stroke{l}{li}{ni}(doc, node_id, id_len) =
  _emit_canvas_str_op(doc, 73, node_id, id_len)

implement canvas_fill_color{l}{li}{ni}(doc, node_id, id_len, r, g, b0, a) = let
  val op_size = 7 + id_len
  val c = _auto_flush(doc, op_size)
  val+ @doc_mk(buf, cursor, _, _, _) = doc
  val off = _write_canvas_id(buf, c, 74, node_id, id_len)
  val () = $A.write_byte(buf, off, $AR.checked_byte(r))
  val () = $A.write_byte(buf, off + 1, $AR.checked_byte(g))
  val () = $A.write_byte(buf, off + 2, $AR.checked_byte(b0))
  val () = $A.write_byte(buf, off + 3, $AR.checked_byte(a))
  val () = cursor := g0ofg1(c + op_size)
  prval () = fold@(doc)
in end

implement canvas_stroke_color{l}{li}{ni}(doc, node_id, id_len, r, g, b0, a) = let
  val op_size = 7 + id_len
  val c = _auto_flush(doc, op_size)
  val+ @doc_mk(buf, cursor, _, _, _) = doc
  val off = _write_canvas_id(buf, c, 75, node_id, id_len)
  val () = $A.write_byte(buf, off, $AR.checked_byte(r))
  val () = $A.write_byte(buf, off + 1, $AR.checked_byte(g))
  val () = $A.write_byte(buf, off + 2, $AR.checked_byte(b0))
  val () = $A.write_byte(buf, off + 3, $AR.checked_byte(a))
  val () = cursor := g0ofg1(c + op_size)
  prval () = fold@(doc)
in end

implement canvas_line_width{l}{li}{ni}(doc, node_id, id_len, w100) =
  _emit_canvas_str_op_i32(doc, 76, node_id, id_len, w100)

implement canvas_fill_text{l}{li}{ni}{tl}(doc, node_id, id_len, x, y, text, text_len) = let
  val op_size = 13 + id_len + text_len
  val c = _auto_flush(doc, op_size)
  val+ @doc_mk(buf, cursor, _, _, _) = doc
  val off = _write_canvas_id(buf, c, 77, node_id, id_len)
  val () = $A.write_i32(buf, off, x)
  val () = $A.write_i32(buf, off + 4, y)
  val () = $A.write_u16le(buf, off + 8, text_len)
  val () = $A.write_text(buf, off + 10, text, text_len)
  val () = cursor := g0ofg1(c + op_size)
  prval () = fold@(doc)
in end

implement canvas_stroke_text{l}{li}{ni}{tl}(doc, node_id, id_len, x, y, text, text_len) = let
  val op_size = 13 + id_len + text_len
  val c = _auto_flush(doc, op_size)
  val+ @doc_mk(buf, cursor, _, _, _) = doc
  val off = _write_canvas_id(buf, c, 78, node_id, id_len)
  val () = $A.write_i32(buf, off, x)
  val () = $A.write_i32(buf, off + 4, y)
  val () = $A.write_u16le(buf, off + 8, text_len)
  val () = $A.write_text(buf, off + 10, text, text_len)
  val () = cursor := g0ofg1(c + op_size)
  prval () = fold@(doc)
in end

implement canvas_set_font{l}{li}{ni}{fl}(doc, node_id, id_len, font, font_len) = let
  val op_size = 5 + id_len + font_len
  val c = _auto_flush(doc, op_size)
  val+ @doc_mk(buf, cursor, _, _, _) = doc
  val off = _write_canvas_id(buf, c, 79, node_id, id_len)
  val () = $A.write_u16le(buf, off, font_len)
  val () = $A.write_text(buf, off + 2, font, font_len)
  val () = cursor := g0ofg1(c + op_size)
  prval () = fold@(doc)
in end

implement canvas_save{l}{li}{ni}(doc, node_id, id_len) =
  _emit_canvas_str_op(doc, 80, node_id, id_len)

implement canvas_restore{l}{li}{ni}(doc, node_id, id_len) =
  _emit_canvas_str_op(doc, 81, node_id, id_len)

implement canvas_translate{l}{li}{ni}(doc, node_id, id_len, x, y) =
  _emit_canvas_str_op_2i32(doc, 82, node_id, id_len, x, y)

implement canvas_rotate{l}{li}{ni}(doc, node_id, id_len, angle1000) =
  _emit_canvas_str_op_i32(doc, 83, node_id, id_len, angle1000)

implement canvas_scale{l}{li}{ni}(doc, node_id, id_len, sx1000, sy1000) =
  _emit_canvas_str_op_2i32(doc, 84, node_id, id_len, sx1000, sy1000)

end (* local *)
