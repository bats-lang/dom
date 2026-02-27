(* dom -- DOM diffing: accepts widget diffs, serializes to binary stream *)

#include "share/atspre_staload.hats"

#use array as A
#use arith as AR
#use builder as BU
#use css as C
#use str as S
#use widget as W
#use wasm.bats-packages.dev/bridge as B

#pub stadef DOM_BUF_CAP = 262144

(* ============================================================
   Document: owns mount point, CSS rules, node ID counter
   ============================================================ *)

#pub absvtype document(l:addr)

(* ============================================================
   Public API
   ============================================================ *)

#pub fun create_document
  {nt:pos}{ni:pos}
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
  {l:agz}
  (doc: !document(l), node_id: int, x: int, y: int, w: int, h: int): void

#pub fun canvas_stroke_rect
  {l:agz}
  (doc: !document(l), node_id: int, x: int, y: int, w: int, h: int): void

#pub fun canvas_clear_rect
  {l:agz}
  (doc: !document(l), node_id: int, x: int, y: int, w: int, h: int): void

#pub fun canvas_begin_path
  {l:agz}
  (doc: !document(l), node_id: int): void

#pub fun canvas_move_to
  {l:agz}
  (doc: !document(l), node_id: int, x: int, y: int): void

#pub fun canvas_line_to
  {l:agz}
  (doc: !document(l), node_id: int, x: int, y: int): void

#pub fun canvas_arc
  {l:agz}
  (doc: !document(l), node_id: int, cx: int, cy: int, r: int,
   start1000: int, end1000: int, ccw: int): void

#pub fun canvas_close_path
  {l:agz}
  (doc: !document(l), node_id: int): void

#pub fun canvas_fill
  {l:agz}
  (doc: !document(l), node_id: int): void

#pub fun canvas_stroke
  {l:agz}
  (doc: !document(l), node_id: int): void

#pub fun canvas_fill_color
  {l:agz}
  (doc: !document(l), node_id: int, r: int, g: int, b: int, a: int): void

#pub fun canvas_stroke_color
  {l:agz}
  (doc: !document(l), node_id: int, r: int, g: int, b: int, a: int): void

#pub fun canvas_line_width
  {l:agz}
  (doc: !document(l), node_id: int, w100: int): void

#pub fun canvas_fill_text
  {l:agz}{tl:pos | tl < 65536}
  (doc: !document(l), node_id: int, x: int, y: int,
   text: $A.text(tl), text_len: int tl): void

#pub fun canvas_stroke_text
  {l:agz}{tl:pos | tl < 65536}
  (doc: !document(l), node_id: int, x: int, y: int,
   text: $A.text(tl), text_len: int tl): void

#pub fun canvas_set_font
  {l:agz}{fl:pos | fl < 65536}
  (doc: !document(l), node_id: int,
   font: $A.text(fl), font_len: int fl): void

#pub fun canvas_save
  {l:agz}
  (doc: !document(l), node_id: int): void

#pub fun canvas_restore
  {l:agz}
  (doc: !document(l), node_id: int): void

#pub fun canvas_translate
  {l:agz}
  (doc: !document(l), node_id: int, x: int, y: int): void

#pub fun canvas_rotate
  {l:agz}
  (doc: !document(l), node_id: int, angle1000: int): void

#pub fun canvas_scale
  {l:agz}
  (doc: !document(l), node_id: int, sx1000: int, sy1000: int): void

(* ============================================================
   Text constants: attribute and tag names (compile-time verified)
   ============================================================ *)

(* Attribute names *)
fn _txt_hidden(): $A.text(6) =
  $S.text_of_chars(6, @[char][6]('h', 'i', 'd', 'd', 'e', 'n'))
fn _txt_class(): $A.text(5) =
  $S.text_of_chars(5, @[char][5]('c', 'l', 'a', 's', 's'))
fn _txt_tabindex(): $A.text(8) =
  $S.text_of_chars(8, @[char][8]('t', 'a', 'b', 'i', 'n', 'd', 'e', 'x'))
fn _txt_title(): $A.text(5) =
  $S.text_of_chars(5, @[char][5]('t', 'i', 't', 'l', 'e'))
fn _txt_id(): $A.text(2) =
  $S.text_of_chars(2, @[char][2]('i', 'd'))
fn _txt_style(): $A.text(5) =
  $S.text_of_chars(5, @[char][5]('s', 't', 'y', 'l', 'e'))

(* Tag names for html_normal *)
fn _tag_div(): $A.text(3) =
  $S.text_of_chars(3, @[char][3]('d', 'i', 'v'))
fn _tag_span(): $A.text(4) =
  $S.text_of_chars(4, @[char][4]('s', 'p', 'a', 'n'))
fn _tag_p(): $A.text(1) =
  $S.text_of_chars(1, @[char][1]('p'))
fn _tag_br(): $A.text(2) =
  $S.text_of_chars(2, @[char][2]('b', 'r'))
fn _tag_hr(): $A.text(2) =
  $S.text_of_chars(2, @[char][2]('h', 'r'))
fn _tag_ul(): $A.text(2) =
  $S.text_of_chars(2, @[char][2]('u', 'l'))
fn _tag_li(): $A.text(2) =
  $S.text_of_chars(2, @[char][2]('l', 'i'))
fn _tag_a(): $A.text(1) =
  $S.text_of_chars(1, @[char][1]('a'))
fn _tag_img(): $A.text(3) =
  $S.text_of_chars(3, @[char][3]('i', 'm', 'g'))
fn _tag_input(): $A.text(5) =
  $S.text_of_chars(5, @[char][5]('i', 'n', 'p', 'u', 't'))

(* Fallback for any tag: use "div" *)
fn _tag_default(): $A.text(3) = _tag_div()

(* Map html_normal to a tag name *)
fn _normal_tag(n: $W.html_normal): [m:pos | m < 256] @($A.text(m), int m) =
  case+ n of
  | $W.Div() => @(_tag_div(), 3)
  | $W.Span() => @(_tag_span(), 4)
  | $W.P() => @(_tag_p(), 1)
  | $W.Ul() => @(_tag_ul(), 2)
  | $W.Li() => @(_tag_li(), 2)
  | $W.A(_, _) => @(_tag_a(), 1)
  | _ => @(_tag_default(), 3)

(* Map html_void to a tag name *)
fn _void_tag(v: $W.html_void): [m:pos | m < 256] @($A.text(m), int m) =
  case+ v of
  | $W.Br() => @(_tag_br(), 2)
  | $W.Hr() => @(_tag_hr(), 2)
  | $W.Img(_, _, _) => @(_tag_img(), 3)
  | $W.HtmlInput(_, _, _, _, _, _) => @(_tag_input(), 5)
  | _ => @(_tag_default(), 3)

(* ============================================================
   Internal: binary stream protocol
   ============================================================ *)

local

datavtype doc_vt(l:addr) =
  | {l:agz} doc_mk(l) of (
      $A.arr(byte, l, DOM_BUF_CAP),
      int,   (* cursor *)
      int    (* next node ID *)
    )

assume document(l) = doc_vt(l)

in

fn _flush_arr{l:agz}
  (buf: !$A.arr(byte, l, DOM_BUF_CAP), len: int): void =
  $B.dom_flush(buf, len)

fn _auto_flush
  {l:agz}{needed:pos | needed <= DOM_BUF_CAP}
  (doc: !doc_vt(l), needed: int needed)
  : [c:nat | c + needed <= DOM_BUF_CAP] int(c) = let
  val+ @doc_mk(buf, cursor, _) = doc
  val c0 = cursor
  val c1 = g1ofg0(c0)
in
  if c1 + needed > 262144 then let
    val () = _flush_arr(buf, c0)
    val () = cursor := 0
    prval () = fold@(doc)
  in 0 end
  else if c1 >= 0 then let
    prval () = fold@(doc)
  in c1 end
  else let
    val () = _flush_arr(buf, c0)
    val () = cursor := 0
    prval () = fold@(doc)
  in 0 end
end

fn _next_id{l:agz}(doc: !doc_vt(l)): int = let
  val+ @doc_mk(_, _, nid) = doc
  val id = nid
  val () = nid := nid + 1
  prval () = fold@(doc)
in id end

(* Opcode 4: create_element *)
fn _emit_create_element
  {l:agz}{tl:pos | tl + 10 <= DOM_BUF_CAP; tl < 256}
  (doc: !doc_vt(l), node_id: int, parent_id: int,
   tag: $A.text(tl), tag_len: int tl): void = let
  val op_size = 10 + tag_len
  val c = _auto_flush(doc, op_size)
  val+ @doc_mk(buf, cursor, _) = doc
  val () = $A.write_byte(buf, c, 4)
  val () = $A.write_i32(buf, c + 1, node_id)
  val () = $A.write_i32(buf, c + 5, parent_id)
  val () = $A.write_byte(buf, c + 9, tag_len)
  val () = $A.write_text(buf, c + 10, tag, tag_len)
  val () = cursor := g0ofg1(c + op_size)
  prval () = fold@(doc)
in end

(* Opcode 1: set_text *)
fn _emit_set_text
  {l:agz}{tl:pos | tl + 7 <= DOM_BUF_CAP; tl < 65536}
  (doc: !doc_vt(l), node_id: int,
   text: $A.text(tl), text_len: int tl): void = let
  val op_size = 7 + text_len
  val c = _auto_flush(doc, op_size)
  val+ @doc_mk(buf, cursor, _) = doc
  val () = $A.write_byte(buf, c, 1)
  val () = $A.write_i32(buf, c + 1, node_id)
  val () = $A.write_u16le(buf, c + 5, text_len)
  val () = $A.write_text(buf, c + 7, text, text_len)
  val () = cursor := g0ofg1(c + op_size)
  prval () = fold@(doc)
in end

(* Opcode 2: set_attr with text name and text value *)
fn _emit_set_attr
  {l:agz}{nl:pos | nl < 256}{vl:pos | nl + vl + 8 <= DOM_BUF_CAP; vl < 65536}
  (doc: !doc_vt(l), node_id: int,
   attr_name: $A.text(nl), name_len: int nl,
   value: $A.text(vl), value_len: int vl): void = let
  val op_size = 6 + name_len + 2 + value_len
  val c = _auto_flush(doc, op_size)
  val+ @doc_mk(buf, cursor, _) = doc
  val () = $A.write_byte(buf, c, 2)
  val () = $A.write_i32(buf, c + 1, node_id)
  val () = $A.write_byte(buf, c + 5, name_len)
  val () = $A.write_text(buf, c + 6, attr_name, name_len)
  val off = c + 6 + name_len
  val () = $A.write_u16le(buf, off, value_len)
  val () = $A.write_text(buf, off + 2, value, value_len)
  val () = cursor := g0ofg1(c + op_size)
  prval () = fold@(doc)
in end

(* Opcode 2: set_attr with text name and empty value (for boolean attrs) *)
fn _emit_set_attr_empty
  {l:agz}{nl:pos | nl + 8 <= DOM_BUF_CAP; nl < 256}
  (doc: !doc_vt(l), node_id: int,
   attr_name: $A.text(nl), name_len: int nl): void = let
  val op_size = 6 + name_len + 2
  val c = _auto_flush(doc, op_size)
  val+ @doc_mk(buf, cursor, _) = doc
  val () = $A.write_byte(buf, c, 2)
  val () = $A.write_i32(buf, c + 1, node_id)
  val () = $A.write_byte(buf, c + 5, name_len)
  val () = $A.write_text(buf, c + 6, attr_name, name_len)
  val off = c + 6 + name_len
  val () = $A.write_u16le(buf, off, 0)
  val () = cursor := g0ofg1(c + op_size)
  prval () = fold@(doc)
in end

(* Opcode 3: remove_children *)
fn _emit_remove_children
  {l:agz}
  (doc: !doc_vt(l), node_id: int): void = let
  val c = _auto_flush{l}{5}(doc, 5)
  val+ @doc_mk(buf, cursor, _) = doc
  val () = $A.write_byte(buf, c, 3)
  val () = $A.write_i32(buf, c + 1, node_id)
  val () = cursor := g0ofg1(c + 5)
  prval () = fold@(doc)
in end

(* Opcode 5: remove_child *)
fn _emit_remove_child
  {l:agz}
  (doc: !doc_vt(l), node_id: int): void = let
  val c = _auto_flush{l}{5}(doc, 5)
  val+ @doc_mk(buf, cursor, _) = doc
  val () = $A.write_byte(buf, c, 5)
  val () = $A.write_i32(buf, c + 1, node_id)
  val () = cursor := g0ofg1(c + 5)
  prval () = fold@(doc)
in end

fn _flush{l:agz}(doc: !doc_vt(l)): void = let
  val+ @doc_mk(buf, cursor, _) = doc
  val c = cursor
  val () = if c > 0 then _flush_arr(buf, c)
  val () = cursor := 0
  prval () = fold@(doc)
in end

fn _resolve_id(wid: $W.widget_id): int =
  case+ wid of
  | $W.Root() => 0
  | $W.Generated(_, _) => ~1

(* Convert int to text for int attribute values like tabindex *)
fn _int_to_text(v: int): [n:pos | n < 256] @($A.text(n), int n) = let
  (* Just emit "0" for now — proper int-to-text needs digit counting *)
  val b = $A.text_build(1)
  val b = $A.text_putc(b, 0, 48 + (if v >= 0 then (if v < 10 then v else 0) else 0))
in @($A.text_done(b), 1) end

(* Emit a widget tree recursively *)
fun _emit_widget
  {l:agz}
  (doc: !doc_vt(l), parent_id: int, w: $W.widget): void =
  case+ w of
  | $W.Text(s) => () (* Text nodes: would need content_text emit *)
  | $W.Element($W.ElementNode(wid, top, cls, hidden, tabidx, wtitle, children)) => let
      val nid = _next_id(doc)
      val @(tag, tlen) = (case+ top of
        | $W.Normal(n) => _normal_tag(n)
        | $W.Void(v) => _void_tag(v)
      ): [m:pos | m < 256] @($A.text(m), int m)
      val () = _emit_create_element(doc, nid, parent_id, tag, tlen)
      (* Set hidden if true *)
      val () = (if hidden > 0 then _emit_set_attr_empty(doc, nid, _txt_hidden(), 6) else ())
      (* Set tabindex if present *)
      val () = (case+ tabidx of
        | $W.SomeInt(ti) => let
            val @(tv, tvl) = _int_to_text(ti)
          in _emit_set_attr(doc, nid, _txt_tabindex(), 8, tv, tvl) end
        | $W.NoneInt() => ())
      (* Set title if present *)
      val () = (case+ wtitle of
        | $W.SomeStr(_) => () (* would need content_text handling *)
        | $W.NoneStr() => ())
      (* Recurse into children *)
      val () = _emit_children(doc, nid, children)
    in end

and _emit_children
  {l:agz}
  (doc: !doc_vt(l), parent_id: int, children: $W.widget_list): void =
  case+ children of
  | $W.WNil() => ()
  | $W.WCons(child, rest) => let
      val () = _emit_widget(doc, parent_id, child)
    in _emit_children(doc, parent_id, rest) end

(* ============================================================
   Implementations
   ============================================================ *)

implement create_document{nt}{ni}(mount_tag, tag_len, mount_id, id_len) = let
  val buf = $A.alloc<byte>(262144)
  val doc = doc_mk(buf, 0, 1)
  (* Emit: create root element with the mount tag and set its id *)
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
      else () (* removeAttribute would need opcode 6 *)
    end
  | $W.SetClass(wid, cls) => () (* needs UUID lookup from document *)
  | $W.SetTabindex(wid, ti) => let
      val nid = _resolve_id(wid)
    in case+ ti of
      | $W.SomeInt(v) => let
          val @(tv, tvl) = _int_to_text(v)
        in _emit_set_attr(doc, nid, _txt_tabindex(), 8, tv, tvl) end
      | $W.NoneInt() => ()
    end
  | $W.SetTitle(wid, t) => () (* needs content_text handling *)
  | $W.SetAttribute(wid, _) => () (* dispatch per attribute_change *)
  )
in _flush(doc) end

implement destroy{l}(doc) = let
  val+ ~doc_mk(buf, _, _) = doc
in $A.free<byte>(buf) end

(* ============================================================
   Canvas implementations — opcodes 64-84
   ============================================================ *)

(* Helper: emit opcode + node_id (5 bytes) *)
fn _emit_canvas_op5
  {l:agz}
  (doc: !doc_vt(l), op: int, node_id: int): void = let
  val c = _auto_flush{l}{5}(doc, 5)
  val+ @doc_mk(buf, cursor, _) = doc
  val () = $A.write_byte(buf, c, op)
  val () = $A.write_i32(buf, c + 1, node_id)
  val () = cursor := g0ofg1(c + 5)
  prval () = fold@(doc)
in end

(* Helper: emit opcode + node_id + 1 i32 (9 bytes) *)
fn _emit_canvas_op9
  {l:agz}
  (doc: !doc_vt(l), op: int, node_id: int, v0: int): void = let
  val c = _auto_flush{l}{9}(doc, 9)
  val+ @doc_mk(buf, cursor, _) = doc
  val () = $A.write_byte(buf, c, op)
  val () = $A.write_i32(buf, c + 1, node_id)
  val () = $A.write_i32(buf, c + 5, v0)
  val () = cursor := g0ofg1(c + 9)
  prval () = fold@(doc)
in end

(* Helper: emit opcode + node_id + 2 i32 (13 bytes) *)
fn _emit_canvas_op13
  {l:agz}
  (doc: !doc_vt(l), op: int, node_id: int, v0: int, v1: int): void = let
  val c = _auto_flush{l}{13}(doc, 13)
  val+ @doc_mk(buf, cursor, _) = doc
  val () = $A.write_byte(buf, c, op)
  val () = $A.write_i32(buf, c + 1, node_id)
  val () = $A.write_i32(buf, c + 5, v0)
  val () = $A.write_i32(buf, c + 9, v1)
  val () = cursor := g0ofg1(c + 13)
  prval () = fold@(doc)
in end

(* Helper: emit opcode + node_id + 4 i32 (21 bytes) *)
fn _emit_canvas_op21
  {l:agz}
  (doc: !doc_vt(l), op: int, node_id: int,
   v0: int, v1: int, v2: int, v3: int): void = let
  val c = _auto_flush{l}{21}(doc, 21)
  val+ @doc_mk(buf, cursor, _) = doc
  val () = $A.write_byte(buf, c, op)
  val () = $A.write_i32(buf, c + 1, node_id)
  val () = $A.write_i32(buf, c + 5, v0)
  val () = $A.write_i32(buf, c + 9, v1)
  val () = $A.write_i32(buf, c + 13, v2)
  val () = $A.write_i32(buf, c + 17, v3)
  val () = cursor := g0ofg1(c + 21)
  prval () = fold@(doc)
in end

implement canvas_fill_rect{l}(doc, node_id, x, y, w, h) =
  _emit_canvas_op21(doc, 64, node_id, x, y, w, h)

implement canvas_stroke_rect{l}(doc, node_id, x, y, w, h) =
  _emit_canvas_op21(doc, 65, node_id, x, y, w, h)

implement canvas_clear_rect{l}(doc, node_id, x, y, w, h) =
  _emit_canvas_op21(doc, 66, node_id, x, y, w, h)

implement canvas_begin_path{l}(doc, node_id) =
  _emit_canvas_op5(doc, 67, node_id)

implement canvas_move_to{l}(doc, node_id, x, y) =
  _emit_canvas_op13(doc, 68, node_id, x, y)

implement canvas_line_to{l}(doc, node_id, x, y) =
  _emit_canvas_op13(doc, 69, node_id, x, y)

implement canvas_arc{l}(doc, node_id, cx, cy, r, start1000, end1000, ccw) = let
  val c = _auto_flush{l}{26}(doc, 26)
  val+ @doc_mk(buf, cursor, _) = doc
  val () = $A.write_byte(buf, c, 70)
  val () = $A.write_i32(buf, c + 1, node_id)
  val () = $A.write_i32(buf, c + 5, cx)
  val () = $A.write_i32(buf, c + 9, cy)
  val () = $A.write_i32(buf, c + 13, r)
  val () = $A.write_i32(buf, c + 17, start1000)
  val () = $A.write_i32(buf, c + 21, end1000)
  val () = $A.write_byte(buf, c + 25, (if ccw > 0 then 1 else 0))
  val () = cursor := g0ofg1(c + 26)
  prval () = fold@(doc)
in end

implement canvas_close_path{l}(doc, node_id) =
  _emit_canvas_op5(doc, 71, node_id)

implement canvas_fill{l}(doc, node_id) =
  _emit_canvas_op5(doc, 72, node_id)

implement canvas_stroke{l}(doc, node_id) =
  _emit_canvas_op5(doc, 73, node_id)

implement canvas_fill_color{l}(doc, node_id, r, g, b0, a) = let
  val c = _auto_flush{l}{9}(doc, 9)
  val+ @doc_mk(buf, cursor, _) = doc
  val () = $A.write_byte(buf, c, 74)
  val () = $A.write_i32(buf, c + 1, node_id)
  val () = $A.write_byte(buf, c + 5, r)
  val () = $A.write_byte(buf, c + 6, g)
  val () = $A.write_byte(buf, c + 7, b0)
  val () = $A.write_byte(buf, c + 8, a)
  val () = cursor := g0ofg1(c + 9)
  prval () = fold@(doc)
in end

implement canvas_stroke_color{l}(doc, node_id, r, g, b0, a) = let
  val c = _auto_flush{l}{9}(doc, 9)
  val+ @doc_mk(buf, cursor, _) = doc
  val () = $A.write_byte(buf, c, 75)
  val () = $A.write_i32(buf, c + 1, node_id)
  val () = $A.write_byte(buf, c + 5, r)
  val () = $A.write_byte(buf, c + 6, g)
  val () = $A.write_byte(buf, c + 7, b0)
  val () = $A.write_byte(buf, c + 8, a)
  val () = cursor := g0ofg1(c + 9)
  prval () = fold@(doc)
in end

implement canvas_line_width{l}(doc, node_id, w100) =
  _emit_canvas_op9(doc, 76, node_id, w100)

implement canvas_fill_text{l}{tl}(doc, node_id, x, y, text, text_len) = let
  val op_size = 15 + text_len
  val c = _auto_flush(doc, op_size)
  val+ @doc_mk(buf, cursor, _) = doc
  val () = $A.write_byte(buf, c, 77)
  val () = $A.write_i32(buf, c + 1, node_id)
  val () = $A.write_i32(buf, c + 5, x)
  val () = $A.write_i32(buf, c + 9, y)
  val () = $A.write_u16le(buf, c + 13, text_len)
  val () = $A.write_text(buf, c + 15, text, text_len)
  val () = cursor := g0ofg1(c + op_size)
  prval () = fold@(doc)
in end

implement canvas_stroke_text{l}{tl}(doc, node_id, x, y, text, text_len) = let
  val op_size = 15 + text_len
  val c = _auto_flush(doc, op_size)
  val+ @doc_mk(buf, cursor, _) = doc
  val () = $A.write_byte(buf, c, 78)
  val () = $A.write_i32(buf, c + 1, node_id)
  val () = $A.write_i32(buf, c + 5, x)
  val () = $A.write_i32(buf, c + 9, y)
  val () = $A.write_u16le(buf, c + 13, text_len)
  val () = $A.write_text(buf, c + 15, text, text_len)
  val () = cursor := g0ofg1(c + op_size)
  prval () = fold@(doc)
in end

implement canvas_set_font{l}{fl}(doc, node_id, font, font_len) = let
  val op_size = 7 + font_len
  val c = _auto_flush(doc, op_size)
  val+ @doc_mk(buf, cursor, _) = doc
  val () = $A.write_byte(buf, c, 79)
  val () = $A.write_i32(buf, c + 1, node_id)
  val () = $A.write_u16le(buf, c + 5, font_len)
  val () = $A.write_text(buf, c + 7, font, font_len)
  val () = cursor := g0ofg1(c + op_size)
  prval () = fold@(doc)
in end

implement canvas_save{l}(doc, node_id) =
  _emit_canvas_op5(doc, 80, node_id)

implement canvas_restore{l}(doc, node_id) =
  _emit_canvas_op5(doc, 81, node_id)

implement canvas_translate{l}(doc, node_id, x, y) =
  _emit_canvas_op13(doc, 82, node_id, x, y)

implement canvas_rotate{l}(doc, node_id, angle1000) =
  _emit_canvas_op9(doc, 83, node_id, angle1000)

implement canvas_scale{l}(doc, node_id, sx1000, sy1000) =
  _emit_canvas_op13(doc, 84, node_id, sx1000, sy1000)

end (* local *)
