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

end (* local *)
