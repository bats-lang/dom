(* dom -- DOM diffing: accepts widget diffs, serializes to binary stream *)

#include "share/atspre_staload.hats"

#use array as A
#use arith as AR
#use builder as BU
#use css as C
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

(* Create a document: sets up mount point, emits <style> element *)
#pub fun create_document
  {nt:pos}{ni:pos}
  (mount_tag: $A.text(nt), tag_len: int nt,
   mount_id: $A.text(ni), id_len: int ni): [l:agz] document(l)

(* Apply a widget diff to the DOM *)
#pub fun apply
  {l:agz}
  (doc: !document(l), d: $W.diff): void

(* Destroy a document *)
#pub fun destroy
  {l:agz}
  (doc: document(l)): void

(* ============================================================
   Internal: binary stream protocol
   ============================================================ *)

local

datavtype doc_vt(l:addr) =
  | {l:agz} doc_mk(l) of (
      $A.arr(byte, l, DOM_BUF_CAP),  (* buffer *)
      int,                            (* cursor *)
      int                             (* next node ID *)
    )

assume document(l) = doc_vt(l)

in

extern fun _bats_dom_flush_raw
  (buf: ptr, len: int): void = "mac#bats_dom_flush"

fn _flush_arr{l:agz}
  (buf: !$A.arr(byte, l, DOM_BUF_CAP), len: int): void =
  _bats_dom_flush_raw(
    $UNSAFE begin $UNSAFE.castvwtp1{ptr}(buf) end,
    len)

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

(* Opcode 4: create_element(node_id, parent_id, tag_len, tag_bytes) *)
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

(* Opcode 1: set_text(node_id, text_len, text_bytes) *)
fn _emit_set_text_str
  {l:agz}{sn:nat}
  (doc: !doc_vt(l), node_id: int, s: string sn): void = let
  val slen = g1u2i(string1_length(s))
  val slen_g0 = g0ofg1(slen)
in
  if slen_g0 <= 0 then ()
  else if slen_g0 + 7 > 262144 then ()
  else let
    val op_size = $AR.checked_pos(slen_g0 + 7)
    val c = _auto_flush(doc, op_size)
    val+ @doc_mk(buf, cursor, _) = doc
    val () = $A.write_byte(buf, c, 1)
    val () = $A.write_i32(buf, c + 1, node_id)
    val () = $A.write_u16le(buf, c + 5, slen_g0)
    (* Write string bytes *)
    fun loop {sn:nat}{fuel:nat} .<fuel>.
      (buf: !$A.arr(byte, l, DOM_BUF_CAP), off: int, s: string sn,
       slen: int sn, i: int, fuel: int fuel): void =
      if fuel <= 0 then ()
      else let val ii = g1ofg0(i) in
        if ii >= 0 then
          if $AR.lt1_int_int(ii, slen) then let
            val ch = char2int0(string_get_at(s, ii))
            val oi = g1ofg0(off + i)
          in
            if oi >= 0 then if oi < 262144 then
              ($A.write_byte(buf, oi, ch);
               loop(buf, off, s, slen, i + 1, fuel - 1))
          end
          else ()
        else ()
      end
    val () = loop(buf, c + 7, s, slen, 0, $AR.checked_nat(slen_g0 + 1))
    val () = cursor := g0ofg1(c + op_size)
    prval () = fold@(doc)
  in end
end

(* Opcode 2: set_attr(node_id, name_len, name, value_len, value) *)
fn _emit_set_attr_str
  {l:agz}{nl:pos | nl < 256}
  (doc: !doc_vt(l), node_id: int,
   attr_name: $A.text(nl), name_len: int nl,
   value: string): void = let
  val vlen = sz2i(string1_length(value))
  val op_size_raw = 6 + name_len + 2 + vlen
in
  if op_size_raw > 262144 then ()
  else if op_size_raw <= 0 then ()
  else let
    val op_size = $AR.checked_pos(op_size_raw)
    val c = _auto_flush(doc, op_size)
    val+ @doc_mk(buf, cursor, _) = doc
    val () = $A.write_byte(buf, c, 2)
    val () = $A.write_i32(buf, c + 1, node_id)
    val () = $A.write_byte(buf, c + 5, name_len)
    val () = $A.write_text(buf, c + 6, attr_name, name_len)
    val off = c + 6 + name_len
    val () = $A.write_u16le(buf, off, vlen)
    (* write value string bytes *)
    fun loop {sn:nat}{fuel:nat} .<fuel>.
      (buf: !$A.arr(byte, l, DOM_BUF_CAP), base: int, s: string sn,
       slen: int sn, i: int, fuel: int fuel): void =
      if fuel <= 0 then ()
      else let val ii = g1ofg0(i) in
        if ii >= 0 then
          if $AR.lt1_int_int(ii, slen) then let
            val ch = char2int0(string_get_at(s, ii))
            val oi = g1ofg0(base + i)
          in
            if oi >= 0 then if oi < 262144 then
              ($A.write_byte(buf, oi, ch);
               loop(buf, base, s, slen, i + 1, fuel - 1))
          end
          else ()
        else ()
      end
    val () = loop(buf, off + 2, value, g1u2i(string1_length(value)),
      0, $AR.checked_nat(vlen + 1))
    val () = cursor := g0ofg1(c + op_size)
    prval () = fold@(doc)
  in end
end

(* Opcode 3: remove_children(node_id) *)
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

(* Opcode 5: remove_child(node_id) *)
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

(* Flush pending operations *)
fn _flush{l:agz}(doc: !doc_vt(l)): void = let
  val+ @doc_mk(buf, cursor, _) = doc
  val c = cursor
  val () = if c > 0 then _flush_arr(buf, c)
  val () = cursor := 0
  prval () = fold@(doc)
in end

(* Resolve widget_id to an integer node ID *)
(* Root = 0, Generated = hash of text *)
fn _resolve_id(wid: $W.widget_id): int =
  case+ wid of
  | $W.Root() => 0
  | $W.Generated(_, _) => ~1  (* TODO: proper ID resolution via document *)

(* ============================================================
   Implementations
   ============================================================ *)

implement create_document{nt}{ni}(mount_tag, tag_len, mount_id, id_len) = let
  val buf = $A.alloc<byte>(262144)
  val doc = doc_mk(buf, 0, 1)  (* node 0 = root *)
in doc end

implement apply{l}(doc, d) =
  case+ d of
  | $W.RemoveAllChildren(wid) =>
      _emit_remove_children(doc, _resolve_id(wid))
  | $W.AddChild(parent_wid, child) => let
      val parent_id = _resolve_id(parent_wid)
    in
      case+ child of
      | $W.Text(s) => let
          val nid = _next_id(doc)
        in _emit_set_text_str(doc, nid, s) end
      | $W.Element(_) => ()  (* TODO: recursive element creation *)
    end
  | $W.RemoveChild(parent_wid, child_wid) =>
      _emit_remove_child(doc, _resolve_id(child_wid))
  | $W.SetHidden(wid, h) => ()  (* TODO: emit set_attr hidden *)
  | $W.SetClass(wid, cls) => ()  (* TODO: emit set_attr class *)
  | $W.SetTabindex(wid, _) => ()
  | $W.SetTitle(wid, _) => ()
  | $W.SetAttribute(wid, _) => ()

implement destroy{l}(doc) = let
  val+ ~doc_mk(buf, _, _) = doc
in $A.free<byte>(buf) end

end (* local *)
