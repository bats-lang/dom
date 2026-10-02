#target wasm binary
#include "share/atspre_staload.hats"
#use array as A
#use wasm.bats-packages.dev/dom as D

(* s's bytes in a fresh array of exactly its length *)
fn bytes {n:pos | n < 256} (s: string n): [l:agz] $A.arr(byte, l, n) = let
  val n = g1u2i(string1_length(s))
  val a = $A.alloc<byte>(n)
  val () = $A.write_text(a, 0, $A.text_lit(s), n)
in a end

(* A link <a id=id> under the mount whose href is url, through set_url;
   its data-set is whether set_url set it *)
fn link {l:agz}{lr:agz}{ni:pos | ni < 256}{nu:pos | nu < 256}
  (doc: !$D.document(l), root: !$A.borrow(byte, lr, 9), id: string ni, url: string nu): void = let
  val id_len = g1u2i(string1_length(id))
  val url_len = g1u2i(string1_length(url))
  val @(id_frozen, id_bytes) = $A.freeze<byte>(bytes(id))
  val @(url_frozen, url_bytes) = $A.freeze<byte>(bytes(url))
  val @(answer_frozen, answer_bytes) = $A.freeze<byte>(bytes("yesno"))
  val () = $D.add_element(doc, root, 9, id_bytes, id_len, $D.A)
  val () = (if $D.set_url(doc, id_bytes, id_len, $D.Href, url_bytes, 0, url_len)
    then $D.set_attr(doc, id_bytes, id_len, $D.Data("set"), answer_bytes, 0, 3)
    else $D.set_attr(doc, id_bytes, id_len, $D.Data("set"), answer_bytes, 3, 2))
  val () = $A.drop<byte>(answer_frozen, answer_bytes)
  val () = $A.free<byte>($A.thaw<byte>(answer_frozen))
  val () = $A.drop<byte>(url_frozen, url_bytes)
  val () = $A.free<byte>($A.thaw<byte>(url_frozen))
  val () = $A.drop<byte>(id_frozen, id_bytes)
in $A.free<byte>($A.thaw<byte>(id_frozen)) end

(* A link <a id=id> under the mount whose href is the literal url *)
fn literal_link {l:agz}{lr:agz}{ni:pos | ni < 256}
  (doc: !$D.document(l), root: !$A.borrow(byte, lr, 9), id: string ni, url: $D.url_literal): void = let
  val id_len = g1u2i(string1_length(id))
  val @(id_frozen, id_bytes) = $A.freeze<byte>(bytes(id))
  val () = $D.add_element(doc, root, 9, id_bytes, id_len, $D.A)
  val () = $D.set_url_literal(doc, id_bytes, id_len, $D.Href, url)
  val () = $A.drop<byte>(id_frozen, id_bytes)
in $A.free<byte>($A.thaw<byte>(id_frozen)) end

(* Builds, through the borrow operations, elements under the mount, text
   from a region of a larger buffer, and attributes: no widget, diff or
   built text is involved. Then links whose hrefs set_url lets through
   (http, https, blob and mailto in any case, relative paths and
   fragments) or refuses (javascript: in any case, after a space or with
   a tab inside, and other schemes), and literal ones *)
implement main0 () = let
  val doc = $D.open_document($A.text_lit("bats-root"), 9)
  val @(fr, br) = $A.freeze<byte>(bytes("bats-root"))
  val @(fp, bp) = $A.freeze<byte>(bytes("p1"))
  val @(fi, bi) = $A.freeze<byte>(bytes("i1"))
  val @(ft, bt) = $A.freeze<byte>(bytes("xxhello worldyy"))
  val @(fs, bs) = $A.freeze<byte>(bytes("blob:http://localhost/picture"))
  val () = $D.add_element(doc, br, 9, bp, 2, $D.P)
  val () = $D.set_text(doc, bp, 2, bt, 2, 11)
  val () = $D.add_element(doc, br, 9, bi, 2, $D.Img)
  val _ = $D.set_url(doc, bi, 2, $D.Src, bs, 0, 29)
  val () = $D.set_attr(doc, bi, 2, $D.Alt, bt, 2, 5)
  val () = $D.set_attr(doc, bi, 2, $D.Aria("describedby"), bp, 0, 2)
  (* a paragraph with a child, then its children removed *)
  val @(fq, bq) = $A.freeze<byte>(bytes("q1"))
  val @(fc, bc) = $A.freeze<byte>(bytes("c1"))
  val () = $D.add_element(doc, br, 9, bq, 2, $D.P)
  val () = $D.add_element(doc, bq, 2, bc, 2, $D.Span)
  val () = $D.remove_children(doc, bq, 2)
  val () = $A.drop<byte>(fq, bq)
  val () = $A.free<byte>($A.thaw<byte>(fq))
  val () = $A.drop<byte>(fc, bc)
  val () = $A.free<byte>($A.thaw<byte>(fc))
  val () = link(doc, br, "l1", "https://example.com/a")
  val () = link(doc, br, "l2", "HTTP://example.com/b")
  val () = link(doc, br, "l3", "mailto:someone@example.com")
  val () = link(doc, br, "l4", "blob:http://localhost/c")
  val () = link(doc, br, "l5", "chapter2.xhtml#note1")
  val () = link(doc, br, "l6", "#top")
  val () = link(doc, br, "l7", "../images/a:b.png")
  val () = link(doc, br, "l8", "?page=2")
  val () = link(doc, br, "l9", "javascript:alert(1)")
  val () = link(doc, br, "l10", "JaVaScRiPt:alert(1)")
  val () = link(doc, br, "l11", " javascript:alert(1)")
  val () = link(doc, br, "l12", "java\tscript:alert(1)")
  val () = link(doc, br, "l13", "data:text/html,hello")
  val () = link(doc, br, "l14", "vbscript:msgbox(1)")
  val () = link(doc, br, "l15", "https:")
  val () = literal_link(doc, br, "m1", $D.Fragment("top"))
  val () = literal_link(doc, br, "m2", $D.Path("chapter.xhtml"))
  val () = literal_link(doc, br, "m3", $D.Https("example.com/"))
  val () = literal_link(doc, br, "m5", $D.EmptyData())
  (* an href set, then removed *)
  val () = literal_link(doc, br, "m4", $D.Mailto("someone@example.com"))
  val @(fm, bm) = $A.freeze<byte>(bytes("m4"))
  val () = $D.remove_url(doc, bm, 2, $D.Href)
  val () = $A.drop<byte>(fm, bm)
  val () = $A.free<byte>($A.thaw<byte>(fm))
  val () = $D.destroy(doc)
  val () = $A.drop<byte>(fr, br)
  val () = $A.free<byte>($A.thaw<byte>(fr))
  val () = $A.drop<byte>(fp, bp)
  val () = $A.free<byte>($A.thaw<byte>(fp))
  val () = $A.drop<byte>(fi, bi)
  val () = $A.free<byte>($A.thaw<byte>(fi))
  val () = $A.drop<byte>(ft, bt)
  val () = $A.free<byte>($A.thaw<byte>(ft))
  val () = $A.drop<byte>(fs, bs)
in $A.free<byte>($A.thaw<byte>(fs)) end
