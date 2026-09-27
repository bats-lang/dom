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

(* Builds, through the borrow operations, elements under the mount, text
   from a region of a larger buffer, and attributes: no widget, diff or
   built text is involved *)
implement main0 () = let
  val doc = $D.open_document($A.text_lit("bats-root"), 9)
  val @(fr, br) = $A.freeze<byte>(bytes("bats-root"))
  val @(fp, bp) = $A.freeze<byte>(bytes("p1"))
  val @(fi, bi) = $A.freeze<byte>(bytes("i1"))
  val @(ft, bt) = $A.freeze<byte>(bytes("xxhello worldyy"))
  val @(fs, bs) = $A.freeze<byte>(bytes("data:,"))
  val () = $D.add_element(doc, br, 9, bp, 2, "p")
  val () = $D.set_text(doc, bp, 2, bt, 2, 11)
  val () = $D.add_element(doc, br, 9, bi, 2, "img")
  val () = $D.set_attr(doc, bi, 2, "src", bs, 0, 6)
  val () = $D.set_attr(doc, bi, 2, "alt", bt, 2, 5)
  (* a paragraph with a child, then its children removed *)
  val @(fq, bq) = $A.freeze<byte>(bytes("q1"))
  val @(fc, bc) = $A.freeze<byte>(bytes("c1"))
  val () = $D.add_element(doc, br, 9, bq, 2, "p")
  val () = $D.add_element(doc, bq, 2, bc, 2, "span")
  val () = $D.remove_children(doc, bq, 2)
  val () = $A.drop<byte>(fq, bq)
  val () = $A.free<byte>($A.thaw<byte>(fq))
  val () = $A.drop<byte>(fc, bc)
  val () = $A.free<byte>($A.thaw<byte>(fc))
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
