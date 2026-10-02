#include "share/atspre_staload.hats"
#use array as A
#use wasm.bats-packages.dev/dom as D

(* s's bytes in a fresh array of exactly its length *)
fn bytes {n:pos | n < 256} (s: string n): [l:agz] $A.arr(byte, l, n) = let
  val n = g1u2i(string1_length(s))
  val a = $A.alloc<byte>(n)
  val () = $A.write_text(a, 0, $A.text_lit(s), n)
in a end

(* A literal href is a url_literal, whose constructor writes its scheme: a javascript: string is not one *)
fn f (): void = let
  val doc = $D.open_document($A.text_lit("bats-root"), 9)
  val @(id_frozen, id_bytes) = $A.freeze<byte>(bytes("e1"))
  val @(value_frozen, value_bytes) = $A.freeze<byte>(bytes("x"))
  val () = $D.set_url_literal(doc, id_bytes, 2, $D.Href, "javascript:alert(1)")
  val () = $A.drop<byte>(value_frozen, value_bytes)
  val () = $A.free<byte>($A.thaw<byte>(value_frozen))
  val () = $A.drop<byte>(id_frozen, id_bytes)
  val () = $A.free<byte>($A.thaw<byte>(id_frozen))
in $D.destroy(doc) end
