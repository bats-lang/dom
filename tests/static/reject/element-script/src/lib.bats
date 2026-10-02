#include "share/atspre_staload.hats"
#use array as A
#use wasm.bats-packages.dev/dom as D

(* s's bytes in a fresh array of exactly its length *)
fn bytes {n:pos | n < 256} (s: string n): [l:agz] $A.arr(byte, l, n) = let
  val n = g1u2i(string1_length(s))
  val a = $A.alloc<byte>(n)
  val () = $A.write_text(a, 0, $A.text_lit(s), n)
in a end

(* A script is not a tag add_element makes: a tag is not a string *)
fn f (): void = let
  val doc = $D.open_document($A.text_lit("bats-root"), 9)
  val @(root_frozen, root_bytes) = $A.freeze<byte>(bytes("bats-root"))
  val @(id_frozen, id_bytes) = $A.freeze<byte>(bytes("e1"))
  val () = $D.add_element(doc, root_bytes, 9, id_bytes, 2, "script")
  val () = $A.drop<byte>(id_frozen, id_bytes)
  val () = $A.free<byte>($A.thaw<byte>(id_frozen))
  val () = $A.drop<byte>(root_frozen, root_bytes)
  val () = $A.free<byte>($A.thaw<byte>(root_frozen))
in $D.destroy(doc) end
