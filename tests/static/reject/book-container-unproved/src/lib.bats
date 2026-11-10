#include "share/atspre_staload.hats"
#use array as A
#use css as C
#use wasm.bats-packages.dev/bridge as B
#use wasm.bats-packages.dev/dom as D
staload OU = "wasm.bats-packages.dev/bridge/src/outside.bats"
staload DB = "wasm.bats-packages.dev/dom/src/book.bats"
staload CB = "css/src/book.sats"

(* A container at a root tag of bytes the program wrote: array's proof of their
   length is not the proof that they are outside bytes *)
fn f {n:pos | n <= 1048576}{cs:$A.cells}{lp:agz}{np:pos | np <= 1024}{li:agz}{ni:pos | ni <= 64}{o:nat}
  (rt: $DB.ROOT_TAG(cs, o) | doc: !$DB.book_document(1), n: int n, at: int o,
   parent: !$A.borrow(byte, lp, np), plen: int np, id: !$A.borrow(byte, li, ni), ilen: int ni): void = let
  val @(pfc | ba) = $A.barr_of_arr($A.alloc<byte>(n))
  val @(fz, cb) = $A.barr_freeze(ba)
  val made = $DB.add_book_container(pfc, rt | doc, cb, at, parent, plen, id, ilen)
  val () = (case+ made of
    | ~$DB.BookContainerMade(c) => $DB.close_book_container(c)
    | ~$DB.BookContainerRefused() => ())
in $A.cborrow_drop(fz, cb) end
