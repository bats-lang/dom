#include "share/atspre_staload.hats"
#use array as A
#use css as C
#use wasm.bats-packages.dev/bridge as B
#use wasm.bats-packages.dev/dom as D
staload OU = "wasm.bats-packages.dev/bridge/src/outside.bats"
staload DB = "wasm.bats-packages.dev/dom/src/book.bats"
staload CB = "css/src/book.sats"

(* A container must be closed *)
fn f {l:agz}{n:pos}{cs:$A.cells}{lp:agz}{np:pos | np <= 1024}{li:agz}{ni:pos | ni <= 64}{o:nat}
  (pf: $OU.OUTSIDE_READ(cs), rt: $DB.ROOT_TAG(cs, o) | doc: !$DB.book_document(1), src: !$A.cborrow(l, n, cs), at: int o,
   parent: !$A.borrow(byte, lp, np), plen: int np, id: !$A.borrow(byte, li, ni), ilen: int ni): void =
  case+ $DB.add_book_container(pf, rt | doc, src, at, parent, plen, id, ilen) of
  | ~$DB.BookContainerMade(c) => ()
  | ~$DB.BookContainerRefused() => ()
