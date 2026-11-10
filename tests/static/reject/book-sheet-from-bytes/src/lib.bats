#include "share/atspre_staload.hats"
#use array as A
#use css as C
#use wasm.bats-packages.dev/bridge as B
#use wasm.bats-packages.dev/dom as D
staload OU = "wasm.bats-packages.dev/bridge/src/outside.bats"
staload DB = "wasm.bats-packages.dev/dom/src/book.bats"
staload CB = "css/src/book.sats"

(* There is no way to give a sheet as bytes *)
fn f {l:agz}{n:pos}{cs:$A.cells}{g:int}{lb:agz}{nb:pos}
  (pf: $OU.OUTSIDE_READ(cs) | doc: !$DB.book_document(g), c: !$DB.book_container(g),
   src: !$A.cborrow(l, n, cs), bytes: !$A.borrow(byte, lb, nb)): void =
  $DB.add_book_sheet(pf | doc, c, src, bytes)
