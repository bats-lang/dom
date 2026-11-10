#include "share/atspre_staload.hats"
#use array as A
#use css as C
#use wasm.bats-packages.dev/bridge as B
#use wasm.bats-packages.dev/dom as D
staload OU = "wasm.bats-packages.dev/bridge/src/outside.bats"
staload DB = "wasm.bats-packages.dev/dom/src/book.bats"
staload CB = "css/src/book.sats"

(* A container is not used after it is closed *)
fn f {l:agz}{n:pos}{cs:$A.cells}{g:int}{li:agz}{ni:pos | ni <= 1024}{lp:agz}{np:pos | np <= 1024}
  (doc: !$DB.book_document(cs, g), c: $DB.book_container(cs, g),
   id: !$A.borrow(byte, li, ni), ilen: int ni, parent: !$A.borrow(byte, lp, np), plen: int np,
   src: !$A.cborrow(l, n, cs)): void = let
  val () = $DB.close_book_container(c)
in $DB.add_book_element(doc, c, id, ilen, parent, plen, $DB.BookTagP(), src, 0, 0) end
