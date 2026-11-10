#include "share/atspre_staload.hats"
#use array as A
#use css as C
#use wasm.bats-packages.dev/bridge as B
#use wasm.bats-packages.dev/dom as D
staload OU = "wasm.bats-packages.dev/bridge/src/outside.bats"
staload DB = "wasm.bats-packages.dev/dom/src/book.bats"
staload CB = "css/src/book.sats"

(* The attribute type of a book document has no Style *)
fn f {cs:$A.cells}{g:int}{li:agz}{ni:pos | ni <= 1024}{lv:agz}{nv:nat | nv <= 65535}
  (doc: !$DB.book_document(cs, g), id: !$A.borrow(byte, li, ni), ilen: int ni,
   v: !$A.borrow(byte, lv, nv), vlen: int nv): void =
  $DB.set_book_attr(doc, id, ilen, $DB.BookStyle(), v, vlen)
