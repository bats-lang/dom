#include "share/atspre_staload.hats"
#use array as A
#use css as C
#use wasm.bats-packages.dev/bridge as B
#use wasm.bats-packages.dev/dom as D
staload OU = "wasm.bats-packages.dev/bridge/src/outside.bats"
staload DB = "wasm.bats-packages.dev/dom/src/book.bats"
staload CB = "css/src/book.sats"

(* A document for the reflow layout (1) given a sheet read in the fixed grammar (2) *)
fn f {l:agz}{n:pos}{cs:$A.cells}
  (doc: !$DB.book_document(cs, 1), c: !$DB.book_container(cs, 1),
   src: !$A.cborrow(l, n, cs), read: !$CB.book_read(cs, n, 2)): void =
  $DB.add_book_sheet(doc, c, src, read)
