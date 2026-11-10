#include "share/atspre_staload.hats"
#use array as A
#use css as C
#use wasm.bats-packages.dev/bridge as B
#use wasm.bats-packages.dev/dom as D
staload OU = "wasm.bats-packages.dev/bridge/src/outside.bats"
staload DB = "wasm.bats-packages.dev/dom/src/book.bats"
staload CB = "css/src/book.sats"

(* The declarations of another array than the document's *)
fn f {l:agz}{l2:agz}{n:pos}{m:pos}{cs:$A.cells}{cs2:$A.cells}{g:int}{li:agz}{ni:pos | ni <= 1024}
  (pf: $OU.OUTSIDE_READ(cs) | doc: !$DB.book_document(g), c: !$DB.book_container(g),
   id: !$A.borrow(byte, li, ni), ilen: int ni,
   src: !$A.cborrow(l, n, cs), other: !$A.cborrow(l2, m, cs2), m: int m,
   read: !$CB.book_read(cs, n, 1), decls: !$CB.book_decls(cs2, m, 1, 0)): void =
  $DB.set_book_style(pf | doc, c, id, ilen, other, read, decls)
