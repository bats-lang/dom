#include "share/atspre_staload.hats"
#use array as A
#use css as C
#use wasm.bats-packages.dev/bridge as B
#use wasm.bats-packages.dev/dom as D
staload OU = "wasm.bats-packages.dev/bridge/src/outside.bats"
staload DB = "wasm.bats-packages.dev/dom/src/book.bats"
staload CB = "css/src/book.sats"

(* A sheet read from bytes the program wrote, written into a document of
   outside bytes *)
fn f {l:agz}{n:pos | n <= 1048576}{cs:$A.cells}{cs2:$A.cells}{g:int}
  (doc: !$DB.book_document(cs, g), c: !$DB.book_container(cs, g),
   mine: !$A.cborrow(l, n, cs2), n: int n): void = let
  val read = $CB.book_sheet_read(mine, n, 1, @(255, 255, 255), @(250, 240, 220))
  val () = $DB.add_book_sheet(doc, c, mine, read)
in $CB.book_read_free(read) end
