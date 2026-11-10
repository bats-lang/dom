#include "share/atspre_staload.hats"
#use array as A
#use css as C
#use wasm.bats-packages.dev/bridge as B
#use wasm.bats-packages.dev/dom as D
staload OU = "wasm.bats-packages.dev/bridge/src/outside.bats"
staload DB = "wasm.bats-packages.dev/dom/src/book.bats"
staload CB = "css/src/book.sats"

(* A sheet read from bytes the program wrote, with array's proof of their length
   in the place of the proof that they are outside bytes *)
fn f {n:pos | n <= 1048576}
  (doc: !$DB.book_document(1), c: !$DB.book_container(1), n: int n): void = let
  val @(pfc | ba) = $A.barr_of_arr($A.alloc<byte>(n))
  val @(fz, cb) = $A.barr_freeze(ba)
  val read = $CB.book_sheet_read(cb, n, 1, @(255, 255, 255), @(250, 240, 220))
  val () = $DB.add_book_sheet(pfc | doc, c, cb, read)
  val () = $CB.book_read_free(read)
in $A.cborrow_drop(fz, cb) end
