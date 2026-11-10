#include "share/atspre_staload.hats"
#use array as A
#use css as C
#use wasm.bats-packages.dev/bridge as B
#use wasm.bats-packages.dev/dom as D
staload OU = "wasm.bats-packages.dev/bridge/src/outside.bats"
staload DB = "wasm.bats-packages.dev/dom/src/book.bats"
staload CB = "css/src/book.sats"

(* A document over bytes the program wrote: array's proof of their length is
   not the proof that they are outside bytes *)
fn f {n:pos | n <= 1048576} (n: int n): void = let
  val @(pfc | ba) = $A.barr_of_arr($A.alloc<byte>(n))
  val doc = $DB.open_book_document(pfc | 1)
in $DB.close_book_document(doc) end
