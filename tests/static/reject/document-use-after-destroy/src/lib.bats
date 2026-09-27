#include "share/atspre_staload.hats"
#use array as A
#use str as S
#use wasm.bats-packages.dev/dom as D

(* A destroyed document cannot be used *)
fn f (): void = let
  var tag = @[char][3]('d', 'i', 'v')
  var mid = @[char][4]('r', 'o', 'o', 't')
  val doc = $D.create_document($S.text_of_chars(tag, 3), 3, $S.text_of_chars(mid, 4), 4)
  val () = $D.destroy(doc)
in $D.destroy(doc) end
