#include "share/atspre_staload.hats"
#use array as A
#use str as S
#use wasm.bats-packages.dev/dom as D

(* A colour component is a byte: 256 is not one *)
fn f (): void = let
  var tag = @[char][3]('d', 'i', 'v')
  var mid = @[char][4]('r', 'o', 'o', 't')
  val doc = $D.create_document($S.text_of_chars(tag, 3), 3, $S.text_of_chars(mid, 4), 4)
  var cv = @[char][2]('c', 'v')
  val arr = $S.from_char_array(cv, 2)
  val @(fz, bv) = $A.freeze<byte>(arr)
  val () = $D.canvas_fill_color(doc, bv, 2, 256, 0, 0, 255)
  val () = $A.drop<byte>(fz, bv)
  val () = $A.free<byte>($A.thaw<byte>(fz))
in $D.destroy(doc) end
