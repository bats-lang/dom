#target wasm binary
#include "share/atspre_staload.hats"
#use array as A
#use str as S
#use wasm.bats-packages.dev/dom as D
#use widget as W

(* An element with id text t[0, n) and no children *)
fn elem {n:pos | n < 256} (t: $A.text(n), n: int n, top: $W.html_normal): $W.widget =
  $W.Element($W.ElementNode($W.Generated(t, n), $W.Normal(top),
    $W.NoClass(), false, $W.NoneInt(), $W.NoneStr(), $W.WNil()))

(* Builds, through dom's diffs, a tree whose HTML check.mjs prints:
   children added, text and class set, one hidden, one added then
   removed, an element added with its children, text nodes appended,
   attributes set on creation and changed after it. The widget tree is
   linear: apply consumes each diff (and the widget an AddChild holds),
   and the tree is freed at the end. *)
implement main0 () = let
  var mid = @[char][9]('b', 'a', 't', 's', '-', 'r', 'o', 'o', 't')
  val doc = $D.open_document($S.text_of_chars(mid, 9), 9)
  val root = $W.Element($W.ElementNode($W.Root(), $W.Normal($W.Div()),
    $W.NoClass(), false, $W.NoneInt(), $W.NoneStr(), $W.WNil()))
  var ca = @[char][1]('a')
  var cb = @[char][1]('b')
  var cc = @[char][1]('c')
  var cd = @[char][1]('d')
  val ia = $S.text_of_chars(ca, 1)
  val ib = $S.text_of_chars(cb, 1)
  val ic = $S.text_of_chars(cc, 1)
  val id = $S.text_of_chars(cd, 1)
  (* <p id=a>hello</p> *)
  val @(root, d1) = $W.add_child(root, elem(ia, 1, $W.P()))
  val () = $D.apply(doc, d1)
  var hello = @[char][5]('h', 'e', 'l', 'l', 'o')
  val () = $D.apply(doc, $W.set_text_content($W.Generated(ia, 1), $S.text_of_chars(hello, 5), 5))
  (* <span id=b class=x hidden> *)
  val @(root, d2) = $W.add_child(root, elem(ib, 1, $W.Span()))
  val () = $D.apply(doc, d2)
  var x = @[char][1]('x')
  val () = $D.apply(doc, $W.set_class_name($W.Generated(ib, 1), $S.text_of_chars(x, 1), 1))
  val () = $D.apply(doc, $W.SetHidden($W.Generated(ib, 1), true))
  (* <div id=c> added, then removed *)
  val @(root, d3) = $W.add_child(root, elem(ic, 1, $W.Div()))
  val () = $D.apply(doc, d3)
  val @(root, d4) = $W.remove_child(root, $W.Generated(ic, 1))
  val () = $D.apply(doc, d4)
  (* <section id=d> under the span, with a text child *)
  var hi = @[char][2]('h', 'i')
  val sec = elem(id, 1, $W.Section())
  val @(sec, dsec) = $W.add_child(sec, $W.Text($S.text_of_chars(hi, 2), 2))
  val () = $W.diff_free(dsec)
  val () = $D.apply(doc, $W.AddChild($W.Generated(ib, 1), sec))
  (* a second text node after the first *)
  var bang = @[char][1]('!')
  val () = $D.apply(doc, $W.AddChild($W.Generated(id, 1), $W.Text($S.text_of_chars(bang, 1), 1)))
  (* <a id=e> with class, tabindex, title, href, target and a text child;
     then its tabindex removed and its href changed *)
  var ce = @[char][1]('e')
  val ie = $S.text_of_chars(ce, 1)
  var href = @[char][2]('/', 'x')
  var ttl = @[char][1]('t')
  var go = @[char][2]('g', 'o')
  val a = $W.Element($W.ElementNode($W.Generated(ie, 1),
    $W.Normal($W.A($S.text_of_chars(href, 2), 2, $W.TargetIs($W.Blank()))),
    $W.ClassIdx(0), false, $W.SomeInt(2), $W.SomeStr($S.text_of_chars(ttl, 1), 1),
    $W.WCons($W.Text($S.text_of_chars(go, 2), 2), $W.WNil())))
  val @(root, d6) = $W.add_child(root, a)
  val () = $D.apply(doc, d6)
  val () = $D.apply(doc, $W.SetTabindex($W.Generated(ie, 1), $W.NoneInt()))
  var href2 = @[char][2]('/', 'y')
  val () = $D.apply(doc, $W.SetAttribute($W.Generated(ie, 1), $W.SetHref($S.text_of_chars(href2, 2), 2)))
  (* <input id=f type=checkbox name=n checked>, then unchecked *)
  var cf = @[char][1]('f')
  val iff = $S.text_of_chars(cf, 1)
  var nm = @[char][1]('n')
  val inp = $W.Element($W.ElementNode($W.Generated(iff, 1),
    $W.Void($W.HtmlInput($W.InputCheckbox(), $W.SomeStr($S.text_of_chars(nm, 1), 1), $W.NoneStr(), false, true, false)),
    $W.NoClass(), false, $W.NoneInt(), $W.NoneStr(), $W.WNil()))
  val @(root, d7) = $W.add_child(root, inp)
  val () = $D.apply(doc, d7)
  val () = $W.widget_free(root)
  val () = $D.apply(doc, $W.SetAttribute($W.Generated(iff, 1), $W.SetInputChecked(false)))
  val () = $D.destroy(doc)
in end
