(* dom -- DOM diffing: accepts widget diffs, serializes to binary stream *)

#include "share/atspre_staload.hats"

#use array as A
#use arith as AR
#use builder as BU
#use css as C
#use str as S
#use widget as W
#use wasm.bats-packages.dev/bridge as B
staload "wasm.bats-packages.dev/bridge/src/dom.bats"

#pub stadef DOM_BUF_CAP = 262144

(* ============================================================
   Document: owns mount point, CSS rules, node ID counter
   ============================================================ *)

#pub datavtype document(l:addr) =
  | {l:agz}{nm:pos | nm < 256} doc_mk(l) of (
      $A.arr(byte, l, DOM_BUF_CAP),
      int,
      int,
      $A.text(nm),
      int nm
    )

vtypedef doc_vt(l:addr) = document(l)

(* ============================================================
   Public API
   ============================================================ *)

#pub fun create_document
  {nt:pos | nt < 256}{ni:pos | ni < 256}
  (mount_tag: $A.text(nt), tag_len: int nt,
   mount_id: $A.text(ni), id_len: int ni): [l:agz] document(l)

(* Open an existing document mount without emitting createElement.
   Use after the mount was already created by create_document.
   next_id is the node counter to resume from (from get_next_id). *)
#pub fun open_document
  {ni:pos | ni < 256}
  (mount_id: $A.text(ni), id_len: int ni,
   next_id: int): [l:agz] document(l)

(* Get the current node ID counter from a document.
   Use before destroy to save the counter for later open_document calls. *)
#pub fun get_next_id
  {l:agz}
  (doc: !document(l)): int

#pub fun apply
  {l:agz}
  (doc: !document(l), d: $W.diff): void

#pub fun apply_list
  {l:agz}
  (doc: !document(l), dl: $W.diff_list): void

#pub fun destroy
  {l:agz}
  (doc: document(l)): void

(* ============================================================
   Canvas API — emit canvas opcodes into the diff buffer
   ============================================================ *)

#pub fun canvas_fill_rect
  {l:agz}{li:agz}{ni:pos | ni < 65536}
  (doc: !document(l), node_id: !$A.borrow(byte, li, ni), id_len: int ni,
   x: int, y: int, w: int, h: int): void

#pub fun canvas_stroke_rect
  {l:agz}{li:agz}{ni:pos | ni < 65536}
  (doc: !document(l), node_id: !$A.borrow(byte, li, ni), id_len: int ni,
   x: int, y: int, w: int, h: int): void

#pub fun canvas_clear_rect
  {l:agz}{li:agz}{ni:pos | ni < 65536}
  (doc: !document(l), node_id: !$A.borrow(byte, li, ni), id_len: int ni,
   x: int, y: int, w: int, h: int): void

#pub fun canvas_begin_path
  {l:agz}{li:agz}{ni:pos | ni < 65536}
  (doc: !document(l), node_id: !$A.borrow(byte, li, ni), id_len: int ni): void

#pub fun canvas_move_to
  {l:agz}{li:agz}{ni:pos | ni < 65536}
  (doc: !document(l), node_id: !$A.borrow(byte, li, ni), id_len: int ni,
   x: int, y: int): void

#pub fun canvas_line_to
  {l:agz}{li:agz}{ni:pos | ni < 65536}
  (doc: !document(l), node_id: !$A.borrow(byte, li, ni), id_len: int ni,
   x: int, y: int): void

#pub fun canvas_arc
  {l:agz}{li:agz}{ni:pos | ni < 65536}
  (doc: !document(l), node_id: !$A.borrow(byte, li, ni), id_len: int ni,
   cx: int, cy: int, r: int,
   start1000: int, end1000: int, ccw: int): void

#pub fun canvas_close_path
  {l:agz}{li:agz}{ni:pos | ni < 65536}
  (doc: !document(l), node_id: !$A.borrow(byte, li, ni), id_len: int ni): void

#pub fun canvas_fill
  {l:agz}{li:agz}{ni:pos | ni < 65536}
  (doc: !document(l), node_id: !$A.borrow(byte, li, ni), id_len: int ni): void

#pub fun canvas_stroke
  {l:agz}{li:agz}{ni:pos | ni < 65536}
  (doc: !document(l), node_id: !$A.borrow(byte, li, ni), id_len: int ni): void

#pub fun canvas_fill_color
  {l:agz}{li:agz}{ni:pos | ni < 65536}
  (doc: !document(l), node_id: !$A.borrow(byte, li, ni), id_len: int ni,
   r: int, g: int, b: int, a: int): void

#pub fun canvas_stroke_color
  {l:agz}{li:agz}{ni:pos | ni < 65536}
  (doc: !document(l), node_id: !$A.borrow(byte, li, ni), id_len: int ni,
   r: int, g: int, b: int, a: int): void

#pub fun canvas_line_width
  {l:agz}{li:agz}{ni:pos | ni < 65536}
  (doc: !document(l), node_id: !$A.borrow(byte, li, ni), id_len: int ni,
   w100: int): void

#pub fun canvas_fill_text
  {l:agz}{li:agz}{ni:pos | ni < 65536}{tl:pos | tl < 65536}
  (doc: !document(l), node_id: !$A.borrow(byte, li, ni), id_len: int ni,
   x: int, y: int,
   text: $A.text(tl), text_len: int tl): void

#pub fun canvas_stroke_text
  {l:agz}{li:agz}{ni:pos | ni < 65536}{tl:pos | tl < 65536}
  (doc: !document(l), node_id: !$A.borrow(byte, li, ni), id_len: int ni,
   x: int, y: int,
   text: $A.text(tl), text_len: int tl): void

#pub fun canvas_set_font
  {l:agz}{li:agz}{ni:pos | ni < 65536}{fl:pos | fl < 65536}
  (doc: !document(l), node_id: !$A.borrow(byte, li, ni), id_len: int ni,
   font: $A.text(fl), font_len: int fl): void

#pub fun canvas_save
  {l:agz}{li:agz}{ni:pos | ni < 65536}
  (doc: !document(l), node_id: !$A.borrow(byte, li, ni), id_len: int ni): void

#pub fun canvas_restore
  {l:agz}{li:agz}{ni:pos | ni < 65536}
  (doc: !document(l), node_id: !$A.borrow(byte, li, ni), id_len: int ni): void

#pub fun canvas_translate
  {l:agz}{li:agz}{ni:pos | ni < 65536}
  (doc: !document(l), node_id: !$A.borrow(byte, li, ni), id_len: int ni,
   x: int, y: int): void

#pub fun canvas_rotate
  {l:agz}{li:agz}{ni:pos | ni < 65536}
  (doc: !document(l), node_id: !$A.borrow(byte, li, ni), id_len: int ni,
   angle1000: int): void

#pub fun canvas_scale
  {l:agz}{li:agz}{ni:pos | ni < 65536}
  (doc: !document(l), node_id: !$A.borrow(byte, li, ni), id_len: int ni,
   sx1000: int, sy1000: int): void

(* ============================================================
   Text constants: attribute and tag names (compile-time verified)
   ============================================================ *)

fn _txt_hidden(): $A.text(6) =
  let var c = @[char][6]('h', 'i', 'd', 'd', 'e', 'n') in $S.text_of_chars(c, 6) end
fn _txt_class(): $A.text(5) =
  let var c = @[char][5]('c', 'l', 'a', 's', 's') in $S.text_of_chars(c, 5) end
fn _txt_tabindex(): $A.text(8) =
  let var c = @[char][8]('t', 'a', 'b', 'i', 'n', 'd', 'e', 'x') in $S.text_of_chars(c, 8) end
fn _txt_title(): $A.text(5) =
  let var c = @[char][5]('t', 'i', 't', 'l', 'e') in $S.text_of_chars(c, 5) end
fn _txt_id(): $A.text(2) =
  let var c = @[char][2]('i', 'd') in $S.text_of_chars(c, 2) end
fn _txt_style(): $A.text(5) =
  let var c = @[char][5]('s', 't', 'y', 'l', 'e') in $S.text_of_chars(c, 5) end

fn _tag_div(): $A.text(3) =
  let var c = @[char][3]('d', 'i', 'v') in $S.text_of_chars(c, 3) end
fn _tag_span(): $A.text(4) =
  let var c = @[char][4]('s', 'p', 'a', 'n') in $S.text_of_chars(c, 4) end
fn _tag_p(): $A.text(1) =
  let var c = @[char][1]('p') in $S.text_of_chars(c, 1) end
fn _tag_br(): $A.text(2) =
  let var c = @[char][2]('b', 'r') in $S.text_of_chars(c, 2) end
fn _tag_hr(): $A.text(2) =
  let var c = @[char][2]('h', 'r') in $S.text_of_chars(c, 2) end
fn _tag_ul(): $A.text(2) =
  let var c = @[char][2]('u', 'l') in $S.text_of_chars(c, 2) end
fn _tag_li(): $A.text(2) =
  let var c = @[char][2]('l', 'i') in $S.text_of_chars(c, 2) end
fn _tag_a(): $A.text(1) =
  let var c = @[char][1]('a') in $S.text_of_chars(c, 1) end
fn _tag_img(): $A.text(3) =
  let var c = @[char][3]('i', 'm', 'g') in $S.text_of_chars(c, 3) end
fn _tag_input(): $A.text(5) =
  let var c = @[char][5]('i', 'n', 'p', 'u', 't') in $S.text_of_chars(c, 5) end

fn _tag_style(): $A.text(5) =
  let var c = @[char][5]('s', 't', 'y', 'l', 'e') in $S.text_of_chars(c, 5) end
fn _tag_h1(): $A.text(2) =
  let var c = @[char][2]('h', '1') in $S.text_of_chars(c, 2) end
fn _tag_h2(): $A.text(2) =
  let var c = @[char][2]('h', '2') in $S.text_of_chars(c, 2) end
fn _tag_h3(): $A.text(2) =
  let var c = @[char][2]('h', '3') in $S.text_of_chars(c, 2) end
fn _tag_h4(): $A.text(2) =
  let var c = @[char][2]('h', '4') in $S.text_of_chars(c, 2) end
fn _tag_h5(): $A.text(2) =
  let var c = @[char][2]('h', '5') in $S.text_of_chars(c, 2) end
fn _tag_h6(): $A.text(2) =
  let var c = @[char][2]('h', '6') in $S.text_of_chars(c, 2) end
fn _tag_section(): $A.text(7) =
  let var c = @[char][7]('s', 'e', 'c', 't', 'i', 'o', 'n') in $S.text_of_chars(c, 7) end
fn _tag_article(): $A.text(7) =
  let var c = @[char][7]('a', 'r', 't', 'i', 'c', 'l', 'e') in $S.text_of_chars(c, 7) end
fn _tag_header(): $A.text(6) =
  let var c = @[char][6]('h', 'e', 'a', 'd', 'e', 'r') in $S.text_of_chars(c, 6) end
fn _tag_footer(): $A.text(6) =
  let var c = @[char][6]('f', 'o', 'o', 't', 'e', 'r') in $S.text_of_chars(c, 6) end
fn _tag_main(): $A.text(4) =
  let var c = @[char][4]('m', 'a', 'i', 'n') in $S.text_of_chars(c, 4) end
fn _tag_nav(): $A.text(3) =
  let var c = @[char][3]('n', 'a', 'v') in $S.text_of_chars(c, 3) end
fn _tag_aside(): $A.text(5) =
  let var c = @[char][5]('a', 's', 'i', 'd', 'e') in $S.text_of_chars(c, 5) end
fn _tag_blockquote(): $A.text(10) =
  let var c = @[char][10]('b', 'l', 'o', 'c', 'k', 'q', 'u', 'o', 't', 'e') in $S.text_of_chars(c, 10) end
fn _tag_pre(): $A.text(3) =
  let var c = @[char][3]('p', 'r', 'e') in $S.text_of_chars(c, 3) end
fn _tag_code(): $A.text(4) =
  let var c = @[char][4]('c', 'o', 'd', 'e') in $S.text_of_chars(c, 4) end
fn _tag_figure(): $A.text(6) =
  let var c = @[char][6]('f', 'i', 'g', 'u', 'r', 'e') in $S.text_of_chars(c, 6) end
fn _tag_figcaption(): $A.text(10) =
  let var c = @[char][10]('f', 'i', 'g', 'c', 'a', 'p', 't', 'i', 'o', 'n') in $S.text_of_chars(c, 10) end
fn _tag_strong(): $A.text(6) =
  let var c = @[char][6]('s', 't', 'r', 'o', 'n', 'g') in $S.text_of_chars(c, 6) end
fn _tag_em(): $A.text(2) =
  let var c = @[char][2]('e', 'm') in $S.text_of_chars(c, 2) end
fn _tag_small(): $A.text(5) =
  let var c = @[char][5]('s', 'm', 'a', 'l', 'l') in $S.text_of_chars(c, 5) end
fn _tag_mark(): $A.text(4) =
  let var c = @[char][4]('m', 'a', 'r', 'k') in $S.text_of_chars(c, 4) end
fn _tag_del(): $A.text(3) =
  let var c = @[char][3]('d', 'e', 'l') in $S.text_of_chars(c, 3) end
fn _tag_ins(): $A.text(3) =
  let var c = @[char][3]('i', 'n', 's') in $S.text_of_chars(c, 3) end
fn _tag_sub(): $A.text(3) =
  let var c = @[char][3]('s', 'u', 'b') in $S.text_of_chars(c, 3) end
fn _tag_sup(): $A.text(3) =
  let var c = @[char][3]('s', 'u', 'p') in $S.text_of_chars(c, 3) end
fn _tag_ol(): $A.text(2) =
  let var c = @[char][2]('o', 'l') in $S.text_of_chars(c, 2) end
fn _tag_button(): $A.text(6) =
  let var c = @[char][6]('b', 'u', 't', 't', 'o', 'n') in $S.text_of_chars(c, 6) end
fn _tag_label(): $A.text(5) =
  let var c = @[char][5]('l', 'a', 'b', 'e', 'l') in $S.text_of_chars(c, 5) end
fn _tag_details(): $A.text(7) =
  let var c = @[char][7]('d', 'e', 't', 'a', 'i', 'l', 's') in $S.text_of_chars(c, 7) end
fn _tag_summary(): $A.text(7) =
  let var c = @[char][7]('s', 'u', 'm', 'm', 'a', 'r', 'y') in $S.text_of_chars(c, 7) end
fn _tag_form(): $A.text(4) =
  let var c = @[char][4]('f', 'o', 'r', 'm') in $S.text_of_chars(c, 4) end
fn _tag_fieldset(): $A.text(8) =
  let var c = @[char][8]('f', 'i', 'e', 'l', 'd', 's', 'e', 't') in $S.text_of_chars(c, 8) end
fn _tag_legend(): $A.text(6) =
  let var c = @[char][6]('l', 'e', 'g', 'e', 'n', 'd') in $S.text_of_chars(c, 6) end
fn _tag_select(): $A.text(6) =
  let var c = @[char][6]('s', 'e', 'l', 'e', 'c', 't') in $S.text_of_chars(c, 6) end
fn _tag_optgroup(): $A.text(8) =
  let var c = @[char][8]('o', 'p', 't', 'g', 'r', 'o', 'u', 'p') in $S.text_of_chars(c, 8) end
fn _tag_option(): $A.text(6) =
  let var c = @[char][6]('o', 'p', 't', 'i', 'o', 'n') in $S.text_of_chars(c, 6) end
fn _tag_textarea(): $A.text(8) =
  let var c = @[char][8]('t', 'e', 'x', 't', 'a', 'r', 'e', 'a') in $S.text_of_chars(c, 8) end
fn _tag_table(): $A.text(5) =
  let var c = @[char][5]('t', 'a', 'b', 'l', 'e') in $S.text_of_chars(c, 5) end
fn _tag_caption(): $A.text(7) =
  let var c = @[char][7]('c', 'a', 'p', 't', 'i', 'o', 'n') in $S.text_of_chars(c, 7) end
fn _tag_thead(): $A.text(5) =
  let var c = @[char][5]('t', 'h', 'e', 'a', 'd') in $S.text_of_chars(c, 5) end
fn _tag_tbody(): $A.text(5) =
  let var c = @[char][5]('t', 'b', 'o', 'd', 'y') in $S.text_of_chars(c, 5) end
fn _tag_tfoot(): $A.text(5) =
  let var c = @[char][5]('t', 'f', 'o', 'o', 't') in $S.text_of_chars(c, 5) end
fn _tag_tr(): $A.text(2) =
  let var c = @[char][2]('t', 'r') in $S.text_of_chars(c, 2) end
fn _tag_th(): $A.text(2) =
  let var c = @[char][2]('t', 'h') in $S.text_of_chars(c, 2) end
fn _tag_td(): $A.text(2) =
  let var c = @[char][2]('t', 'd') in $S.text_of_chars(c, 2) end
fn _tag_video(): $A.text(5) =
  let var c = @[char][5]('v', 'i', 'd', 'e', 'o') in $S.text_of_chars(c, 5) end
fn _tag_audio(): $A.text(5) =
  let var c = @[char][5]('a', 'u', 'd', 'i', 'o') in $S.text_of_chars(c, 5) end
fn _tag_picture(): $A.text(7) =
  let var c = @[char][7]('p', 'i', 'c', 't', 'u', 'r', 'e') in $S.text_of_chars(c, 7) end

fn _txt_type(): $A.text(4) =
  let var c = @[char][4]('t', 'y', 'p', 'e') in $S.text_of_chars(c, 4) end

fn _input_type_text(it: $W.input_type): [m:pos | m < 256] @($A.text(m), int m) =
  case+ it of
  | $W.InputText() => let var c = @[char][4]('t', 'e', 'x', 't') in @($S.text_of_chars(c, 4), 4) end
  | $W.InputPassword() => let var c = @[char][8]('p', 'a', 's', 's', 'w', 'o', 'r', 'd') in @($S.text_of_chars(c, 8), 8) end
  | $W.InputEmail() => let var c = @[char][5]('e', 'm', 'a', 'i', 'l') in @($S.text_of_chars(c, 5), 5) end
  | $W.InputNumber() => let var c = @[char][6]('n', 'u', 'm', 'b', 'e', 'r') in @($S.text_of_chars(c, 6), 6) end
  | $W.InputCheckbox() => let var c = @[char][8]('c', 'h', 'e', 'c', 'k', 'b', 'o', 'x') in @($S.text_of_chars(c, 8), 8) end
  | $W.InputRadio() => let var c = @[char][5]('r', 'a', 'd', 'i', 'o') in @($S.text_of_chars(c, 5), 5) end
  | $W.InputRange() => let var c = @[char][5]('r', 'a', 'n', 'g', 'e') in @($S.text_of_chars(c, 5), 5) end
  | $W.InputDate() => let var c = @[char][4]('d', 'a', 't', 'e') in @($S.text_of_chars(c, 4), 4) end
  | $W.InputTime() => let var c = @[char][4]('t', 'i', 'm', 'e') in @($S.text_of_chars(c, 4), 4) end
  | $W.InputDatetimeLocal() => let var c = @[char][14]('d', 'a', 't', 'e', 't', 'i', 'm', 'e', '-', 'l', 'o', 'c', 'a', 'l') in @($S.text_of_chars(c, 14), 14) end
  | $W.InputFile() => let var c = @[char][4]('f', 'i', 'l', 'e') in @($S.text_of_chars(c, 4), 4) end
  | $W.InputColor() => let var c = @[char][5]('c', 'o', 'l', 'o', 'r') in @($S.text_of_chars(c, 5), 5) end
  | $W.InputHidden() => let var c = @[char][6]('h', 'i', 'd', 'd', 'e', 'n') in @($S.text_of_chars(c, 6), 6) end
  | $W.InputSubmit() => let var c = @[char][6]('s', 'u', 'b', 'm', 'i', 't') in @($S.text_of_chars(c, 6), 6) end
  | $W.InputReset() => let var c = @[char][5]('r', 'e', 's', 'e', 't') in @($S.text_of_chars(c, 5), 5) end
  | $W.InputButton() => let var c = @[char][6]('b', 'u', 't', 't', 'o', 'n') in @($S.text_of_chars(c, 6), 6) end

fn _tag_default(): $A.text(3) = _tag_div()

fn _normal_tag(n: $W.html_normal): [m:pos | m < 256] @($A.text(m), int m) =
  case+ n of
  | $W.Div() => @(_tag_div(), 3)
  | $W.Span() => @(_tag_span(), 4)
  | $W.Section() => @(_tag_section(), 7)
  | $W.Article() => @(_tag_article(), 7)
  | $W.HtmlHeader() => @(_tag_header(), 6)
  | $W.HtmlFooter() => @(_tag_footer(), 6)
  | $W.HtmlMain() => @(_tag_main(), 4)
  | $W.Nav() => @(_tag_nav(), 3)
  | $W.Aside() => @(_tag_aside(), 5)
  | $W.H1() => @(_tag_h1(), 2)
  | $W.H2() => @(_tag_h2(), 2)
  | $W.H3() => @(_tag_h3(), 2)
  | $W.H4() => @(_tag_h4(), 2)
  | $W.H5() => @(_tag_h5(), 2)
  | $W.H6() => @(_tag_h6(), 2)
  | $W.P() => @(_tag_p(), 1)
  | $W.Blockquote() => @(_tag_blockquote(), 10)
  | $W.Pre() => @(_tag_pre(), 3)
  | $W.HtmlCode() => @(_tag_code(), 4)
  | $W.Figure() => @(_tag_figure(), 6)
  | $W.Figcaption() => @(_tag_figcaption(), 10)
  | $W.Strong() => @(_tag_strong(), 6)
  | $W.Em() => @(_tag_em(), 2)
  | $W.Small() => @(_tag_small(), 5)
  | $W.Mark() => @(_tag_mark(), 4)
  | $W.Del() => @(_tag_del(), 3)
  | $W.Ins() => @(_tag_ins(), 3)
  | $W.HtmlSub() => @(_tag_sub(), 3)
  | $W.Sup() => @(_tag_sup(), 3)
  | $W.Ul() => @(_tag_ul(), 2)
  | $W.Ol(_) => @(_tag_ol(), 2)
  | $W.Li() => @(_tag_li(), 2)
  | $W.A(_, _, _) => @(_tag_a(), 1)
  | $W.Button(_) => @(_tag_button(), 6)
  | $W.Label(_) => @(_tag_label(), 5)
  | $W.Details() => @(_tag_details(), 7)
  | $W.Summary() => @(_tag_summary(), 7)
  | $W.Form(_, _, _, _) => @(_tag_form(), 4)
  | $W.Fieldset() => @(_tag_fieldset(), 8)
  | $W.Legend() => @(_tag_legend(), 6)
  | $W.Select(_, _, _) => @(_tag_select(), 6)
  | $W.Optgroup(_, _) => @(_tag_optgroup(), 8)
  | $W.HtmlOption(_, _) => @(_tag_option(), 6)
  | $W.Textarea(_, _, _, _) => @(_tag_textarea(), 8)
  | $W.Table() => @(_tag_table(), 5)
  | $W.Caption() => @(_tag_caption(), 7)
  | $W.Thead() => @(_tag_thead(), 5)
  | $W.Tbody() => @(_tag_tbody(), 5)
  | $W.Tfoot() => @(_tag_tfoot(), 5)
  | $W.Tr() => @(_tag_tr(), 2)
  | $W.Th(_, _, _) => @(_tag_th(), 2)
  | $W.Td(_, _) => @(_tag_td(), 2)
  | $W.Video(_, _, _, _, _, _) => @(_tag_video(), 5)
  | $W.Audio(_, _, _, _, _, _) => @(_tag_audio(), 5)
  | $W.Picture() => @(_tag_picture(), 7)
  | $W.Style() => @(_tag_style(), 5)

fn _tag_wbr(): $A.text(3) =
  let var c = @[char][3]('w', 'b', 'r') in $S.text_of_chars(c, 3) end
fn _tag_source(): $A.text(6) =
  let var c = @[char][6]('s', 'o', 'u', 'r', 'c', 'e') in $S.text_of_chars(c, 6) end
fn _tag_track(): $A.text(5) =
  let var c = @[char][5]('t', 'r', 'a', 'c', 'k') in $S.text_of_chars(c, 5) end

fn _void_tag(v: $W.html_void): [m:pos | m < 256] @($A.text(m), int m) =
  case+ v of
  | $W.Br() => @(_tag_br(), 2)
  | $W.Hr() => @(_tag_hr(), 2)
  | $W.Wbr() => @(_tag_wbr(), 3)
  | $W.Img(_, _, _, _, _) => @(_tag_img(), 3)
  | $W.HtmlInput(_, _, _, _, _, _) => @(_tag_input(), 5)
  | $W.Source(_, _, _, _) => @(_tag_source(), 6)
  | $W.Track(_, _, _, _) => @(_tag_track(), 5)

(* ============================================================
   Internal: binary stream protocol
   ============================================================ *)

local

macdef _CAP = 262144

(* Unsigned right shift *)
fn _ushr(x: int, n: int): int =
  $AR.band_int_int($AR.bsr_int_int(x, n),
    $AR.sub_int_int($AR.bsl_int_int(1, $AR.sub_int_int(32, n)), 1))

(* ============================================================
   Safe g0-to-g1 nibble/byte conversion (same pattern as sha256)
   ============================================================ *)

fun _find_nibble {k:nat | k <= 16} .<16-k>.
  (target: int, k: int(k)): [r:nat | r < 16] int(r) =
  if $AR.gte_g1(k, 16) then 0
  else if $AR.eq_int_int(target, k) then k
  else _find_nibble(target, $AR.add_g1(k, 1))

fn _g1_byte(x: int): [v:nat | v < 256] int(v) = let
  val hi = _find_nibble($AR.band_int_int(_ushr(x, 4), 15), 0)
  val lo = _find_nibble($AR.band_int_int(x, 15), 0)
in $AR.add_g1($AR.mul_g1(hi, 16), lo) end

(* ============================================================
   Safe write helpers — replace array write_ functions
   ============================================================ *)

fn _wb {l:agz}{n:pos}{i:nat | i < n}{v:nat | v < 256}
  (buf: !$A.arr(byte, l, n), i: int(i), v: int(v)): void =
  $A.set<byte>(buf, i, $A.int2byte(v))

fn _wu16le {l:agz}{n:pos}{i:nat | i + 2 <= n}{v:nat | v < 65536}
  (buf: !$A.arr(byte, l, n), i: int(i), v: int(v)): void = let
  val lo = _g1_byte($AR.band_int_int(v, 255))
  val hi = _g1_byte($AR.band_int_int(_ushr(v, 8), 255))
  val () = $A.set<byte>(buf, i, $A.int2byte(lo))
  val () = $A.set<byte>(buf, $AR.add_g1(i, 1), $A.int2byte(hi))
in end

fn _wi32 {l:agz}{n:pos}{i:nat | i + 4 <= n}
  (buf: !$A.arr(byte, l, n), i: int(i), v: int): void = let
  val b0 = _g1_byte($AR.band_int_int(v, 255))
  val b1 = _g1_byte($AR.band_int_int(_ushr(v, 8), 255))
  val b2 = _g1_byte($AR.band_int_int(_ushr(v, 16), 255))
  val b3 = _g1_byte($AR.band_int_int(_ushr(v, 24), 255))
  val () = $A.set<byte>(buf, i, $A.int2byte(b0))
  val () = $A.set<byte>(buf, $AR.add_g1(i, 1), $A.int2byte(b1))
  val () = $A.set<byte>(buf, $AR.add_g1(i, 2), $A.int2byte(b2))
  val () = $A.set<byte>(buf, $AR.add_g1(i, 3), $A.int2byte(b3))
in end

fun _ctext {l:agz}{n:pos}{off:nat}{tl:nat | off + tl <= n}{k:nat | k <= tl} .<tl-k>.
  (buf: !$A.arr(byte, l, n), off: int(off), t: $A.text(tl), tl: int(tl), k: int(k)): void =
  if $AR.gte_g1(k, tl) then ()
  else let
    val b = $A.text_get(t, k)
    val () = $A.set<byte>(buf, $AR.add_g1(off, k), b)
  in _ctext(buf, off, t, tl, $AR.add_g1(k, 1)) end

fun _cborrow {ld:agz}{ls:agz}{n:pos}{m:pos}{off:nat | off + m <= n}{k:nat | k <= m} .<m-k>.
  (dst: !$A.arr(byte, ld, n), off: int(off),
   src: !$A.borrow(byte, ls, m), len: int(m), k: int(k)): void =
  if $AR.gte_g1(k, len) then ()
  else let
    val b = $A.read<byte>(src, k)
    val () = $A.set<byte>(dst, $AR.add_g1(off, k), b)
  in _cborrow(dst, off, src, len, $AR.add_g1(k, 1)) end

in

fn _flush_arr{l:agz}{m:nat | m <= DOM_BUF_CAP}
  (buf: !$A.arr(byte, l, DOM_BUF_CAP), len: int m): void =
  dom_flush(buf, len)

(* Refined auto_flush for canvas ops with compile-time sizes *)
fn _auto_flush
  {l:agz}{needed:pos | needed <= DOM_BUF_CAP}
  (doc: !doc_vt(l), needed: int needed)
  : [c:nat | c + needed <= DOM_BUF_CAP] int(c) = let
  val+ @doc_mk(buf, cursor, _, _, _) = doc
  val c0 = g1ofg0(cursor)
in
  if c0 < 0 then let
    val () = cursor := 0
    prval () = fold@(doc)
  in 0 end
  else if c0 > _CAP then let
    val () = cursor := 0
    prval () = fold@(doc)
  in 0 end
  else if c0 + needed > _CAP then let
    val () = _flush_arr(buf, c0)
    val () = cursor := 0
    prval () = fold@(doc)
  in 0 end
  else let
    prval () = fold@(doc)
  in c0 end
end

(* Inline flush: operates on unfolded buf+cursor, returns g1 cursor *)
fn _iflush
  {l:agz}{needed:pos | needed <= DOM_BUF_CAP}
  (buf: !$A.arr(byte, l, DOM_BUF_CAP), cursor: &int >> int, needed: int(needed))
  : [c:nat | c + needed <= DOM_BUF_CAP] int(c) = let
  val c0 = g1ofg0(cursor)
in
  if c0 < 0 then let
    val () = cursor := 0
  in 0 end
  else if c0 > _CAP then let
    val () = cursor := 0
  in 0 end
  else if c0 + needed > _CAP then let
    val () = if c0 > 0 then _flush_arr(buf, c0)
    val () = cursor := 0
  in 0 end
  else c0
end

(* ---- Node ID helpers ----
   Bridge JS wire format: [u16le str_len][string bytes]
   Root (id <= 0): uses mount_id
   Generated (id > 0): "b" + decimal digits *)

fn _digit_count(n: int): [m:int | 1 <= m; m <= 5] int m =
  if n < 10 then 1
  else if n < 100 then 2
  else if n < 1000 then 3
  else if n < 10000 then 4
  else 5

fun _write_digits_loop
  {l:agz}{base:nat}{dc:pos | dc <= 5; base + dc <= DOM_BUF_CAP}{p:int | p >= ~1; p < dc} .<p+1>.
  (buf: !$A.arr(byte, l, DOM_BUF_CAP), base: int(base), n: int, p: int(p), dc: int(dc)): void =
  if p < 0 then ()
  else let
    val d = _find_nibble($AR.band_int_int(n mod 10, 15), 0)
    val () = _wb(buf, $AR.add_g1(base, p), $AR.add_g1(d, 48))
  in _write_digits_loop(buf, base, n / 10, $AR.sub_g1(p, 1), dc) end

fn _write_nid_root
  {l:agz}{nm:pos | nm < 256}{off:nat | off + 2 + nm <= DOM_BUF_CAP}
  (buf: !$A.arr(byte, l, DOM_BUF_CAP), off: int(off),
   mid: $A.text(nm), mid_len: int nm): void = let
  val () = _wu16le(buf, off, mid_len)
  val () = _ctext(buf, $AR.add_g1(off, 2), mid, mid_len, 0)
in end

fn _write_nid_gen
  {l:agz}{off:nat}{dc:pos | dc <= 5; off + 3 + dc <= DOM_BUF_CAP}
  (buf: !$A.arr(byte, l, DOM_BUF_CAP), off: int(off),
   node_id: int, dc: int(dc)): void = let
  val slen = $AR.add_g1(1, dc)
  val () = _wu16le(buf, off, slen)
  val () = _wb(buf, $AR.add_g1(off, 2), 98)
  val () = _write_digits_loop(buf, $AR.add_g1(off, 3), node_id, $AR.sub_g1(dc, 1), dc)
in end

(* g1 dispatch: write [u16le len][bytes] for an int node_id, return bytes written *)
fn _write_nid_dispatch
  {l:agz}{off:nat | off + 258 <= DOM_BUF_CAP}
  {nm:pos | nm < 256}
  (buf: !$A.arr(byte, l, DOM_BUF_CAP), off: int(off),
   node_id: int, mid: $A.text(nm), midl: int(nm)): [sz:pos | sz <= 257] int(sz) =
  if node_id <= 0 then let
    val () = _write_nid_root(buf, off, mid, midl)
  in $AR.add_g1(2, midl) end
  else let
    val dc = _digit_count(node_id)
    val () = _write_nid_gen(buf, off, node_id, dc)
  in $AR.add_g1(3, dc) end

(* g1 dispatch: write [u16le len][bytes] for a widget_id, return bytes written *)
fn _write_wid_dispatch
  {l:agz}{off:nat | off + 258 <= DOM_BUF_CAP}
  {nm:pos | nm < 256}
  (buf: !$A.arr(byte, l, DOM_BUF_CAP), off: int(off),
   wid: $W.widget_id, mid: $A.text(nm), midl: int(nm)): [sz:pos | sz <= 257] int(sz) =
  case+ wid of
  | $W.Root() => let
      val () = _write_nid_root(buf, off, mid, midl)
    in $AR.add_g1(2, midl) end
  | $W.Generated(text, tlen) => let
      val () = _wu16le(buf, off, tlen)
      val () = _ctext(buf, $AR.add_g1(off, 2), text, tlen, 0)
    in $AR.add_g1(2, tlen) end

(* ---- DOM opcodes with int node IDs (used by create_document) ---- *)

(* Opcode 4: create_element
   Wire: [4][nid:str][pid:str][tag_len:u8][tag_bytes] *)
fn _emit_create_element
  {l:agz}{tl:pos | tl < 256}
  (doc: !doc_vt(l), node_id: int, parent_id: int,
   tag: $A.text(tl), tag_len: int tl): void = let
  val+ @doc_mk(buf, cursor, _, mid, midl) = doc
  val c = _iflush(buf, cursor, 771)
  val () = _wb(buf, c, 4)
  val off = $AR.add_g1(c, 1)
  val sz1 = _write_nid_dispatch(buf, off, node_id, mid, midl)
  val off = $AR.add_g1(off, sz1)
  val sz2 = _write_nid_dispatch(buf, off, parent_id, mid, midl)
  val off = $AR.add_g1(off, sz2)
  val () = _wb(buf, off, tag_len)
  val off = $AR.add_g1(off, 1)
  val () = _ctext(buf, off, tag, tag_len, 0)
  val () = cursor := g0ofg1($AR.add_g1(off, tag_len))
  prval () = fold@(doc)
in end

(* Opcode 2: set_attr with int node ID *)
fn _emit_set_attr
  {l:agz}{nl:pos | nl < 256}{vl:pos | vl < 65536}
  (doc: !doc_vt(l), node_id: int,
   attr_name: $A.text(nl), name_len: int nl,
   attr_value: $A.text(vl), value_len: int vl): void = let
  val+ @doc_mk(buf, cursor, _, mid, midl) = doc
  val c = _iflush(buf, cursor, 66051)
  val () = _wb(buf, c, 2)
  val off = $AR.add_g1(c, 1)
  val sz1 = _write_nid_dispatch(buf, off, node_id, mid, midl)
  val off = $AR.add_g1(off, sz1)
  val () = _wb(buf, off, name_len)
  val off = $AR.add_g1(off, 1)
  val () = _ctext(buf, off, attr_name, name_len, 0)
  val off = $AR.add_g1(off, name_len)
  val () = _wu16le(buf, off, value_len)
  val off = $AR.add_g1(off, 2)
  val () = _ctext(buf, off, attr_value, value_len, 0)
  val () = cursor := g0ofg1($AR.add_g1(off, value_len))
  prval () = fold@(doc)
in end

(* ---- DOM opcodes with widget_id ---- *)

(* Opcode 4: create_element with widget_id for node and parent *)
fn _emit_create_wid
  {l:agz}{tl:pos | tl < 256}
  (doc: !doc_vt(l), node_wid: $W.widget_id, parent_wid: $W.widget_id,
   tag: $A.text(tl), tag_len: int tl): void = let
  val+ @doc_mk(buf, cursor, _, mid, midl) = doc
  val c = _iflush(buf, cursor, 771)
  val () = _wb(buf, c, 4)
  val off = $AR.add_g1(c, 1)
  val sz1 = _write_wid_dispatch(buf, off, node_wid, mid, midl)
  val off = $AR.add_g1(off, sz1)
  val sz2 = _write_wid_dispatch(buf, off, parent_wid, mid, midl)
  val off = $AR.add_g1(off, sz2)
  val () = _wb(buf, off, tag_len)
  val off = $AR.add_g1(off, 1)
  val () = _ctext(buf, off, tag, tag_len, 0)
  val () = cursor := g0ofg1($AR.add_g1(off, tag_len))
  prval () = fold@(doc)
in end

(* Opcode 3: remove_children with widget_id *)
fn _emit_remove_children_wid
  {l:agz}
  (doc: !doc_vt(l), wid: $W.widget_id): void = let
  val+ @doc_mk(buf, cursor, _, mid, midl) = doc
  val c = _iflush(buf, cursor, 259)
  val () = _wb(buf, c, 3)
  val off = $AR.add_g1(c, 1)
  val sz = _write_wid_dispatch(buf, off, wid, mid, midl)
  val () = cursor := g0ofg1($AR.add_g1(off, sz))
  prval () = fold@(doc)
in end

(* Opcode 5: remove_child with widget_id *)
fn _emit_remove_child_wid
  {l:agz}
  (doc: !doc_vt(l), wid: $W.widget_id): void = let
  val+ @doc_mk(buf, cursor, _, mid, midl) = doc
  val c = _iflush(buf, cursor, 259)
  val () = _wb(buf, c, 5)
  val off = $AR.add_g1(c, 1)
  val sz = _write_wid_dispatch(buf, off, wid, mid, midl)
  val () = cursor := g0ofg1($AR.add_g1(off, sz))
  prval () = fold@(doc)
in end

(* Opcode 2: set_attr with empty value (boolean attr), widget_id *)
fn _emit_set_attr_empty_wid
  {l:agz}{nl:pos | nl < 256}
  (doc: !doc_vt(l), wid: $W.widget_id,
   attr_name: $A.text(nl), name_len: int nl): void = let
  val+ @doc_mk(buf, cursor, _, mid, midl) = doc
  val c = _iflush(buf, cursor, 516)
  val () = _wb(buf, c, 2)
  val off = $AR.add_g1(c, 1)
  val sz = _write_wid_dispatch(buf, off, wid, mid, midl)
  val off = $AR.add_g1(off, sz)
  val () = _wb(buf, off, name_len)
  val off = $AR.add_g1(off, 1)
  val () = _ctext(buf, off, attr_name, name_len, 0)
  val off = $AR.add_g1(off, name_len)
  val () = _wu16le(buf, off, 0)
  val () = cursor := g0ofg1($AR.add_g1(off, 2))
  prval () = fold@(doc)
in end

(* Opcode 7: remove_attr, widget_id *)
fn _emit_remove_attr_wid
  {l:agz}{nl:pos | nl < 256}
  (doc: !doc_vt(l), wid: $W.widget_id,
   attr_name: $A.text(nl), name_len: int nl): void = let
  val+ @doc_mk(buf, cursor, _, mid, midl) = doc
  val c = _iflush(buf, cursor, 514)
  val () = _wb(buf, c, 7)
  val off = $AR.add_g1(c, 1)
  val sz = _write_wid_dispatch(buf, off, wid, mid, midl)
  val off = $AR.add_g1(off, sz)
  val () = _wb(buf, off, name_len)
  val off = $AR.add_g1(off, 1)
  val () = _ctext(buf, off, attr_name, name_len, 0)
  val () = cursor := g0ofg1($AR.add_g1(off, name_len))
  prval () = fold@(doc)
in end

fn _flush{l:agz}(doc: !doc_vt(l)): void = let
  val+ @doc_mk(buf, cursor, _, _, _) = doc
  val c = g1ofg0(cursor)
  val () = if c > 0 then
    if c <= _CAP then _flush_arr(buf, c)
  val () = cursor := 0
  prval () = fold@(doc)
in end

(* ---- Text-based wire emission ---- *)

(* Opcode 2: SET_ATTR with text value, widget_id target *)
fn _emit_set_attr_text_wid{l:agz}{nl:pos | nl < 256}{vl:pos | vl < 256}
  (doc: !doc_vt(l), wid: $W.widget_id,
   attr_name: $A.text(nl), name_len: int nl,
   attr_val: $A.text(vl), val_len: int vl): void = let
  val+ @doc_mk(buf, cursor, _, mid, midl) = doc
  val c = _iflush(buf, cursor, 771)
  val () = _wb(buf, c, 2)
  val off = $AR.add_g1(c, 1)
  val sz = _write_wid_dispatch(buf, off, wid, mid, midl)
  val off = $AR.add_g1(off, sz)
  val () = _wb(buf, off, name_len)
  val off = $AR.add_g1(off, 1)
  val () = _ctext(buf, off, attr_name, name_len, 0)
  val off = $AR.add_g1(off, name_len)
  val () = _wu16le(buf, off, val_len)
  val off = $AR.add_g1(off, 2)
  val () = _ctext(buf, off, attr_val, val_len, 0)
  val () = cursor := g0ofg1($AR.add_g1(off, val_len))
  prval () = fold@(doc)
in end

(* Opcode 1: SET_TEXT with text value, widget_id target *)
fn _emit_set_text_text_wid{l:agz}{tl:pos | tl < 65536}
  (doc: !doc_vt(l), wid: $W.widget_id,
   t: $A.text(tl), tlen: int tl): void = let
  val+ @doc_mk(buf, cursor, _, mid, midl) = doc
  val c = _iflush(buf, cursor, 65795)
  val () = _wb(buf, c, 1)
  val off = $AR.add_g1(c, 1)
  val sz = _write_wid_dispatch(buf, off, wid, mid, midl)
  val off = $AR.add_g1(off, sz)
  val () = _wu16le(buf, off, tlen)
  val off = $AR.add_g1(off, 2)
  val () = _ctext(buf, off, t, tlen, 0)
  val () = cursor := g0ofg1($AR.add_g1(off, tlen))
  prval () = fold@(doc)
in end

(* Emit a widget using widget_id for wire IDs *)
fn _emit_widget
  {l:agz}
  (doc: !doc_vt(l), parent_wid: $W.widget_id, w: $W.widget): void =
  case+ w of
  | $W.Text(t, tlen) => _emit_set_text_text_wid(doc, parent_wid, t, tlen)
  | $W.Element($W.ElementNode(wid, top, _, hidden, _, _, _)) => let
      val @(tag, tlen) = (case+ top of
        | $W.Normal(n) => _normal_tag(n)
        | $W.Void(v) => _void_tag(v)
      ): [m:pos | m < 256] @($A.text(m), int m)
      val () = _emit_create_wid(doc, wid, parent_wid, tag, tlen)
      val () = (if hidden > 0 then _emit_set_attr_empty_wid(doc, wid, _txt_hidden(), 6) else ())
      val () = (case+ top of
        | $W.Void($W.HtmlInput(it, _, _, _, _, _)) => let
            val @(tv, tvl) = _input_type_text(it)
          in _emit_set_attr_text_wid(doc, wid, _txt_type(), 4, tv, tvl) end
        | _ => ())
    in end

(* ============================================================
   Implementations
   ============================================================ *)

implement create_document{nt}{ni}(mount_tag, tag_len, mount_id, id_len) = let
  val buf = $A.alloc<byte>(_CAP)
  val doc = doc_mk(buf, 0, 1, mount_id, id_len)
  val () = _emit_create_element(doc, 0, ~1, mount_tag, tag_len)
  val () = _emit_set_attr(doc, 0, _txt_id(), 2, mount_id, id_len)
  val () = _flush(doc)
in doc end

implement apply{l}(doc, d) = let
  val () = (case+ d of
  | $W.RemoveAllChildren(wid) =>
      _emit_remove_children_wid(doc, wid)
  | $W.AddChild(parent_wid, child) =>
      _emit_widget(doc, parent_wid, child)
  | $W.RemoveChild(_, child_wid) =>
      _emit_remove_child_wid(doc, child_wid)
  | $W.SetHidden(wid, h) =>
      if h > 0 then _emit_set_attr_empty_wid(doc, wid, _txt_hidden(), 6)
      else _emit_remove_attr_wid(doc, wid, _txt_hidden(), 6)
  | $W.SetClass(wid, _, cls_text, cls_len) => let
      val+ @doc_mk(buf, cursor, _, mid, midl) = doc
      val c = _iflush(buf, cursor, 521)
      val () = _wb(buf, c, 2)
      val off = $AR.add_g1(c, 1)
      val sz = _write_wid_dispatch(buf, off, wid, mid, midl)
      val off = $AR.add_g1(off, sz)
      val () = _wb(buf, off, 5)
      val off = $AR.add_g1(off, 1)
      val () = _ctext(buf, off, _txt_class(), 5, 0)
      val off = $AR.add_g1(off, 5)
      val () = _wu16le(buf, off, cls_len)
      val off = $AR.add_g1(off, 2)
      val () = _ctext(buf, off, cls_text, cls_len, 0)
      val () = cursor := g0ofg1($AR.add_g1(off, cls_len))
      prval () = fold@(doc)
    in end
  | $W.SetClassName(wid, cls, clen) =>
      _emit_set_attr_text_wid(doc, wid, _txt_class(), 5, cls, clen)
  | $W.SetTextContent(wid, text, tlen) =>
      _emit_set_text_text_wid(doc, wid, text, tlen)
  | $W.SetTabindex(_, _) => ()
  | $W.SetTitle(_, _) => ()
  | $W.SetAttribute(_, _) => ()
  )
in _flush(doc) end

implement apply_list{l}(doc, dl) =
  case+ dl of
  | $W.DLNil() => ()
  | $W.DLCons(d, rest) => let
      val () = apply(doc, d)
    in apply_list(doc, rest) end

implement destroy{l}(doc) = let
  val+ ~doc_mk(buf, _, _, _, _) = doc
in $A.free<byte>(buf) end

implement open_document{ni}(mount_id, id_len, next_id) = let
  val buf = $A.alloc<byte>(_CAP)
in doc_mk(buf, 0, next_id, mount_id, id_len) end

implement get_next_id{l}(doc) = let
  val+ @doc_mk(_, _, nid, _, _) = doc
  val r = nid
  prval () = fold@(doc)
in r end

(* ============================================================
   Canvas implementations — opcodes 64-84
   Wire format: [opcode:1][id_len:u16le:2][id_bytes:ni][...params...]
   ============================================================ *)

fn _write_canvas_id
  {l:agz}{cap:pos}{li:agz}{ni:pos | ni < 65536}
  {c:nat | c + 3 + ni <= cap}
  {v:nat | v < 256}
  (buf: !$A.arr(byte, l, cap), c: int c,
   opc: int v,
   node_id: !$A.borrow(byte, li, ni), id_len: int ni): int(c + 3 + ni) = let
  val () = _wb(buf, c, opc)
  val () = _wu16le(buf, c + 1, id_len)
  val () = _cborrow(buf, c + 3, node_id, id_len, 0)
in c + 3 + id_len end

fn _emit_canvas_str_op
  {l:agz}{li:agz}{ni:pos | ni < 65536}
  {v:nat | v < 256}
  (doc: !doc_vt(l), opc: int v,
   node_id: !$A.borrow(byte, li, ni), id_len: int ni): void = let
  val op_size = 3 + id_len
  val c = _auto_flush(doc, op_size)
  val+ @doc_mk(buf, cursor, _, _, _) = doc
  val _ = _write_canvas_id(buf, c, opc, node_id, id_len)
  val () = cursor := g0ofg1(c + op_size)
  prval () = fold@(doc)
in end

fn _emit_canvas_str_op_i32
  {l:agz}{li:agz}{ni:pos | ni < 65536}
  {v:nat | v < 256}
  (doc: !doc_vt(l), opc: int v,
   node_id: !$A.borrow(byte, li, ni), id_len: int ni,
   v0: int): void = let
  val op_size = 7 + id_len
  val c = _auto_flush(doc, op_size)
  val+ @doc_mk(buf, cursor, _, _, _) = doc
  val off = _write_canvas_id(buf, c, opc, node_id, id_len)
  val () = _wi32(buf, off, v0)
  val () = cursor := g0ofg1(c + op_size)
  prval () = fold@(doc)
in end

fn _emit_canvas_str_op_2i32
  {l:agz}{li:agz}{ni:pos | ni < 65536}
  {v:nat | v < 256}
  (doc: !doc_vt(l), opc: int v,
   node_id: !$A.borrow(byte, li, ni), id_len: int ni,
   v0: int, v1: int): void = let
  val op_size = 11 + id_len
  val c = _auto_flush(doc, op_size)
  val+ @doc_mk(buf, cursor, _, _, _) = doc
  val off = _write_canvas_id(buf, c, opc, node_id, id_len)
  val () = _wi32(buf, off, v0)
  val () = _wi32(buf, off + 4, v1)
  val () = cursor := g0ofg1(c + op_size)
  prval () = fold@(doc)
in end

fn _emit_canvas_str_op_4i32
  {l:agz}{li:agz}{ni:pos | ni < 65536}
  {v:nat | v < 256}
  (doc: !doc_vt(l), opc: int v,
   node_id: !$A.borrow(byte, li, ni), id_len: int ni,
   v0: int, v1: int, v2: int, v3: int): void = let
  val op_size = 19 + id_len
  val c = _auto_flush(doc, op_size)
  val+ @doc_mk(buf, cursor, _, _, _) = doc
  val off = _write_canvas_id(buf, c, opc, node_id, id_len)
  val () = _wi32(buf, off, v0)
  val () = _wi32(buf, off + 4, v1)
  val () = _wi32(buf, off + 8, v2)
  val () = _wi32(buf, off + 12, v3)
  val () = cursor := g0ofg1(c + op_size)
  prval () = fold@(doc)
in end

implement canvas_fill_rect{l}{li}{ni}(doc, node_id, id_len, x, y, w, h) =
  _emit_canvas_str_op_4i32(doc, 64, node_id, id_len, x, y, w, h)

implement canvas_stroke_rect{l}{li}{ni}(doc, node_id, id_len, x, y, w, h) =
  _emit_canvas_str_op_4i32(doc, 65, node_id, id_len, x, y, w, h)

implement canvas_clear_rect{l}{li}{ni}(doc, node_id, id_len, x, y, w, h) =
  _emit_canvas_str_op_4i32(doc, 66, node_id, id_len, x, y, w, h)

implement canvas_begin_path{l}{li}{ni}(doc, node_id, id_len) =
  _emit_canvas_str_op(doc, 67, node_id, id_len)

implement canvas_move_to{l}{li}{ni}(doc, node_id, id_len, x, y) =
  _emit_canvas_str_op_2i32(doc, 68, node_id, id_len, x, y)

implement canvas_line_to{l}{li}{ni}(doc, node_id, id_len, x, y) =
  _emit_canvas_str_op_2i32(doc, 69, node_id, id_len, x, y)

implement canvas_arc{l}{li}{ni}(doc, node_id, id_len, cx, cy, r, start1000, end1000, ccw) = let
  val op_size = 24 + id_len
  val c = _auto_flush(doc, op_size)
  val+ @doc_mk(buf, cursor, _, _, _) = doc
  val off = _write_canvas_id(buf, c, 70, node_id, id_len)
  val () = _wi32(buf, off, cx)
  val () = _wi32(buf, off + 4, cy)
  val () = _wi32(buf, off + 8, r)
  val () = _wi32(buf, off + 12, start1000)
  val () = _wi32(buf, off + 16, end1000)
  val () = if ccw > 0 then _wb(buf, off + 20, 1) else _wb(buf, off + 20, 0)
  val () = cursor := g0ofg1(c + op_size)
  prval () = fold@(doc)
in end

implement canvas_close_path{l}{li}{ni}(doc, node_id, id_len) =
  _emit_canvas_str_op(doc, 71, node_id, id_len)

implement canvas_fill{l}{li}{ni}(doc, node_id, id_len) =
  _emit_canvas_str_op(doc, 72, node_id, id_len)

implement canvas_stroke{l}{li}{ni}(doc, node_id, id_len) =
  _emit_canvas_str_op(doc, 73, node_id, id_len)

implement canvas_fill_color{l}{li}{ni}(doc, node_id, id_len, r, g, b0, a) = let
  val op_size = 7 + id_len
  val c = _auto_flush(doc, op_size)
  val+ @doc_mk(buf, cursor, _, _, _) = doc
  val off = _write_canvas_id(buf, c, 74, node_id, id_len)
  val () = _wb(buf, off, _g1_byte(r))
  val () = _wb(buf, off + 1, _g1_byte(g))
  val () = _wb(buf, off + 2, _g1_byte(b0))
  val () = _wb(buf, off + 3, _g1_byte(a))
  val () = cursor := g0ofg1(c + op_size)
  prval () = fold@(doc)
in end

implement canvas_stroke_color{l}{li}{ni}(doc, node_id, id_len, r, g, b0, a) = let
  val op_size = 7 + id_len
  val c = _auto_flush(doc, op_size)
  val+ @doc_mk(buf, cursor, _, _, _) = doc
  val off = _write_canvas_id(buf, c, 75, node_id, id_len)
  val () = _wb(buf, off, _g1_byte(r))
  val () = _wb(buf, off + 1, _g1_byte(g))
  val () = _wb(buf, off + 2, _g1_byte(b0))
  val () = _wb(buf, off + 3, _g1_byte(a))
  val () = cursor := g0ofg1(c + op_size)
  prval () = fold@(doc)
in end

implement canvas_line_width{l}{li}{ni}(doc, node_id, id_len, w100) =
  _emit_canvas_str_op_i32(doc, 76, node_id, id_len, w100)

implement canvas_fill_text{l}{li}{ni}{tl}(doc, node_id, id_len, x, y, text, text_len) = let
  val op_size = 13 + id_len + text_len
  val c = _auto_flush(doc, op_size)
  val+ @doc_mk(buf, cursor, _, _, _) = doc
  val off = _write_canvas_id(buf, c, 77, node_id, id_len)
  val () = _wi32(buf, off, x)
  val () = _wi32(buf, off + 4, y)
  val () = _wu16le(buf, off + 8, text_len)
  val () = _ctext(buf, off + 10, text, text_len, 0)
  val () = cursor := g0ofg1(c + op_size)
  prval () = fold@(doc)
in end

implement canvas_stroke_text{l}{li}{ni}{tl}(doc, node_id, id_len, x, y, text, text_len) = let
  val op_size = 13 + id_len + text_len
  val c = _auto_flush(doc, op_size)
  val+ @doc_mk(buf, cursor, _, _, _) = doc
  val off = _write_canvas_id(buf, c, 78, node_id, id_len)
  val () = _wi32(buf, off, x)
  val () = _wi32(buf, off + 4, y)
  val () = _wu16le(buf, off + 8, text_len)
  val () = _ctext(buf, off + 10, text, text_len, 0)
  val () = cursor := g0ofg1(c + op_size)
  prval () = fold@(doc)
in end

implement canvas_set_font{l}{li}{ni}{fl}(doc, node_id, id_len, font, font_len) = let
  val op_size = 5 + id_len + font_len
  val c = _auto_flush(doc, op_size)
  val+ @doc_mk(buf, cursor, _, _, _) = doc
  val off = _write_canvas_id(buf, c, 79, node_id, id_len)
  val () = _wu16le(buf, off, font_len)
  val () = _ctext(buf, off + 2, font, font_len, 0)
  val () = cursor := g0ofg1(c + op_size)
  prval () = fold@(doc)
in end

implement canvas_save{l}{li}{ni}(doc, node_id, id_len) =
  _emit_canvas_str_op(doc, 80, node_id, id_len)

implement canvas_restore{l}{li}{ni}(doc, node_id, id_len) =
  _emit_canvas_str_op(doc, 81, node_id, id_len)

implement canvas_translate{l}{li}{ni}(doc, node_id, id_len, x, y) =
  _emit_canvas_str_op_2i32(doc, 82, node_id, id_len, x, y)

implement canvas_rotate{l}{li}{ni}(doc, node_id, id_len, angle1000) =
  _emit_canvas_str_op_i32(doc, 83, node_id, id_len, angle1000)

implement canvas_scale{l}{li}{ni}(doc, node_id, id_len, sx1000, sy1000) =
  _emit_canvas_str_op_2i32(doc, 84, node_id, id_len, sx1000, sy1000)

end (* local *)
