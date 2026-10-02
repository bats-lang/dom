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

(* The cursor (second field) is the number of bytes queued in the
   buffer; its bound is part of the type. *)
#pub datavtype document(l:addr) =
  | {l:agz}{nm:pos | nm < 256}{c:nat | c <= DOM_BUF_CAP} doc_mk(l) of (
      $A.arr(byte, l, DOM_BUF_CAP),
      int c,
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
   Use after the mount was already created by create_document. *)
#pub fun open_document
  {ni:pos | ni < 256}
  (mount_id: $A.text(ni), id_len: int ni): [l:agz] document(l)

#pub fun apply
  {l:agz}
  (doc: !document(l), d: $W.diff): void

#pub fun apply_list
  {l:agz}
  (doc: !document(l), dl: $W.diff_list): void

(* Flushes what is still queued, then frees the document *)
#pub fun destroy
  {l:agz}
  (doc: document(l)): void

(* Element, text and attribute operations (and removing children) whose ids and text are held in
   borrows: they are copied into the buffer, and nothing is allocated
   (where a text built from bytes is allocated and never freed). Names
   are string literals. *)

(* A new element <tag id=id> as the last child of the element parent *)
#pub fun add_element
  {l:agz}{lp,li:agz}{np,ni:pos | np < 256; ni < 256}{tl:pos | tl < 256}
  (doc: !document(l), parent: !$A.borrow(byte, lp, np), plen: int np,
   id: !$A.borrow(byte, li, ni), ilen: int ni, tag: string tl): void

(* Removes every child of element id *)
#pub fun remove_children
  {l:agz}{li:agz}{ni:pos | ni < 256}
  (doc: !document(l), id: !$A.borrow(byte, li, ni), ilen: int ni): void

(* The text of element id: t[off, off + len) *)
#pub fun set_text
  {l:agz}{li,lt:agz}{ni:pos | ni < 256}{nt:pos}{o,k:nat | o + k <= nt; k < 65536}
  (doc: !document(l), id: !$A.borrow(byte, li, ni), ilen: int ni,
   t: !$A.borrow(byte, lt, nt), off: int o, len: int k): void

(* The attributes set_attr sets. None of them runs script:
   * none is an event handler (a name starting with "on", in any case):
     there is no constructor for one, and Aria and Data write "aria-"
     and "data-" before the rest of their name;
   * none takes a URL: href, src, action, formaction and xlink:href are
     url_attribute's, and only set_url and set_url_literal set them.
   The list is of what is allowed, not of what is not, so an attribute
   not thought of (srcdoc, poster, ping, ...) cannot be set at all *)
#pub datatype attribute =
  | Accept | Alt | Autocapitalize | Autocomplete | Checked | Class | Cols
  | Colspan | Dir | Disabled | Download | Draggable | Enterkeyhint | For
  | Height | Hidden | Inputmode | Lang | Loading | Max | Maxlength | Min
  | Minlength | Multiple | Name | Open | Pattern | Placeholder | Readonly
  | Rel | Required | Role | Rows | Rowspan | Selected | Size | Spellcheck
  | Step | Style | Tabindex | Target | Title | Translate | Type | Value
  | Width
  | {n:pos | n < 240} Aria of (string n)   (* aria-<the n bytes> *)
  | {n:pos | n < 240} Data of (string n)   (* data-<the n bytes> *)

(* Attribute name of element id: v[off, off + len) *)
#pub fun set_attr
  {l:agz}{li,lv:agz}{ni:pos | ni < 256}{nv:pos}{o,k:nat | o + k <= nv; k < 65536}
  (doc: !document(l), id: !$A.borrow(byte, li, ni), ilen: int ni, name: attribute,
   v: !$A.borrow(byte, lv, nv), off: int o, len: int k): void

(* The attributes whose value is a URL. A URL is put in one only by
   set_url (bytes checked as they are set) or set_url_literal (whose
   scheme is its constructor's), so a javascript: URL never is *)
#pub datatype url_attribute = Href | Src | Action | Formaction | XlinkHref

(* URL attribute name of element id: v[off, off + len), only when it is
   a URL that runs no script: an http:, https:, blob: or mailto: URL
   (the scheme in any case), a relative path or a fragment. It is
   refused when it starts with a control or a space (which the URL
   parser drops), or when a ':' comes before any '/', '?' or '#' and
   what is before it is not one of those schemes. The bytes are checked
   and copied in one call, so they cannot change in between. Whether it
   was set *)
#pub fun set_url
  {l:agz}{li,lv:agz}{ni:pos | ni < 256}{nv:pos}{o,k:nat | o + k <= nv; k < 65536}
  (doc: !document(l), id: !$A.borrow(byte, li, ni), ilen: int ni, name: url_attribute,
   v: !$A.borrow(byte, lv, nv), off: int o, len: int k): bool

(* A URL written in the code: its constructor writes its scheme (or its
   "#", or the "./" that makes it a path), so what follows cannot make
   it a javascript: URL. EmptyData is "data:," and nothing after it: an
   empty text, the usual placeholder for an image not yet given its
   source *)
#pub datatype url_literal =
  | {n:nat | n < 240} Https of (string n)     (* https://<the n bytes> *)
  | {n:nat | n < 240} Http of (string n)      (* http://<the n bytes> *)
  | {n:nat | n < 240} Mailto of (string n)    (* mailto:<the n bytes> *)
  | {n:nat | n < 240} Fragment of (string n)  (* #<the n bytes> *)
  | {n:nat | n < 240} Path of (string n)      (* ./<the n bytes> *)
  | EmptyData                                 (* data:, (nothing, as text) *)

(* URL attribute name of element id: the literal value *)
#pub fun set_url_literal
  {l:agz}{li:agz}{ni:pos | ni < 256}
  (doc: !document(l), id: !$A.borrow(byte, li, ni), ilen: int ni, name: url_attribute,
   value: url_literal): void

(* Removes URL attribute name of element id *)
#pub fun remove_url
  {l:agz}{li:agz}{ni:pos | ni < 256}
  (doc: !document(l), id: !$A.borrow(byte, li, ni), ilen: int ni, name: url_attribute): void

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
   start1000: int, end1000: int, ccw: bool): void

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
  {l:agz}{li:agz}{ni:pos | ni < 65536}{r,g,b,a:nat | r < 256; g < 256; b < 256; a < 256}
  (doc: !document(l), node_id: !$A.borrow(byte, li, ni), id_len: int ni,
   r: int r, g: int g, b: int b, a: int a): void

#pub fun canvas_stroke_color
  {l:agz}{li:agz}{ni:pos | ni < 65536}{r,g,b,a:nat | r < 256; g < 256; b < 256; a < 256}
  (doc: !document(l), node_id: !$A.borrow(byte, li, ni), id_len: int ni,
   r: int r, g: int g, b: int b, a: int a): void

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
  $A.text_lit("hidden")
fn _txt_class(): $A.text(5) =
  $A.text_lit("class")
fn _txt_tabindex(): $A.text(8) =
  $A.text_lit("tabindex")
fn _txt_title(): $A.text(5) =
  $A.text_lit("title")
fn _txt_id(): $A.text(2) =
  $A.text_lit("id")
fn _txt_style(): $A.text(5) =
  $A.text_lit("style")

fn _tag_div(): $A.text(3) =
  $A.text_lit("div")
fn _tag_span(): $A.text(4) =
  $A.text_lit("span")
fn _tag_p(): $A.text(1) =
  $A.text_lit("p")
fn _tag_br(): $A.text(2) =
  $A.text_lit("br")
fn _tag_hr(): $A.text(2) =
  $A.text_lit("hr")
fn _tag_ul(): $A.text(2) =
  $A.text_lit("ul")
fn _tag_li(): $A.text(2) =
  $A.text_lit("li")
fn _tag_a(): $A.text(1) =
  $A.text_lit("a")
fn _tag_img(): $A.text(3) =
  $A.text_lit("img")
fn _tag_input(): $A.text(5) =
  $A.text_lit("input")

fn _tag_style(): $A.text(5) =
  $A.text_lit("style")
fn _tag_h1(): $A.text(2) =
  $A.text_lit("h1")
fn _tag_h2(): $A.text(2) =
  $A.text_lit("h2")
fn _tag_h3(): $A.text(2) =
  $A.text_lit("h3")
fn _tag_h4(): $A.text(2) =
  $A.text_lit("h4")
fn _tag_h5(): $A.text(2) =
  $A.text_lit("h5")
fn _tag_h6(): $A.text(2) =
  $A.text_lit("h6")
fn _tag_section(): $A.text(7) =
  $A.text_lit("section")
fn _tag_article(): $A.text(7) =
  $A.text_lit("article")
fn _tag_header(): $A.text(6) =
  $A.text_lit("header")
fn _tag_footer(): $A.text(6) =
  $A.text_lit("footer")
fn _tag_main(): $A.text(4) =
  $A.text_lit("main")
fn _tag_nav(): $A.text(3) =
  $A.text_lit("nav")
fn _tag_aside(): $A.text(5) =
  $A.text_lit("aside")
fn _tag_blockquote(): $A.text(10) =
  $A.text_lit("blockquote")
fn _tag_pre(): $A.text(3) =
  $A.text_lit("pre")
fn _tag_code(): $A.text(4) =
  $A.text_lit("code")
fn _tag_figure(): $A.text(6) =
  $A.text_lit("figure")
fn _tag_figcaption(): $A.text(10) =
  $A.text_lit("figcaption")
fn _tag_strong(): $A.text(6) =
  $A.text_lit("strong")
fn _tag_em(): $A.text(2) =
  $A.text_lit("em")
fn _tag_small(): $A.text(5) =
  $A.text_lit("small")
fn _tag_mark(): $A.text(4) =
  $A.text_lit("mark")
fn _tag_del(): $A.text(3) =
  $A.text_lit("del")
fn _tag_ins(): $A.text(3) =
  $A.text_lit("ins")
fn _tag_sub(): $A.text(3) =
  $A.text_lit("sub")
fn _tag_sup(): $A.text(3) =
  $A.text_lit("sup")
fn _tag_ol(): $A.text(2) =
  $A.text_lit("ol")
fn _tag_button(): $A.text(6) =
  $A.text_lit("button")
fn _tag_label(): $A.text(5) =
  $A.text_lit("label")
fn _tag_details(): $A.text(7) =
  $A.text_lit("details")
fn _tag_summary(): $A.text(7) =
  $A.text_lit("summary")
fn _tag_form(): $A.text(4) =
  $A.text_lit("form")
fn _tag_fieldset(): $A.text(8) =
  $A.text_lit("fieldset")
fn _tag_legend(): $A.text(6) =
  $A.text_lit("legend")
fn _tag_select(): $A.text(6) =
  $A.text_lit("select")
fn _tag_optgroup(): $A.text(8) =
  $A.text_lit("optgroup")
fn _tag_option(): $A.text(6) =
  $A.text_lit("option")
fn _tag_textarea(): $A.text(8) =
  $A.text_lit("textarea")
fn _tag_table(): $A.text(5) =
  $A.text_lit("table")
fn _tag_caption(): $A.text(7) =
  $A.text_lit("caption")
fn _tag_thead(): $A.text(5) =
  $A.text_lit("thead")
fn _tag_tbody(): $A.text(5) =
  $A.text_lit("tbody")
fn _tag_tfoot(): $A.text(5) =
  $A.text_lit("tfoot")
fn _tag_tr(): $A.text(2) =
  $A.text_lit("tr")
fn _tag_th(): $A.text(2) =
  $A.text_lit("th")
fn _tag_td(): $A.text(2) =
  $A.text_lit("td")
fn _tag_video(): $A.text(5) =
  $A.text_lit("video")
fn _tag_audio(): $A.text(5) =
  $A.text_lit("audio")
fn _tag_picture(): $A.text(7) =
  $A.text_lit("picture")

fn _txt_type(): $A.text(4) =
  $A.text_lit("type")

fn _input_type_text(it: $W.input_type): [m:pos | m < 256] @($A.text(m), int m) =
  case+ it of
  | $W.InputText() => @($A.text_lit("text"), 4)
  | $W.InputPassword() => @($A.text_lit("password"), 8)
  | $W.InputEmail() => @($A.text_lit("email"), 5)
  | $W.InputNumber() => @($A.text_lit("number"), 6)
  | $W.InputCheckbox() => @($A.text_lit("checkbox"), 8)
  | $W.InputRadio() => @($A.text_lit("radio"), 5)
  | $W.InputRange() => @($A.text_lit("range"), 5)
  | $W.InputDate() => @($A.text_lit("date"), 4)
  | $W.InputTime() => @($A.text_lit("time"), 4)
  | $W.InputDatetimeLocal() => @($A.text_lit("datetime-local"), 14)
  | $W.InputFile() => @($A.text_lit("file"), 4)
  | $W.InputColor() => @($A.text_lit("color"), 5)
  | $W.InputHidden() => @($A.text_lit("hidden"), 6)
  | $W.InputSubmit() => @($A.text_lit("submit"), 6)
  | $W.InputReset() => @($A.text_lit("reset"), 5)
  | $W.InputButton() => @($A.text_lit("button"), 6)

fn _tag_default(): $A.text(3) = _tag_div()

fn _normal_tag(n: !$W.html_normal): [m:pos | m < 256] @($A.text(m), int m) =
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
  $A.text_lit("wbr")
fn _tag_source(): $A.text(6) =
  $A.text_lit("source")
fn _tag_track(): $A.text(5) =
  $A.text_lit("track")

fn _void_tag(v: !$W.html_void): [m:pos | m < 256] @($A.text(m), int m) =
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
   Safe write helpers — replace array write_ functions
   ============================================================ *)

fn _wb {l:agz}{n:pos}{i:nat | i < n}{v:nat | v < 256}
  (buf: !$A.arr(byte, l, n), i: int(i), v: int(v)): void =
  $A.set<byte>(buf, i, $A.int2byte(v))

fn _wu16le {l:agz}{n:pos}{i:nat | i + 2 <= n}{v:nat | v < 65536}
  (buf: !$A.arr(byte, l, n), i: int(i), v: int(v)): void = let
  val lo = $AR.low_byte(v)
  val hi = $AR.low_byte(_ushr(v, 8))
  val () = $A.set<byte>(buf, i, $A.int2byte(lo))
  val () = $A.set<byte>(buf, $AR.add_g1(i, 1), $A.int2byte(hi))
in end

fn _wi32 {l:agz}{n:pos}{i:nat | i + 4 <= n}
  (buf: !$A.arr(byte, l, n), i: int(i), v: int): void = let
  val b0 = $AR.low_byte(v)
  val b1 = $AR.low_byte(_ushr(v, 8))
  val b2 = $AR.low_byte(_ushr(v, 16))
  val b3 = $AR.low_byte(_ushr(v, 24))
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
  val+ @doc_mk(buf, cursor, _, _) = doc
  val c0 = cursor
in
  if c0 + needed > _CAP then let
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
  {l:agz}{c0:nat | c0 <= DOM_BUF_CAP}{needed:pos | needed <= DOM_BUF_CAP}
  (buf: !$A.arr(byte, l, DOM_BUF_CAP),
   cursor: &int(c0) >> [c1:nat | c1 <= DOM_BUF_CAP] int(c1), needed: int(needed))
  : [c:nat | c + needed <= DOM_BUF_CAP] int(c) = let
  val c = cursor
in
  if c + needed > _CAP then let
    val () = if c > 0 then _flush_arr(buf, c)
    val () = cursor := 0
  in 0 end
  else c
end

(* ---- Node ID helpers ----
   Bridge JS wire format: [u16le str_len][string bytes]
   Root (the empty id): the mount's id
   Generated: its text *)

fn _write_nid_root
  {l:agz}{nm:pos | nm < 256}{off:nat | off + 2 + nm <= DOM_BUF_CAP}
  (buf: !$A.arr(byte, l, DOM_BUF_CAP), off: int(off),
   mid: $A.text(nm), mid_len: int nm): void = let
  val () = _wu16le(buf, off, mid_len)
  val () = _ctext(buf, $AR.add_g1(off, 2), mid, mid_len, 0)
in end

(* g1 dispatch: write [u16le len][bytes] for a widget_id, return bytes written *)
fn _write_wid_dispatch
  {l:agz}{off:nat | off + 258 <= DOM_BUF_CAP}
  {nm:pos | nm < 256}
  (buf: !$A.arr(byte, l, DOM_BUF_CAP), off: int(off),
   wid: $W.widget_id, mid: $A.text(nm), midl: int(nm)): [sz:pos | sz <= 257] int(sz) =
  let
    val @(text, tlen) = wid
  in
    (* The empty id is the root's: the mount's id *)
    if tlen <= 0 then let
      val () = _write_nid_root(buf, off, mid, midl)
    in $AR.add_g1(2, midl) end
    else let
      val () = _wu16le(buf, off, tlen)
      val () = _ctext(buf, $AR.add_g1(off, 2), text, tlen, 0)
    in $AR.add_g1(2, tlen) end
  end

(* ---- DOM opcodes with int node IDs (used by create_document) ---- *)

(* ---- DOM opcodes with widget_id ---- *)

(* Opcode 4: create_element with widget_id for node and parent *)
fn _emit_create_wid
  {l:agz}{tl:pos | tl < 256}
  (doc: !doc_vt(l), node_wid: $W.widget_id, parent_wid: $W.widget_id,
   tag: $A.text(tl), tag_len: int tl): void = let
  val+ @doc_mk(buf, cursor, mid, midl) = doc
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
  val () = cursor := $AR.add_g1(off, tag_len)
  prval () = fold@(doc)
in end

(* Opcode 3: remove_children with widget_id *)
fn _emit_remove_children_wid
  {l:agz}
  (doc: !doc_vt(l), wid: $W.widget_id): void = let
  val+ @doc_mk(buf, cursor, mid, midl) = doc
  val c = _iflush(buf, cursor, 259)
  val () = _wb(buf, c, 3)
  val off = $AR.add_g1(c, 1)
  val sz = _write_wid_dispatch(buf, off, wid, mid, midl)
  val () = cursor := $AR.add_g1(off, sz)
  prval () = fold@(doc)
in end

(* Opcode 5: remove_child with widget_id *)
fn _emit_remove_child_wid
  {l:agz}
  (doc: !doc_vt(l), wid: $W.widget_id): void = let
  val+ @doc_mk(buf, cursor, mid, midl) = doc
  val c = _iflush(buf, cursor, 259)
  val () = _wb(buf, c, 5)
  val off = $AR.add_g1(c, 1)
  val sz = _write_wid_dispatch(buf, off, wid, mid, midl)
  val () = cursor := $AR.add_g1(off, sz)
  prval () = fold@(doc)
in end

(* Opcode 2: set_attr with empty value (boolean attr), widget_id *)
fn _emit_set_attr_empty_wid
  {l:agz}{nl:pos | nl < 256}
  (doc: !doc_vt(l), wid: $W.widget_id,
   attr_name: $A.text(nl), name_len: int nl): void = let
  val+ @doc_mk(buf, cursor, mid, midl) = doc
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
  val () = cursor := $AR.add_g1(off, 2)
  prval () = fold@(doc)
in end

(* Opcode 7: remove_attr, widget_id *)
fn _emit_remove_attr_wid
  {l:agz}{nl:pos | nl < 256}
  (doc: !doc_vt(l), wid: $W.widget_id,
   attr_name: $A.text(nl), name_len: int nl): void = let
  val+ @doc_mk(buf, cursor, mid, midl) = doc
  val c = _iflush(buf, cursor, 514)
  val () = _wb(buf, c, 7)
  val off = $AR.add_g1(c, 1)
  val sz = _write_wid_dispatch(buf, off, wid, mid, midl)
  val off = $AR.add_g1(off, sz)
  val () = _wb(buf, off, name_len)
  val off = $AR.add_g1(off, 1)
  val () = _ctext(buf, off, attr_name, name_len, 0)
  val () = cursor := $AR.add_g1(off, name_len)
  prval () = fold@(doc)
in end

fn _flush{l:agz}(doc: !doc_vt(l)): void = let
  val+ @doc_mk(buf, cursor, _, _) = doc
  val c = cursor
  val () = if c > 0 then _flush_arr(buf, c)
  val () = cursor := 0
  prval () = fold@(doc)
in end

(* ---- Text-based wire emission ---- *)

(* Opcode 2: SET_ATTR with text value, widget_id target *)
fn _emit_set_attr_text_wid{l:agz}{nl:pos | nl < 256}{vl:pos | vl < 256}
  (doc: !doc_vt(l), wid: $W.widget_id,
   attr_name: $A.text(nl), name_len: int nl,
   attr_val: $A.text(vl), val_len: int vl): void = let
  val+ @doc_mk(buf, cursor, mid, midl) = doc
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
  val () = cursor := $AR.add_g1(off, val_len)
  prval () = fold@(doc)
in end

(* Opcode o (1: SET_TEXT, the element's whole text; 6: APPEND_TEXT, a
   text node after its children), with text value, widget_id target *)
fn _emit_text_op_wid{l:agz}{o:nat | o < 256}{tl:pos | tl < 65536}
  (doc: !doc_vt(l), code: int o, wid: $W.widget_id,
   t: $A.text(tl), tlen: int tl): void = let
  val+ @doc_mk(buf, cursor, mid, midl) = doc
  val c = _iflush(buf, cursor, 65795)
  val () = _wb(buf, c, code)
  val off = $AR.add_g1(c, 1)
  val sz = _write_wid_dispatch(buf, off, wid, mid, midl)
  val off = $AR.add_g1(off, sz)
  val () = _wu16le(buf, off, tlen)
  val off = $AR.add_g1(off, 2)
  val () = _ctext(buf, off, t, tlen, 0)
  val () = cursor := $AR.add_g1(off, tlen)
  prval () = fold@(doc)
in end

fn _emit_set_text_text_wid{l:agz}{tl:pos | tl < 65536}
  (doc: !doc_vt(l), wid: $W.widget_id,
   t: $A.text(tl), tlen: int tl): void =
  _emit_text_op_wid(doc, 1, wid, t, tlen)

(* ---- URLs ---- *)

(* A URL read from its first byte, to know whether it can run script.
   UrlScheme(i, http, https, blob, mailto): i bytes read, none of them
   ':', '/', '?' or '#', and whether they are still the start of each
   scheme let through. A ':' then ends the scheme; a '/', '?' or '#'
   before any ':', or the end, means there is none (a relative path or
   a fragment) *)
datatype url_scan =
  | {i:nat} UrlScheme of (int i, bool, bool, bool, bool)
  | UrlLetThrough
  | UrlStopped

fn _lower_case (c: int): int = if c >= 65 && c <= 90 then c + 32 else c

(* Whether bytes 0 to i of a scheme were word's, given that bytes 0 to
   i - 1 were (still) and byte i is c *)
fn _still_word {i:nat}{n:nat} (word: string n, i: int i, c: int, still: bool): bool =
  if still then
    (if i < g1u2i(string1_length(word)) then char2int0(string_get_at(word, i)) = c else false)
  else false

fn _url_step (scan: url_scan, c: int): url_scan =
  case+ scan of
  | UrlScheme(i, http, https, blob, mailto) =>
      if c = 58 then (* ':' *)
        (if (http && i = 4) || (https && i = 5) || (blob && i = 4) || (mailto && i = 6)
         then UrlLetThrough() else UrlStopped())
      else if c = 47 || c = 63 || c = 35 then UrlLetThrough() (* '/', '?', '#' *)
      else if i = 0 && c <= 32 then UrlStopped() (* a control or a space first *)
      else let
        val lower = _lower_case(c)
      in UrlScheme(i + 1, _still_word("http", i, lower, http), _still_word("https", i, lower, https),
          _still_word("blob", i, lower, blob), _still_word("mailto", i, lower, mailto)) end
  | UrlLetThrough() => UrlLetThrough()
  | UrlStopped() => UrlStopped()

fn _url_start (): url_scan = UrlScheme(0, true, true, true, true)

fn _url_runs_no_script (scan: url_scan): bool =
  case+ scan of
  | UrlStopped() => false
  | _ => true

(* The scan of v[i, stop) *)
fun _scan_borrow {l:agz}{n:pos}{i,stop:nat | i <= stop; stop <= n} .<stop - i>.
  (v: !$A.borrow(byte, l, n), i: int i, stop: int stop, scan: url_scan): url_scan =
  if i >= stop then scan
  else case+ scan of
    | UrlScheme(_, _, _, _, _) => _scan_borrow(v, i + 1, stop, _url_step(scan, byte2int0($A.read<byte>(v, i))))
    | _ => scan

(* The scan of t[i, n) *)
fun _scan_text {n:nat}{i:nat | i <= n} .<n - i>.
  (t: $A.text(n), i: int i, n: int n, scan: url_scan): url_scan =
  if i >= n then scan
  else case+ scan of
    | UrlScheme(_, _, _, _, _) => _scan_text(t, i + 1, n, _url_step(scan, byte2int0($A.text_get(t, i))))
    | _ => scan

(* ---- Attributes named by string literals ---- *)

fun _cstr {l:agz}{n:pos}{off:nat}{sn:nat | off + sn <= n}{k:nat | k <= sn} .<sn-k>.
  (buf: !$A.arr(byte, l, n), off: int(off), s: string sn, sn: int(sn), k: int(k)): void =
  if $AR.gte_g1(k, sn) then ()
  else let
    val () = $A.set<byte>(buf, $AR.add_g1(off, k), $A.int2byte($AR.byte_of_char(string_get_at(s, k))))
  in _cstr(buf, off, s, sn, $AR.add_g1(k, 1)) end

(* Writes [op][node id][u8 name length][name] at c; the offset after it *)
fn _attr_head
  {l:agz}{c:nat | c + 514 <= DOM_BUF_CAP}{o:nat | o < 256}
  {nm:pos | nm < 256}{nl:pos | nl < 256}
  (buf: !$A.arr(byte, l, DOM_BUF_CAP), c: int c, code: int o,
   wid: $W.widget_id, mid: $A.text(nm), midl: int nm, name: string nl)
  : [r:nat | r <= c + 514] int r = let
  val () = _wb(buf, c, code)
  val off = $AR.add_g1(c, 1)
  val sz = _write_wid_dispatch(buf, off, wid, mid, midl)
  val off = $AR.add_g1(off, sz)
  val nlen = g1u2i(string1_length(name))
  val () = _wb(buf, off, nlen)
  val off = $AR.add_g1(off, 1)
  val () = _cstr(buf, off, name, nlen, 0)
in $AR.add_g1(off, nlen) end

(* Opcode 2: name = v (a literal; "" for a boolean attribute) *)
fn _attr_lit {l:agz}{nl:pos | nl < 256}{vl:nat | vl < 256}
  (doc: !doc_vt(l), wid: $W.widget_id, name: string nl, v: string vl): void = let
  val+ @doc_mk(buf, cursor, mid, midl) = doc
  val c = _iflush(buf, cursor, 771)
  val off = _attr_head(buf, c, 2, wid, mid, midl, name)
  val vlen = g1u2i(string1_length(v))
  val () = _wu16le(buf, off, vlen)
  val () = _cstr(buf, $AR.add_g1(off, 2), v, vlen, 0)
  val () = cursor := $AR.add_g1(off, 2 + vlen)
  prval () = fold@(doc)
in end

(* Opcode 2: name = t[0, n) *)
fn _attr_text {l:agz}{nl:pos | nl < 256}{vl:pos | vl < 256}
  (doc: !doc_vt(l), wid: $W.widget_id, name: string nl, t: $A.text(vl), vlen: int vl): void = let
  val+ @doc_mk(buf, cursor, mid, midl) = doc
  val c = _iflush(buf, cursor, 771)
  val off = _attr_head(buf, c, 2, wid, mid, midl, name)
  val () = _wu16le(buf, off, vlen)
  val () = _ctext(buf, $AR.add_g1(off, 2), t, vlen, 0)
  val () = cursor := $AR.add_g1(off, 2 + vlen)
  prval () = fold@(doc)
in end

(* Opcode 2: name = v's decimal digits *)
fn _attr_int {l:agz}{nl:pos | nl < 256}{v:int}
  (doc: !doc_vt(l), wid: $W.widget_id, name: string nl, v: int v): void = let
  val+ @doc_mk(buf, cursor, mid, midl) = doc
  val c = _iflush(buf, cursor, 771)
  val off = _attr_head(buf, c, 2, wid, mid, midl, name)
  val r = $S.int_to_str(buf, $AR.add_g1(off, 2), _CAP, v)
  val () = _wu16le(buf, off, r - off - 2)
  val () = cursor := r
  prval () = fold@(doc)
in end

(* Opcode 7: remove attribute name *)
fn _attr_unset {l:agz}{nl:pos | nl < 256}
  (doc: !doc_vt(l), wid: $W.widget_id, name: string nl): void = let
  val+ @doc_mk(buf, cursor, mid, midl) = doc
  val c = _iflush(buf, cursor, 771)
  val off = _attr_head(buf, c, 7, wid, mid, midl, name)
  val () = cursor := off
  prval () = fold@(doc)
in end

(* Opcode 2: the URL attribute name = t[0, n) when it runs no script
   (see set_url), else opcode 7: removed, so a URL refused also takes
   away the one it replaces *)
fn _attr_url_text {l:agz}{nl:pos | nl < 256}{vl:pos | vl < 256}
  (doc: !doc_vt(l), wid: $W.widget_id, name: string nl, t: $A.text(vl), vlen: int vl): void =
  if _url_runs_no_script(_scan_text(t, 0, vlen, _url_start())) then _attr_text(doc, wid, name, t, vlen)
  else _attr_unset(doc, wid, name)

(* A boolean attribute: present when b *)
fn _attr_bool {l:agz}{nl:pos | nl < 256}
  (doc: !doc_vt(l), wid: $W.widget_id, name: string nl, b: bool): void =
  if b then _attr_lit(doc, wid, name, "") else _attr_unset(doc, wid, name)

(* ---- Attribute values of widget's enumerations ---- *)

fn _ol_type_str (t: $W.ol_list_type): [n:pos | n < 256] string n =
  case+ t of
  | $W.OlDecimal() => "1" | $W.OlLowerAlpha() => "a" | $W.OlUpperAlpha() => "A"
  | $W.OlLowerRoman() => "i" | $W.OlUpperRoman() => "I"

fn _button_type_str (t: $W.button_type): [n:pos | n < 256] string n =
  case+ t of
  | $W.ButtonSubmit() => "submit" | $W.ButtonReset() => "reset" | $W.ButtonButton() => "button"

fn _method_str (m: $W.form_method): [n:pos | n < 256] string n =
  case+ m of
  | $W.FormGet() => "get" | $W.FormPost() => "post"

fn _enctype_str (e: $W.form_enctype): [n:pos | n < 256] string n =
  case+ e of
  | $W.EnctypeUrlencoded() => "application/x-www-form-urlencoded"
  | $W.EnctypeMultipart() => "multipart/form-data"
  | $W.EnctypePlain() => "text/plain"

fn _scope_str (s: $W.th_scope): [n:pos | n < 256] string n =
  case+ s of
  | $W.ScopeCol() => "col" | $W.ScopeRow() => "row"
  | $W.ScopeColgroup() => "colgroup" | $W.ScopeRowgroup() => "rowgroup"

fn _loading_str (x: $W.img_loading): [n:pos | n < 256] string n =
  case+ x of
  | $W.LoadingLazy() => "lazy" | $W.LoadingEager() => "eager"

fn _track_kind_str (k: $W.track_kind): [n:pos | n < 256] string n =
  case+ k of
  | $W.TrackSubtitles() => "subtitles" | $W.TrackCaptions() => "captions"
  | $W.TrackDescriptions() => "descriptions" | $W.TrackChapters() => "chapters"
  | $W.TrackMetadata() => "metadata"

fn _emit_target {l:agz} (doc: !doc_vt(l), wid: $W.widget_id, t: !$W.link_target): void =
  case+ t of
  | $W.Blank() => _attr_lit(doc, wid, "target", "_blank")
  | $W.Self_() => _attr_lit(doc, wid, "target", "_self")
  | $W.Parent_() => _attr_lit(doc, wid, "target", "_parent")
  | $W.Top_() => _attr_lit(doc, wid, "target", "_top")
  | $W.NamedTarget(t, n) => _attr_text(doc, wid, "target", t, n)

fn _emit_opt_str {l:agz}{nl:pos | nl < 256}
  (doc: !doc_vt(l), wid: $W.widget_id, name: string nl, o: !$W.option_str): void =
  case+ o of
  | $W.SomeStr(t, n) => _attr_text(doc, wid, name, t, n)
  | $W.NoneStr() => _attr_unset(doc, wid, name)

(* The attributes an element's type carries, set on a new element *)
fn _emit_top_attrs {l:agz} (doc: !doc_vt(l), wid: $W.widget_id, top: !$W.html_top): void =
  case+ top of
  | $W.Normal(n) => (case+ n of
    | $W.Ol($W.OlTypeIs(t)) => _attr_lit(doc, wid, "type", _ol_type_str(t))
    | $W.A(href, hl, target) => let
        val () = _attr_url_text(doc, wid, "href", href, hl)
      in case+ target of
        | $W.TargetIs(t) => _emit_target(doc, wid, t)
        | $W.NoTarget() => ()
      end
    | $W.Button(bt) => _attr_lit(doc, wid, "type", _button_type_str(bt))
    | $W.Label($W.SomeStr(t, tl)) => _attr_text(doc, wid, "for", t, tl)
    | $W.Form(action, al, m, e) => let
        val () = _attr_url_text(doc, wid, "action", action, al)
        val () = _attr_lit(doc, wid, "method", _method_str(m))
      in _attr_lit(doc, wid, "enctype", _enctype_str(e)) end
    | $W.Select(name, nl, multiple) => let
        val () = _attr_text(doc, wid, "name", name, nl)
      in if multiple then _attr_lit(doc, wid, "multiple", "") end
    | $W.Optgroup(label, ll) => _attr_text(doc, wid, "label", label, ll)
    | $W.HtmlOption(v, vl) => _attr_text(doc, wid, "value", v, vl)
    | $W.Textarea(name, nl, rows, cols) => let
        val () = _attr_text(doc, wid, "name", name, nl)
        val () = _attr_int(doc, wid, "rows", rows)
      in _attr_int(doc, wid, "cols", cols) end
    | $W.Th(cs, rs, scope) => let
        val () = if cs > 1 then _attr_int(doc, wid, "colspan", cs)
        val () = if rs > 1 then _attr_int(doc, wid, "rowspan", rs)
      in case+ scope of
        | $W.ScopeIs(sc) => _attr_lit(doc, wid, "scope", _scope_str(sc))
        | $W.NoScope() => ()
      end
    | $W.Td(cs, rs) => let
        val () = if cs > 1 then _attr_int(doc, wid, "colspan", cs)
      in if rs > 1 then _attr_int(doc, wid, "rowspan", rs) end
    | $W.Video(src, sl, controls, autoplay, loop, muted) => let
        val () = _attr_url_text(doc, wid, "src", src, sl)
        val () = if controls then _attr_lit(doc, wid, "controls", "")
        val () = if autoplay then _attr_lit(doc, wid, "autoplay", "")
        val () = if loop then _attr_lit(doc, wid, "loop", "")
      in if muted then _attr_lit(doc, wid, "muted", "") end
    | $W.Audio(src, sl, controls, autoplay, loop, muted) => let
        val () = _attr_url_text(doc, wid, "src", src, sl)
        val () = if controls then _attr_lit(doc, wid, "controls", "")
        val () = if autoplay then _attr_lit(doc, wid, "autoplay", "")
        val () = if loop then _attr_lit(doc, wid, "loop", "")
      in if muted then _attr_lit(doc, wid, "muted", "") end
    | _ => ())
  | $W.Void(v) => (case+ v of
    | $W.Img(src, sl, alt, al, loading) => let
        val () = _attr_url_text(doc, wid, "src", src, sl)
        val () = _attr_text(doc, wid, "alt", alt, al)
      in _attr_lit(doc, wid, "loading", _loading_str(loading)) end
    | $W.HtmlInput(it, name, value, disabled, checked, required) => let
        val @(tv, tvl) = _input_type_text(it)
        val () = _emit_set_attr_text_wid(doc, wid, _txt_type(), 4, tv, tvl)
        val () = (case+ name of
          | $W.SomeStr(t, n) => _attr_text(doc, wid, "name", t, n) | $W.NoneStr() => ())
        val () = (case+ value of
          | $W.SomeStr(t, n) => _attr_text(doc, wid, "value", t, n) | $W.NoneStr() => ())
        val () = if disabled then _attr_lit(doc, wid, "disabled", "")
        val () = if checked then _attr_lit(doc, wid, "checked", "")
      in if required then _attr_lit(doc, wid, "required", "") end
    | $W.Source(src, sl, t, tl) => let
        val () = _attr_url_text(doc, wid, "src", src, sl)
      in _attr_text(doc, wid, "type", t, tl) end
    | $W.Track(src, sl, kind, srclang) => let
        val () = _attr_url_text(doc, wid, "src", src, sl)
        val () = _attr_lit(doc, wid, "kind", _track_kind_str(kind))
      in case+ srclang of
        | $W.SomeStr(t, n) => _attr_text(doc, wid, "srclang", t, n)
        | $W.NoneStr() => ()
      end
    | _ => ())

(* A SetAttribute diff *)
fn _emit_attr_change {l:agz} (doc: !doc_vt(l), wid: $W.widget_id, ch: $W.attribute_change): void =
  case+ ch of
  | ~$W.SetHref(t, n) => _attr_url_text(doc, wid, "href", t, n)
  | ~$W.SetATarget(o) => let
      val () = (case+ o of
        | $W.TargetIs(t) => _emit_target(doc, wid, t)
        | $W.NoTarget() => _attr_unset(doc, wid, "target"))
    in $W.target_opt_free(o) end
  | ~$W.SetButtonType(bt) => _attr_lit(doc, wid, "type", _button_type_str(bt))
  | ~$W.SetButtonDisabled(b) => _attr_bool(doc, wid, "disabled", b)
  | ~$W.SetFormAction(t, n) => _attr_url_text(doc, wid, "action", t, n)
  | ~$W.SetFormMethod(m) => _attr_lit(doc, wid, "method", _method_str(m))
  | ~$W.SetFormEnctype(e) => _attr_lit(doc, wid, "enctype", _enctype_str(e))
  | ~$W.SetSelectDisabled(b) => _attr_bool(doc, wid, "disabled", b)
  | ~$W.SetSelectMultiple(b) => _attr_bool(doc, wid, "multiple", b)
  | ~$W.SetOptionValue(t, n) => _attr_text(doc, wid, "value", t, n)
  | ~$W.SetOptionDisabled(b) => _attr_bool(doc, wid, "disabled", b)
  | ~$W.SetOptionSelected(b) => _attr_bool(doc, wid, "selected", b)
  (* a textarea's value is its text *)
  | ~$W.SetTextareaValue(t, n) => _emit_text_op_wid(doc, 1, wid, t, n)
  | ~$W.SetTextareaDisabled(b) => _attr_bool(doc, wid, "disabled", b)
  | ~$W.SetTextareaReadonly(b) => _attr_bool(doc, wid, "readonly", b)
  | ~$W.SetTextareaRows(r) => _attr_int(doc, wid, "rows", r)
  | ~$W.SetTextareaCols(c) => _attr_int(doc, wid, "cols", c)
  | ~$W.SetColspan(c) => _attr_int(doc, wid, "colspan", c)
  | ~$W.SetRowspan(r) => _attr_int(doc, wid, "rowspan", r)
  | ~$W.SetThScope(o) => let
      val () = (case+ o of
        | $W.ScopeIs(sc) => _attr_lit(doc, wid, "scope", _scope_str(sc))
        | $W.NoScope() => _attr_unset(doc, wid, "scope"))
    in $W.scope_opt_free(o) end
  | ~$W.SetImgSrc(t, n) => _attr_url_text(doc, wid, "src", t, n)
  | ~$W.SetImgAlt(t, n) => _attr_text(doc, wid, "alt", t, n)
  | ~$W.SetImgLoading(x) => _attr_lit(doc, wid, "loading", _loading_str(x))
  | ~$W.SetInputType(it) => let
      val @(tv, tvl) = _input_type_text(it)
    in _emit_set_attr_text_wid(doc, wid, _txt_type(), 4, tv, tvl) end
  | ~$W.SetInputName(o) => let
      val () = _emit_opt_str(doc, wid, "name", o)
    in $W.option_str_free(o) end
  | ~$W.SetInputValue(o) => let
      val () = _emit_opt_str(doc, wid, "value", o)
    in $W.option_str_free(o) end
  | ~$W.SetInputDisabled(b) => _attr_bool(doc, wid, "disabled", b)
  | ~$W.SetInputChecked(b) => _attr_bool(doc, wid, "checked", b)
  | ~$W.SetInputRequired(b) => _attr_bool(doc, wid, "required", b)
  | ~$W.SetInputReadonly(b) => _attr_bool(doc, wid, "readonly", b)
  | ~$W.SetDetailsOpen(b) => _attr_bool(doc, wid, "open", b)

(* Emits w, a new child of parent_wid, with everything under it: an
   element with its class, attributes and children, a text as a text node
   after the parent's children. The walk terminates on w's size. *)
fun _emit_node {l:agz}{s:pos} .<s, 0>.
  (doc: !doc_vt(l), parent_wid: $W.widget_id, w: !$W.widget_sz(s)): void =
  case+ w of
  | $W.Text(t, tlen) => _emit_text_op_wid(doc, 6, parent_wid, t, tlen)
  | $W.Element(en) => (case+ en of
    | $W.ElementNode(wid, top, cls, hidden, ti, title, kids) => let
      val @(tag, tlen) = (case+ top of
        | $W.Normal(n) => _normal_tag(n)
        | $W.Void(v) => _void_tag(v)
      ): [m:pos | m < 256] @($A.text(m), int m)
      val () = _emit_create_wid(doc, wid, parent_wid, tag, tlen)
      val () = (case+ cls of
        | $W.ClassIdx(i) => let
            val @(ct, cl) = $C.class_text(i)
          in _emit_set_attr_text_wid(doc, wid, _txt_class(), 5, ct, cl) end
        | $W.NoClass() => ())
      val () = if hidden then _emit_set_attr_empty_wid(doc, wid, _txt_hidden(), 6)
      val () = (case+ ti of
        | $W.SomeInt(v) => _attr_int(doc, wid, "tabindex", v)
        | $W.NoneInt() => ())
      val () = (case+ title of
        | $W.SomeStr(t, n) => _attr_text(doc, wid, "title", t, n)
        | $W.NoneStr() => ())
      val () = _emit_top_attrs(doc, wid, top)
    in _emit_kids(doc, wid, kids) end)

and _emit_kids {l:agz}{k,s:nat} .<s, 1>.
  (doc: !doc_vt(l), parent_wid: $W.widget_id, kids: !$W.widget_list(k, s)): void =
  case+ kids of
  | $W.WNil() => ()
  | $W.WCons(w, rest) => let
      val () = _emit_node(doc, parent_wid, w)
    in _emit_kids(doc, parent_wid, rest) end

fn _emit_widget
  {l:agz}
  (doc: !doc_vt(l), parent_wid: $W.widget_id, w: !$W.widget): void =
  _emit_node(doc, parent_wid, w)

(* ============================================================
   Implementations
   ============================================================ *)

implement create_document{nt}{ni}(mount_tag, tag_len, mount_id, id_len) = let
  val buf = $A.alloc<byte>(_CAP)
  val doc = doc_mk(buf, 0, mount_id, id_len)
  (* Clear the mount point before creating the root element.
     This removes any loading spinner or other pre-WASM content. *)
  val () = _emit_remove_children_wid(doc, $W.Root())
  val () = _emit_create_wid(doc, $W.Root(), $W.Root(), mount_tag, tag_len)
  val () = _emit_set_attr_text_wid(doc, $W.Root(), _txt_id(), 2, mount_id, id_len)
  val () = _flush(doc)
in doc end

implement apply{l}(doc, d) = let
  val () = (case+ d of
  | ~$W.RemoveAllChildren(wid) =>
      _emit_remove_children_wid(doc, wid)
  | ~$W.AddChild(parent_wid, child) => let
      val () = _emit_widget(doc, parent_wid, child)
    in $W.widget_free(child) end
  | ~$W.RemoveChild(_, child_wid) =>
      _emit_remove_child_wid(doc, child_wid)
  | ~$W.SetHidden(wid, h) =>
      if h then _emit_set_attr_empty_wid(doc, wid, _txt_hidden(), 6)
      else _emit_remove_attr_wid(doc, wid, _txt_hidden(), 6)
  | ~$W.SetClass(wid, _, cls_text, cls_len) => let
      val+ @doc_mk(buf, cursor, mid, midl) = doc
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
      val () = cursor := $AR.add_g1(off, cls_len)
      prval () = fold@(doc)
    in end
  | ~$W.SetClassName(wid, cls, clen) =>
      _emit_set_attr_text_wid(doc, wid, _txt_class(), 5, cls, clen)
  | ~$W.SetTextContent(wid, text, tlen) =>
      _emit_set_text_text_wid(doc, wid, text, tlen)
  | ~$W.SetTabindex(wid, ti) => let
      val () = (case+ ti of
        | $W.SomeInt(v) => _attr_int(doc, wid, "tabindex", v)
        | $W.NoneInt() => _attr_unset(doc, wid, "tabindex"))
    in $W.option_int_free(ti) end
  | ~$W.SetTitle(wid, t) => let
      val () = _emit_opt_str(doc, wid, "title", t)
    in $W.option_str_free(t) end
  | ~$W.SetAttribute(wid, ch) => _emit_attr_change(doc, wid, ch)
  )
in _flush(doc) end

implement apply_list{l}(doc, dl) = let
  fun loop {n:nat} .<n>. (doc: !document(l), dl: $W.diff_seq(n)): void =
    case+ dl of
    | ~$W.DLNil() => ()
    | ~$W.DLCons(d, rest) => let
        val () = apply(doc, d)
      in loop(doc, rest) end
in loop(doc, dl) end

(* What is still queued (the borrow operations, canvas operations) is
   flushed first *)
implement destroy{l}(doc) = let
  val () = _flush(doc)
  val+ ~doc_mk(buf, _, _, _) = doc
in $A.free<byte>(buf) end

implement open_document{ni}(mount_id, id_len) = let
  val buf = $A.alloc<byte>(_CAP)
in doc_mk(buf, 0, mount_id, id_len) end

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
  val+ @doc_mk(buf, cursor, _, _) = doc
  val _ = _write_canvas_id(buf, c, opc, node_id, id_len)
  val () = cursor := c + op_size
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
  val+ @doc_mk(buf, cursor, _, _) = doc
  val off = _write_canvas_id(buf, c, opc, node_id, id_len)
  val () = _wi32(buf, off, v0)
  val () = cursor := c + op_size
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
  val+ @doc_mk(buf, cursor, _, _) = doc
  val off = _write_canvas_id(buf, c, opc, node_id, id_len)
  val () = _wi32(buf, off, v0)
  val () = _wi32(buf, off + 4, v1)
  val () = cursor := c + op_size
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
  val+ @doc_mk(buf, cursor, _, _) = doc
  val off = _write_canvas_id(buf, c, opc, node_id, id_len)
  val () = _wi32(buf, off, v0)
  val () = _wi32(buf, off + 4, v1)
  val () = _wi32(buf, off + 8, v2)
  val () = _wi32(buf, off + 12, v3)
  val () = cursor := c + op_size
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
  val+ @doc_mk(buf, cursor, _, _) = doc
  val off = _write_canvas_id(buf, c, 70, node_id, id_len)
  val () = _wi32(buf, off, cx)
  val () = _wi32(buf, off + 4, cy)
  val () = _wi32(buf, off + 8, r)
  val () = _wi32(buf, off + 12, start1000)
  val () = _wi32(buf, off + 16, end1000)
  val () = _wb(buf, off + 20, (if ccw then 1 else 0): [v:nat | v < 2] int v)
  val () = cursor := c + op_size
  prval () = fold@(doc)
in end

implement canvas_close_path{l}{li}{ni}(doc, node_id, id_len) =
  _emit_canvas_str_op(doc, 71, node_id, id_len)

implement canvas_fill{l}{li}{ni}(doc, node_id, id_len) =
  _emit_canvas_str_op(doc, 72, node_id, id_len)

implement canvas_stroke{l}{li}{ni}(doc, node_id, id_len) =
  _emit_canvas_str_op(doc, 73, node_id, id_len)

implement canvas_fill_color{l}{li}{ni}{r,g,b,a}(doc, node_id, id_len, r, g, b0, a) = let
  val op_size = 7 + id_len
  val c = _auto_flush(doc, op_size)
  val+ @doc_mk(buf, cursor, _, _) = doc
  val off = _write_canvas_id(buf, c, 74, node_id, id_len)
  val () = _wb(buf, off, r)
  val () = _wb(buf, off + 1, g)
  val () = _wb(buf, off + 2, b0)
  val () = _wb(buf, off + 3, a)
  val () = cursor := c + op_size
  prval () = fold@(doc)
in end

implement canvas_stroke_color{l}{li}{ni}{r,g,b,a}(doc, node_id, id_len, r, g, b0, a) = let
  val op_size = 7 + id_len
  val c = _auto_flush(doc, op_size)
  val+ @doc_mk(buf, cursor, _, _) = doc
  val off = _write_canvas_id(buf, c, 75, node_id, id_len)
  val () = _wb(buf, off, r)
  val () = _wb(buf, off + 1, g)
  val () = _wb(buf, off + 2, b0)
  val () = _wb(buf, off + 3, a)
  val () = cursor := c + op_size
  prval () = fold@(doc)
in end

implement canvas_line_width{l}{li}{ni}(doc, node_id, id_len, w100) =
  _emit_canvas_str_op_i32(doc, 76, node_id, id_len, w100)

implement canvas_fill_text{l}{li}{ni}{tl}(doc, node_id, id_len, x, y, text, text_len) = let
  val op_size = 13 + id_len + text_len
  val c = _auto_flush(doc, op_size)
  val+ @doc_mk(buf, cursor, _, _) = doc
  val off = _write_canvas_id(buf, c, 77, node_id, id_len)
  val () = _wi32(buf, off, x)
  val () = _wi32(buf, off + 4, y)
  val () = _wu16le(buf, off + 8, text_len)
  val () = _ctext(buf, off + 10, text, text_len, 0)
  val () = cursor := c + op_size
  prval () = fold@(doc)
in end

implement canvas_stroke_text{l}{li}{ni}{tl}(doc, node_id, id_len, x, y, text, text_len) = let
  val op_size = 13 + id_len + text_len
  val c = _auto_flush(doc, op_size)
  val+ @doc_mk(buf, cursor, _, _) = doc
  val off = _write_canvas_id(buf, c, 78, node_id, id_len)
  val () = _wi32(buf, off, x)
  val () = _wi32(buf, off + 4, y)
  val () = _wu16le(buf, off + 8, text_len)
  val () = _ctext(buf, off + 10, text, text_len, 0)
  val () = cursor := c + op_size
  prval () = fold@(doc)
in end

implement canvas_set_font{l}{li}{ni}{fl}(doc, node_id, id_len, font, font_len) = let
  val op_size = 5 + id_len + font_len
  val c = _auto_flush(doc, op_size)
  val+ @doc_mk(buf, cursor, _, _) = doc
  val off = _write_canvas_id(buf, c, 79, node_id, id_len)
  val () = _wu16le(buf, off, font_len)
  val () = _ctext(buf, off + 2, font, font_len, 0)
  val () = cursor := c + op_size
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


(* src[o, o + k) to dst[off, off + k) *)
fun _cregion {ld,ls:agz}{n,m:pos}{off,o,k:nat | o + k <= m; off + k <= n}{i:nat | i <= k} .<k - i>.
  (dst: !$A.arr(byte, ld, n), off: int off, src: !$A.borrow(byte, ls, m), o: int o, k: int k, i: int i): void =
  if i >= k then ()
  else let
    val () = $A.set<byte>(dst, off + i, $A.read<byte>(src, o + i))
  in _cregion(dst, off, src, o, k, i + 1) end

(* [u16 length][bytes] of the id b[0, n) at off; the offset after it *)
fn _wid_borrow {l:agz}{lb:agz}{n:pos | n < 256}{off:nat | off + 2 + n <= DOM_BUF_CAP}
  (buf: !$A.arr(byte, l, DOM_BUF_CAP), off: int off, b: !$A.borrow(byte, lb, n), n: int n): int(off + 2 + n) = let
  val () = _wu16le(buf, off, n)
  val () = _cborrow(buf, off + 2, b, n, 0)
in off + 2 + n end

implement add_element{l}{lp,li}{np,ni}{tl}(doc, parent, plen, id, ilen, tag) = let
  val+ @doc_mk(buf, cursor, _, _) = doc
  val c = _iflush(buf, cursor, 771)
  val () = _wb(buf, c, 4)
  val off = _wid_borrow(buf, c + 1, id, ilen)
  val off = _wid_borrow(buf, off, parent, plen)
  val tlen = g1u2i(string1_length(tag))
  val () = _wb(buf, off, tlen)
  val () = _cstr(buf, off + 1, tag, tlen, 0)
  val () = cursor := off + 1 + tlen
  prval () = fold@(doc)
in end

implement remove_children{l}{li}{ni}(doc, id, ilen) = let
  val+ @doc_mk(buf, cursor, _, _) = doc
  val c = _iflush(buf, cursor, 259)
  val () = _wb(buf, c, 3)
  val off = _wid_borrow(buf, c + 1, id, ilen)
  val () = cursor := off
  prval () = fold@(doc)
in end

implement set_text{l}{li,lt}{ni}{nt}{o,k}(doc, id, ilen, t, off0, len) = let
  val+ @doc_mk(buf, cursor, _, _) = doc
  val c = _iflush(buf, cursor, 65795)
  val () = _wb(buf, c, 1)
  val off = _wid_borrow(buf, c + 1, id, ilen)
  val () = _wu16le(buf, off, len)
  val () = _cregion(buf, off + 2, t, off0, len, 0)
  val () = cursor := off + 2 + len
  prval () = fold@(doc)
in end

(* ---- Attribute names ---- *)

(* An attribute's name is its word followed by its rest: the whole name
   for most, with an empty rest; "aria-" or "data-" for Aria and Data,
   followed by what they hold *)
fn _attribute_word (name: attribute): [n:pos | n < 16] string n =
  case+ name of
  | Accept() => "accept" | Alt() => "alt" | Autocapitalize() => "autocapitalize"
  | Autocomplete() => "autocomplete" | Checked() => "checked" | Class() => "class"
  | Cols() => "cols" | Colspan() => "colspan" | Dir() => "dir" | Disabled() => "disabled"
  | Download() => "download" | Draggable() => "draggable" | Enterkeyhint() => "enterkeyhint"
  | For() => "for" | Height() => "height" | Hidden() => "hidden" | Inputmode() => "inputmode"
  | Lang() => "lang" | Loading() => "loading" | Max() => "max" | Maxlength() => "maxlength"
  | Min() => "min" | Minlength() => "minlength" | Multiple() => "multiple" | Name() => "name"
  | Open() => "open" | Pattern() => "pattern" | Placeholder() => "placeholder"
  | Readonly() => "readonly" | Rel() => "rel" | Required() => "required" | Role() => "role"
  | Rows() => "rows" | Rowspan() => "rowspan" | Selected() => "selected" | Size() => "size"
  | Spellcheck() => "spellcheck" | Step() => "step" | Style() => "style"
  | Tabindex() => "tabindex" | Target() => "target" | Title() => "title"
  | Translate() => "translate" | Type() => "type" | Value() => "value" | Width() => "width"
  | Aria(_) => "aria-" | Data(_) => "data-"

fn _attribute_rest (name: attribute): [n:nat | n < 240] string n =
  case+ name of
  | Aria(rest) => rest
  | Data(rest) => rest
  | _ => ""

fn _url_attribute_word (name: url_attribute): [n:pos | n < 16] string n =
  case+ name of
  | Href() => "href" | Src() => "src" | Action() => "action"
  | Formaction() => "formaction" | XlinkHref() => "xlink:href"

(* [u8 length][word][rest] at off; the offset after it *)
fn _write_name
  {l:agz}{off:nat | off + 266 <= DOM_BUF_CAP}{nw:pos | nw < 16}{nr:nat | nr < 240}
  (buf: !$A.arr(byte, l, DOM_BUF_CAP), off: int off, word: string nw, rest: string nr)
  : int(off + 1 + nw + nr) = let
  val word_len = g1u2i(string1_length(word))
  val rest_len = g1u2i(string1_length(rest))
  val () = _wb(buf, off, word_len + rest_len)
  val () = _cstr(buf, off + 1, word, word_len, 0)
  val () = _cstr(buf, off + 1 + word_len, rest, rest_len, 0)
in off + 1 + word_len + rest_len end

fn _url_literal_word (value: url_literal): [n:pos | n < 16] string n =
  case+ value of
  | Https(_) => "https://" | Http(_) => "http://" | Mailto(_) => "mailto:"
  | Fragment(_) => "#" | Path(_) => "./" | EmptyData() => "data:,"

fn _url_literal_rest (value: url_literal): [n:nat | n < 240] string n =
  case+ value of
  | Https(rest) => rest | Http(rest) => rest | Mailto(rest) => rest
  | Fragment(rest) => rest | Path(rest) => rest | EmptyData() => ""

implement set_attr{l}{li,lv}{ni}{nv}{o,k}(doc, id, ilen, name, v, off0, len) = let
  val+ @doc_mk(buf, cursor, _, _) = doc
  val c = _iflush(buf, cursor, 66051)
  val () = _wb(buf, c, 2)
  val off = _wid_borrow(buf, c + 1, id, ilen)
  val off = _write_name(buf, off, _attribute_word(name), _attribute_rest(name))
  val () = _wu16le(buf, off, len)
  val () = _cregion(buf, off + 2, v, off0, len, 0)
  val () = cursor := off + 2 + len
  prval () = fold@(doc)
in end

implement set_url{l}{li,lv}{ni}{nv}{o,k}(doc, id, ilen, name, v, off0, len) =
  if _url_runs_no_script(_scan_borrow(v, off0, off0 + len, _url_start())) then let
    val+ @doc_mk(buf, cursor, _, _) = doc
    val c = _iflush(buf, cursor, 66051)
    val () = _wb(buf, c, 2)
    val off = _wid_borrow(buf, c + 1, id, ilen)
    val off = _write_name(buf, off, _url_attribute_word(name), "")
    val () = _wu16le(buf, off, len)
    val () = _cregion(buf, off + 2, v, off0, len, 0)
    val () = cursor := off + 2 + len
    prval () = fold@(doc)
  in true end
  else false

implement set_url_literal{l}{li}{ni}(doc, id, ilen, name, value) = let
  val+ @doc_mk(buf, cursor, _, _) = doc
  val c = _iflush(buf, cursor, 66051)
  val () = _wb(buf, c, 2)
  val off = _wid_borrow(buf, c + 1, id, ilen)
  val off = _write_name(buf, off, _url_attribute_word(name), "")
  val word = _url_literal_word(value)
  val word_len = g1u2i(string1_length(word))
  val rest = _url_literal_rest(value)
  val rest_len = g1u2i(string1_length(rest))
  val () = _wu16le(buf, off, word_len + rest_len)
  val () = _cstr(buf, off + 2, word, word_len, 0)
  val () = _cstr(buf, off + 2 + word_len, rest, rest_len, 0)
  val () = cursor := off + 2 + word_len + rest_len
  prval () = fold@(doc)
in end

implement remove_url{l}{li}{ni}(doc, id, ilen, name) = let
  val+ @doc_mk(buf, cursor, _, _) = doc
  val c = _iflush(buf, cursor, 66051)
  val () = _wb(buf, c, 7)
  val off = _wid_borrow(buf, c + 1, id, ilen)
  val () = cursor := _write_name(buf, off, _url_attribute_word(name), "")
  prval () = fold@(doc)
in end

end (* local *)
