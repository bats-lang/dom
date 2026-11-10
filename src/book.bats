(* book -- a book chapter's document: the operations that join bridge's book
   stream (proofs of outside bytes) with css's trees of proved spans (keyword:
   book document; bats-lang/quire#1063, design sections 3.1 to 3.6, dom#126)

   A book_document holds bridge's book_stream and the OUTSIDE_READ(cs) proof
   of the one array of outside bytes (cs) its chapter is read from. Every
   operation that copies a span of bytes takes that array's cborrow, so a span
   of an array the program wrote does not type-check (book-container-unproved).
   The css trees are indexed by the same cells cs and the layout g, so a tree
   of another source (book-style-other-source) or another layout
   (book-layout-mismatch) does not type-check either. No operation takes bytes
   for a sheet or a style: they are written from the tree, as pieces (css's
   book_sheet_pieces and book_style_pieces), so book-sheet-from-bytes does not
   type-check.

   A book_container is the linear token of the container made at the chapter's
   root element: only a ROOT_TAG(cs, o), the proof that the bytes at o are
   <html followed by white space or >, makes one. Every operation on a book
   element borrows it, and close_book_container consumes it, so a container is
   not used after it is closed (book-container-after-close) and is not left
   open (book-container-leaked). The ordinary operations of a book document
   take a narrower attribute (book_attribute), which has no Style, and a data
   attribute name only with BOOK_DATA_FREE, the proof that it does not start
   with book- (bridge refuses data-book-* on every element, and no name of
   that form is written). *)

#include "share/atspre_staload.hats"

#use array as A

staload OU = "wasm.bats-packages.dev/bridge/src/outside.bats"
staload BC = "wasm.bats-packages.dev/bridge/src/book_content.bats"
staload CB = "css/src/book.sats"

(* ============================================================
   Public API
   ============================================================ *)

(* The bytes at o are <html and then white space or > (the start tag of the
   chapter's root). Made only from the byte of each position (NTH facts, which
   cborrow_get gives); a root written with a prefix has none *)
#pub dataprop ROOT_TAG(cs: $A.cells, o: int) =
  | {cs:$A.cells}{o:nat}{c:int | c == 9 || c == 10 || c == 12 || c == 13 || c == 32 || c == 62}
    ROOT_TAG_mk(cs, o) of
      ($A.NTH(cs, o, 60), $A.NTH(cs, o + 1, 104), $A.NTH(cs, o + 2, 116),
       $A.NTH(cs, o + 3, 109), $A.NTH(cs, o + 4, 108), $A.NTH(cs, o + 5, c))

#pub dataprop ROOT_TAG_OPT(cs: $A.cells, o: int, b: bool) =
  | {cs:$A.cells}{o:nat} ROOT_TAG_yes(cs, o, true) of ROOT_TAG(cs, o)
  | {cs:$A.cells}{o:nat} ROOT_TAG_no(cs, o, false)

(* Whether the n bytes of src hold a root tag at o, with the proof when they do *)
#pub fun book_root_scan {l:agz}{n:nat}{cs:$A.cells}{o:nat}
  (src: !$A.cborrow(l, n, cs), len: int n, at: int o): [b:bool] (ROOT_TAG_OPT(cs, o, b) | bool b)

(* The data attribute name of n bytes does not start with book- (in any case) *)
#pub dataprop BOOK_DATA_FREE(cs: $A.cells, n: int) =
  | {cs:$A.cells}{n:nat | n < 5} BOOK_DATA_FREE_short(cs, n)
  | {cs:$A.cells}{n:pos}{c:int | c != 66; c != 98} BOOK_DATA_FREE_first(cs, n) of $A.NTH(cs, 0, c)
  | {cs:$A.cells}{n:int | n > 1}{c:int | c != 79; c != 111} BOOK_DATA_FREE_second(cs, n) of $A.NTH(cs, 1, c)
  | {cs:$A.cells}{n:int | n > 2}{c:int | c != 79; c != 111} BOOK_DATA_FREE_third(cs, n) of $A.NTH(cs, 2, c)
  | {cs:$A.cells}{n:int | n > 3}{c:int | c != 75; c != 107} BOOK_DATA_FREE_fourth(cs, n) of $A.NTH(cs, 3, c)
  | {cs:$A.cells}{n:int | n > 4}{c:int | c != 45} BOOK_DATA_FREE_fifth(cs, n) of $A.NTH(cs, 4, c)

#pub dataprop BOOK_DATA_FREE_OPT(cs: $A.cells, n: int, b: bool) =
  | {cs:$A.cells}{n:int} BOOK_DATA_FREE_yes(cs, n, true) of BOOK_DATA_FREE(cs, n)
  | {cs:$A.cells}{n:int} BOOK_DATA_FREE_no(cs, n, false)

#pub fun book_data_scan {l:agz}{n:pos}{cs:$A.cells}
  (src: !$A.cborrow(l, n, cs), len: int n): [b:bool] (BOOK_DATA_FREE_OPT(cs, n, b) | bool b)

(* A chapter's open stream, for the layout g (1 reflow, 2 fixed) and the
   outside bytes cs. Linear: close_book_document flushes and frees it *)
#pub datavtype book_document(cs: $A.cells, g: int) =
  | {cs:$A.cells}{g:int | g == 1 || g == 2}
    book_document_mk(cs, g) of ($OU.OUTSIDE_READ(cs) | $BC.book_stream, int g)

(* The container of a chapter's document: the id it was made with (kept as
   outside-independent cells of its own, with the proof bridge's encoder took).
   Linear: close_book_container consumes it *)
#pub datavtype book_container(cs: $A.cells, g: int) =
  | {cs:$A.cells}{g:int}{lc:agz}{nc:pos | nc <= 64}{csc:$A.cells}
    book_container_mk(cs, g) of ($A.cfrozen(lc, nc, 1, csc), $A.cborrow(lc, nc, csc), int nc)

#pub datavtype book_container_made(cs: $A.cells, g: int) =
  | {cs:$A.cells}{g:int} BookContainerMade(cs, g) of book_container(cs, g)
  | {cs:$A.cells}{g:int} BookContainerRefused(cs, g) of ()

(* The elements a book may make: dom's tags without the controls (button,
   canvas, dialog, fieldset, input, label, legend, meter, optgroup, option,
   output, progress, select, textarea) and without <style>; a book's other
   names are made as BookTagDiv or BookTagSpan, with the name kept in
   data-book-element *)
#pub datatype book_tag =
  | BookTagA | BookTagAbbr | BookTagArticle | BookTagAside | BookTagAudio | BookTagB | BookTagBdi
  | BookTagBdo | BookTagBlockquote | BookTagBr | BookTagCaption | BookTagCite | BookTagCode
  | BookTagDd | BookTagDel | BookTagDetails | BookTagDfn | BookTagDiv | BookTagDl | BookTagDt
  | BookTagEm | BookTagFigcaption | BookTagFigure | BookTagFooter | BookTagH1 | BookTagH2
  | BookTagH3 | BookTagH4 | BookTagH5 | BookTagH6 | BookTagHeader | BookTagHr | BookTagI
  | BookTagImg | BookTagIns | BookTagKbd | BookTagLi | BookTagMain | BookTagMark | BookTagNav
  | BookTagOl | BookTagP | BookTagPre | BookTagQ | BookTagRb | BookTagRp | BookTagRt | BookTagRtc
  | BookTagRuby | BookTagS | BookTagSamp | BookTagSection | BookTagSmall | BookTagSource
  | BookTagSpan | BookTagStrong | BookTagSub | BookTagSummary | BookTagSup | BookTagTable
  | BookTagTbody | BookTagTd | BookTagTfoot | BookTagTh | BookTagThead | BookTagTime | BookTagTr
  | BookTagTrack | BookTagU | BookTagUl | BookTagVar | BookTagVideo | BookTagWbr

(* The attributes a book document sets by the ordinary operation. No Style (a
   book's style is written from css's tree, by set_book_style), and no Data (a
   data attribute is set by set_book_data) *)
#pub datavtype book_attribute =
  | BookAlt | BookClass | BookColspan | BookDir | BookHeight | BookHidden | BookLang | BookRole
  | BookRowspan | BookTabindex | BookTitle | BookWidth
  | {n:pos | n < 240} BookAria of (string n)   (* aria-<the n bytes> *)

(* A book document for the layout g over the outside bytes cs *)
#pub fun open_book_document {cs:$A.cells}{g:int | g == 1 || g == 2}
  (pf: $OU.OUTSIDE_READ(cs) | layout: int g): book_document(cs, g)

(* Flushes the stream and frees the document *)
#pub fun close_book_document {cs:$A.cells}{g:int} (doc: book_document(cs, g)): void

(* The container of the chapter: opcode 13 under the element parent, with
   the id id (a letter, then letters, digits or hyphens), at the root tag at
   at. Refused (nothing is written) when id is no container id *)
#pub fun add_book_container
  {l:agz}{n:nat}{cs:$A.cells}{o:nat}{g:int}{lp:agz}{np:pos | np <= 1024}{li:agz}{ni:pos | ni <= 64}
  (root: ROOT_TAG(cs, o) |
   doc: !book_document(cs, g), src: !$A.cborrow(l, n, cs), at: int o,
   parent: !$A.borrow(byte, lp, np), plen: int np,
   id: !$A.borrow(byte, li, ni), ilen: int ni): book_container_made(cs, g)

(* Consumes the container *)
#pub fun close_book_container {cs:$A.cells}{g:int} (c: book_container(cs, g)): void

(* A book element: opcode 14, named (data-book-element) by src[name_off, name_off + name_len) *)
#pub fun add_book_element
  {l:agz}{n:nat}{cs:$A.cells}{g:int}{li:agz}{ni:pos | ni <= 1024}{lp:agz}{np:pos | np <= 1024}
  {o,k:nat | o + k <= n; k <= 255}
  (doc: !book_document(cs, g), c: !book_container(cs, g),
   id: !$A.borrow(byte, li, ni), ilen: int ni, parent: !$A.borrow(byte, lp, np), plen: int np,
   tag: book_tag, src: !$A.cborrow(l, n, cs), name_off: int o, name_len: int k): void

(* The class names (kind 1), id (2) or epub:type (3) of a book element: opcode 16 *)
#pub fun set_book_names
  {l:agz}{n:nat}{cs:$A.cells}{g:int}{li:agz}{ni:pos | ni <= 1024}{kind:int | 1 <= kind; kind <= 3}
  {o,k:nat | o + k <= n; k <= 65535}
  (doc: !book_document(cs, g), c: !book_container(cs, g),
   id: !$A.borrow(byte, li, ni), ilen: int ni, kind: int kind,
   src: !$A.cborrow(l, n, cs), off: int o, len: int k): void

(* The style of a book element, written from the declarations css read
   (opcodes 19 and 20); a face the style names by a container-made name is
   written without the container's id (bridge has no id piece for a style) *)
#pub fun set_book_style
  {l:agz}{n:nat}{cs:$A.cells}{g:int}{t:nat}{li:agz}{ni:pos | ni <= 1024}
  (doc: !book_document(cs, g), c: !book_container(cs, g),
   id: !$A.borrow(byte, li, ni), ilen: int ni,
   src: !$A.cborrow(l, n, cs), read: !$CB.book_read(cs, n, g), decls: !$CB.book_decls(cs, n, g, t)): void

(* The chapter's sheet, written from the rules css read (opcodes 17 and 18) *)
#pub fun add_book_sheet
  {l:agz}{n:nat}{cs:$A.cells}{g:int}
  (doc: !book_document(cs, g), c: !book_container(cs, g),
   src: !$A.cborrow(l, n, cs), read: !$CB.book_read(cs, n, g)): void

(* The face number face (below 32) of the chapter from the bytes src[off, off +
   len) of an outside file (its own proof): what is queued is flushed first, so
   the container is there. weight is 1 to 1000 and style 0 normal, 1 italic,
   2 oblique *)
#pub fun add_book_font
  {l:agz}{n:nat}{csf:$A.cells}{cs:$A.cells}{g:int}{o:nat}{k:pos | o + k <= n}
  {face:nat | face < 32}{weight:int | 1 <= weight; weight <= 1000}{style:int | 0 <= style; style <= 2}
  (pf: $OU.OUTSIDE_READ(csf) |
   doc: !book_document(cs, g), c: !book_container(cs, g),
   src: !$A.cborrow(l, n, csf), off: int o, len: int k,
   face: int face, weight: int weight, style: int style): void

(* The ordinary operations of a book document: an element of the app inside
   the container (add_book_app_element), text, attributes. Inside a container
   bridge refuses style and data-book-* on them *)
#pub fun add_book_app_element
  {cs:$A.cells}{g:int}{li:agz}{ni:pos | ni <= 1024}{lp:agz}{np:pos | np <= 1024}
  (doc: !book_document(cs, g),
   id: !$A.borrow(byte, li, ni), ilen: int ni, parent: !$A.borrow(byte, lp, np), plen: int np,
   tag: book_tag): void

#pub fun set_book_text
  {cs:$A.cells}{g:int}{li:agz}{ni:pos | ni <= 1024}{lt:agz}{nt:nat | nt <= 65535}
  (doc: !book_document(cs, g), id: !$A.borrow(byte, li, ni), ilen: int ni,
   t: !$A.borrow(byte, lt, nt), tlen: int nt): void

#pub fun append_book_text
  {cs:$A.cells}{g:int}{li:agz}{ni:pos | ni <= 1024}{lt:agz}{nt:nat | nt <= 65535}
  (doc: !book_document(cs, g), id: !$A.borrow(byte, li, ni), ilen: int ni,
   t: !$A.borrow(byte, lt, nt), tlen: int nt): void

#pub fun set_book_attr
  {cs:$A.cells}{g:int}{li:agz}{ni:pos | ni <= 1024}{lv:agz}{nv:nat | nv <= 65535}
  (doc: !book_document(cs, g), id: !$A.borrow(byte, li, ni), ilen: int ni, name: book_attribute,
   v: !$A.borrow(byte, lv, nv), vlen: int nv): void

#pub fun remove_book_attr
  {cs:$A.cells}{g:int}{li:agz}{ni:pos | ni <= 1024}
  (doc: !book_document(cs, g), id: !$A.borrow(byte, li, ni), ilen: int ni, name: book_attribute): void

(* The data attribute data-<name> of an element: name does not start with book- *)
#pub fun set_book_data
  {cs:$A.cells}{g:int}{li:agz}{ni:pos | ni <= 1024}{ln:agz}{nn:pos | nn < 240}{csn:$A.cells}
  {lv:agz}{nv:nat | nv <= 65535}
  (free: BOOK_DATA_FREE(csn, nn) |
   doc: !book_document(cs, g), id: !$A.borrow(byte, li, ni), ilen: int ni,
   name: !$A.cborrow(ln, nn, csn), nlen: int nn, v: !$A.borrow(byte, lv, nv), vlen: int nv): void

(* ============================================================
   The root tag
   ============================================================ *)

implement book_root_scan{l}{n}{cs}{o}(src, len, at) =
  if at + 5 >= len then (ROOT_TAG_no() | false)
  else let
    val @(p0 | c0) = $A.cborrow_get(src, at)
    val @(p1 | c1) = $A.cborrow_get(src, at + 1)
    val @(p2 | c2) = $A.cborrow_get(src, at + 2)
    val @(p3 | c3) = $A.cborrow_get(src, at + 3)
    val @(p4 | c4) = $A.cborrow_get(src, at + 4)
    val @(p5 | c5) = $A.cborrow_get(src, at + 5)
  in
    if c0 = 60 then
      (if c1 = 104 then
         (if c2 = 116 then
            (if c3 = 109 then
               (if c4 = 108 then
                  (if c5 = 62 then (ROOT_TAG_yes(ROOT_TAG_mk(p0, p1, p2, p3, p4, p5)) | true)
                   else if c5 = 32 then (ROOT_TAG_yes(ROOT_TAG_mk(p0, p1, p2, p3, p4, p5)) | true)
                   else if c5 = 10 then (ROOT_TAG_yes(ROOT_TAG_mk(p0, p1, p2, p3, p4, p5)) | true)
                   else if c5 = 9 then (ROOT_TAG_yes(ROOT_TAG_mk(p0, p1, p2, p3, p4, p5)) | true)
                   else if c5 = 13 then (ROOT_TAG_yes(ROOT_TAG_mk(p0, p1, p2, p3, p4, p5)) | true)
                   else if c5 = 12 then (ROOT_TAG_yes(ROOT_TAG_mk(p0, p1, p2, p3, p4, p5)) | true)
                   else (ROOT_TAG_no() | false))
                else (ROOT_TAG_no() | false))
             else (ROOT_TAG_no() | false))
          else (ROOT_TAG_no() | false))
       else (ROOT_TAG_no() | false))
    else (ROOT_TAG_no() | false)
  end

(* ============================================================
   A data attribute name that does not start with book-
   ============================================================ *)

fn _data_fifth {l:agz}{n:int | n > 4}{cs:$A.cells}{m:nat | m == n} (src: !$A.cborrow(l, m, cs))
  : [b:bool] (BOOK_DATA_FREE_OPT(cs, n, b) | bool b) = let
  val @(p4 | c4) = $A.cborrow_get(src, 4)
in
  if c4 = 45 then (BOOK_DATA_FREE_no() | false)
  else (BOOK_DATA_FREE_yes(BOOK_DATA_FREE_fifth(p4)) | true)
end

fn _data_fourth {l:agz}{n:int | n > 4}{cs:$A.cells}{m:nat | m == n} (src: !$A.cborrow(l, m, cs))
  : [b:bool] (BOOK_DATA_FREE_OPT(cs, n, b) | bool b) = let
  val @(p3 | c3) = $A.cborrow_get(src, 3)
in
  if c3 = 107 then _data_fifth(src)
  else if c3 = 75 then _data_fifth(src)
  else (BOOK_DATA_FREE_yes(BOOK_DATA_FREE_fourth(p3)) | true)
end

fn _data_third {l:agz}{n:int | n > 4}{cs:$A.cells}{m:nat | m == n} (src: !$A.cborrow(l, m, cs))
  : [b:bool] (BOOK_DATA_FREE_OPT(cs, n, b) | bool b) = let
  val @(p2 | c2) = $A.cborrow_get(src, 2)
in
  if c2 = 111 then _data_fourth(src)
  else if c2 = 79 then _data_fourth(src)
  else (BOOK_DATA_FREE_yes(BOOK_DATA_FREE_third(p2)) | true)
end

fn _data_second {l:agz}{n:int | n > 4}{cs:$A.cells}{m:nat | m == n} (src: !$A.cborrow(l, m, cs))
  : [b:bool] (BOOK_DATA_FREE_OPT(cs, n, b) | bool b) = let
  val @(p1 | c1) = $A.cborrow_get(src, 1)
in
  if c1 = 111 then _data_third(src)
  else if c1 = 79 then _data_third(src)
  else (BOOK_DATA_FREE_yes(BOOK_DATA_FREE_second(p1)) | true)
end

implement book_data_scan{l}{n}{cs}(src, len) =
  if len < 5 then (BOOK_DATA_FREE_yes(BOOK_DATA_FREE_short()) | true)
  else let
    val @(p0 | c0) = $A.cborrow_get(src, 0)
  in
    if c0 = 98 then _data_second(src)
    else if c0 = 66 then _data_second(src)
    else (BOOK_DATA_FREE_yes(BOOK_DATA_FREE_first(p0)) | true)
  end

(* ============================================================
   Strings as borrows
   ============================================================ *)

(* The bytes of s, as a borrow of exactly its length; let go with _rel *)
fn _lit {n:pos | n < 256} (s: string n): [l:agz] @($A.frozen(byte, l, n, 1), $A.borrow(byte, l, n)) = let
  val n = g1u2i(string1_length(s))
  val a = $A.alloc<byte>(n)
  val () = $A.write_text(a, 0, $A.text_lit(s), n)
in $A.freeze<byte>(a) end

fn _rel {l:agz}{n:nat} (fz: $A.frozen(byte, l, n, 1), b: $A.borrow(byte, l, n)): void = let
  val () = $A.drop<byte>(fz, b)
in $A.free<byte>($A.thaw<byte>(fz)) end

(* ============================================================
   Names
   ============================================================ *)

fn _tag_name (t: book_tag): [m:pos | m < 256] string m =
  case+ t of
  | BookTagA() => "a" | BookTagAbbr() => "abbr" | BookTagArticle() => "article"
  | BookTagAside() => "aside" | BookTagAudio() => "audio" | BookTagB() => "b"
  | BookTagBdi() => "bdi" | BookTagBdo() => "bdo" | BookTagBlockquote() => "blockquote"
  | BookTagBr() => "br" | BookTagCaption() => "caption" | BookTagCite() => "cite"
  | BookTagCode() => "code" | BookTagDd() => "dd" | BookTagDel() => "del"
  | BookTagDetails() => "details" | BookTagDfn() => "dfn" | BookTagDiv() => "div"
  | BookTagDl() => "dl" | BookTagDt() => "dt" | BookTagEm() => "em"
  | BookTagFigcaption() => "figcaption" | BookTagFigure() => "figure" | BookTagFooter() => "footer"
  | BookTagH1() => "h1" | BookTagH2() => "h2" | BookTagH3() => "h3" | BookTagH4() => "h4"
  | BookTagH5() => "h5" | BookTagH6() => "h6" | BookTagHeader() => "header" | BookTagHr() => "hr"
  | BookTagI() => "i" | BookTagImg() => "img" | BookTagIns() => "ins" | BookTagKbd() => "kbd"
  | BookTagLi() => "li" | BookTagMain() => "main" | BookTagMark() => "mark" | BookTagNav() => "nav"
  | BookTagOl() => "ol" | BookTagP() => "p" | BookTagPre() => "pre" | BookTagQ() => "q"
  | BookTagRb() => "rb" | BookTagRp() => "rp" | BookTagRt() => "rt" | BookTagRtc() => "rtc"
  | BookTagRuby() => "ruby" | BookTagS() => "s" | BookTagSamp() => "samp"
  | BookTagSection() => "section" | BookTagSmall() => "small" | BookTagSource() => "source"
  | BookTagSpan() => "span" | BookTagStrong() => "strong" | BookTagSub() => "sub"
  | BookTagSummary() => "summary" | BookTagSup() => "sup" | BookTagTable() => "table"
  | BookTagTbody() => "tbody" | BookTagTd() => "td" | BookTagTfoot() => "tfoot"
  | BookTagTh() => "th" | BookTagThead() => "thead" | BookTagTime() => "time" | BookTagTr() => "tr"
  | BookTagTrack() => "track" | BookTagU() => "u" | BookTagUl() => "ul" | BookTagVar() => "var"
  | BookTagVideo() => "video" | BookTagWbr() => "wbr"

(* An attribute's name is its word followed by its rest ("aria-" and what Aria
   holds). Consumes the attribute *)
fn _attribute_parts (name: book_attribute)
  : [nw:pos | nw < 16][nr:nat | nr < 240] @(string nw, string nr) =
  case+ name of
  | ~BookAlt() => @("alt", "") | ~BookClass() => @("class", "") | ~BookColspan() => @("colspan", "")
  | ~BookDir() => @("dir", "") | ~BookHeight() => @("height", "") | ~BookHidden() => @("hidden", "")
  | ~BookLang() => @("lang", "") | ~BookRole() => @("role", "") | ~BookRowspan() => @("rowspan", "")
  | ~BookTabindex() => @("tabindex", "") | ~BookTitle() => @("title", "") | ~BookWidth() => @("width", "")
  | ~BookAria(rest) => @("aria-", rest)

(* The two parts as one borrow; let go with _rel *)
fn _name_borrow {nw:pos | nw < 16}{nr:nat | nr < 240} (word: string nw, rest: string nr)
  : [l:agz][n:pos | n < 256] @($A.frozen(byte, l, n, 1), $A.borrow(byte, l, n), int n) = let
  val wl = g1u2i(string1_length(word))
  val rl = g1u2i(string1_length(rest))
  val a = $A.alloc<byte>(wl + rl)
  val () = $A.write_text(a, 0, $A.text_lit(word), wl)
  val () = $A.write_text(a, wl, $A.text_lit(rest), rl)
  val @(fz, b) = $A.freeze<byte>(a)
in @(fz, b, wl + rl) end

(* ============================================================
   The document and the container
   ============================================================ *)

implement open_book_document{cs}{g}(pf | layout) = book_document_mk(pf | $BC.book_stream_new(), layout)

implement close_book_document{cs}{g}(doc) = let
  val ~book_document_mk(_ | s, _) = doc
  val () = $BC.dom_flush_book(s)
in $BC.book_stream_free(s) end

implement add_book_container{l}{n}{cs}{o}{g}{lp}{np}{li}{ni}(root | doc, src, at, parent, plen, id, ilen) = let
  val+ @book_document_mk(pf | s, layout) = doc
  val ida = $A.alloc<byte>(ilen)
  val () = $A.write_borrow(ida, 0, id, ilen)
  val @(_ | idb) = $A.barr_of_arr(ida)
  val @(fz, cb) = $A.barr_freeze(idb)
  val @(r | ok) = $BC.book_id_scan(cb, ilen)
in
  if ok then let
    prval $BC.BOOK_ID_yes(pid) = r
    val () = $BC.book_container(pf, pid | s, src, cb, ilen, parent, plen, layout)
    prval () = fold@(doc)
  in BookContainerMade(book_container_mk(fz, cb, ilen)) end
  else let
    prval $BC.BOOK_ID_no() = r
    val () = $A.cborrow_drop(fz, cb)
    val () = $A.free<byte>($A.barr_to_arr($A.barr_thaw(fz)))
    prval () = fold@(doc)
  in BookContainerRefused() end
end

implement close_book_container{cs}{g}(c) = let
  val ~book_container_mk(fz, cb, _) = c
  val () = $A.cborrow_drop(fz, cb)
in $A.free<byte>($A.barr_to_arr($A.barr_thaw(fz))) end

(* ============================================================
   Elements and names
   ============================================================ *)

implement add_book_element{l}{n}{cs}{g}{li}{ni}{lp}{np}{o,k}
  (doc, c, id, ilen, parent, plen, tag, src, name_off, name_len) = let
  val+ @book_document_mk(pf | s, _) = doc
  val+ @book_container_mk(fz, cb, cn) = c
  val cv = $A.cborrow_view(fz, cb)
  val tn = _tag_name(tag)
  val @(tz, tb) = _lit(tn)
  val () = $BC.book_element(pf | s, cv, cn, id, ilen, parent, plen, tb, g1u2i(string1_length(tn)),
    src, name_off, name_len)
  val () = _rel(tz, tb)
  val () = $A.cborrow_view_drop(fz, cv)
  prval () = fold@(c)
  prval () = fold@(doc)
in end

implement set_book_names{l}{n}{cs}{g}{li}{ni}{kind}{o,k}(doc, c, id, ilen, kind, src, off, len) = let
  val+ @book_document_mk(pf | s, _) = doc
  val+ @book_container_mk(fz, cb, cn) = c
  val cv = $A.cborrow_view(fz, cb)
  val () = $BC.book_names(pf | s, cv, cn, id, ilen, kind, src, off, len)
  val () = $A.cborrow_view_drop(fz, cv)
  prval () = fold@(c)
  prval () = fold@(doc)
in end

(* ============================================================
   The pieces of a sheet and of a style
   ============================================================ *)

(* A digit, as a literal of the program *)
fn _digit (d: int): [m:nat | m <= 1024] string m =
  if d <= 0 then "0" else if d = 1 then "1" else if d = 2 then "2" else if d = 3 then "3"
  else if d = 4 then "4" else if d = 5 then "5" else if d = 6 then "6" else if d = 7 then "7"
  else if d = 8 then "8" else "9"

(* The decimal digits of v from the place p (a power of ten, k places left), no
   leading zero; the last place is always written *)
fun _sheet_num {lc:agz}{nc:pos | nc <= 64}{k:nat} .<k>.
  (s: !$BC.book_stream, cb: !$A.borrow(byte, lc, nc), cn: int nc, v: int, p: int, k: int k, started: bool): void =
  if k <= 1 then $BC.book_sheet_lit(s, cb, cn, _digit(v % 10))
  else let
    val d = (v / p) % 10
  in
    if d > 0 || started then let
      val () = $BC.book_sheet_lit(s, cb, cn, _digit(d))
    in _sheet_num(s, cb, cn, v, p / 10, k - 1, true) end
    else _sheet_num(s, cb, cn, v, p / 10, k - 1, false)
  end

fun _sheet_pieces {l:agz}{n:nat}{cs:$A.cells}{lc:agz}{nc:pos | nc <= 64}{t:nat} .<t>.
  (pf: $OU.OUTSIDE_READ(cs) | s: !$BC.book_stream, src: !$A.cborrow(l, n, cs),
   cb: !$A.borrow(byte, lc, nc), cn: int nc, xs: $CB.book_pieces(cs, n, t)): void =
  case+ xs of
  | ~$CB.BPNil() => ()
  | ~$CB.BPLit(lit, rest) => let
      val () = $BC.book_sheet_lit(s, cb, cn, lit)
    in _sheet_pieces(pf | s, src, cb, cn, rest) end
  | ~$CB.BPNum(v, rest) => let
      val () = _sheet_num(s, cb, cn, v, 1000000000, 10, false)
    in _sheet_pieces(pf | s, src, cb, cn, rest) end
  | ~$CB.BPSpan(o, k, rest) => let
      val () = $BC.book_sheet_span(pf | s, cb, cn, src, o, k)
    in _sheet_pieces(pf | s, src, cb, cn, rest) end
  | ~$CB.BPContainer(rest) => let
      val () = $BC.book_sheet_id(s, cb, cn)
    in _sheet_pieces(pf | s, src, cb, cn, rest) end

implement add_book_sheet{l}{n}{cs}{g}(doc, c, src, read) = let
  val+ @book_document_mk(pf | s, _) = doc
  val+ @book_container_mk(fz, cb, cn) = c
  val cv = $A.cborrow_view(fz, cb)
  val () = _sheet_pieces(pf | s, src, cv, cn, $CB.book_sheet_pieces(src, read))
  val () = $BC.book_sheet_end(s, cv, cn)
  val () = $A.cborrow_view_drop(fz, cv)
  prval () = fold@(c)
  prval () = fold@(doc)
in end

fun _style_num {lc:agz}{nc:pos | nc <= 64}{li:agz}{ni:pos | ni <= 1024}{k:nat} .<k>.
  (s: !$BC.book_stream, run: !$BC.book_style_run(lc, li),
   cb: !$A.borrow(byte, lc, nc), cn: int nc, id: !$A.borrow(byte, li, ni), idn: int ni,
   v: int, p: int, k: int k, started: bool): void =
  if k <= 1 then $BC.book_style_lit(s, run, cb, cn, id, idn, _digit(v % 10))
  else let
    val d = (v / p) % 10
  in
    if d > 0 || started then let
      val () = $BC.book_style_lit(s, run, cb, cn, id, idn, _digit(d))
    in _style_num(s, run, cb, cn, id, idn, v, p / 10, k - 1, true) end
    else _style_num(s, run, cb, cn, id, idn, v, p / 10, k - 1, false)
  end

fun _style_pieces {l:agz}{n:nat}{cs:$A.cells}{lc:agz}{nc:pos | nc <= 64}{li:agz}{ni:pos | ni <= 1024}{t:nat} .<t>.
  (pf: $OU.OUTSIDE_READ(cs) | s: !$BC.book_stream, run: !$BC.book_style_run(lc, li), src: !$A.cborrow(l, n, cs),
   cb: !$A.borrow(byte, lc, nc), cn: int nc, id: !$A.borrow(byte, li, ni), idn: int ni,
   xs: $CB.book_pieces(cs, n, t)): void =
  case+ xs of
  | ~$CB.BPNil() => ()
  | ~$CB.BPLit(lit, rest) => let
      val () = $BC.book_style_lit(s, run, cb, cn, id, idn, lit)
    in _style_pieces(pf | s, run, src, cb, cn, id, idn, rest) end
  | ~$CB.BPNum(v, rest) => let
      val () = _style_num(s, run, cb, cn, id, idn, v, 1000000000, 10, false)
    in _style_pieces(pf | s, run, src, cb, cn, id, idn, rest) end
  | ~$CB.BPSpan(o, k, rest) => let
      val () = $BC.book_style_span(pf | s, run, cb, cn, id, idn, src, o, k)
    in _style_pieces(pf | s, run, src, cb, cn, id, idn, rest) end
  | ~$CB.BPContainer(rest) => _style_pieces(pf | s, run, src, cb, cn, id, idn, rest)

implement set_book_style{l}{n}{cs}{g}{t}{li}{ni}(doc, c, id, ilen, src, read, decls) = let
  val+ @book_document_mk(pf | s, _) = doc
  val+ @book_container_mk(fz, cb, cn) = c
  val cv = $A.cborrow_view(fz, cb)
  val run = $BC.book_style_begin(cv, id)
  val () = _style_pieces(pf | s, run, src, cv, cn, id, ilen, $CB.book_style_pieces(src, read, decls))
  val () = $BC.book_style_end(s, run, cv, cn, id, ilen)
  val () = $A.cborrow_view_drop(fz, cv)
  prval () = fold@(c)
  prval () = fold@(doc)
in end

(* ============================================================
   Faces
   ============================================================ *)

implement add_book_font{l}{n}{csf}{cs}{g}{o}{k}{face}{weight}{style}
  (pff | doc, c, src, off, len, face, weight, style) = let
  val+ @book_document_mk(pf | s, _) = doc
  val+ @book_container_mk(fz, cb, cn) = c
  val cv = $A.cborrow_view(fz, cb)
  val () = $BC.dom_flush_book(s)
  val () = $BC.book_font_add(pff | src, off, len, cv, cn, face, weight, style)
  val () = $A.cborrow_view_drop(fz, cv)
  prval () = fold@(c)
  prval () = fold@(doc)
in end

(* ============================================================
   The ordinary operations
   ============================================================ *)

implement add_book_app_element{cs}{g}{li}{ni}{lp}{np}(doc, id, ilen, parent, plen, tag) = let
  val+ @book_document_mk(pf | s, _) = doc
  val tn = _tag_name(tag)
  val @(tz, tb) = _lit(tn)
  val () = $BC.book_create_element(s, id, ilen, parent, plen, tb, g1u2i(string1_length(tn)))
  val () = _rel(tz, tb)
  prval () = fold@(doc)
in end

implement set_book_text{cs}{g}{li}{ni}{lt}{nt}(doc, id, ilen, t, tlen) = let
  val+ @book_document_mk(pf | s, _) = doc
  val () = $BC.book_set_text(s, id, ilen, t, tlen)
  prval () = fold@(doc)
in end

implement append_book_text{cs}{g}{li}{ni}{lt}{nt}(doc, id, ilen, t, tlen) = let
  val+ @book_document_mk(pf | s, _) = doc
  val () = $BC.book_append_text(s, id, ilen, t, tlen)
  prval () = fold@(doc)
in end

implement set_book_attr{cs}{g}{li}{ni}{lv}{nv}(doc, id, ilen, name, v, vlen) = let
  val+ @book_document_mk(pf | s, _) = doc
  val @(word, rest) = _attribute_parts(name)
  val @(nz, nb, nn) = _name_borrow(word, rest)
  val () = $BC.book_set_attr(s, id, ilen, nb, nn, v, vlen)
  val () = _rel(nz, nb)
  prval () = fold@(doc)
in end

implement remove_book_attr{cs}{g}{li}{ni}(doc, id, ilen, name) = let
  val+ @book_document_mk(pf | s, _) = doc
  val @(word, rest) = _attribute_parts(name)
  val @(nz, nb, nn) = _name_borrow(word, rest)
  val () = $BC.book_remove_attr(s, id, ilen, nb, nn)
  val () = _rel(nz, nb)
  prval () = fold@(doc)
in end

(* The bytes of src from k on, written into dst from off + k on *)
fun _copy_name {ld:agz}{m:nat}{o:addr}{ln:agz}{nn:nat}{cs:$A.cells}{off:nat | off + nn <= m}{k:nat | k <= nn} .<nn - k>.
  (dst: !$A.arrx(byte, ld, m, o), off: int off, src: !$A.cborrow(ln, nn, cs), len: int nn, k: int k): void =
  if k >= len then ()
  else let
    val @(_ | c) = $A.cborrow_get(src, k)
    val () = $A.write_byte(dst, off + k, c)
  in _copy_name(dst, off, src, len, k + 1) end

implement set_book_data{cs}{g}{li}{ni}{ln}{nn}{csn}{lv}{nv}(free | doc, id, ilen, name, nlen, v, vlen) = let
  val+ @book_document_mk(pf | s, _) = doc
  val a = $A.alloc<byte>(5 + nlen)
  val () = $A.write_text(a, 0, $A.text_lit("data-"), 5)
  val () = _copy_name(a, 5, name, nlen, 0)
  val @(fz, nb) = $A.freeze<byte>(a)
  val () = $BC.book_set_attr(s, id, ilen, nb, 5 + nlen, v, vlen)
  val () = _rel(fz, nb)
  prval () = fold@(doc)
in end
