#target wasm binary
#include "share/atspre_staload.hats"
#use array as A
#use css as C
#use promise as P
#use wasm.bats-packages.dev/bridge as B
#use wasm.bats-packages.dev/dom as D
staload OU = "wasm.bats-packages.dev/bridge/src/outside.bats"
staload DM = "wasm.bats-packages.dev/bridge/src/dom.bats"
staload WI = "wasm.bats-packages.dev/bridge/src/window.bats"
staload DB = "wasm.bats-packages.dev/dom/src/book.bats"
staload CB = "css/src/book.sats"

(* A chapter through dom's book operations, over the bytes of an outside file
   that check.mjs picks. The file is three parts, read three times (so each
   array has an outside proof of its own):
     0 to 23    the chapter: <html><body>p intro lead
                (the root tag at 0, the element name "p" at 12, the class
                names "intro lead" at 14)
     24 to 51   the sheet: p{color:red}.intro{margin:0}
     52 to 69   the style: color:red;margin:0
   The app logs "@snap book" at the end, when check.mjs reads the document and
   the bytes of the flushes. *)

fn exact {n:pos | n < 256} (s: string n): [l:agz] $A.arr(byte, l, n) = let
  val n = g1u2i(string1_length(s))
  val a = $A.alloc<byte>(n)
  val () = $A.write_text(a, 0, $A.text_lit(s), n)
in a end

fn lit {n:pos | n < 256} (s: string n): [l:agz] @($A.frozen(byte, l, n, 1), $A.borrow(byte, l, n)) =
  $A.freeze<byte>(exact(s))

fn rel {l:agz}{n:nat} (fz: $A.frozen(byte, l, n, 1), b: $A.borrow(byte, l, n)): void = let
  val () = $A.drop<byte>(fz, b)
in $A.free<byte>($A.thaw<byte>(fz)) end

fn say {n:pos | n < 256} (line: string n): void = let
  val @(fz, b) = lit(line)
  val () = $WI.log($WI.Info(), b, g1u2i(string1_length(line)))
in rel(fz, b) end

(* The data attribute data-<name>, when the name does not start with book- *)
fn data_attr {g:int}{li:agz}{ni:pos | ni <= 1024}{nn:pos | nn < 240}{lv:agz}{nv:nat | nv <= 65535}
  (doc: !$DB.book_document(g), id: !$A.borrow(byte, li, ni), ilen: int ni, name: string nn,
   v: !$A.borrow(byte, lv, nv), vlen: int nv): void = let
  val nl = g1u2i(string1_length(name))
  val @(_ | nb) = $A.barr_of_arr(exact(name))
  val @(fz, cb) = $A.barr_freeze(nb)
  val @(r | ok) = $DB.book_data_scan(cb, nl)
  val () = (if ok then let
      prval $DB.BOOK_DATA_FREE_yes(pf) = r
    in $DB.set_book_data(pf | doc, id, ilen, cb, nl, v, vlen) end
    else let
      prval $DB.BOOK_DATA_FREE_no() = r
    in () end)
  val () = $A.cborrow_drop(fz, cb)
in $A.free<byte>($A.barr_to_arr($A.barr_thaw(fz))) end

fn play {l:agz}{n:pos}{cs:$A.cells}{l2:agz}{n2:pos}{cs2:$A.cells}{l3:agz}{n3:pos}{cs3:$A.cells}
  (pfd: $OU.OUTSIDE_READ(cs), pfs: $OU.OUTSIDE_READ(cs2), pfy: $OU.OUTSIDE_READ(cs3) |
   chapter: !$A.cborrow(l, n, cs), n: int n,
   sheet: !$A.cborrow(l2, n2, cs2), n2: int n2,
   style: !$A.cborrow(l3, n3, cs3), n3: int n3): void = let
  val doc = $DB.open_book_document(1)
  val @(rp | root) = $DB.book_root_scan(chapter, n, 0)
  val @(sz, sb) = lit("stage")
  val @(cz, cbid) = lit("chap")
  val @(ez, eb) = lit("c1")
  val @(az, ab) = lit("app1")
  val @(tz, tb) = lit("hello")
  val @(vz, vb) = lit("mine")
  val () = (if root then let
      prval $DB.ROOT_TAG_yes(rt) = rp
    in
      case+ $DB.add_book_container(pfd, rt | doc, chapter, 0, sb, 5, cbid, 4) of
      | ~$DB.BookContainerMade(c) => let
          val () = (if n >= 24 then
              $DB.add_book_element(pfd | doc, c, eb, 2, cbid, 4, $DB.BookTagP(), chapter, 12, 1) else ())
          val () = (if n >= 24 then $DB.set_book_names(pfd | doc, c, eb, 1, chapter, 14, 10) else ())
          val read = $CB.book_sheet_read(sheet, n2, 1, @(255, 255, 255), @(250, 240, 220))
          val decls = $CB.book_style_read(style, n3, 1, @(255, 255, 255), @(250, 240, 220))
          val () = $DB.set_book_style(pfy | doc, c, eb, 2, style, read, decls)
          val () = $DB.add_book_sheet(pfs | doc, c, sheet, read)
          val () = $CB.book_decls_free(decls)
          val () = $CB.book_read_free(read)
          val () = $DB.add_book_app_element(doc, ab, 4, eb, 2, $DB.BookTagSpan())
          val () = $DB.set_book_text(doc, ab, 4, tb, 5)
          val () = $DB.append_book_text(doc, ab, 4, tb, 5)
          val () = $DB.set_book_attr(doc, ab, 4, $DB.BookTitle(), vb, 4)
          val () = $DB.remove_book_attr(doc, ab, 4, $DB.BookTabindex())
          val () = data_attr(doc, ab, 4, "x-y", vb, 4)
          val () = data_attr(doc, ab, 4, "Book-z", vb, 4)
          val () = (if n >= 24 then $DB.add_book_font(pfd | doc, c, chapter, 12, 1, 0, 400, 0) else ())
        in $DB.close_book_container(c) end
      | ~$DB.BookContainerRefused() => say("container refused")
    end
    else let
      prval $DB.ROOT_TAG_no() = rp
    in say("no root tag") end)
  val () = rel(vz, vb)
  val () = rel(tz, tb)
  val () = rel(az, ab)
  val () = rel(ez, eb)
  val () = rel(cz, cbid)
  val () = rel(sz, sb)
  val () = $DB.close_book_document(doc)
in say("@snap book") end

fn read_and_play {m:nat}{n:int | n >= 70} (f: !$OU.infile_outside(m), n: int n): void = let
  val a0 = $A.alloc<byte>(24)
  val @(p0, c0, l0 | b0) = $OU.file_read_outside(f, 0, a0, 24)
  val a1 = $A.alloc<byte>(28)
  val @(p1, c1, l1 | b1) = $OU.file_read_outside(f, 24, a1, 28)
  val a2 = $A.alloc<byte>(18)
  val @(p2, c2, l2 | b2) = $OU.file_read_outside(f, 52, a2, 18)
  val @(z0, v0) = $A.barr_freeze(b0)
  val @(z1, v1) = $A.barr_freeze(b1)
  val @(z2, v2) = $A.barr_freeze(b2)
  val () = play(p0, p1, p2 | v0, 24, v1, 28, v2, 18)
  val () = $A.cborrow_drop(z0, v0)
  val () = $A.cborrow_drop(z1, v1)
  val () = $A.cborrow_drop(z2, v2)
  val back0 = $A.barr_to_arrx(l0 | $A.barr_thaw(z0))
  val back1 = $A.barr_to_arrx(l1 | $A.barr_thaw(z1))
  val back2 = $A.barr_to_arrx(l2 | $A.barr_thaw(z2))
  val () = $A.free<byte>(back0)
  val () = $A.free<byte>(back1)
in $A.free<byte>(back2) end

fn handle {m:nat} (f: $OU.infile_outside(m)): void = let
  val n = $OU.file_size_outside(f)
  val () = (if n >= 70 then read_and_play(f, n) else say("short"))
in $OU.file_close_outside(f) end

implement main0 () = let
  val @(iz, ib) = lit("script")
  val opened = $OU.file_open_at_outside(ib, 6, 0)
  val () = rel(iz, ib)
in $P.finish<$OU.opened_outside>(opened, llam (got) =>
  case+ got of
  | ~$OU.OpenedOutside(f) => handle(f)
  | ~$OU.NotOpenedOutside() => say("not opened")
  | ~$OU.OpenFailedOutside() => say("open failed")) end
