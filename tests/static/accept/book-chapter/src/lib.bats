#include "share/atspre_staload.hats"
#use array as A
#use css as C
#use wasm.bats-packages.dev/bridge as B
#use wasm.bats-packages.dev/dom as D
staload OU = "wasm.bats-packages.dev/bridge/src/outside.bats"
staload DB = "wasm.bats-packages.dev/dom/src/book.bats"
staload CB = "css/src/book.sats"

(* A chapter from an outside file: the bytes are minted through bridge
   (file_read_outside), the document and its container are opened over them,
   and an element, its names, a style, a sheet and a face are written from
   the same bytes, then everything is closed *)
fn lit {n:pos | n < 256} (s: string n): [l:agz] @($A.frozen(byte, l, n, 1), $A.borrow(byte, l, n)) = let
  val n = g1u2i(string1_length(s))
  val a = $A.alloc<byte>(n)
  val () = $A.write_text(a, 0, $A.text_lit(s), n)
in $A.freeze<byte>(a) end

fn rel {l:agz}{n:nat} (fz: $A.frozen(byte, l, n, 1), b: $A.borrow(byte, l, n)): void = let
  val () = $A.drop<byte>(fz, b)
in $A.free<byte>($A.thaw<byte>(fz)) end

fn write_chapter {l:agz}{n:pos}{cs:$A.cells}
  (pf: $OU.OUTSIDE_READ(cs) | src: !$A.cborrow(l, n, cs), n: int n): void = let
  val doc = $DB.open_book_document(pf | 1)
  val @(rp | root) = $DB.book_root_scan(src, n, 0)
  val @(sz, sb) = lit("stage")
  val @(cz, cbid) = lit("chap")
  val @(ez, eb) = lit("c1")
  val () = (if root then let
      prval $DB.ROOT_TAG_yes(rt) = rp
    in
      case+ $DB.add_book_container(rt | doc, src, 0, sb, 5, cbid, 4) of
      | ~$DB.BookContainerMade(c) => let
          val () = $DB.add_book_element(doc, c, eb, 2, sb, 5, $DB.BookTagP(), src, 0, 1)
          val () = $DB.set_book_names(doc, c, eb, 2, 1, src, 0, 1)
          val read = $CB.book_sheet_read(src, n, 1, @(255, 255, 255), @(250, 240, 220))
          val decls = $CB.book_style_read(src, n, 1, @(255, 255, 255), @(250, 240, 220))
          val () = $DB.set_book_style(doc, c, eb, 2, src, read, decls)
          val () = $DB.add_book_sheet(doc, c, src, read)
          val () = $CB.book_decls_free(decls)
          val () = $CB.book_read_free(read)
          val () = $DB.add_book_font(pf | doc, c, src, 0, 1, 0, 400, 0)
        in $DB.close_book_container(c) end
      | ~$DB.BookContainerRefused() => ()
    end
    else let
      prval $DB.ROOT_TAG_no() = rp
    in () end)
  val () = rel(ez, eb)
  val () = rel(cz, cbid)
  val () = rel(sz, sb)
in $DB.close_book_document(doc) end

fn chapter {n:pos | n <= 1048576} (f: !$OU.infile_outside(n), n: int n): void = let
  val a = $A.alloc<byte>(n)
  val @(pfo, pfc, pflent | ba) = $OU.file_read_outside(f, 0, a, n)
  val @(fz, cb) = $A.barr_freeze(ba)
  val () = write_chapter(pfo | cb, n)
  val () = $A.cborrow_drop(fz, cb)
  val back = $A.barr_to_arrx(pflent | $A.barr_thaw(fz))
in $A.free<byte>(back) end
