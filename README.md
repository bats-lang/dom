# dom

Type-safe DOM diffing with batched streaming. Operations accumulate in a 256 KB
buffer and auto-flush when full, so you never pay for more than one
synchronous bridge call per batch.

All stream operations consume and return the stream linearly — you cannot
forget to end a batch or use a stream after ending it.

## Types

```
absvtype state(l:addr)     (* owns the 256 KB diff buffer *)
absvtype stream(l:addr)    (* accumulates ops, auto-flushes *)
```

## API

```
#use wasm.bats-packages.dev/dom as D
#use array as A

(* Lifecycle *)
$D.init()                  : [l:agz] state(l)
$D.fini{l:agz}(s: state(l)) : void

(* Batch begin / end *)
$D.begin{l:agz}(s: state(l))  : stream(l)
$D.end{l:agz}(s: stream(l))   : state(l)

(* Create a DOM element under parent *)
$D.create_element{l:agz}{lb:agz}{n:nat}
  (s: stream(l), node_id: int, parent_id: int,
   tag: !A.borrow(byte, lb, n), tag_len: int n) : stream(l)

(* Set text content from a borrow *)
$D.set_text{l:agz}{lb:agz}{n:nat}
  (s: stream(l), node_id: int,
   text: !A.borrow(byte, lb, n), text_len: int n) : stream(l)

(* Set an attribute from borrows *)
$D.set_attr{l:agz}{ln:agz}{lv:agz}{nn:nat}{nv:nat}
  (s: stream(l), node_id: int,
   attr_name: !A.borrow(byte, ln, nn), name_len: int nn,
   value: !A.borrow(byte, lv, nv), value_len: int nv) : stream(l)

(* Set inline style from a borrow *)
$D.set_style{l:agz}{lb:agz}{n:nat}
  (s: stream(l), node_id: int,
   style: !A.borrow(byte, lb, n), style_len: int n) : stream(l)

(* Remove all children of a node *)
$D.remove_children{l:agz}
  (s: stream(l), node_id: int) : stream(l)

(* Remove a single child node *)
$D.remove_child{l:agz}
  (s: stream(l), node_id: int) : stream(l)

(* Set text from safe text (no borrow needed) *)
$D.set_safe_text{l:agz}{n:nat}
  (s: stream(l), node_id: int,
   text: A.text(n), text_len: int n) : stream(l)

(* Set attribute from safe text *)
$D.set_attr_safe{l:agz}{nn:nat}{nv:nat}
  (s: stream(l), node_id: int,
   attr_name: A.text(nn), name_len: int nn,
   value: A.text(nv), value_len: int nv) : stream(l)

(* Set image src via blob URL (direct bridge call) *)
$D.set_image_src{l:agz}{ld:agz}{lm:agz}{nd:nat}{nm:nat}
  (s: stream(l), node_id: int,
   data: !A.borrow(byte, ld, nd), data_len: int nd,
   mime_type: !A.borrow(byte, lm, nm), mime_len: int nm) : stream(l)
```

## Attributes and URLs

No attribute dom writes can run script, and the types say so:

- `set_attr` takes an `attribute`, a datatype of the attributes it may
  set (`Class`, `Title`, `Lang`, `Role`, `Value`, ...,
  `Aria("label")` for `aria-label`, `Data("set")` for `data-set`). None
  is an event handler: there is no constructor for a name starting with
  `on`, and `Aria`/`Data` write their prefix first. A string name, such
  as `"onclick"` or `"ONLOAD"`, does not type-check. The list names what
  is allowed, so an attribute not thought of (`srcdoc`, `poster`, ...)
  cannot be set at all.
- `href`, `src`, `action`, `formaction` and `xlink:href` are a
  `url_attribute` (`Href`, `Src`, `Action`, `Formaction`, `XlinkHref`),
  which `set_attr` does not take. They are set by:
  - `set_url(doc, id, ilen, name, v, off, len): bool`: bytes read at run
    time, set only when they are an `http:`, `https:`, `blob:` or
    `mailto:` URL (scheme in any case), a relative path or a fragment.
    Anything that starts with a control or a space, or has a `:` before
    any `/`, `?` or `#` after another scheme (`javascript:`, `data:`,
    ...), is refused. The check and the copy happen in one call; the
    result says whether it was set.
  - `set_url_literal(doc, id, ilen, name, value)`: a `url_literal`,
    whose constructor writes the scheme (`Https("example.com/")`,
    `Http(...)`, `Mailto(...)`, `Fragment("top")` for `#top`,
    `Path("a.html")` for `./a.html`, `EmptyData` for `data:,`, an
    image's placeholder), so no literal can be a `javascript:` URL.
  - `remove_url(doc, id, ilen, name)` removes one.
- A widget's `href`, `src` and `action` (`A`, `Img`, `Form`, `Video`,
  `Audio`, `Source`, `Track` and their changes) get the same check as
  `set_url`. One that is refused is removed.

bridge's flush also skips `on*` names and `javascript:` URLs, as
defence in depth (bats-lang/bridge#99).

## Dependencies

- **array**
