# dom

Type-safe DOM diffing with batched streaming. Operations accumulate in a 256 KB
buffer and auto-flush when full, so you never pay for more than one
synchronous bridge call per batch.

All stream operations consume and return the stream linearly — you cannot
forget to end a batch or use a stream after ending it.

## Elements

`add_element` takes a `tag`, a datatype of the elements it may make
(`Div`, `P`, `Span`, `A`, `Img`, `Button`, `Input`, `Table`, `Ruby`, ...).
Nothing that runs or loads code, or changes how the page is read, has a
constructor: `script`, `iframe`, `frame`, `object`, `embed`, `base`,
`link`, `meta`, `template` and `noscript`, nor `form`. A string tag such
as `"script"` does not type-check. A `<style>` is made only with a
reason, `Stylesheet(AppStylesheet)`: CSS runs no script, and the one
reason is a sheet whose text the app writes itself. bridge's flush
refuses the excluded tags too (bats-lang/dom#60).

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
