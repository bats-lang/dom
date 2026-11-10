// Runs dist/debug/app.wasm through the bridge.js that gen wrote with
// produce_bridge_app_book, in jsdom. The app reads the outside file "script"
// (an input whose first file is the three parts below) and writes a chapter
// with dom's book operations. This prints the document, the sheets and faces
// the bridge adopted (jsdom has no constructable style sheets or FontFace, so
// they are stood in for), and checks the bytes of the book stream against a
// written expectation.
import { JSDOM } from 'jsdom';
import { readFileSync, writeFileSync, unlinkSync } from 'node:fs';
import { join } from 'node:path';
import { tmpdir } from 'node:os';

const chapter = '<html><body>p intro lead'; // 24 bytes
const sheet = 'p{color:red}.intro{margin:0}'; // 28
const style = 'color:red;margin:0'; // 18
const script = chapter + sheet + style;

const dom = new JSDOM(
  '<!DOCTYPE html><html><body><div id="bats-root"><div id="stage"></div>' +
  '<input id="script" type="file"></div></body></html>',
  { url: 'http://localhost', pretendToBeVisual: true });
global.document = dom.window.document;
global.window = dom.window;
global.FileReader = dom.window.FileReader;
const file = new dom.window.File([script], 'script.txt');
Object.defineProperty(document.getElementById('script'), 'files', { value: [file] });

class Sheet { replaceSync(text) { this.text = text; } }
global.CSSStyleSheet = Sheet;
document.adoptedStyleSheets = [];
class Face {
  constructor(family, source, descriptors) {
    this.family = family; this.size = source.length; this.weight = descriptors.weight; this.style = descriptors.style;
  }
}
global.FontFace = Face;
const faces = new Set();
document.fonts = { add: (f) => faces.add(f), delete: (f) => faces.delete(f) };

const out = [];
const errors = [];
console.error = (...parts) => errors.push(parts.join(' '));

const src = readFileSync('dist/bridge.js', 'utf-8');
const boot = src.lastIndexOf("\nconst root = document.getElementById('bats-root');");
if (boot < 0) throw new Error('bridge.js: boot code not found');
const tmp = join(tmpdir(), `dom-book-${process.pid}.mjs`);
// The bytes of each book flush are kept (the flush function is wrapped, nothing else changes)
const flushLine = '  function batsDomFlushBook(bufPtr, len) { bookFlush(bufPtr, len, true); }\n';
if (!src.includes(flushLine)) throw new Error('bridge.js: batsDomFlushBook not found');
const flushes = [];
globalThis.__bookFlushes = flushes;
const recording = '  function batsDomFlushBook(bufPtr, len) {\n' +
  '    globalThis.__bookFlushes.push(Array.from(new Uint8Array(instance.exports.memory.buffer, bufPtr, len)));\n' +
  '    bookFlush(bufPtr, len, true);\n  }\n';
writeFileSync(tmp, src.slice(0, boot).replace(flushLine, recording) + '\n');
const { loadWASM } = await import(tmp);
unlinkSync(tmp);

const shown = [];
console.log = (...parts) => shown.push(parts.join(' ').replace(/^\[bats:\w+\] /, ''));
console.info = console.log;
await loadWASM(readFileSync('dist/debug/app.wasm'), document.getElementById('bats-root'), {});
await new Promise((r) => setTimeout(r, 200));

// The stream the operations write, as bytes: a name is [u16 length][bytes]
const bytes = (s) => [...Buffer.from(s)];
const name = (s) => [s.length & 255, s.length >> 8, ...bytes(s)];
const p = '[data-book-container="chap"] ';
const op = {
  // CREATE_BOOK_CONTAINER [13][id][parent][u8 layout]
  container: [13, ...name('chap'), ...name('stage'), 1],
  // CREATE_BOOK_ELEMENT [14][container][id][parent][u8 tag length][tag][u8 name length][name]
  element: [14, ...name('chap'), ...name('c1'), ...name('chap'), 1, ...bytes('p'), 1, ...bytes('p')],
  // SET_BOOK_NAMES [16][container][id][u8 kind][u16 length][bytes]
  names: [16, ...name('chap'), ...name('c1'), 1, ...name('intro lead')],
  // the style color:red;margin:0 in the pieces css gives: a property name, ":", the value
  // (a span of the file), ";": BOOK_STYLE_TEXT [19][container][id][u16 length][bytes], BOOK_STYLE_END [20][container][id]
  style: [
    ...[ 'color', ':', 'red', ';', 'margin', ':', '0', ';' ].flatMap((t) => [19, ...name('chap'), ...name('c1'), ...name(t)]),
    20, ...name('chap'), ...name('c1'),
  ],
  // the sheet in the pieces of css: each rule is the container prefix, the
  // selector rewritten to the book's elements, and the declarations;
  // BOOK_SHEET_TEXT [17][container][u16 length][bytes], BOOK_SHEET_END [18][container]
  sheet: null,
  // CREATE_ELEMENT [4][id][parent][u8 length][tag], SET_TEXT [1][id][u16 length][bytes],
  // APPEND_TEXT [6], SET_ATTR [2][id][u8 name length][name][u16 length][value], REMOVE_ATTR [7][id][u8 length][name]
  app: [
    4, ...name('app1'), ...name('c1'), 4, ...bytes('span'),
    1, ...name('app1'), ...name('hello'),
    6, ...name('app1'), ...name('hello'),
    2, ...name('app1'), 5, ...bytes('title'), ...name('mine'),
    7, ...name('app1'), 8, ...bytes('tabindex'),
    2, ...name('app1'), 6, ...bytes('data-x-y'), ...name('mine'),
  ],
};
// the sheet text as the pieces concatenate: the pieces of one rule are written in order
const sheetPieces = [];
let rest = null;
const flush = flushes[0] ?? [];
// split the flush into its operations by walking the formats
let i = 0;
const ops = [];
const u16 = () => { const v = flush[i] | (flush[i + 1] << 8); i += 2; return v; };
const str = () => { const n = u16(); const s = Buffer.from(flush.slice(i, i + n)).toString(); i += n; return s; };
while (i < flush.length) {
  const start = i;
  const code = flush[i++];
  let kind;
  if (code === 13) { str(); str(); i += 1; kind = 'container'; }
  else if (code === 14) { str(); str(); str(); const t = flush[i++]; i += t; const m = flush[i++]; i += m; kind = 'element'; }
  else if (code === 16) { str(); str(); i += 1; str(); kind = 'names'; }
  else if (code === 19) { str(); str(); str(); kind = 'style'; }
  else if (code === 20) { str(); str(); kind = 'style'; }
  else if (code === 17) { str(); const t = str(); sheetPieces.push(t); kind = 'sheet'; }
  else if (code === 18) { str(); kind = 'sheet'; }
  else if (code === 4) { str(); str(); const t = flush[i++]; i += t; kind = 'app'; }
  else if (code === 1 || code === 6) { str(); str(); kind = 'app'; }
  else if (code === 2) { str(); const n = flush[i++]; i += n; str(); kind = 'app'; }
  else if (code === 7) { str(); const n = flush[i++]; i += n; kind = 'app'; }
  else throw new Error(`unknown opcode ${code} at ${start}`);
  const seg = flush.slice(start, i);
  const last = ops[ops.length - 1];
  if (last && last.kind === kind) last.bytes.push(...seg); else ops.push({ kind, bytes: [...seg] });
}
const check = (label, got, want) => {
  if (JSON.stringify(got) !== JSON.stringify(want)) {
    throw new Error(`${label}: got ${JSON.stringify(Buffer.from(got).toString('latin1'))}, expected ${JSON.stringify(Buffer.from(want).toString('latin1'))}`);
  }
  out.push(`${label}: ${want.length} bytes, as written`);
};
const order = ['container', 'element', 'names', 'style', 'sheet', 'app'];
check('operations in order', ops.map((o) => o.kind), order);
for (const o of ops) if (o.kind !== 'sheet') check(o.kind, o.bytes, op[o.kind]);
// the sheet: the pieces joined are the text of the sheet, written here by hand
const sheetText = sheetPieces.join('');
const wantSheet =
  `${p}:is(p):where([data-book-element="p"]){color:red;}` ;
out.push(`sheet pieces: ${JSON.stringify(sheetPieces)}`);
out.push(`sheet text: ${sheetText}`);
check('flushes', [flushes.length], [1]);

out.push('== book');
out.push(document.getElementById('stage').innerHTML.replace(/></g, '>\n<'));
out.push(`sheets: ${document.adoptedStyleSheets.length}`);
for (const s of document.adoptedStyleSheets) out.push(`  ${s.text}`);
out.push(`faces: ${faces.size}`);
for (const f of faces) out.push(`  ${f.family} ${f.size} bytes weight ${f.weight} ${f.style}`);
for (const e of errors) out.push(`! ${e}`);
for (const l of shown) if (!l.startsWith('@snap')) out.push(`log: ${l}`);
process.stdout.write(out.join('\n') + '\n');
