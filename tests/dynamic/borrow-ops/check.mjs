// Runs dist/pwa/app.wasm through the bridge.js that pwa generated, in
// jsdom, and prints the mount's HTML and where the clone k1 is scrolled
// (compared with `expected`).
import { JSDOM } from 'jsdom';
import { readFileSync, writeFileSync, unlinkSync } from 'node:fs';
import { join } from 'node:path';
import { tmpdir } from 'node:os';

const dom = new JSDOM(
  '<!DOCTYPE html><html><body><div id="bats-root"></div></body></html>',
  { url: 'http://localhost', pretendToBeVisual: true });
global.document = dom.window.document;
global.window = dom.window;
// jsdom lays nothing out, so a scroll is only what was set
for (const name of ['scrollLeft', 'scrollTop']) {
  Object.defineProperty(dom.window.HTMLElement.prototype, name, {
    get() { return this['_' + name] || 0; },
    set(v) { this['_' + name] = v; },
    configurable: true,
  });
}

// bridge.js boots itself at its end; keep only loadWASM
const src = readFileSync('dist/pwa/bridge.js', 'utf-8');
const boot = src.lastIndexOf("\nconst root = document.getElementById('bats-root');");
if (boot < 0) throw new Error('bridge.js: boot code not found');
const tmp = join(tmpdir(), `dom-borrow-ops-${process.pid}.mjs`);
writeFileSync(tmp, src.slice(0, boot) + '\n');
const { loadWASM } = await import(tmp);
unlinkSync(tmp);

const root = document.getElementById('bats-root');
await loadWASM(readFileSync('dist/pwa/app.wasm'), root, {});
console.log(root.innerHTML);
const clone = document.getElementById('k1');
console.log(`k1 scrolled to ${clone.scrollLeft}, ${clone.scrollTop}`);
