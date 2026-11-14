// 09_verify_cfe_raw.js ----------------------------------------------------------
// Checks every reading of CFE's tariff pages in data/raw/cfe/ against the hash its
// own header carries. The readings were made inside a browser (04_cfe_extract.js),
// each block came out with an FNV-1a hash computed there, and the text reached the
// disk through a chat session; the hash is what proves the copy. This script repeats
// that check for all files at once, so a block edited or damaged later is caught.
//
// A block starts at a header line "# <name> <hash>" (for example "# 2021 e86cede1" or
// "# Tarifa1D FEBRERO d4f309b0") and runs to the next header or the end of the file.
// Prose header lines also start with "#" but do not end in a hash.
//
// Run from the post root: node scripts/09_verify_cfe_raw.js
const fs = require('fs');
const path = require('path');
const DIR = 'data/raw/cfe';
const fnv = (s) => {
  let h = 0x811c9dc5;
  for (const c of new TextEncoder().encode(s)) { h ^= c; h = Math.imul(h, 0x01000193) >>> 0; }
  return h.toString(16);
};
const HEAD = /^# (\S+(?: \S+)?) ([0-9a-f]{5,8})$/;
let bad = 0; let total = 0;
for (const f of fs.readdirSync(DIR).filter((x) => x.endsWith('.txt')).sort()) {
  const lines = fs.readFileSync(path.join(DIR, f), 'utf8').replace(/\n+$/, '').split('\n');
  const blocks = []; let cur = null;
  for (const l of lines) {
    const m = l.match(HEAD);
    if (m) { cur = { name: m[1], hash: m[2], body: [] }; blocks.push(cur); } else if (cur) cur.body.push(l);
  }
  if (!blocks.length) { console.log('NO BLOCKS  ' + f); bad++; continue; }
  const wrong = blocks.filter((b) => fnv(b.body.join('\n')) !== b.hash);
  total += blocks.length; bad += wrong.length;
  console.log((wrong.length ? 'MISMATCH   ' : 'ok         ') + f + '  ' + blocks.length + ' blocks' +
    (wrong.length ? '  wrong: ' + wrong.map((b) => b.name).join(', ') : ''));
}
console.log(total + ' blocks checked, ' + bad + ' problems');
process.exit(bad ? 1 : 0);
