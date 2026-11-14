// 04_cfe_extract.js -------------------------------------------------------------
// Browser snippet: CFE's household tariff pages (app.cfe.mx) refuse scripted
// clients and any browser outside Mexico ("Access denied", Imperva), so the monthly
// charges are read from inside a browser session opened on one of those pages
// (from Mexico, or through a VPN with a Mexican exit).
//
// How to run: open
//   https://app.cfe.mx/Aplicaciones/CCFE/Tarifas/TarifasCRECasa/Tarifas/Tarifa1.aspx
// paste this file in the console, then call for example
//   await cfeExtract('Tarifa1A', 2026)      // one tariff, every month offered
// and save the returned lines as data/raw/cfe/<page>_<year>.txt.
// Pages: Tarifa1, Tarifa1A ... Tarifa1F, TarifaDAC.
//
// Each line: page ; year ; summer start chosen (or -) ; month consulted ; text of
// one charge row as the page prints it. For 1A to 1F the page asks for the month
// in which the locality's summer starts (February to May); the snippet asks twice,
// with February and with May, so every month comes back with its out-of-summer
// charges and February to October also with their summer charges.
// Characters = & ? are replaced by spaces (the session tool blocks them).
//
// For past years one summer start is enough: await cfeExtract('Tarifa1A', 2021, ['ABRIL'])
// gives April to September at summer charges and the other months outside summer.
// Years 2016 and 2017 sit in the archive: cfeExtract('Tarifa1', 2016, ['ABRIL'], 'AcervoHistorico'),
// run from a tab opened on .../AcervoHistorico/Tarifas/Tarifa1. The archive's 2017 holds
// January to November (the scheme before Acuerdo 123/2017).
//
// A fifth argument limits the months asked. The summer prices the yearly history lacks
// (February and March where summer starts in February, October where it starts in May):
//   cfeExtract(page, year, ['FEBRERO'], folder, ['FEBRERO', 'MARZO']) and
//   cfeExtract(page, year, ['MAYO'], folder, ['OCTUBRE']), compacted by cfeSummerBlock.

window.cfeExtract = async (page, year, summerStarts = ['FEBRERO', 'MAYO'], folder = 'TarifasCRECasa', onlyMonths = null) => {
  // The archive answers a POST only at the address without ".aspx" (with it, the
  // year stays on 2017 and no charges come back).
  const url = 'https://app.cfe.mx/Aplicaciones/CCFE/Tarifas/' + folder + '/Tarifas/' + page +
    (folder === 'AcervoHistorico' ? '' : '.aspx');
  const parse = async (r) => new DOMParser().parseFromString(await r.text(), 'text/html');
  const sleep = (ms) => new Promise((ok) => setTimeout(ok, ms));
  const post = async (doc, target, vals) => {
    const fd = new URLSearchParams();
    for (const el of doc.forms[0].elements) {
      if (!el.name || ['submit', 'button', 'image'].includes(el.type)) continue;
      fd.set(el.name, el.value);
    }
    for (const [k, v] of Object.entries(vals)) fd.set(k, v);
    fd.set('__EVENTTARGET', target);
    fd.set('__EVENTARGUMENT', '');
    const r = await fetch(url, {
      method: 'POST', body: fd, credentials: 'include',
      headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
    });
    if (r.status !== 200) throw new Error(page + ': HTTP ' + r.status);
    const d = await parse(r);
    // a refusal ("Access denied") also comes back with status 200: stop at once
    if (!sel(d, /ddAnio/)) throw new Error(page + ' ' + year + ': no tariff page in the answer');
    return d;
  };
  const sel = (doc, re) => [...doc.querySelectorAll('select')].find((s) => re.test(s.name));
  const rows = (doc) => [...doc.querySelectorAll('table tr')]
    .map((tr) => tr.innerText || tr.textContent)
    .map((t) => t.replace(/\s+/g, ' ').replace(/[=&?]/g, ' ').trim())
    .filter((t) => /\d\.\d{1,3}/.test(t) && t.length < 260 &&   // CFE drops trailing zeros (1.5)
                   /Consumo|Baja California|Central|Noroeste|Norte|Sur y|Cargo fijo/i.test(t));

  let doc = await parse(await fetch(url, { credentials: 'include' }));
  const ySel = sel(doc, /ddAnio/);
  if (ySel.value !== String(year)) doc = await post(doc, ySel.name, { [ySel.name]: String(year) });

  const out = [];
  const starts = sel(doc, /ddMesVerano$/) ? summerStarts : ['-'];
  for (const start of starts) {
    if (start !== '-') {
      const s = sel(doc, /ddMesVerano$/);
      const o = [...s.options].find((x) => x.text.trim() === start);
      doc = await post(doc, s.name, { [s.name]: o.value });
      await sleep(350);
    }
    // the month list is ddMesConsulta, or ddMesCRE on the archive's 2017 pages
    const mSel = sel(doc, /ddMes(Consulta|CRE)$/);
    const months = [...mSel.options].filter((o) => /^[A-Z]+$/.test(o.text.trim()))
      .filter((o) => !onlyMonths || onlyMonths.includes(o.text.trim()));
    for (const m of months) {
      const d = await post(doc, mSel.name, { [mSel.name]: m.value });
      const got = [...new Set(rows(d))];
      for (const g of got) out.push([page, year, start, m.text.trim(), g].join(' ; '));
      if (!got.length) out.push([page, year, start, m.text.trim(), 'NO ROWS'].join(' ; '));
      await sleep(350);
    }
  }
  return out.join('\n');
};

// Compaction used for the yearly history (data/raw/cfe/cfe_tarifas_2018_2025.txt) ---
// After filling H[page + '_' + year] with cfeExtract(page, year, ['ABRIL']) for the
// eight pages of a year, cfeYearBlock(H, year) returns the year as a short block:
// the lines T, Z, S, D and F described in that file's header, under a header line
// that carries an FNV-1a hash of the block. The checks behind line F run here, on
// the full readings, before anything is dropped.
window.cfeFnv = (s) => {
  let h = 0x811c9dc5;
  for (const c of new TextEncoder().encode(s)) { h ^= c; h = Math.imul(h, 0x01000193) >>> 0; }
  return h.toString(16);
};
window.cfeYearBlock = (H, y) => {
  const MM = ['ENERO', 'FEBRERO', 'MARZO', 'ABRIL', 'MAYO', 'JUNIO', 'JULIO', 'AGOSTO', 'SEPTIEMBRE', 'OCTUBRE', 'NOVIEMBRE', 'DICIEMBRE'];
  const tarRows = (txt) => {
    const by = {};
    for (const l of txt.split('\n')) {
      const p = l.split(' ; '); const r = p[4];
      const prices = r.match(/\d+\.\d{1,3}/g) || [];
      if (/^Consumo/.test(r) && prices.length === 1) {
        const m = MM.indexOf(p[3]) + 1;
        const kwh = (r.match(/(?:primeros|siguientes) ([\d,]+)/) || [, 'r'])[1].replace(',', '');
        (by[m] = by[m] || []).push([prices[0], kwh]);
      }
    }
    return by;
  };
  const dacRows = (txt) => {
    const by = {};
    for (const l of txt.split('\n')) {
      const p = l.split(' ; ');
      // the fixed charge may print as an integer (98 in August 2017)
      const m = p[4].match(/^(Baja California Sur|Baja California|Central|Noroeste|Norte y Noreste|Sur y Peninsular) ?(\d+(?:\.\d+)?) ?((?:\$ ?\d+\.\d+ ?)+)$/);
      if (!m) continue;
      const k = MM.indexOf(p[3]) + 1;
      by[k] = by[k] || {};
      by[k][m[1]] = [m[2], ...m[3].match(/\d+\.\d+/g)];
    }
    return by;
  };
  const eq = (a, b) => JSON.stringify(a.map(Number)) === JSON.stringify(b.map(Number));
  const nums = (o) => Object.keys(o).map(Number).sort((a, b) => a - b);
  const T = {};
  for (const t of ['1', '1A', '1B', '1C', '1D', '1E', '1F']) T[t] = tarRows(H['Tarifa' + t + '_' + y]);
  const L = []; const flags = []; const sizes = {}; const N = {};
  const months = nums(T['1']);
  for (const m of months) L.push('T ' + m + ' ' + T['1'][m].map((x) => x[0]).join(' '));
  for (const t of ['1A', '1B', '1C', '1D', '1E', '1F']) {
    for (const m of nums(T[t])) {
      const rows = T[t][m];
      const summer = Number(rows[0][1]) > 75;
      const key = t + (summer ? 's' : 'n');
      const sz = rows.map((x) => x[1]).join('/');
      if (sizes[key] && sizes[key] !== sz) flags.push('SIZECHANGE ' + key + ' ' + m);
      sizes[key] = sz;
      // a month the tariff 1 page does not offer (December 2017 in the archive): the first
      // tariff read stands as the reference and goes to a line N
      const ref = T['1'][m] ? T['1'][m].map((x) => x[0]) : (N[m] = N[m] || rows.map((x) => x[0]));
      if (!summer && !eq(rows.map((x) => x[0]), ref)) flags.push('NSDIFF ' + t + ' ' + m);
    }
  }
  for (const m of nums(N)) L.push('N ' + m + ' ' + N[m].join(' '));
  sizes['1'] = T['1'][months[0]].map((x) => x[1]).join('/');
  L.push('Z ' + Object.entries(sizes).sort().map(([k, v]) => k + ':' + v).join(' '));
  for (const m of nums(T['1A']).filter((k) => Number(T['1A'][k][0][1]) > 75)) {
    const pr = (t) => (T[t][m] || []).map((x) => x[0]);
    const a = pr('1A'), b = pr('1B'), c = pr('1C'), d = pr('1D'), e = pr('1E'), f = pr('1F');
    if (!eq(a.slice(0, 2), b.slice(0, 2)) || !eq(a.slice(0, 2), c.slice(0, 2)) || !eq(a.slice(0, 2), d.slice(0, 2))) flags.push('ABCD ' + m);
    if (Number(c[2]) !== Number(d[2])) flags.push('CD ' + m);
    if (!eq(e.slice(0, 2), f.slice(0, 2))) flags.push('EF ' + m);
    const exc = [a[2], b[2], c[3], d[3], e[3], f[3]].map(Number);
    if (new Set(exc).size !== 1 || exc[0] !== Number(T['1'][m][2][0])) flags.push('EXC ' + m);
    L.push('S ' + m + ' ' + a.slice(0, 2).join(' ') + ' ' + c[2] + ' ' + e.slice(0, 3).join(' ') + ' ' + f[2]);
  }
  const D = dacRows(H['TarifaDAC_' + y]);
  for (const m of nums(D)) {
    const r = D[m];
    if (new Set(Object.values(r).map((v) => v[0])).size !== 1) flags.push('FIXED ' + m);
    const bc = r['Baja California'], bs = r['Baja California Sur'];
    L.push('D ' + m + ' ' + bc[0] + ' ' + bc.slice(1).join(' ') + ' ' + bs.slice(1).join(' ') + ' ' +
           ['Central', 'Noroeste', 'Norte y Noreste', 'Sur y Peninsular'].map((k) => r[k][1]).join(' '));
  }
  L.push('F ' + (flags.length ? flags.join(',') : 'none'));
  const body = L.join('\n');
  return '# ' + y + ' ' + window.cfeFnv(body) + '\n' + body;
};

// Summer prices of February, March and October (data/raw/cfe/cfe_verano_feb_mar_oct.txt) ---
// H[page + '_' + year] holds, for the six pages 1A to 1F, the lines of the two limited
// calls described at the top. Lines: S month and the seven summer prices in the order of
// cfeYearBlock; E month and the excess price, the same in the six tariffs (to compare on
// disk with tariff 1's); F the checks, as in cfeYearBlock.
window.cfeSummerBlock = (H, y) => {
  const MM = ['ENERO', 'FEBRERO', 'MARZO', 'ABRIL', 'MAYO', 'JUNIO', 'JULIO', 'AGOSTO', 'SEPTIEMBRE', 'OCTUBRE', 'NOVIEMBRE', 'DICIEMBRE'];
  const eq = (a, b) => JSON.stringify(a.map(Number)) === JSON.stringify(b.map(Number));
  const T = {}; const L = []; const E = []; const flags = [];
  for (const t of ['1A', '1B', '1C', '1D', '1E', '1F']) {
    T[t] = {};
    for (const l of H['Tarifa' + t + '_' + y].split('\n')) {
      const p = l.split(' ; '); const r = p[4];
      const prices = r.match(/\d+\.\d{1,3}/g) || [];
      if (/^Consumo/.test(r) && prices.length === 1) {
        const m = MM.indexOf(p[3]) + 1;
        const kwh = (r.match(/(?:primeros|siguientes) ([\d,]+)/) || [, 'r'])[1].replace(',', '');
        (T[t][m] = T[t][m] || []).push([prices[0], kwh]);
      }
    }
  }
  for (const m of [2, 3, 10]) {
    const pr = (t) => (T[t][m] || []).map((x) => x[0]);
    const a = pr('1A'), b = pr('1B'), c = pr('1C'), d = pr('1D'), e = pr('1E'), f = pr('1F');
    if (![a, b].every((x) => x.length === 3) || ![c, d, e, f].every((x) => x.length === 4)) { flags.push('ROWS ' + m); continue; }
    if (['1A', '1B', '1C', '1D', '1E', '1F'].some((t) => !(Number(T[t][m][0][1]) > 75))) flags.push('NOTSUMMER ' + m);
    if (!eq(a.slice(0, 2), b.slice(0, 2)) || !eq(a.slice(0, 2), c.slice(0, 2)) || !eq(a.slice(0, 2), d.slice(0, 2))) flags.push('ABCD ' + m);
    if (Number(c[2]) !== Number(d[2])) flags.push('CD ' + m);
    if (!eq(e.slice(0, 2), f.slice(0, 2))) flags.push('EF ' + m);
    const exc = [a[2], b[2], c[3], d[3], e[3], f[3]].map(Number);
    if (new Set(exc).size !== 1) flags.push('EXC ' + m);
    L.push('S ' + m + ' ' + a.slice(0, 2).join(' ') + ' ' + c[2] + ' ' + e.slice(0, 3).join(' ') + ' ' + f[2]);
    E.push('E ' + m + ' ' + a[2]);
  }
  const body = [...L, ...E, 'F ' + (flags.length ? flags.join(',') : 'none')].join('\n');
  return '# ' + y + ' ' + window.cfeFnv(body) + '\n' + body;
};
