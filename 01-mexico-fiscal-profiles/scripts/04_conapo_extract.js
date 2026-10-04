// 04_conapo_extract.js -------------------------------------------------------
// Builds the two CONAPO extracts in data/conapo/ that 05_projection.R reads.
//
// Source: CONAPO, Conciliación demográfica 1950-2019 y Proyecciones de la
// población de México y de las entidades federativas 2020-2070, listed on
// datos.gob.mx (dataset "Proyecciones de población",
// id f2b9b220-3ef7-4e3a-bde6-87e1dac78c6a) and hosted at
// https://repodatos.atdt.gob.mx/CONAPO/proyecciones/.
//
// Why a browser snippet: on 2026-10-02 the host answered 403 "Access Denied"
// to curl, R and Python (any user agent, from a Mexican IP), but served the
// files to a browser. So: open https://repodatos.atdt.gob.mx/CONAPO/ in a
// browser, paste this into the developer console, and it saves the two CSVs.
//
// The population and deaths files hold the 32 states and no national row, so
// the snippet sums the states (and checks there are 32). The fertility file
// has national rows (CVE_GEO 0). Printed sums let you check the files that
// were saved against the run that made the committed copies:
//   pob_inicio 2025 Hombres 64,983,857   Mujeres 67,849,944
//   pob_mitad  2025 Hombres 65,247,137   Mujeres 68,120,291
//   defunciones 2025 Hombres 472,483     Mujeres 364,102

(async () => {
  const base = "https://repodatos.atdt.gob.mx/CONAPO/proyecciones/";
  const YEAR = "2025";

  async function bySexAge(file, valCol) {
    const lines = (await (await fetch(base + file)).text()).split(/\r?\n/);
    const h = lines[0].split(",");
    const iy = h.indexOf("ANIO"), ia = h.indexOf("EDAD"), is = h.indexOf("SEXO"),
          iv = h.indexOf(valCol), ig = h.indexOf("CVE_GEO");
    const out = new Map(), states = new Set();
    for (const l of lines.slice(1)) {
      if (!l) continue;
      const c = l.split(",");
      if (c[iy] !== YEAR || c[ig] === "0") continue;
      states.add(c[ig]);
      const k = c[ia] + "," + c[is];
      out.set(k, (out.get(k) || 0) + Number(c[iv]));
    }
    if (states.size !== 32) throw new Error(file + ": " + states.size + " states");
    return out;
  }

  const ini = await bySexAge("00_Pob_Inicio_1950_2070.csv", "POBLACION");
  const mid = await bySexAge("00_Pob_Mitad_1950_2070.csv", "POBLACION");
  const dth = await bySexAge("01_Defunciones_1950_2070.csv", "DEFUNCIONES");

  const rows = ["edad,sexo,pob_inicio_2025,pob_mitad_2025,defunciones_2025"];
  const sums = {};
  for (const sex of ["Hombres", "Mujeres"]) {
    for (let a = 0; a <= 109; a++) {
      const k = a + "," + sex;
      rows.push([a, sex, ini.get(k), mid.get(k), dth.get(k)].join(","));
      for (const [n, m] of [["ini", ini], ["mid", mid], ["dth", dth]])
        sums[n + sex] = (sums[n + sex] || 0) + m.get(k);
    }
  }
  console.log(sums);

  const fert = (await (await fetch(base + "04_Tasas_Especificas_Fecundidad_proyecciones.csv")).text())
    .split(/\r?\n/)
    .filter(l => /,República Mexicana,0,/.test(l) && /,(2024|2025),/.test(l))
    .map(l => { const c = l.split(","); return [c[1], c[4], c[5], c[6]].join(","); });

  const save = (name, text) => {
    const a = document.createElement("a");
    a.href = URL.createObjectURL(new Blob([text + "\n"], { type: "text/csv" }));
    a.download = name;
    a.click();
  };
  save("conapo_2025_national.csv", rows.join("\n"));
  save("conapo_fecundidad_national_2024_2025.csv",
       ["anio,gpo_edad,tasa_por_mil,nacimientos", ...fert].join("\n"));
})();
