import { useMemo, useState } from "react";
import lispCode from "./RRMSEC.lsp?raw";

type Pt = [number, number];
type Poly = { layer: string; pts: Pt[] };

const LAYERS: Record<string, { aci: number; css: string }> = {
  STONE_SOLING: { aci: 8, css: "#8a8a8a" },
  PCC_BED: { aci: 2, css: "#ffff00" },
  RRM_WALL: { aci: 30, css: "#ff7f00" },
  PCC_CAP: { aci: 4, css: "#00ffff" },
};

const parseHeights = (s: string) =>
  s.split(/[,;\s]+/).filter(Boolean).map((x) => parseFloat(x));

function section(v: Record<string, number>, h: number, xo: number) {
  const { ts, tp, cv, tc, wc, bf } = v;
  const sl = v.sl / 100;
  const bk = v.bk / 100;
  const bb = bf * h;
  const bw = bb + 2 * cv;
  const d = sl * bw;
  const r0x = cv, r0y = -sl * r0x;
  const r1x = cv + bb, r1y = -sl * r1x;
  const t0x = r0x + bk * h, t1x = t0x + wc, ty = r0y + h;
  const err = t1x >= r1x ? `Height ${h}: top width + back slope is wider than the RRM base (front face would overhang).` : null;
  const s = (pts: Pt[]) => pts.map(([x, y]) => [x + xo, y] as Pt);
  const polys: Poly[] = [
    { layer: "STONE_SOLING", pts: s([[0, -tp], [bw, -d - tp], [bw, -d - tp - ts], [0, -tp - ts]]) },
    { layer: "PCC_BED", pts: s([[0, 0], [bw, -d], [bw, -d - tp], [0, -tp]]) },
    { layer: "RRM_WALL", pts: s([[r0x, r0y], [r1x, r1y], [t1x, ty], [t0x, ty]]) },
    { layer: "PCC_CAP", pts: s([[t0x, ty], [t1x, ty], [t1x, ty + tc], [t0x, ty + tc]]) },
  ];
  return { polys, bw, bb, d, back: bk * h, err };
}

const FIELDS: { k: string; label: string; unit?: string; wide?: boolean }[] = [
  { k: "ts", label: "Thickness of stone soling", unit: "m" },
  { k: "tp", label: "Thickness of PCC bed", unit: "m" },
  { k: "cv", label: "Cover", unit: "m" },
  { k: "hh", label: "RRM height(s) e.g. 2,2.5,3,4", unit: "m", wide: true },
  { k: "tc", label: "Thickness of PCC cap", unit: "m" },
  { k: "wc", label: "Width of PCC cap (top width)", unit: "m" },
  { k: "sl", label: "Bed slope (% of bottom width)", unit: "%" },
  { k: "bf", label: "Base factor (RRM base / height)" },
  { k: "bk", label: "Back slope (% of RRM height)", unit: "%" },
  { k: "gp", label: "Gap between sections", unit: "m" },
];

export default function App() {
  const [vals, setVals] = useState<Record<string, string>>({
    ts: "0.15", tp: "0.10", cv: "0.10", hh: "2, 2.5, 3, 4", tc: "0.05", wc: "0.5", sl: "10", bf: "0.6", bk: "10", gp: "2",
  });
  const [copied, setCopied] = useState(false);
  const [showCode, setShowCode] = useState(false);

  const nums = useMemo(() => {
    const o: Record<string, number> = {};
    for (const k in vals) if (k !== "hh") o[k] = parseFloat(vals[k]) || 0;
    return o;
  }, [vals]);

  const { items, polys, error } = useMemo(() => {
    const hs = parseHeights(vals.hh);
    if (!hs.length) return { items: [], polys: [] as Poly[], error: "Enter RRM height(s), e.g. 2,2.5,3,4" };
    if (hs.some((h) => !(h > 0))) return { items: [], polys: [] as Poly[], error: "Heights must be numbers greater than 0." };
    let xo = 0;
    const items = hs.map((h) => {
      const r = section(nums, h, xo);
      xo += r.bw + nums.gp;
      return { h, ...r };
    });
    const bad = items.find((i) => i.err);
    return { items, polys: items.flatMap((i) => i.polys), error: bad ? bad.err : null };
  }, [vals.hh, nums]);

  const view = useMemo(() => {
    if (!polys.length) return "0 0 10 10";
    const all = polys.flatMap((p) => p.pts);
    const xs = all.map((p) => p[0]), ys = all.map((p) => -p[1]);
    const w = Math.max(...xs) - Math.min(...xs), h = Math.max(...ys) - Math.min(...ys);
    const pad = 0.08 * Math.max(w, h);
    return `${Math.min(...xs) - pad} ${Math.min(...ys) - pad} ${w + 2 * pad} ${h + 2 * pad}`;
  }, [polys]);

  const maxH = Math.max(1, ...items.map((i) => i.h));

  const copy = async () => {
    await navigator.clipboard.writeText(lispCode);
    setCopied(true);
    setTimeout(() => setCopied(false), 1500);
  };
  const download = () => {
    const url = URL.createObjectURL(new Blob([lispCode], { type: "text/plain" }));
    const a = document.createElement("a");
    a.href = url;
    a.download = "RRMSEC.lsp";
    a.click();
    URL.revokeObjectURL(url);
  };

  return (
    <div className="min-h-screen bg-slate-900 text-slate-100">
      <header className="border-b border-slate-700 px-6 py-4">
        <h1 className="text-2xl font-bold">RRM Wall Section – multiple heights, one go</h1>
        <p className="text-sm text-slate-400">
          AutoCAD &amp; ZWCAD · command <code className="text-amber-300">RRMSEC</code> (alias <code className="text-amber-300">RRMMULTI</code>) · enter heights like{" "}
          <code className="text-amber-300">2,2.5,3,4</code> · layered, no text
        </p>
      </header>

      <main className="mx-auto grid max-w-6xl gap-6 p-6 lg:grid-cols-5">
        <section className="rounded-lg border border-slate-700 bg-slate-800 p-4 lg:col-span-2">
          <h2 className="mb-3 font-semibold">Parameters (mirrors the LISP dialog)</h2>
          <div className="grid gap-2">
            {FIELDS.map((f) => (
              <label key={f.k} className="flex items-center justify-between gap-3 text-sm">
                <span>{f.label}</span>
                <span className="flex items-center gap-1">
                  <input
                    value={vals[f.k]}
                    onChange={(e) => setVals({ ...vals, [f.k]: e.target.value })}
                    className={`${f.wide ? "w-36" : "w-24"} rounded border px-2 py-1 text-right bg-slate-900 ${
                      f.wide ? "border-amber-500" : "border-slate-600"
                    }`}
                  />
                  <span className="w-4 text-slate-400">{f.unit}</span>
                </span>
              </label>
            ))}
          </div>
          {!error && items.length > 0 && (
            <div className="mt-4 overflow-x-auto rounded bg-slate-900 p-3 text-xs text-slate-300">
              <table className="w-full text-right">
                <thead className="text-slate-500">
                  <tr><th className="text-left">Height</th><th>RRM base</th><th>Bottom w.</th><th>Bed drop</th><th>Back off.</th></tr>
                </thead>
                <tbody>
                  {items.map((i, n) => (
                    <tr key={n}>
                      <td className="text-left">{i.h}</td>
                      <td>{i.bb.toFixed(3)}</td><td>{i.bw.toFixed(3)}</td><td>{i.d.toFixed(3)}</td><td>{i.back.toFixed(3)}</td>
                    </tr>
                  ))}
                </tbody>
              </table>
              <div className="mt-1 text-slate-500">RRM top width = {nums.wc.toFixed(3)}</div>
            </div>
          )}
        </section>

        <section className="rounded-lg border border-slate-700 bg-slate-800 p-4 lg:col-span-3">
          <h2 className="mb-3 font-semibold">Section preview ({items.length} section{items.length === 1 ? "" : "s"})</h2>
          <div className="aspect-[16/10] rounded bg-black">
            {error ? (
              <div className="flex h-full items-center justify-center p-6 text-center text-sm text-red-400">{error}</div>
            ) : (
              <svg viewBox={view} className="h-full w-full" preserveAspectRatio="xMidYMid meet">
                {polys.map((p, n) => (
                  <polygon
                    key={n}
                    points={p.pts.map(([x, y]) => `${x},${-y}`).join(" ")}
                    fill={LAYERS[p.layer].css}
                    fillOpacity={0.35}
                    stroke={LAYERS[p.layer].css}
                    strokeWidth={0.006 * maxH * Math.max(1, items.length / 2)}
                    strokeLinejoin="round"
                  />
                ))}
              </svg>
            )}
          </div>
          <div className="mt-3 flex flex-wrap gap-3 text-xs">
            {Object.entries(LAYERS).map(([n, l]) => (
              <span key={n} className="flex items-center gap-1">
                <span className="inline-block h-3 w-3 rounded-sm" style={{ background: l.css }} />
                {n} <span className="text-slate-500">(colour {l.aci})</span>
              </span>
            ))}
          </div>
        </section>

        <section className="rounded-lg border border-slate-700 bg-slate-800 p-4 lg:col-span-5">
          <div className="mb-3 flex flex-wrap items-center gap-2">
            <h2 className="mr-auto font-semibold">RRMSEC.lsp</h2>
            <button onClick={() => setShowCode(!showCode)} className="rounded bg-slate-700 px-3 py-1.5 text-sm hover:bg-slate-600">
              {showCode ? "Hide code" : "Show code"}
            </button>
            <button onClick={copy} className="rounded bg-slate-700 px-3 py-1.5 text-sm hover:bg-slate-600">
              {copied ? "Copied ✓" : "Copy code"}
            </button>
            <button onClick={download} className="rounded bg-amber-500 px-3 py-1.5 text-sm font-semibold text-black hover:bg-amber-400">
              Download RRMSEC.lsp
            </button>
          </div>
          <ol className="mb-3 list-decimal space-y-1 pl-5 text-sm text-slate-300">
            <li>Save as <code>RRMSEC.lsp</code>, then in CAD type <code>APPLOAD</code> and load it.</li>
            <li>Type <code>RRMSEC</code>. In <b>RRM height(s)</b> type one value or many separated by commas: <code>2,2.5,3,4</code>.</li>
            <li>One section per height is drawn left to right, starting at the insertion point, spaced by the gap value. All other inputs are shared.</li>
            <li>Set "Drawing units per metre" to 1000 if your drawing is in millimetres.</li>
          </ol>
          {showCode && (
            <pre className="max-h-[480px] overflow-auto rounded bg-black p-4 text-xs leading-relaxed text-green-300">{lispCode}</pre>
          )}
        </section>
      </main>
    </div>
  );
}
