const fs = require('node:fs');
const path = require('node:path');
const root = path.resolve(__dirname, '..');
const required = [
  'package.json','src/main.js','src/preload.js','src/renderer/index.html','src/renderer/styles.css','src/renderer/app.js',
  'r/pipeline.R','r/check_environment.R','r/install_packages.R','assets/icon.ico',
  '.github/workflows/build-windows.yml','docs/SOURCE_MAPPING.md','docs/reference_tutorial.html','docs/reference_MAS_POPO_v0.4.1.R','docs/V0.4.1_POINTCLOUD.md','docs/TEST_PLAN.md'
];
let failed = false;
for (const item of required) {
  const full = path.join(root, item);
  if (!fs.existsSync(full) || fs.statSync(full).size === 0) { console.error(`MISSING: ${item}`); failed = true; }
  else console.log(`OK: ${item}`);
}
const assertions = [
  ['package.json', '0.4.1'],
  ['package.json', '--publish never'],
  ['src/renderer/index.html', 'DIRECT POINT-CLOUD METRICS'],
  ['src/renderer/index.html', 'P95'],
  ['src/renderer/index.html', 'residual zone'],
  ['src/renderer/index.html', 'EXTERNAL_DTM'],
  ['src/preload.js', 'openTutorial'],
  ['src/main.js', "help:openTutorial"],
  ['r/pipeline.R', "Classification != 18L"],
  ['r/pipeline.R', "field_name('h'"],
  ['r/pipeline.R', "field_name('dh'"],
  ['r/pipeline.R', "DELTA_LOW"],
  ['r/pipeline.R', "DELTA_HIGH"],
  ['r/pipeline.R', "residual_zone_"],
  ['r/pipeline.R', "p95"],
  ['r/pipeline.R', "top30"],
  ['r/pipeline.R', "normalize_height"],
  ['r/pipeline.R', "rasterize_canopy"],
  ['docs/reference_tutorial.html', 'Monitoring Pertumbuhan Tinggi Pohon Sawit']
];
for (const [file, token] of assertions) {
  const text = fs.readFileSync(path.join(root, file), 'utf8');
  if (!text.includes(token)) { console.error(`ASSERT FAILED: ${file} tidak memuat ${token}`); failed = true; }
  else console.log(`ASSERT OK: ${file} -> ${token}`);
}
process.exit(failed ? 1 : 0);
