const $ = (id) => document.getElementById(id);
const state = { running: false, currentRunDir: '', reportPath: '', summary: null, environmentReady: false, missingPackages: [] };

const stageOrder = ['validation','load','clean','density','ground','normalize','chm','tree','metrics','monitoring','residual','export','complete'];
const pageTitles = { 'data-page': 'Data Input', 'parameter-page': 'Parameter Analisis', 'process-page': 'Proses & Log', 'result-page': 'Hasil Analisis' };

function showAlert(message, type = 'info', timeout = 0) {
  const box = document.createElement('div'); box.className = `alert ${type}`;
  const text = document.createElement('span'); text.textContent = message;
  const close = document.createElement('button'); close.className = 'ghost'; close.textContent = '×'; close.addEventListener('click', () => box.remove());
  box.append(text, close); $('alert-area').prepend(box); if (timeout) setTimeout(() => box.remove(), timeout);
}

function appendLog(message, level = 'info', timestamp = '') {
  const stamp = timestamp ? timestamp.replace('T', ' ').slice(0, 19) : new Date().toLocaleTimeString('id-ID');
  const prefix = level === 'error' ? '[ERROR]' : level === 'warning' ? '[WARN ]' : level === 'success' ? '[ OK  ]' : '[INFO ]';
  $('log-output').textContent += `\n${stamp} ${prefix} ${message}`; $('log-output').scrollTop = $('log-output').scrollHeight;
}

function switchPage(pageId) {
  document.querySelectorAll('.page').forEach((p) => p.classList.toggle('active', p.id === pageId));
  document.querySelectorAll('.nav-btn').forEach((b) => b.classList.toggle('active', b.dataset.page === pageId));
  $('page-title').textContent = pageTitles[pageId] || '';
}

function setRunning(running) {
  state.running = running;
  $('run-btn').disabled = running; $('validate-btn').disabled = running; $('check-env-btn').disabled = running;
  $('install-packages-btn').disabled = running || !state.missingPackages.length; $('cancel-btn').disabled = !running;
}

function setProgress(progress, label = '', stage = '') {
  const value = Math.max(0, Math.min(100, Number(progress) || 0));
  $('progress-fill').style.width = `${value}%`; $('progress-number').textContent = `${Math.round(value)}%`;
  if (label) $('stage-label').textContent = label; if (stage) updateStages(stage);
}

function updateStages(currentStage) {
  const currentIndex = stageOrder.indexOf(currentStage);
  document.querySelectorAll('#stage-list [data-stage]').forEach((el) => {
    const idx = stageOrder.indexOf(el.dataset.stage);
    el.classList.toggle('active', idx === currentIndex);
    el.classList.toggle('done', currentIndex > idx || currentStage === 'complete');
  });
}

function setRuntimeStatus(status, detail) {
  const pill = $('runtime-status'); pill.className = `status-pill ${status}`;
  pill.textContent = status === 'ready' ? 'Siap' : status === 'missing' ? 'Package belum lengkap' : status === 'error' ? 'Error' : 'Belum diperiksa';
  $('runtime-version').textContent = detail || '';
}

function numeric(id) { return Number($(id).value); }
function checked(id) { return $(id).checked; }

function collectConfig() {
  return {
    inputs: {
      point_cloud: $('point-cloud').value.trim(),
      tree_points: $('tree-points').value.trim(),
      external_dtm: $('external-dtm').value.trim(),
      previous_result_shp: $('previous-result-shp').value.trim(),
      output_root: $('output-root').value.trim()
    },
    parameters: {
      fallback_epsg: numeric('fallback-epsg'),
      tree_id_field: $('tree-id-field').value.trim(),
      terrain_mode: $('terrain-mode').value,
      monitoring_mode: $('monitoring-mode').value,
      period_code: $('period-code').value.trim().toUpperCase(),
      previous_period_code: $('previous-period-code').value.trim().toUpperCase(),
      remove_duplicates: checked('remove-duplicates'),
      run_noise_filter: checked('run-noise-filter'),
      sor_k: numeric('sor-k'),
      sor_m: numeric('sor-m'),
      csf_cloth_resolution: numeric('csf-cloth-resolution'),
      csf_class_threshold: numeric('csf-class-threshold'),
      csf_rigidness: numeric('csf-rigidness'),
      dtm_resolution_m: numeric('dtm-resolution'),
      buffer_radius_m: numeric('buffer-radius'),
      min_veg_h_m: numeric('min-veg-h'),
      min_normalized_z_m: numeric('min-normalized-z'),
      threads: numeric('threads'),
      create_nchm: checked('create-nchm'),
      create_qc_plots: checked('create-qc-plots'),
      chm_auto_resolution: checked('chm-auto-resolution'),
      chm_resolution: numeric('chm-resolution'),
      chm_min_auto_resolution: numeric('chm-min-auto-resolution'),
      save_normalized_laz: checked('save-normalized-laz')
    }
  };
}

function applyConfig(config) {
  if (!config) return;
  const i = config.inputs || {}; const p = config.parameters || {};
  const map = {
    'point-cloud': i.point_cloud, 'tree-points': i.tree_points, 'external-dtm': i.external_dtm,
    'previous-result-shp': i.previous_result_shp, 'output-root': i.output_root,
    'fallback-epsg': p.fallback_epsg, 'tree-id-field': p.tree_id_field,
    'terrain-mode': p.terrain_mode || 'CSF_TIN', 'monitoring-mode': p.monitoring_mode || 'first',
    'period-code': p.period_code || 'D1', 'previous-period-code': p.previous_period_code,
    'sor-k': p.sor_k, 'sor-m': p.sor_m, 'csf-cloth-resolution': p.csf_cloth_resolution,
    'csf-class-threshold': p.csf_class_threshold, 'csf-rigidness': p.csf_rigidness,
    'dtm-resolution': p.dtm_resolution_m, 'buffer-radius': p.buffer_radius_m,
    'min-veg-h': p.min_veg_h_m, 'min-normalized-z': p.min_normalized_z_m,
    'threads': p.threads, 'chm-resolution': p.chm_resolution,
    'chm-min-auto-resolution': p.chm_min_auto_resolution
  };
  Object.entries(map).forEach(([id, value]) => { if (value !== undefined && value !== null) $(id).value = value; });
  const bools = {
    'remove-duplicates': p.remove_duplicates, 'run-noise-filter': p.run_noise_filter,
    'create-nchm': p.create_nchm, 'create-qc-plots': p.create_qc_plots,
    'chm-auto-resolution': p.chm_auto_resolution, 'save-normalized-laz': p.save_normalized_laz
  };
  Object.entries(bools).forEach(([id, value]) => { if (typeof value === 'boolean') $(id).checked = value; });
  syncConditionalFields();
}

function syncConditionalFields() {
  const external = $('terrain-mode').value === 'EXTERNAL_DTM';
  $('external-dtm-card').classList.toggle('hidden', !external);
  $('csf-section-title').classList.toggle('hidden', external);
  $('csf-section-grid').classList.toggle('hidden', external);
  const monitoring = $('monitoring-mode').value === 'monitoring';
  $('previous-period-card').classList.toggle('hidden', !monitoring);
  $('previous-result-card').classList.toggle('hidden', !monitoring);
  $('chm-resolution').disabled = checked('chm-auto-resolution');
}

async function checkEnvironment() {
  setRunning(true); setRuntimeStatus('neutral', 'Memeriksa R dan package...');
  try {
    const result = await window.sawitHeight.checkEnvironment($('rscript-path').value.trim());
    $('rscript-path').value = result.rscriptPath;
  } catch (error) {
    setRuntimeStatus('error', error.message); showAlert(error.message, 'error'); setRunning(false);
  }
}

async function validateInputs() {
  setRunning(true); switchPage('process-page'); setProgress(3, 'Validasi input', 'validation'); appendLog('Validasi input dimulai...');
  try {
    const response = await window.sawitHeight.validateAnalysis({ config: collectConfig(), rscriptPath: $('rscript-path').value.trim() });
    if (!response.ok) throw new Error((response.errors || ['Validasi gagal.']).join('\n'));
    showAlert('Validasi berhasil. Data siap diproses.', 'success'); appendLog('Validasi berhasil.', 'success'); setProgress(100, 'Validasi berhasil', 'complete');
  } catch (error) { showAlert(error.message, 'error'); appendLog(error.message, 'error'); }
  finally { setRunning(false); }
}

async function startAnalysis() {
  state.summary = null; $('log-output').textContent = 'Memulai backend R...'; switchPage('process-page'); setProgress(1, 'Menyiapkan run', 'validation'); setRunning(true);
  try {
    const response = await window.sawitHeight.startAnalysis({ config: collectConfig(), rscriptPath: $('rscript-path').value.trim() });
    if (!response.ok) throw new Error((response.errors || ['Gagal memulai analisis.']).join('\n'));
    state.currentRunDir = response.runDir; $('run-dir-line').textContent = response.runDir; appendLog(`Folder run: ${response.runDir}`);
  } catch (error) { showAlert(error.message, 'error'); appendLog(error.message, 'error'); setRunning(false); }
}

function renderResults(summary, reportPath) {
  state.summary = summary; state.currentRunDir = summary.run_dir || state.currentRunDir; state.reportPath = reportPath || '';
  $('empty-result').classList.add('hidden'); $('result-content').classList.remove('hidden');
  const metrics = [
    ['Periode', summary.period_code || '-'],
    ['Mode', summary.monitoring_mode === 'monitoring' ? `Monitoring dari ${summary.previous_period_code || '-'}` : 'Baseline'],
    ['Terrain', summary.terrain_mode || '-'],
    ['TREE_ID', Number(summary.tree_count || 0).toLocaleString('id-ID')],
    ['Tinggi valid', Number(summary.valid_height_count || 0).toLocaleString('id-ID')],
    ['Median P95', summary.h_median_m == null ? '-' : `${Number(summary.h_median_m).toFixed(2)} m`],
    ['Residual', Number(summary.residual_count || 0).toLocaleString('id-ID')],
    ['Residual %', summary.residual_pct == null ? '-' : `${Number(summary.residual_pct).toFixed(2)}%`],
    ['Delta median', summary.monitoring_mode === 'monitoring' && summary.delta_median_m != null ? `${Number(summary.delta_median_m).toFixed(3)} m` : '-'],
    ['Densitas', summary.avg_density_points_m2 == null ? '-' : `${Number(summary.avg_density_points_m2).toFixed(2)} titik/m²`],
    ['nCHM res', summary.chm_resolution_m == null ? '-' : `${summary.chm_resolution_m} m`],
    ['Ground CSF', summary.ground_pct == null ? '-' : `${Number(summary.ground_pct).toFixed(2)}%`]
  ];
  $('metric-grid').innerHTML = '';
  metrics.forEach(([label, value]) => {
    const card = document.createElement('div'); card.className = 'metric-card';
    const s = document.createElement('span'); s.textContent = label; const strong = document.createElement('strong'); strong.textContent = value;
    card.append(s, strong); $('metric-grid').append(card);
  });

  const preview = Array.isArray(summary.preview) ? summary.preview : [];
  const table = $('preview-table'); table.innerHTML = '';
  if (preview.length) {
    const headers = Object.keys(preview[0]); const thead = document.createElement('thead'); const trh = document.createElement('tr');
    headers.forEach((h) => { const th = document.createElement('th'); th.textContent = h; trh.append(th); }); thead.append(trh); table.append(thead);
    const tbody = document.createElement('tbody');
    preview.forEach((row) => { const tr = document.createElement('tr'); headers.forEach((h) => { const td = document.createElement('td'); td.textContent = row[h] ?? ''; tr.append(td); }); tbody.append(tr); });
    table.append(tbody);
  }

  $('output-list').innerHTML = '';
  (summary.output_files || []).forEach((filePath) => {
    const item = document.createElement('div'); item.className = 'output-item';
    const info = document.createElement('div'); const name = document.createElement('b'); name.textContent = String(filePath).split(/[\\/]/).pop();
    const pathEl = document.createElement('small'); pathEl.textContent = filePath; info.append(name, pathEl);
    const btn = document.createElement('button'); btn.className = 'ghost'; btn.textContent = 'Tampilkan'; btn.addEventListener('click', () => window.sawitHeight.showItem(filePath));
    item.append(info, btn); $('output-list').append(item);
  });
}

function handleBackendEvent(event) {
  if (!event || typeof event !== 'object') return;
  if (event.type === 'log') appendLog(event.message || '', event.level || 'info', event.timestamp || '');
  if (event.type === 'progress') setProgress(event.progress, event.label, event.stage);
  if (event.type === 'run-created') { state.currentRunDir = event.runDir; $('run-dir-line').textContent = event.runDir; }
  if (event.type === 'environment') {
    state.missingPackages = (event.packages || []).filter((p) => !p.installed).map((p) => p.name);
    state.environmentReady = !state.missingPackages.length;
    const packageText = state.missingPackages.length ? `Hilang: ${state.missingPackages.join(', ')}` : 'Semua package tersedia';
    setRuntimeStatus(state.environmentReady ? 'ready' : 'missing', `${event.r_version} · ${packageText}`);
    $('install-packages-btn').disabled = state.running || !state.missingPackages.length;
    appendLog(`${event.r_version}; ${packageText}`, state.environmentReady ? 'success' : 'warning');
  }
  if (event.type === 'package-install') {
    appendLog(event.message || '', event.level || 'info'); if (event.progress != null) setProgress(event.progress, 'Instalasi package R', 'validation');
  }
  if (event.type === 'validation-result' && ['valid','success'].includes(event.status)) appendLog(event.message || `Validasi berhasil; ${event.tree_count || ''} TREE_ID.`, 'success');
  if (event.type === 'fatal') { appendLog(event.message || 'Fatal error.', 'error'); showAlert(`Gagal pada tahap ${event.stage || '-'}: ${event.message}`, 'error'); setRunning(false); }
  if (event.type === 'result') { renderResults(event.summary, event.reportPath); showAlert('Analisis selesai. Hasil monitoring dan residual zone telah disimpan.', 'success'); setRunning(false); switchPage('result-page'); }
  if (event.type === 'process') {
    if (event.status === 'started') setRunning(true);
    if (['completed','failed','cancelled','error'].includes(event.status)) {
      setRunning(false);
      if (event.status === 'cancelled') showAlert('Proses dibatalkan. Folder run mungkin berisi output parsial.', 'warning');
      if (event.status === 'failed' && !state.summary) appendLog(`Rscript berhenti dengan kode ${event.code}.`, 'error');
    }
  }
}

async function initialize() {
  document.querySelectorAll('.nav-btn').forEach((btn) => btn.addEventListener('click', () => switchPage(btn.dataset.page)));
  $('terrain-mode').addEventListener('change', syncConditionalFields);
  $('monitoring-mode').addEventListener('change', syncConditionalFields);
  $('chm-auto-resolution').addEventListener('change', syncConditionalFields);
  $('clear-log-btn').addEventListener('click', () => { $('log-output').textContent = 'Tampilan log dibersihkan. File analysis.log tetap tersimpan.'; });

  document.querySelectorAll('[data-picker]').forEach((btn) => btn.addEventListener('click', async () => {
    const target = btn.dataset.picker;
    const actions = {
      'point-cloud': window.sawitHeight.selectPointCloud, 'tree-points': window.sawitHeight.selectTreePoints,
      'previous-result-shp': window.sawitHeight.selectPreviousResult, 'external-dtm': window.sawitHeight.selectDtm,
      'output-root': window.sawitHeight.selectOutputFolder, 'rscript-path': window.sawitHeight.selectRscript
    };
    const value = await actions[target](); if (value) $(target).value = value;
  }));

  $('check-env-btn').addEventListener('click', checkEnvironment);
  $('install-packages-btn').addEventListener('click', async () => {
    setRunning(true); switchPage('process-page'); setProgress(0, 'Instalasi package R', 'validation');
    try { await window.sawitHeight.installPackages($('rscript-path').value.trim()); showAlert('Instalasi package selesai. Environment akan diperiksa ulang.', 'success'); await checkEnvironment(); }
    catch (error) { showAlert(error.message, 'error'); setRunning(false); }
  });
  $('tutorial-btn').addEventListener('click', async () => { try { await window.sawitHeight.openTutorial(); } catch (error) { showAlert(error.message, 'error'); } });
  $('validate-btn').addEventListener('click', validateInputs); $('run-btn').addEventListener('click', startAnalysis);
  $('cancel-btn').addEventListener('click', () => window.sawitHeight.cancelAnalysis());
  $('open-output-btn').addEventListener('click', () => window.sawitHeight.openPath(state.currentRunDir));
  $('open-report-btn').addEventListener('click', () => window.sawitHeight.openPath(state.reportPath));

  window.sawitHeight.onAnalysisEvent(handleBackendEvent);
  const appState = await window.sawitHeight.getState(); $('app-version').textContent = `v${appState.version}`;
  if (appState.settings?.lastConfig) applyConfig(appState.settings.lastConfig);
  if (appState.settings?.rscriptPath) $('rscript-path').value = appState.settings.rscriptPath;
  if (!$('threads').value || Number($('threads').value) < 1) $('threads').value = Math.max(1, Math.min(8, navigator.hardwareConcurrency || 4));

  const detection = await window.sawitHeight.detectEnvironment($('rscript-path').value.trim());
  if (detection.found) { $('rscript-path').value = detection.rscriptPath; setRuntimeStatus('neutral', detection.bundled ? 'Bundled R ditemukan; belum diperiksa.' : 'Rscript ditemukan; belum diperiksa.'); }
  else setRuntimeStatus('error', 'Rscript.exe tidak ditemukan. Pilih secara manual.');
  syncConditionalFields();
}

initialize().catch((error) => showAlert(error.message, 'error'));
