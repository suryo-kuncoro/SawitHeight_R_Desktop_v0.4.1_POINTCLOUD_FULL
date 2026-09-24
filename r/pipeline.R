options(warn = 1, stringsAsFactors = FALSE)

args <- commandArgs(trailingOnly = TRUE)
mode <- if (length(args) >= 1) args[[1]] else ''
config_path <- if (length(args) >= 2) args[[2]] else ''

base_event <- function(type, message, level = 'error') {
  fields <- c(
    paste0('"type":', encodeString(as.character(type), quote = '"')),
    paste0('"level":', encodeString(as.character(level), quote = '"')),
    paste0('"message":', encodeString(as.character(message), quote = '"'))
  )
  cat('APP_EVENT:{', paste(fields, collapse = ','), '}\n', sep = '')
  flush.console()
}

if (!mode %in% c('validate', 'run') || !nzchar(config_path) || !file.exists(config_path)) {
  base_event('fatal', 'Argumen backend tidak valid atau file konfigurasi tidak ditemukan.')
  quit(status = 2)
}

if (!requireNamespace('jsonlite', quietly = TRUE)) {
  base_event('fatal', 'Package jsonlite belum terpasang.')
  quit(status = 3)
}

required_packages <- c('lidR', 'terra', 'sf', 'dplyr')
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  base_event('fatal', paste('Package R belum tersedia:', paste(missing_packages, collapse = ', ')))
  quit(status = 4)
}

suppressPackageStartupMessages({
  library(lidR)
  library(terra)
  library(sf)
  library(dplyr)
})
options(lidR.raster.default = 'terra')

cfg <- jsonlite::fromJSON(config_path, simplifyVector = TRUE)
inputs <- cfg$inputs
p <- cfg$parameters
run_dir <- normalizePath(cfg$app$run_dir, winslash = '/', mustWork = FALSE)
dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
log_path <- file.path(run_dir, if (mode == 'run') 'analysis.log' else 'validation.log')
current_stage <- 'initialization'

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) return(y)
  if (length(x) == 1L && (is.na(x) || identical(x, ''))) return(y)
  x
}

emit <- function(type, ..., .level = NULL) {
  payload <- c(list(type = type, timestamp = format(Sys.time(), '%Y-%m-%dT%H:%M:%S%z')), list(...))
  if (!is.null(.level)) payload$level <- .level
  cat('APP_EVENT:', jsonlite::toJSON(payload, auto_unbox = TRUE, null = 'null', na = 'null', digits = 10), '\n', sep = '')
  flush.console()
}

write_log <- function(level, message) {
  line <- sprintf('[%s] [%s] [%s] %s', format(Sys.time(), '%Y-%m-%d %H:%M:%S'), level, current_stage, message)
  cat(line, '\n', file = log_path, append = TRUE)
  emit('log', level = tolower(level), stage = current_stage, message = message)
}

set_stage <- function(id, label, progress) {
  current_stage <<- id
  emit('progress', stage = id, label = label, progress = progress)
  cat(sprintf('[%s] [STAGE] [%s] %s\n', format(Sys.time(), '%Y-%m-%d %H:%M:%S'), id, label), file = log_path, append = TRUE)
}

stop_app <- function(...) stop(paste0(...), call. = FALSE)
as_num <- function(x, default = NA_real_) { v <- suppressWarnings(as.numeric(x)); if (!length(v) || is.na(v)) default else v }
as_int <- function(x, default = NA_integer_) { v <- suppressWarnings(as.integer(x)); if (!length(v) || is.na(v)) default else v }
as_bool <- function(x, default = FALSE) { if (is.null(x) || !length(x) || is.na(x)) default else isTRUE(x) }

period_clean <- function(x, label = 'PERIOD') {
  x <- toupper(gsub('[^A-Z0-9]', '', as.character(x %||% '')))
  if (!nzchar(x) || nchar(x) > 3L) stop_app(label, ' harus 1-3 karakter huruf/angka, misalnya D1, D2, S1.')
  x
}

field_name <- function(prefix, period) {
  out <- paste0(prefix, '_', period)
  if (nchar(out) > 10L) stop_app('Nama field Shapefile melebihi 10 karakter: ', out)
  out
}

ensure_writable_dir <- function(x) {
  dir.create(x, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(x)) stop_app('Folder output tidak dapat dibuat: ', x)
  f <- file.path(x, paste0('.write_test_', Sys.getpid()))
  ok <- tryCatch({ writeLines('ok', f); unlink(f); TRUE }, error = function(e) FALSE)
  if (!ok) stop_app('Folder output tidak dapat ditulis: ', x)
}

safe_write_sf <- function(x, out_path) {
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  suppressWarnings(sf::st_write(x, out_path, delete_layer = TRUE, quiet = TRUE))
}

top_mean <- function(z, prop) {
  z <- z[is.finite(z)]
  if (!length(z)) return(NA_real_)
  z <- sort(z, decreasing = TRUE)
  n_take <- max(1L, ceiling(length(z) * prop))
  mean(z[seq_len(n_take)], na.rm = TRUE)
}

calc_tree_metrics <- function(las_i, min_veg_h) {
  if (is.null(las_i)) return(data.frame(npts = 0L, p95 = NA_real_, p99 = NA_real_, top10 = NA_real_, top20 = NA_real_, top30 = NA_real_))
  n_i <- tryCatch(lidR::npoints(las_i), error = function(e) 0L)
  if (is.na(n_i) || n_i == 0L) return(data.frame(npts = 0L, p95 = NA_real_, p99 = NA_real_, top10 = NA_real_, top20 = NA_real_, top30 = NA_real_))
  z <- las_i$Z
  z <- z[is.finite(z) & z >= min_veg_h]
  if (!length(z)) return(data.frame(npts = 0L, p95 = NA_real_, p99 = NA_real_, top10 = NA_real_, top20 = NA_real_, top30 = NA_real_))
  data.frame(
    npts = as.integer(length(z)),
    p95 = as.numeric(stats::quantile(z, 0.95, na.rm = TRUE, names = FALSE)),
    p99 = as.numeric(stats::quantile(z, 0.99, na.rm = TRUE, names = FALSE)),
    top10 = top_mean(z, 0.10),
    top20 = top_mean(z, 0.20),
    top30 = top_mean(z, 0.30)
  )
}

tukey_low_points <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) return(0)
  q1 <- as.numeric(stats::quantile(x, 0.25, na.rm = TRUE, names = FALSE))
  i <- stats::IQR(x, na.rm = TRUE)
  if (!is.finite(i)) return(0)
  max(0, q1 - 1.5 * i)
}

tukey_fences <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 4L) return(c(low = -Inf, high = Inf))
  q1 <- as.numeric(stats::quantile(x, 0.25, na.rm = TRUE, names = FALSE))
  q3 <- as.numeric(stats::quantile(x, 0.75, na.rm = TRUE, names = FALSE))
  i <- stats::IQR(x, na.rm = TRUE)
  if (!is.finite(i) || i <= .Machine$double.eps) return(c(low = -Inf, high = Inf))
  c(low = q1 - 1.5 * i, high = q3 + 1.5 * i)
}

get_hist_cols <- function(df) {
  nms <- names(df)
  nms[grepl('^(h_|p99_|t10_|t20_|t30_|np_|qc_|dh_|rsn_|res_)', nms) | nms == 'tree_id']
}

make_html_report <- function(summary, preview, output_path) {
  esc <- function(x) {
    x <- gsub('&', '&amp;', as.character(x), fixed = TRUE)
    x <- gsub('<', '&lt;', x, fixed = TRUE)
    x <- gsub('>', '&gt;', x, fixed = TRUE)
    x
  }
  cards <- c(
    c('Periode', summary$period_code),
    c('Mode', if (summary$monitoring_mode == 'monitoring') paste0('Monitoring dari ', summary$previous_period_code) else 'Baseline'),
    c('Terrain', summary$terrain_mode),
    c('TREE_ID', summary$tree_count),
    c('Tinggi valid', summary$valid_height_count),
    c('Median P95', ifelse(is.null(summary$h_median_m) || is.na(summary$h_median_m), '-', paste0(round(summary$h_median_m, 3), ' m'))),
    c('Residual', summary$residual_count),
    c('Residual %', paste0(round(summary$residual_pct, 2), '%'))
  )
  card_html <- paste(vapply(cards, function(z) paste0('<div class="card"><b>', esc(z[1]), '</b><span>', esc(z[2]), '</span></div>'), character(1)), collapse = '')
  if (nrow(preview)) {
    th <- paste0('<th>', esc(names(preview)), '</th>', collapse = '')
    rows <- apply(preview, 1, function(row) paste0('<tr>', paste0('<td>', esc(row), '</td>', collapse = ''), '</tr>'))
    table_html <- paste0('<table><thead><tr>', th, '</tr></thead><tbody>', paste(rows, collapse = ''), '</tbody></table>')
  } else table_html <- '<p>Tidak ada preview.</p>'
  html <- paste0('<!doctype html><html><head><meta charset="utf-8"><title>SawitHeight R Report</title><style>',
    'body{font-family:Segoe UI,Arial;background:#0b0f11;color:#e7edee;margin:0;padding:32px}.wrap{max-width:1200px;margin:auto}',
    'h1{margin:0 0 8px}.muted{color:#91a1a5}.cards{display:grid;grid-template-columns:repeat(4,1fr);gap:10px;margin:24px 0}',
    '.card{border:1px solid #29363a;background:#12181b;border-radius:9px;padding:15px}.card b{display:block;color:#91a1a5;font-size:11px}.card span{display:block;color:#e3a73f;font-size:22px;margin-top:7px}',
    'table{width:100%;border-collapse:collapse;font-size:12px}th,td{border:1px solid #29363a;padding:8px;text-align:left}th{color:#e3a73f;background:#172024}',
    '</style></head><body><div class="wrap"><h1>SawitHeight R v0.4.1</h1><div class="muted">Direct point-cloud P95 · multi-period delta · residual zone</div>',
    '<div class="cards">', card_html, '</div><h2>Preview hasil</h2>', table_html, '</div></body></html>')
  writeLines(html, output_path, useBytes = TRUE)
}

validate_config <- function(load_header = TRUE) {
  set_stage('validation', 'Validasi data & konfigurasi', 5)
  point_cloud <- normalizePath(inputs$point_cloud, winslash = '/', mustWork = TRUE)
  tree_file <- normalizePath(inputs$tree_points, winslash = '/', mustWork = TRUE)
  output_root <- normalizePath(inputs$output_root, winslash = '/', mustWork = FALSE)
  ensure_writable_dir(output_root)
  if (!tolower(tools::file_ext(point_cloud)) %in% c('las','laz')) stop_app('Point cloud harus LAS/LAZ.')

  period <- period_clean(p$period_code %||% 'D1', 'PERIOD')
  monitoring_mode <- as.character(p$monitoring_mode %||% 'first')
  if (!monitoring_mode %in% c('first','monitoring')) stop_app('Mode periode harus first atau monitoring.')
  prev_period <- ''
  prev_path <- ''
  if (monitoring_mode == 'monitoring') {
    prev_period <- period_clean(p$previous_period_code, 'PREV_PERIOD')
    if (identical(prev_period, period)) stop_app('PREV_PERIOD tidak boleh sama dengan PERIOD.')
    prev_path <- normalizePath(inputs$previous_result_shp, winslash = '/', mustWork = TRUE)
    if (tolower(tools::file_ext(prev_path)) != 'shp') stop_app('Hasil periode sebelumnya harus Shapefile.')
  }

  terrain_mode <- toupper(as.character(p$terrain_mode %||% 'CSF_TIN'))
  if (!terrain_mode %in% c('CSF_TIN','EXTERNAL_DTM')) stop_app('TERRAIN_MODE harus CSF_TIN atau EXTERNAL_DTM.')
  external_dtm <- ''
  if (terrain_mode == 'EXTERNAL_DTM') {
    external_dtm <- normalizePath(inputs$external_dtm, winslash = '/', mustWork = TRUE)
    if (!tolower(tools::file_ext(external_dtm)) %in% c('tif','tiff')) stop_app('External DTM harus GeoTIFF.')
  }

  id_field <- trimws(as.character(p$tree_id_field %||% ''))
  if (!nzchar(id_field)) stop_app('Field ID pokok wajib diisi.')

  points <- suppressWarnings(sf::st_read(tree_file, quiet = TRUE))
  if (!inherits(points, 'sf') || !nrow(points)) stop_app('Titik pokok gagal dibaca atau kosong.')
  geom_type <- unique(as.character(sf::st_geometry_type(points)))
  if (!all(geom_type %in% c('POINT','MULTIPOINT'))) stop_app('TREE_FILE harus geometry POINT/MULTIPOINT.')
  if (!id_field %in% names(points)) stop_app('Field ID pohon tidak ditemukan pada data titik: ', id_field)
  if (any(is.na(points[[id_field]]) | trimws(as.character(points[[id_field]])) == '')) stop_app('Terdapat TREE_ID kosong/NA.')
  if (anyDuplicated(as.character(points[[id_field]])) > 0L) stop_app('TREE_ID tidak unik. Perbaiki duplikasi sebelum monitoring.')

  header <- NULL
  las_crs <- NULL
  if (load_header) {
    header <- lidR::readLASheader(point_cloud)
    if (is.null(header)) stop_app('Header LAS/LAZ gagal dibaca.')
    las_crs <- suppressWarnings(sf::st_crs(header))
    fallback_epsg <- as_int(p$fallback_epsg, 0L)
    if (is.na(las_crs) && fallback_epsg > 0L) las_crs <- sf::st_crs(fallback_epsg)
    if (is.na(las_crs)) stop_app('CRS LAS kosong. Isi EPSG fallback yang benar.')
    if (isTRUE(sf::st_is_longlat(las_crs))) stop_app('CRS point cloud masih geographic/derajat. Gunakan CRS proyeksi meter.')
    if (is.na(sf::st_crs(points))) stop_app('CRS titik pokok kosong.')
    points <- sf::st_transform(points, las_crs)

    if (terrain_mode == 'EXTERNAL_DTM') {
      dtm <- terra::rast(external_dtm)
      if (terra::nlyr(dtm) != 1L) stop_app('External DTM harus satu band elevasi.')
      dtm_crs <- sf::st_crs(terra::crs(dtm, proj = TRUE))
      if (is.na(dtm_crs)) stop_app('CRS external DTM kosong.')
      if (!isTRUE(dtm_crs == las_crs)) stop_app('CRS external DTM berbeda dengan point cloud. Samakan CRS terlebih dahulu.')
    }
  }

  if (monitoring_mode == 'monitoring') {
    prev <- suppressWarnings(sf::st_read(prev_path, quiet = TRUE))
    if (!inherits(prev, 'sf') || !nrow(prev)) stop_app('Shapefile periode sebelumnya gagal dibaca atau kosong.')
    if (!'tree_id' %in% names(prev)) stop_app('PREV_SHP tidak memiliki field tree_id.')
    if (anyDuplicated(as.character(prev$tree_id)) > 0L) stop_app('tree_id pada PREV_SHP tidak unik.')
    prev_h <- field_name('h', prev_period)
    if (!prev_h %in% names(prev)) stop_app('Field tinggi periode sebelumnya tidak ditemukan: ', prev_h)
    current_h <- field_name('h', period)
    if (current_h %in% names(prev)) stop_app('Periode ', period, ' sudah ada pada Shapefile sebelumnya.')
  }

  numeric_positive <- c(
    sor_k = as_num(p$sor_k), sor_m = as_num(p$sor_m),
    csf_cloth_resolution = as_num(p$csf_cloth_resolution), csf_class_threshold = as_num(p$csf_class_threshold),
    dtm_resolution_m = as_num(p$dtm_resolution_m), buffer_radius_m = as_num(p$buffer_radius_m),
    min_veg_h_m = as_num(p$min_veg_h_m), chm_resolution = as_num(p$chm_resolution),
    chm_min_auto_resolution = as_num(p$chm_min_auto_resolution), threads = as_num(p$threads)
  )
  bad <- names(numeric_positive)[!is.finite(numeric_positive) | numeric_positive <= 0]
  if (length(bad)) stop_app('Parameter harus > 0: ', paste(bad, collapse = ', '))
  if (!as_int(p$csf_rigidness) %in% 1:3) stop_app('CSF rigidness harus 1, 2, atau 3.')
  if (!is.finite(as_num(p$min_normalized_z_m))) stop_app('Minimum normalized Z harus numerik.')

  emit('validation-result', status = 'valid', message = 'Validasi input berhasil.', tree_count = nrow(points), period_code = period, terrain_mode = terrain_mode, monitoring_mode = monitoring_mode)
  list(point_cloud = point_cloud, tree_file = tree_file, output_root = output_root, period = period,
       monitoring_mode = monitoring_mode, prev_period = prev_period, prev_path = prev_path,
       terrain_mode = terrain_mode, external_dtm = external_dtm, id_field = id_field,
       points = points, header = header, las_crs = las_crs)
}

run_pipeline <- function() {
  v <- validate_config(TRUE)
  PERIOD <- v$period
  PREV_PERIOD <- v$prev_period
  is_baseline <- v$monitoring_mode == 'first'
  SHP_DIR <- file.path(run_dir, 'shapefile')
  QC_DIR <- file.path(run_dir, 'qc')
  dir.create(SHP_DIR, recursive = TRUE, showWarnings = FALSE)
  dir.create(QC_DIR, recursive = TRUE, showWarnings = FALSE)

  F_H <- field_name('h', PERIOD); F_P99 <- field_name('p99', PERIOD)
  F_T10 <- field_name('t10', PERIOD); F_T20 <- field_name('t20', PERIOD); F_T30 <- field_name('t30', PERIOD)
  F_NP <- field_name('np', PERIOD); F_QC <- field_name('qc', PERIOD); F_DH <- field_name('dh', PERIOD)
  F_RSN <- field_name('rsn', PERIOD); F_RES <- field_name('res', PERIOD)

  threads <- as_int(p$threads, 4L)
  try({ if ('set_lidr_threads' %in% getNamespaceExports('lidR')) lidR::set_lidr_threads(threads) }, silent = TRUE)

  set_stage('load', 'Load dense point cloud', 12)
  write_log('INFO', paste('Load point cloud:', v$point_cloud))
  las <- lidR::readLAS(v$point_cloud)
  if (is.null(las) || lidR::npoints(las) == 0L) stop_app('Point cloud kosong atau gagal dibaca.')
  las_crs <- suppressWarnings(sf::st_crs(las))
  if (is.na(las_crs)) { sf::st_crs(las) <- v$las_crs; las_crs <- v$las_crs }
  input_points <- lidR::npoints(las)
  write_log('INFO', paste('Jumlah titik awal:', format(input_points, big.mark = ',')))

  set_stage('clean', 'Cleaning duplikat & noise', 20)
  if (as_bool(p$remove_duplicates, TRUE)) {
    before <- lidR::npoints(las); las <- lidR::filter_duplicates(las)
    write_log('INFO', paste('Duplikat dibuang:', format(before - lidR::npoints(las), big.mark = ',')))
  }
  if (as_bool(p$run_noise_filter, TRUE)) {
    las <- tryCatch({
      tmp <- lidR::classify_noise(las, lidR::sor(k = as_int(p$sor_k, 10L), m = as_num(p$sor_m, 3)))
      lidR::filter_poi(tmp, Classification != 18L)
    }, error = function(e) {
      write_log('WARNING', paste('Noise filter gagal dan dilewati:', conditionMessage(e))); las
    })
  }

  set_stage('density', 'Hitung densitas point cloud', 28)
  density_r <- lidR::rasterize_density(las, res = 1)
  avg_density <- tryCatch(as.numeric(terra::global(density_r, 'mean', na.rm = TRUE)[1,1]), error = function(e) NA_real_)
  if (is.finite(avg_density) && avg_density > 0) {
    avg_spacing <- 1 / sqrt(avg_density)
    auto_res <- round(avg_spacing * 2, 2)
  } else { avg_spacing <- NA_real_; auto_res <- as_num(p$chm_resolution, 0.10) }
  res_chm <- if (as_bool(p$chm_auto_resolution, TRUE)) max(as_num(p$chm_min_auto_resolution, 0.05), auto_res, na.rm = TRUE) else as_num(p$chm_resolution, 0.10)
  write_log('INFO', paste('Densitas rata-rata:', round(avg_density, 2), 'titik/m2 | nCHM res:', res_chm, 'm'))

  set_stage('ground', 'Membangun / memuat terrain', 38)
  ground_pct <- NA_real_
  dtm_path <- ''
  dtm_output_path <- ''
  if (v$terrain_mode == 'CSF_TIN') {
    las <- lidR::classify_ground(las, lidR::csf(
      cloth_resolution = as_num(p$csf_cloth_resolution, 0.5),
      class_threshold = as_num(p$csf_class_threshold, 0.3),
      rigidness = as_int(p$csf_rigidness, 2L)
    ))
    n_ground <- sum(las$Classification == 2L, na.rm = TRUE)
    ground_pct <- n_ground / lidR::npoints(las) * 100
    write_log('INFO', paste('Ground Class 2:', format(n_ground, big.mark = ','), '(', round(ground_pct, 2), '%)'))
    if (n_ground == 0L) stop_app('Tidak ada titik ground hasil CSF. Perbaiki parameter atau gunakan EXTERNAL_DTM.')
    dtm <- lidR::rasterize_terrain(las, res = as_num(p$dtm_resolution_m, 0.5), algorithm = lidR::tin())
    dtm_path <- file.path(run_dir, paste0('DTM_', PERIOD, '.tif'))
    dtm_output_path <- dtm_path
    terra::writeRaster(dtm, dtm_path, overwrite = TRUE)
  } else {
    dtm <- terra::rast(v$external_dtm)
    dtm_path <- v$external_dtm
    write_log('INFO', paste('Menggunakan external DTM:', dtm_path))
  }

  set_stage('normalize', 'Normalisasi tinggi terhadap terrain', 48)
  nlas <- lidR::normalize_height(las, dtm)
  if (is.null(nlas) || lidR::npoints(nlas) == 0L) stop_app('Normalisasi menghasilkan point cloud kosong.')
  gnd <- tryCatch(lidR::filter_ground(nlas), error = function(e) NULL)
  if (!is.null(gnd) && lidR::npoints(gnd) > 0L) {
    gz <- gnd$Z
    write_log('INFO', paste('QC normalized ground mean=', round(mean(gz, na.rm = TRUE), 4), 'm | min=', round(min(gz, na.rm = TRUE), 4), '| max=', round(max(gz, na.rm = TRUE), 4)))
  }
  nlas <- lidR::filter_poi(nlas, Z >= as_num(p$min_normalized_z_m, -0.10))
  norm_path <- ''
  if (as_bool(p$save_normalized_laz, TRUE)) {
    norm_path <- file.path(run_dir, paste0('normalized_pointcloud_', PERIOD, '.laz'))
    lidR::writeLAS(nlas, norm_path)
  }

  set_stage('chm', 'nCHM visualisasi & spatial QC', 56)
  chm_path <- ''
  if (as_bool(p$create_nchm, TRUE)) {
    chm <- tryCatch(
      lidR::rasterize_canopy(nlas, res = res_chm, algorithm = lidR::pitfree(thresholds = c(0,2,5,10), max_edge = c(0,1.5))),
      error = function(e) {
        write_log('WARNING', paste('Pitfree gagal:', conditionMessage(e), '| fallback p2r.'))
        lidR::rasterize_canopy(nlas, res = res_chm, algorithm = lidR::p2r())
      }
    )
    chm_path <- file.path(run_dir, paste0('nCHM_', PERIOD, '.tif'))
    terra::writeRaster(chm, chm_path, overwrite = TRUE)
  }

  set_stage('tree', 'Load titik pokok & buffer', 64)
  titik_pokok <- suppressWarnings(sf::st_read(v$tree_file, quiet = TRUE))
  geom_type <- unique(as.character(sf::st_geometry_type(titik_pokok)))
  if (any(geom_type == 'MULTIPOINT')) titik_pokok <- suppressWarnings(sf::st_cast(titik_pokok, 'POINT'))
  titik_pokok <- sf::st_transform(titik_pokok, sf::st_crs(nlas))
  if (anyDuplicated(as.character(titik_pokok[[v$id_field]])) > 0L) stop_app('TREE_ID menjadi tidak unik setelah transform/cast.')
  titik_pokok$tree_id <- as.character(titik_pokok[[v$id_field]])
  buffer_radius <- as_num(p$buffer_radius_m, 2.0)
  coords <- sf::st_coordinates(titik_pokok)
  if (nrow(coords) != nrow(titik_pokok)) stop_app('Koordinat titik pokok tidak satu-ke-satu dengan TREE_ID.')

  set_stage('metrics', 'Ekstraksi P95/P99/Top10/20/30', 72)
  las_tree <- tryCatch(lidR::clip_circle(nlas, xcenter = coords[,1], ycenter = coords[,2], radius = buffer_radius), error = function(e) NULL)
  vectorized_ok <- is.list(las_tree) && length(las_tree) == nrow(titik_pokok)
  if (!vectorized_ok) {
    write_log('WARNING', 'Vectorized clip_circle tidak kompatibel; fallback loop per TREE_ID.')
    las_tree <- vector('list', nrow(titik_pokok))
    for (i in seq_len(nrow(titik_pokok))) {
      las_tree[[i]] <- tryCatch(lidR::clip_circle(nlas, xcenter = coords[i,1], ycenter = coords[i,2], radius = buffer_radius), error = function(e) {
        write_log('WARNING', paste('Clip gagal TREE_ID=', titik_pokok$tree_id[i], ':', conditionMessage(e))); NULL
      })
      if (i %% max(1L, floor(nrow(titik_pokok) / 10L)) == 0L) emit('progress', stage = 'metrics', label = paste('Ekstraksi TREE_ID', i, '/', nrow(titik_pokok)), progress = 72 + round(i / nrow(titik_pokok) * 7))
    }
  }
  metrics <- dplyr::bind_rows(lapply(las_tree, calc_tree_metrics, min_veg_h = as_num(p$min_veg_h_m, 0.30)))
  if (nrow(metrics) != nrow(titik_pokok)) stop_app('Jumlah hasil metrics tidak sama dengan jumlah TREE_ID.')
  titik_current <- dplyr::bind_cols(titik_pokok, metrics)
  titik_current$h_main <- titik_current$p95
  low_n_cut <- tukey_low_points(titik_current$npts)
  titik_current$qc_current <- ifelse(is.na(titik_current$h_main), 'NO_DATA', ifelse(titik_current$npts < low_n_cut, 'LOW_POINTS', 'OK'))

  set_stage('monitoring', 'Membentuk baseline / delta antarperiode', 82)
  titik_result <- titik_current
  titik_result[[F_H]] <- titik_current$h_main
  titik_result[[F_P99]] <- titik_current$p99
  titik_result[[F_T10]] <- titik_current$top10
  titik_result[[F_T20]] <- titik_current$top20
  titik_result[[F_T30]] <- titik_current$top30
  titik_result[[F_NP]] <- titik_current$npts
  titik_result[[F_QC]] <- titik_current$qc_current
  drop_temp <- intersect(c('npts','p95','p99','top10','top20','top30','h_main','qc_current'), names(titik_result))
  titik_result <- dplyr::select(titik_result, -dplyr::all_of(drop_temp))

  low_d <- -Inf; high_d <- Inf
  if (is_baseline) {
    titik_result[[F_DH]] <- NA_real_
    titik_result[[F_RSN]] <- ifelse(is.na(titik_result[[F_H]]), 'NO_DATA', ifelse(titik_result[[F_QC]] != 'OK', 'LOW_POINTS', 'OK'))
    titik_result[[F_RES]] <- titik_result[[F_RSN]] != 'OK'
  } else {
    prev <- suppressWarnings(sf::st_read(v$prev_path, quiet = TRUE))
    prev_h_field <- field_name('h', PREV_PERIOD)
    prev_qc_field <- field_name('qc', PREV_PERIOD)
    hist_cols <- get_hist_cols(prev)
    prev_hist <- dplyr::select(sf::st_drop_geometry(prev), dplyr::all_of(hist_cols))
    overlap_cols <- setdiff(intersect(names(titik_result), names(prev_hist)), 'tree_id')
    if (length(overlap_cols)) titik_result <- dplyr::select(titik_result, -dplyr::all_of(overlap_cols))
    titik_result <- dplyr::left_join(titik_result, prev_hist, by = 'tree_id')
    titik_result[[F_DH]] <- titik_result[[F_H]] - titik_result[[prev_h_field]]
    fences <- tukey_fences(titik_result[[F_DH]]); low_d <- fences[['low']]; high_d <- fences[['high']]
    reason <- rep('OK', nrow(titik_result))
    no_data <- is.na(titik_result[[prev_h_field]]) | is.na(titik_result[[F_H]])
    reason[no_data] <- 'NO_DATA'
    if (prev_qc_field %in% names(titik_result)) {
      bad_prev_qc <- !no_data & !is.na(titik_result[[prev_qc_field]]) & titik_result[[prev_qc_field]] != 'OK'
      reason[bad_prev_qc] <- 'PREV_QC'
    }
    low_points_now <- reason == 'OK' & titik_result[[F_QC]] != 'OK'; reason[low_points_now] <- 'LOW_POINTS'
    delta_low <- reason == 'OK' & is.finite(titik_result[[F_DH]]) & titik_result[[F_DH]] < low_d; reason[delta_low] <- 'DELTA_LOW'
    delta_high <- reason == 'OK' & is.finite(titik_result[[F_DH]]) & titik_result[[F_DH]] > high_d; reason[delta_high] <- 'DELTA_HIGH'
    titik_result[[F_RSN]] <- reason; titik_result[[F_RES]] <- reason != 'OK'
    write_log('INFO', paste('Tukey delta fence: low=', ifelse(is.finite(low_d), round(low_d,4), '-Inf'), '| high=', ifelse(is.finite(high_d), round(high_d,4), 'Inf')))
  }

  set_stage('residual', 'Membuat residual point & polygon zone', 89)
  residual_idx <- which(titik_result[[F_RES]] %in% TRUE)
  residual_point_path <- ''; residual_zone_path <- ''; residual_csv_path <- ''
  if (length(residual_idx)) {
    residual_point <- titik_result[residual_idx, , drop = FALSE]
    residual_point_path <- file.path(SHP_DIR, paste0('residual_point_', PERIOD, '.shp'))
    safe_write_sf(residual_point, residual_point_path)
    residual_zone <- sf::st_buffer(residual_point, dist = buffer_radius)
    residual_zone_path <- file.path(SHP_DIR, paste0('residual_zone_', PERIOD, '.shp'))
    safe_write_sf(residual_zone, residual_zone_path)
    residual_csv_path <- file.path(run_dir, paste0('residual_zone_', PERIOD, '.csv'))
    utils::write.csv(sf::st_drop_geometry(residual_zone), residual_csv_path, row.names = FALSE)
  }
  write_log('INFO', paste('Jumlah TREE_ID residual:', length(residual_idx)))

  set_stage('export', 'Export monitoring, summary & QC', 95)
  monitoring_shp <- file.path(SHP_DIR, paste0('monitoring_tinggi_', PERIOD, '.shp'))
  monitoring_csv <- file.path(run_dir, paste0('monitoring_tinggi_', PERIOD, '.csv'))
  safe_write_sf(titik_result, monitoring_shp)
  utils::write.csv(sf::st_drop_geometry(titik_result), monitoring_csv, row.names = FALSE)

  h_current <- titik_result[[F_H]]
  dh_current <- titik_result[[F_DH]]
  summary_row <- data.frame(
    period = PERIOD, n_tree = nrow(titik_result), n_height_valid = sum(is.finite(h_current)),
    n_residual = sum(titik_result[[F_RES]] %in% TRUE), residual_pct = round(mean(titik_result[[F_RES]] %in% TRUE) * 100, 2),
    h_median_m = ifelse(any(is.finite(h_current)), stats::median(h_current, na.rm = TRUE), NA_real_),
    h_p25_m = ifelse(any(is.finite(h_current)), as.numeric(stats::quantile(h_current, 0.25, na.rm = TRUE)), NA_real_),
    h_p75_m = ifelse(any(is.finite(h_current)), as.numeric(stats::quantile(h_current, 0.75, na.rm = TRUE)), NA_real_),
    npts_low_cut = low_n_cut, buffer_radius_m = buffer_radius, min_veg_h_m = as_num(p$min_veg_h_m, 0.30),
    estimator = 'P95', terrain_mode = v$terrain_mode, stringsAsFactors = FALSE
  )
  if (!is_baseline) {
    summary_row$prev_period <- PREV_PERIOD
    summary_row$delta_median_m <- ifelse(any(is.finite(dh_current)), stats::median(dh_current, na.rm = TRUE), NA_real_)
    summary_row$delta_p25_m <- ifelse(any(is.finite(dh_current)), as.numeric(stats::quantile(dh_current, 0.25, na.rm = TRUE)), NA_real_)
    summary_row$delta_p75_m <- ifelse(any(is.finite(dh_current)), as.numeric(stats::quantile(dh_current, 0.75, na.rm = TRUE)), NA_real_)
  }
  summary_csv <- file.path(run_dir, paste0('summary_', PERIOD, '.csv'))
  utils::write.csv(summary_row, summary_csv, row.names = FALSE)

  qc_files <- character(0)
  if (as_bool(p$create_qc_plots, TRUE)) {
    valid_h <- h_current[is.finite(h_current)]
    if (length(valid_h) > 1L) {
      f <- file.path(QC_DIR, paste0('hist_height_', PERIOD, '.png'))
      tryCatch({ grDevices::png(f, 1600, 1000, res = 150); hist(valid_h, breaks = 'FD', main = paste0('Distribusi Tinggi P95 - ', PERIOD), xlab = 'Tinggi P95 (m)'); grDevices::dev.off(); qc_files <- c(qc_files, f) }, error = function(e) { try(grDevices::dev.off(), silent = TRUE); write_log('WARNING', paste('Gagal membuat histogram tinggi:', conditionMessage(e))) })
    }
    if (!is_baseline) {
      valid_dh <- dh_current[is.finite(dh_current)]
      if (length(valid_dh) > 1L) {
        f <- file.path(QC_DIR, paste0('hist_delta_', PERIOD, '.png'))
        tryCatch({ grDevices::png(f, 1600, 1000, res = 150); hist(valid_dh, breaks = 'FD', main = paste0('Distribusi Delta ', PREV_PERIOD, ' -> ', PERIOD), xlab = 'Delta P95 (m)'); abline(v = 0, lty = 2); grDevices::dev.off(); qc_files <- c(qc_files, f) }, error = function(e) { try(grDevices::dev.off(), silent = TRUE); write_log('WARNING', paste('Gagal membuat histogram delta:', conditionMessage(e))) })
      }
    }
  }

  preview_cols <- unique(c('tree_id', F_H, F_P99, F_T10, F_T20, F_T30, F_NP, F_QC, F_DH, F_RSN, F_RES))
  preview_cols <- preview_cols[preview_cols %in% names(titik_result)]
  preview <- head(sf::st_drop_geometry(titik_result)[, preview_cols, drop = FALSE], 20)
  output_files <- c(monitoring_shp, monitoring_csv, summary_csv, dtm_output_path, norm_path, chm_path, residual_point_path, residual_zone_path, residual_csv_path, qc_files, log_path)
  output_files <- unique(output_files[nzchar(output_files) & file.exists(output_files)])

  summary <- list(
    app_version = as.character(cfg$app$version %||% '0.4.1'), run_dir = run_dir, run_name = basename(run_dir),
    period_code = PERIOD, previous_period_code = if (is_baseline) '' else PREV_PERIOD,
    monitoring_mode = if (is_baseline) 'first' else 'monitoring', terrain_mode = v$terrain_mode,
    estimator = 'P95', input_points = input_points, avg_density_points_m2 = ifelse(is.finite(avg_density), avg_density, NA_real_),
    chm_resolution_m = res_chm, ground_pct = ground_pct, tree_count = nrow(titik_result),
    valid_height_count = sum(is.finite(h_current)), residual_count = length(residual_idx), residual_pct = round(length(residual_idx) / nrow(titik_result) * 100, 2),
    h_median_m = ifelse(any(is.finite(h_current)), stats::median(h_current, na.rm = TRUE), NA_real_),
    delta_median_m = if (!is_baseline && any(is.finite(dh_current))) stats::median(dh_current, na.rm = TRUE) else NA_real_,
    delta_low_fence_m = if (!is_baseline && is.finite(low_d)) low_d else NA_real_, delta_high_fence_m = if (!is_baseline && is.finite(high_d)) high_d else NA_real_,
    buffer_radius_m = buffer_radius, min_veg_h_m = as_num(p$min_veg_h_m, 0.30), output_files = output_files,
    preview = preview
  )
  result_json <- file.path(run_dir, 'result_summary.json')
  jsonlite::write_json(summary, result_json, pretty = TRUE, auto_unbox = TRUE, na = 'null')
  report_path <- file.path(run_dir, 'report.html')
  make_html_report(summary, preview, report_path)
  output_files <- unique(c(output_files, result_json, report_path))
  summary$output_files <- output_files
  jsonlite::write_json(summary, result_json, pretty = TRUE, auto_unbox = TRUE, na = 'null')

  manifest <- data.frame(file = basename(output_files), path = output_files, stringsAsFactors = FALSE)
  manifest_path <- file.path(run_dir, 'output_manifest.csv')
  utils::write.csv(manifest, manifest_path, row.names = FALSE)
  summary$output_files <- unique(c(summary$output_files, manifest_path))
  jsonlite::write_json(summary, result_json, pretty = TRUE, auto_unbox = TRUE, na = 'null')

  set_stage('complete', 'Analisis selesai', 100)
  emit('result', status = 'success', runDir = run_dir, summaryPath = result_json, reportPath = report_path, summary = summary)
  write_log('INFO', 'Analisis selesai tanpa fatal error.')
}

status <- 0L
withCallingHandlers(
  tryCatch({
    if (mode == 'validate') {
      validate_config(TRUE)
      set_stage('complete', 'Validasi berhasil', 100)
      emit('validation-result', status = 'success', message = 'Semua input utama valid untuk diproses.')
    } else run_pipeline()
  }, error = function(e) {
    status <<- 10L
    message <- conditionMessage(e)
    try(cat(sprintf('[%s] [FATAL] [%s] %s\n', format(Sys.time(), '%Y-%m-%d %H:%M:%S'), current_stage, message), file = log_path, append = TRUE), silent = TRUE)
    emit('fatal', stage = current_stage, message = message, runDir = run_dir, logPath = log_path)
  }),
  warning = function(w) {
    message <- conditionMessage(w)
    try(write_log('WARNING', message), silent = TRUE)
    invokeRestart('muffleWarning')
  }
)
quit(status = status)
