# =============================================================================
# MAS POPO - Monitoring Tinggi & Pertumbuhan Pokok Sawit TBM
# Direct Point-Cloud Metrics + Multi-Period Delta + Residual Zone
# Versi: v0.4.1
#
# Basis metode:
# - Dense point cloud SfM-MVS foto udara UAV
# - Akuisisi PPK
# - Fixed buffer radius 2 m per TREE_ID
# - Tinggi utama sementara: P95 normalized point cloud
# - Pembanding: P99, mean Top 10%, Top 20%, Top 30%
# - nCHM hanya untuk visualisasi / spatial QC
# - Tidak menggunakan threshold agronomi minimum pertumbuhan
# - Residual zone = QC teknis + outlier statistik delta (Tukey)
#
# Kompatibilitas target:
# - R 4.2.2
# - lidR 4.0.x / 4.1.x
#
# PENTING:
# 1) Edit BAGIAN A - KONFIGURASI sebelum RUN.
# 2) Untuk D1, PREV_PERIOD dan PREV_SHP isi NA.
# 3) Untuk D2+, arahkan PREV_SHP ke shapefile hasil periode sebelumnya.
# 4) PPK menjaga georeferensi, tetapi TINGGI tetap harus dinormalisasi
#    terhadap terrain yang valid dan konsisten.
# 5) Jika terrain tidak berubah antarsemester, penggunaan DTM referensi
#    yang sama untuk D2+ lebih aman untuk analisis delta.
# =============================================================================


# =============================================================================
# A. KONFIGURASI - EDIT BAGIAN INI
# =============================================================================

# Folder proyek utama
PROJECT_DIR <- "C:/Project/MAS_POPO"

# Kode periode, disarankan D1, D2, D3, ...
PERIOD <- "D1"

# Dense point cloud LAS/LAZ periode saat ini
LAS_FILE <- file.path(PROJECT_DIR, "data", "dense_cloud_D1.laz")

# Titik pokok sawit. Harus memiliki ID permanen dan unik.
TREE_FILE <- file.path(PROJECT_DIR, "data", "titik_pokok_sawit.shp")
ID_FIELD  <- "TREE_ID"

# Jika CRS LAS kosong, isi EPSG yang benar.
# Contoh UTM 48S = 32748. Biarkan NA jika LAS sudah punya CRS.
EPSG_IF_MISSING <- NA_integer_

# ---------------------------------------------------------------------------
# TERRAIN MODE
# ---------------------------------------------------------------------------
# Pilihan:
#   "CSF_TIN"      = klasifikasi ground dari point cloud saat ini -> DTM TIN
#   "EXTERNAL_DTM" = pakai DTM referensi yang sudah tervalidasi
#
# Rekomendasi multi-periode:
# - D1: boleh "CSF_TIN", lalu simpan DTM hasil D1.
# - D2+: jika terrain fisik tidak berubah, gunakan DTM D1 yang sama dengan
#        mode "EXTERNAL_DTM" untuk mengurangi bias antarperiode.
#
TERRAIN_MODE <- "CSF_TIN"

# Digunakan hanya jika TERRAIN_MODE = "EXTERNAL_DTM"
DTM_FILE <- file.path(PROJECT_DIR, "reference", "DTM_reference_D1.tif")

# Parameter CSF - gunakan konsisten antarperiode jika tetap memakai CSF_TIN
CSF_CLOTH_RES <- 0.5
CSF_CLASS_THR <- 0.3
CSF_RIGIDNESS <- 2

# Resolusi DTM bila dibuat dari CSF -> TIN
DTM_RES <- 0.5

# ---------------------------------------------------------------------------
# BUFFER & METRIK TINGGI
# ---------------------------------------------------------------------------
BUFFER_RADIUS_M <- 2.0
MIN_VEG_H_M     <- 0.30  # filter teknis titik sangat rendah, bukan threshold agronomi

# ---------------------------------------------------------------------------
# PERIODE SEBELUMNYA
# ---------------------------------------------------------------------------
# D1:
# PREV_PERIOD <- NA_character_
# PREV_SHP    <- NA_character_
#
# Contoh D2:
# PREV_PERIOD <- "D1"
# PREV_SHP    <- file.path(PROJECT_DIR, "output_D1", "shapefile",
#                          "monitoring_tinggi_D1.shp")
#
PREV_PERIOD <- NA_character_
PREV_SHP    <- NA_character_

# ---------------------------------------------------------------------------
# OUTPUT OPTIONAL
# ---------------------------------------------------------------------------
CREATE_NCHM     <- TRUE
CREATE_QC_PLOTS <- TRUE

# Pitfree untuk visualisasi nCHM
CHM_THRESHOLDS <- c(0, 2, 5, 10)
CHM_MAX_EDGE   <- c(0, 1.5)

# Bersihkan noise SfM sebelum klasifikasi ground
RUN_NOISE_FILTER <- TRUE

# Nilai negatif kecil setelah normalisasi masih dapat terjadi karena interpolasi.
# Ini hanya filter teknis ekstrem.
MIN_NORMALIZED_Z <- -0.10


# =============================================================================
# B. PACKAGE CHECK
# =============================================================================

required_packages <- c("lidR", "sf", "terra", "dplyr")

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    paste0(
      "Package berikut belum tersedia: ",
      paste(missing_packages, collapse = ", "),
      "\nInstall sesuai tutorial R 4.2.2 terlebih dahulu."
    ),
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(lidR)
  library(sf)
  library(terra)
  library(dplyr)
})


# =============================================================================
# C. HELPER FUNCTIONS
# =============================================================================

period_clean <- function(x) {
  x <- toupper(gsub("[^A-Z0-9]", "", x))
  if (!nzchar(x)) stop("PERIOD tidak valid.", call. = FALSE)
  if (nchar(x) > 3) {
    stop(
      "PERIOD maksimal 3 karakter agar nama field aman untuk Shapefile. ",
      "Gunakan misalnya D1, D2, S1, S2.",
      call. = FALSE
    )
  }
  x
}

PERIOD <- period_clean(PERIOD)

if (!is.na(PREV_PERIOD)[1]) {
  PREV_PERIOD <- period_clean(PREV_PERIOD)
}

OUTPUT_DIR <- file.path(PROJECT_DIR, paste0("output_", PERIOD))
SHP_DIR    <- file.path(OUTPUT_DIR, "shapefile")
QC_DIR     <- file.path(OUTPUT_DIR, "qc")

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(SHP_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(QC_DIR, recursive = TRUE, showWarnings = FALSE)

LOG_FILE <- file.path(OUTPUT_DIR, paste0("MAS_POPO_", PERIOD, "_process_log.txt"))
if (file.exists(LOG_FILE)) file.remove(LOG_FILE)

log_msg <- function(..., level = "INFO") {
  msg <- paste0(...)
  line <- paste0(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    " [", level, "] ", msg
  )
  cat(line, "\n")
  cat(line, "\n", file = LOG_FILE, append = TRUE)
}

stop_run <- function(...) {
  msg <- paste0(...)
  log_msg(msg, level = "ERROR")
  stop(msg, call. = FALSE)
}

safe_write_sf <- function(x, path) {
  if (file.exists(path)) {
    try(unlink(path), silent = TRUE)
  }
  suppressWarnings(
    st_write(x, path, delete_layer = TRUE, quiet = TRUE)
  )
}

top_mean <- function(z, prop) {
  z <- z[is.finite(z)]
  if (length(z) == 0) return(NA_real_)
  z <- sort(z, decreasing = TRUE)
  n_take <- max(1L, ceiling(length(z) * prop))
  mean(z[seq_len(n_take)], na.rm = TRUE)
}

calc_tree_metrics <- function(las_i, min_veg_h) {
  if (is.null(las_i)) {
    return(data.frame(
      npts = 0L, p95 = NA_real_, p99 = NA_real_,
      top10 = NA_real_, top20 = NA_real_, top30 = NA_real_
    ))
  }

  n_i <- tryCatch(npoints(las_i), error = function(e) 0L)

  if (is.na(n_i) || n_i == 0L) {
    return(data.frame(
      npts = 0L, p95 = NA_real_, p99 = NA_real_,
      top10 = NA_real_, top20 = NA_real_, top30 = NA_real_
    ))
  }

  z <- las_i$Z
  z <- z[is.finite(z) & z >= min_veg_h]

  if (length(z) == 0L) {
    return(data.frame(
      npts = 0L, p95 = NA_real_, p99 = NA_real_,
      top10 = NA_real_, top20 = NA_real_, top30 = NA_real_
    ))
  }

  data.frame(
    npts  = as.integer(length(z)),
    p95   = as.numeric(quantile(z, probs = 0.95, na.rm = TRUE, names = FALSE)),
    p99   = as.numeric(quantile(z, probs = 0.99, na.rm = TRUE, names = FALSE)),
    top10 = top_mean(z, 0.10),
    top20 = top_mean(z, 0.20),
    top30 = top_mean(z, 0.30)
  )
}

tukey_low_points <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) return(0)

  q1 <- as.numeric(quantile(x, 0.25, na.rm = TRUE, names = FALSE))
  i  <- IQR(x, na.rm = TRUE)

  if (!is.finite(i)) return(0)

  max(0, q1 - 1.5 * i)
}

tukey_fences <- function(x) {
  x <- x[is.finite(x)]

  # Dengan data terlalu sedikit, outlier statistik tidak dipaksakan.
  if (length(x) < 4L) {
    return(c(low = -Inf, high = Inf))
  }

  q1 <- as.numeric(quantile(x, 0.25, na.rm = TRUE, names = FALSE))
  q3 <- as.numeric(quantile(x, 0.75, na.rm = TRUE, names = FALSE))
  i  <- IQR(x, na.rm = TRUE)

  # Jika IQR nol, distribusi tidak memberikan batas outlier yang stabil.
  if (!is.finite(i) || i <= .Machine$double.eps) {
    return(c(low = -Inf, high = Inf))
  }

  c(low = q1 - 1.5 * i, high = q3 + 1.5 * i)
}

get_hist_cols <- function(df) {
  nms <- names(df)
  nms[
    grepl(
      "^(h_|p99_|t10_|t20_|t30_|np_|qc_|dh_|rsn_|res_)",
      nms
    ) | nms == "tree_id"
  ]
}

field_name <- function(prefix, period) {
  out <- paste0(prefix, "_", period)
  if (nchar(out) > 10) {
    stop(
      paste0("Nama field '", out, "' melebihi batas aman DBF/Shapefile."),
      call. = FALSE
    )
  }
  out
}

# Nama field periode saat ini
F_H   <- field_name("h",   PERIOD)
F_P99 <- field_name("p99", PERIOD)
F_T10 <- field_name("t10", PERIOD)
F_T20 <- field_name("t20", PERIOD)
F_T30 <- field_name("t30", PERIOD)
F_NP  <- field_name("np",  PERIOD)
F_QC  <- field_name("qc",  PERIOD)
F_DH  <- field_name("dh",  PERIOD)
F_RSN <- field_name("rsn", PERIOD)
F_RES <- field_name("res", PERIOD)


# =============================================================================
# D. VALIDASI INPUT
# =============================================================================

log_msg("============================================================")
log_msg("MAS POPO v0.4.1 - mulai proses periode ", PERIOD)
log_msg("============================================================")

if (!file.exists(LAS_FILE)) stop_run("LAS/LAZ tidak ditemukan: ", LAS_FILE)
if (!file.exists(TREE_FILE)) stop_run("Titik pokok tidak ditemukan: ", TREE_FILE)

if (!(TERRAIN_MODE %in% c("CSF_TIN", "EXTERNAL_DTM"))) {
  stop_run("TERRAIN_MODE harus 'CSF_TIN' atau 'EXTERNAL_DTM'.")
}

if (TERRAIN_MODE == "EXTERNAL_DTM" && !file.exists(DTM_FILE)) {
  stop_run("DTM_FILE tidak ditemukan: ", DTM_FILE)
}

is_baseline <- is.na(PREV_PERIOD)[1] || is.na(PREV_SHP)[1]

if (!is_baseline) {
  if (!file.exists(PREV_SHP)) {
    stop_run("PREV_SHP tidak ditemukan: ", PREV_SHP)
  }
  if (PREV_PERIOD == PERIOD) {
    stop_run("PREV_PERIOD tidak boleh sama dengan PERIOD.")
  }
}

log_msg("Mode: ", ifelse(is_baseline, "BASELINE", "MULTI-PERIODE"))
log_msg("Terrain mode: ", TERRAIN_MODE)
log_msg("Buffer pokok: ", BUFFER_RADIUS_M, " m")
log_msg("Estimator tinggi utama: P95")


# =============================================================================
# E. LOAD DENSE POINT CLOUD
# =============================================================================

log_msg("Load point cloud: ", LAS_FILE)

las <- readLAS(LAS_FILE)

if (is.null(las) || npoints(las) == 0L) {
  stop_run("Point cloud kosong atau gagal dibaca.")
}

log_msg("Jumlah titik awal: ", format(npoints(las), big.mark = ","))

las_crs <- st_crs(las)

if (is.na(las_crs)) {
  if (is.na(EPSG_IF_MISSING)) {
    stop_run(
      "CRS LAS kosong. Isi EPSG_IF_MISSING dengan EPSG proyek yang benar."
    )
  } else {
    st_crs(las) <- EPSG_IF_MISSING
    las_crs <- st_crs(las)
    log_msg("CRS LAS di-set ke EPSG:", EPSG_IF_MISSING)
  }
}

if (isTRUE(st_is_longlat(las_crs))) {
  stop_run(
    "CRS point cloud masih geographic/derajat. ",
    "Gunakan CRS proyeksi meter sebelum buffer 2 m."
  )
}

log_msg("CRS point cloud OK.")


# =============================================================================
# F. QC POINT CLOUD
# =============================================================================

log_msg("QC point cloud: hapus duplikat.")

n_before_dup <- npoints(las)
las <- filter_duplicates(las)
n_after_dup <- npoints(las)

log_msg(
  "Duplikat dibuang: ",
  format(n_before_dup - n_after_dup, big.mark = ",")
)

if (RUN_NOISE_FILTER) {
  log_msg("Noise filtering SOR dimulai.")

  noise_ok <- TRUE

  las <- tryCatch({
    tmp <- classify_noise(las, sor(k = 10, m = 3))
    filter_poi(tmp, Classification != 18L)
  }, error = function(e) {
    noise_ok <<- FALSE
    log_msg(
      "Noise filter dilewati karena fungsi gagal: ",
      conditionMessage(e),
      level = "WARN"
    )
    las
  })

  if (noise_ok) {
    log_msg("Noise filtering selesai.")
  }
}

# Densitas aktual untuk resolusi nCHM
log_msg("Hitung densitas point cloud.")

density_r <- rasterize_density(las, res = 1)

avg_density <- tryCatch(
  as.numeric(terra::global(density_r, "mean", na.rm = TRUE)[1, 1]),
  error = function(e) NA_real_
)

if (!is.finite(avg_density) || avg_density <= 0) {
  log_msg(
    "Densitas rata-rata tidak dapat dihitung. nCHM akan memakai 0.10 m.",
    level = "WARN"
  )
  res_chm <- 0.10
} else {
  avg_spacing <- 1 / sqrt(avg_density)
  res_chm <- round(avg_spacing * 2, 2)

  if (!is.finite(res_chm) || res_chm <= 0) res_chm <- 0.10

  log_msg("Densitas rata-rata: ", round(avg_density, 2), " titik/m2")
  log_msg("Average spacing: ", round(avg_spacing, 3), " m")
  log_msg("Resolusi nCHM berbasis spacing: ", res_chm, " m")
}


# =============================================================================
# G. TERRAIN & NORMALISASI TINGGI
# =============================================================================

if (TERRAIN_MODE == "CSF_TIN") {

  log_msg("Klasifikasi ground CSF dimulai.")

  las <- classify_ground(
    las,
    csf(
      cloth_resolution = CSF_CLOTH_RES,
      class_threshold  = CSF_CLASS_THR,
      rigidness        = CSF_RIGIDNESS
    )
  )

  n_ground <- sum(las$Classification == 2L, na.rm = TRUE)
  pct_ground <- n_ground / npoints(las) * 100

  log_msg(
    "Ground Class 2: ",
    format(n_ground, big.mark = ","),
    " (", round(pct_ground, 2), "%)"
  )

  if (n_ground == 0L) {
    stop_run(
      "Tidak ada titik ground hasil CSF. ",
      "Perbaiki parameter atau gunakan EXTERNAL_DTM."
    )
  }

  log_msg("Membuat DTM TIN resolusi ", DTM_RES, " m.")

  dtm <- rasterize_terrain(
    las,
    res = DTM_RES,
    algorithm = tin()
  )

  dtm_out <- file.path(OUTPUT_DIR, paste0("DTM_", PERIOD, ".tif"))
  terra::writeRaster(dtm, dtm_out, overwrite = TRUE)

  log_msg("DTM tersimpan: ", dtm_out)

} else {

  log_msg("Load external DTM: ", DTM_FILE)
  dtm <- terra::rast(DTM_FILE)

  dtm_crs <- st_crs(terra::crs(dtm))

  if (is.na(dtm_crs)) {
    stop_run("CRS external DTM kosong.")
  }

  if (!identical(st_crs(las)$wkt, dtm_crs$wkt)) {
    stop_run(
      "CRS external DTM berbeda dengan LAS. ",
      "Jangan reproject otomatis untuk pengukuran tinggi; samakan CRS terlebih dahulu."
    )
  }

  log_msg("CRS external DTM cocok dengan point cloud.")
}

log_msg("Normalisasi tinggi terhadap terrain dimulai.")

nlas <- normalize_height(las, dtm)

if (is.null(nlas) || npoints(nlas) == 0L) {
  stop_run("Normalisasi menghasilkan point cloud kosong.")
}

# Simpan statistik sebelum filter negatif
z_summary <- summary(nlas$Z)
capture.output(
  z_summary,
  file = file.path(QC_DIR, paste0("normalized_Z_summary_", PERIOD, ".txt"))
)

# QC ground normalized
gnd <- tryCatch(filter_ground(nlas), error = function(e) NULL)

if (!is.null(gnd) && npoints(gnd) > 0L) {
  gz <- gnd$Z
  log_msg(
    "QC normalized ground | mean=",
    round(mean(gz, na.rm = TRUE), 4),
    " m | min=",
    round(min(gz, na.rm = TRUE), 4),
    " m | max=",
    round(max(gz, na.rm = TRUE), 4),
    " m"
  )
}

nlas <- filter_poi(nlas, Z >= MIN_NORMALIZED_Z)

norm_out <- file.path(OUTPUT_DIR, paste0("normalized_pointcloud_", PERIOD, ".laz"))
writeLAS(nlas, norm_out)

log_msg("Normalized point cloud tersimpan: ", norm_out)


# =============================================================================
# H. OPTIONAL nCHM - VISUALISASI / SPATIAL QC
# =============================================================================

if (CREATE_NCHM) {
  log_msg("Generate nCHM untuk visualisasi/QC.")

  chm <- tryCatch(
    rasterize_canopy(
      nlas,
      res = res_chm,
      algorithm = pitfree(
        thresholds = CHM_THRESHOLDS,
        max_edge = CHM_MAX_EDGE
      )
    ),
    error = function(e) {
      log_msg(
        "Pitfree gagal: ", conditionMessage(e),
        ". Mencoba p2r sebagai fallback.",
        level = "WARN"
      )

      rasterize_canopy(
        nlas,
        res = res_chm,
        algorithm = p2r()
      )
    }
  )

  chm_out <- file.path(OUTPUT_DIR, paste0("nCHM_", PERIOD, ".tif"))
  terra::writeRaster(chm, chm_out, overwrite = TRUE)
  log_msg("nCHM tersimpan: ", chm_out)
}


# =============================================================================
# I. LOAD TITIK POKOK & BUFFER 2 M
# =============================================================================

log_msg("Load titik pokok: ", TREE_FILE)

titik_pokok <- st_read(TREE_FILE, quiet = TRUE)

if (!(ID_FIELD %in% names(titik_pokok))) {
  stop_run("Field ID '", ID_FIELD, "' tidak ditemukan pada titik pokok.")
}

if (any(is.na(titik_pokok[[ID_FIELD]]))) {
  stop_run("Terdapat TREE_ID kosong/NA.")
}

if (anyDuplicated(titik_pokok[[ID_FIELD]]) > 0L) {
  stop_run("TREE_ID tidak unik. Perbaiki duplikasi sebelum monitoring.")
}

geom_type <- unique(as.character(st_geometry_type(titik_pokok)))

if (!all(geom_type %in% c("POINT", "MULTIPOINT"))) {
  stop_run("TREE_FILE harus berupa geometry POINT/MULTIPOINT.")
}

if (any(geom_type == "MULTIPOINT")) {
  titik_pokok <- st_cast(titik_pokok, "POINT")
  if (anyDuplicated(titik_pokok[[ID_FIELD]]) > 0L) {
    stop_run(
      "MULTIPOINT menghasilkan lebih dari satu titik untuk TREE_ID yang sama."
    )
  }
}

titik_pokok <- st_transform(titik_pokok, st_crs(nlas))
titik_pokok$tree_id <- as.character(titik_pokok[[ID_FIELD]])

buffer_pokok <- st_buffer(titik_pokok, dist = BUFFER_RADIUS_M)

log_msg("Jumlah TREE_ID: ", nrow(titik_pokok))
log_msg("Buffer radius: ", BUFFER_RADIUS_M, " m")


# =============================================================================
# J. EKSTRAK POINT CLOUD PER TREE_ID
# =============================================================================

coords <- st_coordinates(titik_pokok)

if (nrow(coords) != nrow(titik_pokok)) {
  stop_run("Koordinat titik pokok tidak satu-ke-satu dengan TREE_ID.")
}

log_msg("Clip point cloud per buffer dimulai.")

# Coba vectorized clip_circle lebih dulu.
las_tree <- tryCatch(
  clip_circle(
    nlas,
    xcenter = coords[, 1],
    ycenter = coords[, 2],
    radius  = BUFFER_RADIUS_M
  ),
  error = function(e) NULL
)

vectorized_ok <- is.list(las_tree) && length(las_tree) == nrow(titik_pokok)

if (!vectorized_ok) {

  log_msg(
    "Vectorized clip_circle tidak tersedia/kompatibel. ",
    "Fallback ke loop per TREE_ID.",
    level = "WARN"
  )

  las_tree <- vector("list", nrow(titik_pokok))

  pb <- txtProgressBar(
    min = 0,
    max = nrow(titik_pokok),
    style = 3
  )

  for (i in seq_len(nrow(titik_pokok))) {

    las_tree[[i]] <- tryCatch(
      clip_circle(
        nlas,
        xcenter = coords[i, 1],
        ycenter = coords[i, 2],
        radius = BUFFER_RADIUS_M
      ),
      error = function(e) {
        log_msg(
          "Clip gagal TREE_ID=", titik_pokok$tree_id[i],
          " | ", conditionMessage(e),
          level = "WARN"
        )
        NULL
      }
    )

    setTxtProgressBar(pb, i)
  }

  close(pb)
}

log_msg("Hitung P95, P99, Top10, Top20, Top30.")

metrics_list <- lapply(
  las_tree,
  calc_tree_metrics,
  min_veg_h = MIN_VEG_H_M
)

metrics <- bind_rows(metrics_list)

if (nrow(metrics) != nrow(titik_pokok)) {
  stop_run(
    "Jumlah hasil metrics tidak sama dengan jumlah TREE_ID. ",
    "Proses dihentikan agar tidak terjadi salah pasangan."
  )
}

titik_current <- bind_cols(titik_pokok, metrics)

# Tinggi utama sementara
titik_current$h_main <- titik_current$p95


# =============================================================================
# K. QC JUMLAH TITIK - DATA DRIVEN
# =============================================================================

low_n_cut <- tukey_low_points(titik_current$npts)

titik_current$qc_current <- ifelse(
  is.na(titik_current$h_main),
  "NO_DATA",
  ifelse(
    titik_current$npts < low_n_cut,
    "LOW_POINTS",
    "OK"
  )
)

log_msg("Tukey lower fence npts: ", round(low_n_cut, 2))
log_msg(
  "TREE_ID valid tinggi: ",
  sum(is.finite(titik_current$h_main)),
  " / ", nrow(titik_current)
)


# =============================================================================
# L. BENTUK FIELD PERIODE SAAT INI
# =============================================================================

titik_result <- titik_current

titik_result[[F_H]]   <- titik_current$h_main
titik_result[[F_P99]] <- titik_current$p99
titik_result[[F_T10]] <- titik_current$top10
titik_result[[F_T20]] <- titik_current$top20
titik_result[[F_T30]] <- titik_current$top30
titik_result[[F_NP]]  <- titik_current$npts
titik_result[[F_QC]]  <- titik_current$qc_current

# Buang field sementara agar output lebih bersih
drop_temp <- intersect(
  c(
    "npts", "p95", "p99", "top10", "top20", "top30",
    "h_main", "qc_current"
  ),
  names(titik_result)
)

titik_result <- titik_result %>%
  select(-all_of(drop_temp))


# =============================================================================
# M. BASELINE ATAU MULTI-PERIODE
# =============================================================================

if (is_baseline) {

  log_msg("Membentuk baseline ", PERIOD, ".")

  titik_result[[F_DH]] <- NA_real_

  titik_result[[F_RSN]] <- ifelse(
    is.na(titik_result[[F_H]]),
    "NO_DATA",
    ifelse(
      titik_result[[F_QC]] != "OK",
      "LOW_POINTS",
      "OK"
    )
  )

  titik_result[[F_RES]] <- titik_result[[F_RSN]] != "OK"

} else {

  log_msg(
    "Load hasil periode sebelumnya ",
    PREV_PERIOD,
    ": ", PREV_SHP
  )

  prev <- st_read(PREV_SHP, quiet = TRUE)

  if (!("tree_id" %in% names(prev))) {
    stop_run("PREV_SHP tidak memiliki field tree_id.")
  }

  if (anyDuplicated(prev$tree_id) > 0L) {
    stop_run("tree_id pada PREV_SHP tidak unik.")
  }

  prev_h_field  <- field_name("h",  PREV_PERIOD)
  prev_qc_field <- field_name("qc", PREV_PERIOD)

  if (!(prev_h_field %in% names(prev))) {
    stop_run(
      "Field tinggi periode sebelumnya tidak ditemukan: ",
      prev_h_field
    )
  }

  hist_cols <- get_hist_cols(prev)
  prev_hist <- prev %>%
    st_drop_geometry() %>%
    select(all_of(hist_cols))

  # Hindari duplikasi field histori yang mungkin sudah ada di current source.
  overlap_cols <- setdiff(
    intersect(names(titik_result), names(prev_hist)),
    "tree_id"
  )

  if (length(overlap_cols) > 0L) {
    titik_result <- titik_result %>%
      select(-all_of(overlap_cols))
  }

  titik_result <- titik_result %>%
    left_join(prev_hist, by = "tree_id")

  # Delta tinggi P95 periode sekarang - periode sebelumnya
  titik_result[[F_DH]] <- (
    titik_result[[F_H]] - titik_result[[prev_h_field]]
  )

  fences <- tukey_fences(titik_result[[F_DH]])
  low_d  <- fences[["low"]]
  high_d <- fences[["high"]]

  log_msg(
    "Tukey delta fence: low=",
    ifelse(is.finite(low_d), round(low_d, 4), "-Inf"),
    " | high=",
    ifelse(is.finite(high_d), round(high_d, 4), "Inf")
  )

  # Residual = flag re-check, BUKAN diagnosis agronomis.
  reason <- rep("OK", nrow(titik_result))

  no_data <- (
    is.na(titik_result[[prev_h_field]]) |
    is.na(titik_result[[F_H]])
  )
  reason[no_data] <- "NO_DATA"

  # QC periode sebelumnya
  if (prev_qc_field %in% names(titik_result)) {
    bad_prev_qc <- (
      !no_data &
      !is.na(titik_result[[prev_qc_field]]) &
      titik_result[[prev_qc_field]] != "OK"
    )
    reason[bad_prev_qc] <- "PREV_QC"
  }

  low_points_now <- (
    reason == "OK" &
    titik_result[[F_QC]] != "OK"
  )
  reason[low_points_now] <- "LOW_POINTS"

  delta_low <- (
    reason == "OK" &
    is.finite(titik_result[[F_DH]]) &
    titik_result[[F_DH]] < low_d
  )
  reason[delta_low] <- "DELTA_LOW"

  delta_high <- (
    reason == "OK" &
    is.finite(titik_result[[F_DH]]) &
    titik_result[[F_DH]] > high_d
  )
  reason[delta_high] <- "DELTA_HIGH"

  titik_result[[F_RSN]] <- reason
  titik_result[[F_RES]] <- reason != "OK"
}


# =============================================================================
# N. RESIDUAL ZONE POLYGON 2 M
# =============================================================================

residual_idx <- which(titik_result[[F_RES]] %in% TRUE)

log_msg("Jumlah TREE_ID residual: ", length(residual_idx))

# Simpan point residual juga
residual_point <- titik_result[residual_idx, , drop = FALSE]

residual_point_path <- file.path(
  SHP_DIR,
  paste0("residual_point_", PERIOD, ".shp")
)

if (nrow(residual_point) > 0L) {
  safe_write_sf(residual_point, residual_point_path)
}

# Polygon residual zone = buffer 2 m dari TREE_ID residual.
# Tidak dissolve, agar TREE_ID dan reason tetap dapat ditelusuri.
if (length(residual_idx) > 0L) {

  residual_zone <- st_buffer(
    titik_result[residual_idx, , drop = FALSE],
    dist = BUFFER_RADIUS_M
  )

  residual_zone_path <- file.path(
    SHP_DIR,
    paste0("residual_zone_", PERIOD, ".shp")
  )

  safe_write_sf(residual_zone, residual_zone_path)

  write.csv(
    st_drop_geometry(residual_zone),
    file.path(OUTPUT_DIR, paste0("residual_zone_", PERIOD, ".csv")),
    row.names = FALSE
  )

  log_msg("Residual zone tersimpan: ", residual_zone_path)

} else {

  log_msg(
    "Tidak ada residual pada periode ini; residual zone tidak dibuat.",
    level = "INFO"
  )
}


# =============================================================================
# O. EXPORT MONITORING POINT & CSV
# =============================================================================

monitoring_shp <- file.path(
  SHP_DIR,
  paste0("monitoring_tinggi_", PERIOD, ".shp")
)

monitoring_csv <- file.path(
  OUTPUT_DIR,
  paste0("monitoring_tinggi_", PERIOD, ".csv")
)

safe_write_sf(titik_result, monitoring_shp)

write.csv(
  st_drop_geometry(titik_result),
  monitoring_csv,
  row.names = FALSE
)

log_msg("Monitoring SHP tersimpan: ", monitoring_shp)
log_msg("Monitoring CSV tersimpan: ", monitoring_csv)


# =============================================================================
# P. SUMMARY OUTPUT
# =============================================================================

h_current <- titik_result[[F_H]]

summary_row <- data.frame(
  period           = PERIOD,
  n_tree           = nrow(titik_result),
  n_height_valid   = sum(is.finite(h_current)),
  n_residual       = sum(titik_result[[F_RES]] %in% TRUE),
  residual_pct     = round(
    mean(titik_result[[F_RES]] %in% TRUE) * 100,
    2
  ),
  h_median_m       = ifelse(
    any(is.finite(h_current)),
    median(h_current, na.rm = TRUE),
    NA_real_
  ),
  h_p25_m          = ifelse(
    any(is.finite(h_current)),
    as.numeric(quantile(h_current, 0.25, na.rm = TRUE)),
    NA_real_
  ),
  h_p75_m          = ifelse(
    any(is.finite(h_current)),
    as.numeric(quantile(h_current, 0.75, na.rm = TRUE)),
    NA_real_
  ),
  npts_low_cut     = low_n_cut,
  buffer_radius_m  = BUFFER_RADIUS_M,
  min_veg_h_m      = MIN_VEG_H_M,
  estimator        = "P95",
  terrain_mode     = TERRAIN_MODE,
  stringsAsFactors = FALSE
)

if (!is_baseline) {

  dh_current <- titik_result[[F_DH]]

  summary_row$prev_period <- PREV_PERIOD
  summary_row$delta_median_m <- ifelse(
    any(is.finite(dh_current)),
    median(dh_current, na.rm = TRUE),
    NA_real_
  )

  summary_row$delta_p25_m <- ifelse(
    any(is.finite(dh_current)),
    as.numeric(quantile(dh_current, 0.25, na.rm = TRUE)),
    NA_real_
  )

  summary_row$delta_p75_m <- ifelse(
    any(is.finite(dh_current)),
    as.numeric(quantile(dh_current, 0.75, na.rm = TRUE)),
    NA_real_
  )
}

summary_csv <- file.path(
  OUTPUT_DIR,
  paste0("summary_", PERIOD, ".csv")
)

write.csv(summary_row, summary_csv, row.names = FALSE)


# =============================================================================
# Q. QC PLOTS
# =============================================================================

if (CREATE_QC_PLOTS) {

  # Tinggi periode saat ini
  valid_h <- h_current[is.finite(h_current)]

  if (length(valid_h) > 1L) {

    png(
      file.path(QC_DIR, paste0("hist_height_", PERIOD, ".png")),
      width = 1600,
      height = 1000,
      res = 150
    )

    hist(
      valid_h,
      breaks = "FD",
      main = paste0("Distribusi Tinggi P95 - ", PERIOD),
      xlab = "Tinggi P95 (m)"
    )

    dev.off()
  }

  # Delta untuk D2+
  if (!is_baseline) {

    valid_dh <- titik_result[[F_DH]]
    valid_dh <- valid_dh[is.finite(valid_dh)]

    if (length(valid_dh) > 1L) {

      png(
        file.path(QC_DIR, paste0("hist_delta_", PERIOD, ".png")),
        width = 1600,
        height = 1000,
        res = 150
      )

      hist(
        valid_dh,
        breaks = "FD",
        main = paste0(
          "Distribusi Delta Tinggi ",
          PREV_PERIOD, " -> ", PERIOD
        ),
        xlab = "Delta P95 (m)"
      )

      abline(v = 0, lty = 2)

      dev.off()
    }
  }
}


# =============================================================================
# R. FINAL REPORT
# =============================================================================

log_msg("============================================================")
log_msg("PROSES SELESAI")
log_msg("Periode              : ", PERIOD)
log_msg("Jumlah TREE_ID       : ", nrow(titik_result))
log_msg(
  "Tinggi valid         : ",
  sum(is.finite(titik_result[[F_H]]))
)
log_msg(
  "Residual             : ",
  sum(titik_result[[F_RES]] %in% TRUE)
)
log_msg("Output folder        : ", OUTPUT_DIR)
log_msg("Estimator tinggi     : P95")
log_msg("Buffer               : ", BUFFER_RADIUS_M, " m")
log_msg("============================================================")

cat(
  "\nMAS POPO selesai.\n",
  "Cek output di:\n",
  normalizePath(OUTPUT_DIR, winslash = "/", mustWork = FALSE),
  "\n"
)
