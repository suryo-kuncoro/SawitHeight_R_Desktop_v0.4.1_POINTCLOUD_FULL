# Test Plan — v0.4.1

## Test 1 — Environment
- Rscript terdeteksi.
- Package jsonlite, lidR, terra, sf, dplyr tersedia.

## Test 2 — Baseline D1
Input subset LAS/LAZ + TREE_ID permanen.
Expected:
- monitoring_tinggi_D1.shp/csv
- h_D1, p99_D1, t10_D1, t20_D1, t30_D1, np_D1, qc_D1
- DTM_D1.tif jika CSF_TIN
- normalized_pointcloud_D1.laz bila aktif
- nCHM_D1.tif bila aktif
- summary_D1.csv

## Test 3 — Monitoring D2
Gunakan point cloud D2 dan monitoring_tinggi_D1.shp sebagai previous result.
Expected:
- histori D1 tetap terbawa
- h_D2 dan dh_D2 = h_D2 - h_D1
- rsn_D2 / res_D2 terisi
- residual_point_D2.shp dan residual_zone_D2.shp bila ada residual

## Test 4 — External DTM
Gunakan DTM D1 yang sudah tervalidasi pada D2.
Expected: CRS sama; proses normalisasi berjalan tanpa klasifikasi ground ulang.

## Test 5 — ID integrity
Uji TREE_ID duplikat, kosong, atau field salah. Expected: proses berhenti sebelum analisis berat.

## Test 6 — Scientific QC
Bandingkan P95, P99, Top10/20/30 pada sampel pokok. Jangan menetapkan P95 sebagai standar biologis final sebelum ada kalibrasi lapangan.
