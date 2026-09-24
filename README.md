# SawitHeight R Desktop v0.4.1

Aplikasi desktop Windows untuk monitoring tinggi dan pertumbuhan pokok sawit TBM dari dense point cloud fotogrametri SfM-MVS.

## Metode utama

- Akuisisi diasumsikan menggunakan positioning PPK.
- Point cloud dibersihkan dari duplikat dan noise SOR.
- Terrain menggunakan `CSF_TIN` atau `EXTERNAL_DTM` tervalidasi.
- Tinggi dinormalisasi terhadap terrain.
- Tinggi per pokok dihitung **langsung dari normalized point cloud** dalam buffer radius 2 m.
- Estimator utama sementara: **P95**.
- Pembanding: P99, mean Top 10%, Top 20%, Top 30%.
- nCHM hanya untuk visualisasi dan spatial QC.
- Monitoring D2+ memakai delta P95 antarperiode.
- Residual zone = flag re-check berbasis QC teknis + outlier Tukey, bukan diagnosis agronomis.

## Output utama

Per run:

- `shapefile/monitoring_tinggi_Dx.shp`
- `monitoring_tinggi_Dx.csv`
- `DTM_Dx.tif` bila mode CSF_TIN
- `normalized_pointcloud_Dx.laz` bila diaktifkan
- `nCHM_Dx.tif` bila diaktifkan
- `shapefile/residual_point_Dx.shp` bila ada residual
- `shapefile/residual_zone_Dx.shp` bila ada residual
- `residual_zone_Dx.csv` bila ada residual
- `summary_Dx.csv`
- `result_summary.json`
- `report.html`
- `analysis.log`
- histogram QC

## Field monitoring

Nama field dirancang aman untuk batas 10 karakter Shapefile:

- `h_D1`: P95 / tinggi utama
- `p99_D1`: P99
- `t10_D1`, `t20_D1`, `t30_D1`: mean titik tertinggi
- `np_D1`: jumlah titik valid
- `qc_D1`: quality-control
- `dh_D2`: delta P95 terhadap periode sebelumnya
- `rsn_D2`: residual reason
- `res_D2`: residual flag

## Build Windows

GitHub Actions -> **Build Windows EXE** -> Run workflow.

Pilih `bundle_r = true` untuk portable self-contained.

Hasil:
- `SawitHeight-R-Portable-0.4.1.exe`
- `SawitHeight-R-Setup-0.4.1.exe`

`package.json` menggunakan `--publish never`, sehingga electron-builder hanya membangun EXE; GitHub Actions menangani artifact/release.

## R 4.2.2

System-R dapat memakai R 4.2.2 apabila `lidR` dan dependency telah terpasang. Karena binary lama dapat tidak tersedia dari indeks CRAN saat ini, aplikasi menyertakan tutorial manual R 4.2.2 pada tombol **Buka Tutorial v0.4.0**.

Untuk distribusi antar-PC, build self-contained lebih direkomendasikan.

## Sumber metode

- Tutorial Fotogrametri — Monitoring Tinggi Sawit v0.4.0.
- MAS POPO — Monitoring Tinggi & Pertumbuhan Pokok Sawit TBM v0.4.1.
