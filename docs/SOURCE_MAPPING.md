# Source Mapping — SawitHeight R v0.4.1

Aplikasi v0.4.1 diselaraskan dengan dua sumber proyek:

1. `Tutorial_LidR_v0.4.0_Monitoring_R422_PointCloud_Delta_ResidualZone.html`
   - normalized dense point cloud sebagai sumber tinggi utama;
   - buffer tetap 2 m per TREE_ID;
   - P95 sebagai estimator utama sementara;
   - P99 dan mean Top10/20/30 sebagai pembanding;
   - nCHM hanya untuk visualisasi dan spatial QC;
   - monitoring delta kontinu dan residual zone.

2. `MAS_POPO_PointCloud_Monitoring_v0.4.1.R`
   - mode terrain CSF_TIN / EXTERNAL_DTM;
   - QC jumlah titik dengan Tukey lower fence;
   - delta antarperiode `dh_Dx = h_Dx - h_Dprev`;
   - residual reason berdasarkan NO_DATA, PREV_QC, LOW_POINTS, DELTA_LOW, DELTA_HIGH;
   - residual point dan polygon buffer 2 m;
   - output SHP/CSV/GeoTIFF.

Perubahan dari v0.3.1:
- GCP bias/anchor dihapus dari UI karena tidak ada pada metode v0.4.1 terbaru.
- `kelas_Dx` dan threshold NORMAL/ANOMALI dihapus.
- tinggi tidak lagi berasal dari nCHM / zonal statistics.
- output monitoring memakai field `h_`, `p99_`, `t10_`, `t20_`, `t30_`, `np_`, `qc_`, `dh_`, `rsn_`, `res_`.
