# SawitHeight R Desktop v0.8.1

Aplikasi desktop Windows untuk monitoring tinggi relatif pokok sawit TBM dari dense point cloud SfM-MVS dengan arsitektur **Register (opsional) → Process One Period → Compare Two Periods**.

## Workflow operasional

### ① Registration QC / Master Alignment — OPSIONAL
Jalankan **hanya** bila periode target tidak konsisten terhadap frame referensi/TREE_ID.

- Input target LAS/LAZ.
- Input master full PPK/GCP LAS/LAZ sebagai reference frame.
- Input matched control/stable points untuk target dan master dengan ID yang sama.
- Model transform:
  - `TRANSLATION_3D` — offset XYZ saja; default dan paling konservatif.
  - `RIGID_3D` — rotation + translation, scale = 1.
  - `SIMILARITY_3D` — 7-parameter style similarity/Helmert: rotation + translation + scale.
- QC: RMSE before/after + leave-one-out RMSE.
- Output utama: `registered_<TARGET>_to_<MASTER>.laz`.

**Tidak ada ICP otomatis.** Bila PPK/GCP periode sudah baik, skip modul ini.

### ② Process One Period — WAJIB
Memproses satu periode saja dan menyimpannya menjadi `Period Package`:

- point cloud QC;
- terrain CSF→TIN atau external DTM;
- terrain hold-out QC;
- normalized point cloud;
- P95 sebagai tinggi utama;
- P99 + Top10/20/30 sebagai diagnostic metrics;
- presence support (`n_all`, `n_veg`, `veg_ratio`);
- metrics CSV/SHP + `period_manifest.json`.

Raw LAS yang sudah baik **atau** registered LAS dari modul ① dapat menjadi input.

### ③ Compare Two Periods — MONITORING
Membaca dua `Period Package`, bukan dua LAS mentah:

- delta multi-metrik;
- T_TERRAIN, T_METRIC, T_TECH;
- peer-based negative outlier;
- detail status untuk audit;
- keputusan operasional hanya `RECHECK` / `NO_RECHECK`;
- recheck point, buffer, dan dissolved zone.

## Siklus rolling

Contoh P1 dan P2 sudah selesai. Saat P3 masuk:

- bila P3 sudah baik secara PPK/GCP: **skip ① → process P3 → compare P2/P3**;
- bila P3 tidak konsisten: **align P3 ke master pada ① → process registered P3 pada ② → compare P2/P3 pada ③**.

P1 tidak perlu dibuka kembali.

## Live Process Log

Ketiga backend menampilkan structured log R + stdout/stderr + heartbeat Electron sekitar 2 detik. Operasi berat tetap terlihat `RUNNING/BUSY` meskipun satu fungsi R belum mencetak progress baru.

## Registration controls

Control file dapat berupa CSV atau POINT SHP/GPKG/GeoJSON.

- CSV default fields: `CONTROL_ID`, `X`, `Y`, `Z`.
- Spatial POINT: X/Y diambil dari geometry. Z memakai geometry Z bila ada, jika tidak dari field `Z`.
- `TRANSLATION_3D` minimal 1 matched pair.
- `RIGID_3D` / `SIMILARITY_3D` minimal 3 control pair yang tidak kollinear.

Untuk registrasi yang defensible, gunakan control/stable feature yang tersebar di area, bukan tajuk sawit yang berubah antarperiode.

## Build Windows

```bash
npm ci
npm run verify
npm run dist
```

GitHub Actions tetap mendukung `bundle_r=true` untuk menghasilkan Portable/Setup self-contained.

Expected artifacts:

- `SawitHeight-R-Portable-0.8.1.exe`
- `SawitHeight-R-Setup-0.8.1.exe`


## v0.8.1 bugfix
- Compare 2 Periods sekarang memaksa `tree_id` CSV dan SHP menjadi character sebelum join.
- Mempertahankan ID numerik/teks secara konsisten dan mencegah error `character` vs `integer`.
