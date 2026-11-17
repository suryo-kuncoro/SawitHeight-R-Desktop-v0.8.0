# Architecture v0.8.0

```text
Electron Renderer
  ├─ ① Registration QC / Master Alignment (optional)
  │     └─ registration_pipeline.R
  ├─ ② Process One Period
  │     └─ period_pipeline.R
  └─ ③ Compare Two Period Packages
        └─ compare_pipeline.R

All R child processes
  → structured APP_EVENT log
  → raw stdout/stderr
  → Electron heartbeat
  → Live Process Log UI
```

Registration and period processing are deliberately decoupled. A good PPK/GCP period never needs to enter the registration backend.
