# Scientific Notes v0.8.0

- Registration is a pre-processing correction, not a mandatory part of height estimation.
- If PPK/GCP is already consistent, do not add another transform.
- Matched stable controls should be independent of changing palm crowns.
- Translation-only is the default conservative model.
- Rigid/similarity transforms require spatially distributed, non-collinear controls.
- Registration fit residual is not independent accuracy. Leave-one-out RMSE is included as an additional cross-check.
- A master full PPK/GCP period may occur at any point in the historical sequence.
- Period packages remain independent. Old LAS files do not need reprocessing for each rolling comparison.
