# Source Mapping v0.8.0

| Tutorial / agreed workflow concept | Implementation |
|---|---|
| Master registration is optional | Separate `registration_pipeline.R`; never called by period backend |
| Good PPK/GCP should not be transformed again | User skips module ① and confirms XY in module ② |
| Alignment before TREE_ID buffer measurement | Registered LAZ is produced before `period_pipeline.R` |
| Rolling two-period monitoring | Compare consumes PREV/CURR period packages only |
| Master can be any full PPK/GCP period | `master_period_code` independent from target period chronology |
| Avoid blind ICP | Control-pair translation/rigid/similarity only |
| Live visibility | Structured R log + raw streams + 2s heartbeat |
