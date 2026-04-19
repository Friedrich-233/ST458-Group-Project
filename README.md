# ST458 Group Project — GBM Based Trading Strategy

## How to Use

Dead simple, three steps:

1. Clone the whole repo, keep the folder structure as-is
2. Open `Report Template.Rmd` in RStudio
3. Hit **Knit** (or `Knit to PDF`) — it will produce `Report-Template.pdf`

### Notes

- Several chunks are set to `cache=TRUE`, so the first knit takes a bit longer (mainly training LightGBM and running the walk-forward backtest). Subsequent knits are fast.
- The hyperparameter tuning chunk is set to `eval=FALSE` by default — it just reads `./Results/hp_tuning_results.rds` so you don't have to re-run the grid search every time. If you want to re-run it, remove `eval=FALSE` from that chunk.

---

## Folder Overview

`Report Template.Rmd` | Main report file — this is the one you knit
`Report-Template.pdf` | Rendered PDF output, for reference |
`Data/` | Training data `df_train.csv` — 100 synthetic ETFs with open/close/volume |
`Strategy/` | Three strategy scripts: `GroupA.R` (our LightGBM strategy, the main one), `Real.R` (momentum benchmark), `Equal.R` (equal-weight baseline) |
`Test/` | `walk_forward.R` — the backtest framework, sourced by the Rmd |
`Results/` | Cached hyperparameter tuning results (`hp_tuning_results.rds`) so the grid search doesn't re-run on every knit |

---
