# api

Generates the static JSON/plotly data files served by the site's API (deployed via Netlify, see `netlify.toml`). `uk/` and `int/` each hold numbered pipeline scripts; `run_all.R` sources them in order and `toc.R` writes the top-level index JSON.

## Environment

Dependencies are pinned with `renv` (installer backend: `pak`, enabled via `.Rprofile`). Includes `uklr` and `ivx` from `kvasilopoulos/*` on GitHub.

- Restore: `Rscript -e 'renv::restore()'`
- Add a dependency: `Rscript -e 'renv::install("pkg"); renv::snapshot()'`
- After changing dependencies: `Rscript -e 'renv::snapshot()'`

## Commands

- Rebuild all data: `Rscript run_all.R`
