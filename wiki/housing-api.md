---
title: housing-api
type: project
status: active
created: 2026-10-27
updated: 2026-10-29
sources: []
scope: housing-observatory
tags: [project, backend]
verified_commit: 1c5d010f82a4d2e51c23cd28771434c4fcaec5cc
files: [run_all.R, toc.R, netlify.toml, uk, int, public, rproject.toml]
---
# housing-api

Generates the static JSON and Plotly files that the observatory sites and dashboards read, published as `api.housing-observatory.com` via Netlify.

Part of [[concepts/housing-observatory]]. GitHub: `lancs-macro/api`.

## Stack
R. `uk/` and `int/` each hold numbered pipeline scripts (`01-download.R`, `02-write-json.R`, `03-write-plotly.R`, helpers in `00-functions*.R`). `ivx` comes from GitHub (`kvasilopoulos/ivx`, no CRAN release). R 4.6.1 via `rig`; dependencies with `rv` (`rproject.toml` + `rv.lock`), never `install.packages()` or `renv`; format with `air format .`, lint with `jarl check .` before committing (repo `CLAUDE.md`).

## Run and test
`Rscript run_all.R` rebuilds everything. `toc.R` writes the index files (`public/index.json`, `public/datasets/index.json`). Output is committed under `public/`.

## Endpoints (from consumers)
- `/datasets/<int|uk>/index.json` lists releases; `/datasets/int/<release>/rhpi.json` and `rhpi-radf.json` ([[projects/idash/idash]])
- `/visualizations/<int|uk>/rhpi` and `rhpi_yoy` ([[projects/housing-site-int/housing-site-int]], [[projects/housing-site-uk/housing-site-uk]])

## Layout
`uk/`, `int/` pipelines; `data-raw/`; `public/` served files; `netlify.toml` (publish `public`, redirects from `/` to `index.json`); `news/` a Scrapy project with `uncertainty/` (purpose undocumented).

## Unknowns
- Where raw quarterly inputs come from beyond `01-download.R` (the Dallas Fed IHPD per [[projects/04-technical-report/04-technical-report]]).
- What `news/` feeds; the uncertainty tab of [[projects/ukdash/ukdash]] is a likely consumer (unverified).

Related: [[concepts/housing-observatory]], [[conventions]], [[inbox/housing-api]].
