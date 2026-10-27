# api

Generates the static JSON/plotly data files served by the site's API (deployed via Netlify, see `netlify.toml`). `uk/` and `int/` each hold numbered pipeline scripts; `run_all.R` sources them in order and `toc.R` writes the top-level index JSON.

## Environment

- R version: managed with `rig` (currently 4.6.1 - `rig default 4.6.1`)
- Dependencies: managed with `rv` (`rproject.toml` + `rv.lock`), not renv. Includes `ivx` from `kvasilopoulos/ivx` on GitHub (no CRAN release).
  - Setup: `rv sync`
  - Add a dependency: `rv add <pkg>`
- Format: `air format .` (check only: `air format --check .`)
- Lint: `jarl check .` (autofix: `jarl check . --fix`)

### Rules
- Never use `install.packages()` or `renv::*` - all dependency changes go through `rv add`/`rv remove`. (A past unconditional `install.packages()` call in `run_all.R` is what silently drifted this project off its pinned `exuber` version and caused a half-day of unnecessary debugging - don't reintroduce that pattern.)
- Run `air format .` then `jarl check .` before committing.
- No `# nolint` comments - jarl uses `# jarl-ignore <rule>: <reason>` on the line before the flagged code instead.

## Commands

- Rebuild all data: `Rscript run_all.R`

<!-- knowledge:start -->
## Knowledge wiki
This repo's wiki lives in `./wiki` (committed with the code; overview: `wiki/housing-api.md`). It is linked into the
shared vault at `./kb` (-> C:\Users\User\knowledge), where `kb/CLAUDE.md` holds the rules (frontmatter, wikilinks, INGEST/QUERY/LINT).
- Session start: read `kb/inbox/housing-api.md`, act on items, move them under `## Done`.
- Write the wiki to the repo wiki standard: `kb/REPO-WIKI.md` (layout, page types, frontmatter, links). Scaffold pages with `python kb/scripts/new_page.py housing-api architecture|decision|runbook|integration|concept "<Title>"`.
- Update the wiki page that describes a behaviour in the same change that alters it; then run `python kb/scripts/reindex.py` and `python kb/scripts/lint.py`.
- Find things with `python kb/scripts/search.py "<terms>"` before browsing `kb/wiki/INDEX.md`. Never edit `kb/raw/`.
<!-- knowledge:end -->
