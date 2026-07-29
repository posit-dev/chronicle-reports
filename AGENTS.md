# AGENTS.md — Chronicle Reports

Context for AI coding assistants (Posit Assistant, Claude Code, and other tools that read
`AGENTS.md`). This is the single source of truth for assistants working in
this repository: **analyzing Chronicle data, building or altering reports, and the
development workflow** (setup, testing, deployment, conventions).

## Project Overview

This is an R package (`chronicle.reports`) that provides:

- **Pre-built Shiny dashboards** for Posit Connect and Posit Workbench usage analytics
  (in [inst/apps/](inst/apps/), launched with `chronicle_run_app()`).
- **Programmatic data access** to Chronicle metrics for custom analysis
  (`chronicle_data()` and related functions in [R/utils.R](R/utils.R)).

Chronicle helps data science managers understand their organization's use of Posit Connect and Posit Workbench.

**Repository**: https://github.com/posit-dev/chronicle-reports

## Key Components

### Available Reports

1. **Connect Dashboard** ([inst/apps/connect/app.R](inst/apps/connect/app.R))
   - Comprehensive Posit Connect usage analysis
   - Tracks: licensed users, daily active users, publishers, content metrics
   - Visualizes trends over time and activity patterns
   - Entry point: `chronicle_run_app("connect")`

2. **Workbench Dashboard** ([inst/apps/workbench/app.R](inst/apps/workbench/app.R))
   - Comprehensive Posit Workbench usage analysis
   - Tracks: licensed users, daily active users, session metrics
   - Entry point: `chronicle_run_app("workbench")`

### Key Files

- [R/utils.R](R/utils.R) - Shared utility functions
- [R/chronicle_constants.R](R/chronicle_constants.R) - Constants and configuration
- [DESCRIPTION](DESCRIPTION) - Package metadata and dependencies
- [tests/](tests/) - Test suite using testthat

## Technology Stack

- **Language**: R (>= 4.2.0)
- **UI Framework**: Shiny with bslib for modern Bootstrap UI
- **Data Processing**: arrow, dplyr
- **Visualization**: ggplot2, plotly
- **Testing**: testthat, shinytest2

## Architecture Patterns

### Data Flow

1. **Data Loading**: Reports use `chronicle_data()` (for curated data) or `chronicle_raw_data()` (for raw data) from utils.R to load Chronicle metrics
2. **Data Processing**: Each app has its own calculation function (e.g., `calculate_connect_daily_user_counts()`)
3. **Reactivity**: Shiny reactive expressions handle data filtering and updates
4. **Visualization**: Combination of plotly (interactive) and ggplot2 (static) charts

### UI Structure

Reports use `bslib::page_sidebar()` layout with:
- Sidebar for filters (e.g., date range)
- Value boxes for current metrics
- Cards with charts for historical trends

### Color Scheme

Brand colors are defined in `chronicle_constants.R` and used consistently across reports:
- Blue: Licensed users
- Green: Daily/active users
- Burgundy: Publishers

You can change these to match your company branding.

## Important Conventions

1. **Function Documentation**: Use roxygen2 comments with `@param`, `@return`, `@export`
2. **Internal Functions**: Mark with `@noRd` to exclude from package docs
3. **Pipe Operator**: Use `|>` (base R pipe) not `%>%` (magrittr)
4. **Dependencies**: Import functions explicitly with `@importFrom` or use `package::function()`
5. **Code Style**: Follow tidyverse style guide (enforced by `.lintr` config)

## Common Tasks

### Setup

```r
# Install the package from GitHub
pak::pak("posit-dev/chronicle-reports")

# Or for development, load the package locally
library(devtools)
load_all()
```

### Running Reports Locally

Reports expect Chronicle data at a base path (default: `/var/lib/posit-chronicle/data`):

```r
# List available apps
chronicle_list_apps()
# Returns: c("connect", "workbench")

# Run an app with local filesystem data
chronicle_run_app("connect", base_path = "/path/to/chronicle/data")

# Run an app with S3 data
chronicle_run_app("workbench", base_path = "s3://chronicle-bucket/optional-prefix")
```

### Adding a New Report

1. Create a new directory in `inst/apps/` (e.g., `inst/apps/my_report/`)
2. Create an `app.R` file inside that directory
3. Define UI function using `bslib` components
4. Define server function with reactive data processing
5. The app will automatically be available via `chronicle_run_app("my_report")`
6. Add entry to README.md

### Modifying Data Calculations

Look for functions named `calculate_*_daily_*_counts()` in the app files within `inst/apps/*/app.R`. These contain the business logic for metric calculations.

### Updating UI Components

- Value boxes: `bslib::value_box()`
- Cards: `bslib::card()` with `bslib::card_header()`
- Layout: `bslib::layout_columns()` with `col_widths`
- Spinners: `shinycssloaders::withSpinner()` for loading states

## Working with Chronicle Data

### Data Sources

Reports read Chronicle data files using arrow from:

- Local filesystem: `/var/lib/posit-chronicle/data` (default)
- S3: `s3://bucket-name/prefix`
- Custom path: specified via `base_path` parameter or `CHRONICLE_BASE_PATH` env var

Any code you write should support both local and `s3://` paths.

Use **sample data** to explore without access to a real Chronicle deployment:

```r
sample_path <- chronicle_sample_data()   # temp dir, ~30 days, cached per session
```

### Data Access API

| Function | Purpose | Returns |
|---|---|---|
| `chronicle_data(metric, base_path)` | Load a **curated** metric (recommended) | arrow `Dataset` — call `dplyr::collect()` |
| `chronicle_list_data(base_path)` | List available curated metrics | character vector |
| `chronicle_raw_data(metric, base_path, frequency = "daily", ymd = NULL, schema = NULL)` | Load **raw** metrics (advanced) | arrow `Dataset` |
| `chronicle_list_raw_data(base_path, frequency = "daily")` | List available raw metrics | character vector |

**Important:** `chronicle_data()` returns a lazy **arrow dataset**, not a data frame. Push
`filter()` / `select()` / `group_by() |> summarise()` before `collect()` so work happens
in arrow and only the result is pulled into memory:

```r
library(dplyr)
chronicle_data("connect/user_totals", sample_path) |>
  filter(date >= as.Date("2026-06-15")) |>
  collect()
```

Prefer **curated** metrics over raw metrics unless you specifically need raw daily
snapshots — curated datasets are pre-processed for analysis.

### Curated Metric Data Dictionary

Column names and types below are the source of truth for writing analysis code. Every
curated dataset has a `date` (`Date`) column. `environment` (e.g. Production, Staging,
Development) appears on most and is a common grouping/filter dimension.

#### Connect

**`connect/user_totals`** — one row per day; org-level user counts.

| column | type | meaning |
|---|---|---|
| `named_users` | int | total named users |
| `active_users_30days` | int | users active in trailing 30 days |
| `active_users_1day` | int | daily active users |
| `administrators`, `publishers`, `viewers` | int | counts by role |
| `licensed_user_seats` | int | licensed seats |
| `date` | Date | day of snapshot |

**`connect/user_list`** — one row per user per snapshot day.

| column | type | meaning |
|---|---|---|
| `environment` | chr | environment |
| `id` | chr | user GUID |
| `username`, `email`, `first_name`, `last_name` | chr | identity |
| `user_role` | chr | administrator / publisher / viewer |
| `created_at`, `updated_at`, `last_active_at` | POSIXct | lifecycle timestamps |
| `active_today` | lgl | active on `date` |
| `date` | Date | snapshot day |

**`connect/content_totals`** — content counts by `type` and `environment` per day
(`count` int, `type` chr, `environment` chr, `date`).

**`connect/content_list`** — one row per content item (49 columns). Key fields: `id`
(content GUID), `name`, `title`, `type`, `content_category`, `access_type`, `owner_guid`,
`created_time`, `last_deployed_time`, `environment`, `r_version`/`py_version`/
`quarto_version`, `tags`, plus many runtime/resource settings. Join to hits via
`id` = `content_guid` and to owners via `owner_guid` = `user_list$id`.

**`connect/content_hits_totals`** — daily hits per content item
(`environment`, `content_guid`, `hits` int, `unique_users` int, `date`).

**`connect/content_hits_totals_by_user`** — daily hits per content item **per user**
(`environment`, `content_guid`, `user_guid`, `hits` int, `date`).

#### Workbench

**`workbench/user_totals`** — one row per day; org-level user counts
(`named_users`, `active_users_30days`, `active_users_1day`, `administrators`,
`super_administrators`, `users`, `licensed_user_seats`, `date`).

**`workbench/user_list`** — one row per user per day (`environment`, `id`, `username`,
`email`, `user_role`, `created_at`, `last_active_at`, `active_today`, `date`).

**`workbench/session_start_totals`** — daily session starts by `session_type` and
`environment` (`sessions_started` int, `median_startup_duration_ms` int,
`p95_startup_duration_ms` int, `date`).

**`workbench/session_start_totals_by_user`** — same as above but per user
(adds `user_guid`, `username`).

**`workbench/session_duration`** — one row per session (`host_name`, `environment`,
`user_guid`, `session_type`, `session_id`, `duration_seconds` int,
`session_started_at`, `session_ended_at`, `exit_code` int, `exit_reason`, `date`).
Covers only sessions that have **ended**, so session-start totals will be >= the row
counts here; still-running sessions have no duration or exit outcome yet.

### Common Analysis Recipes

```r
library(dplyr)
sp <- chronicle_sample_data()

# Connect daily active users over time
chronicle_data("connect/user_totals", sp) |>
  select(date, active_users_1day) |>
  collect()

# Top content by total hits over the loaded window
chronicle_data("connect/content_hits_totals", sp) |>
  group_by(content_guid) |>
  summarise(hits = sum(hits)) |>
  collect() |>
  arrange(desc(hits))

# Median Workbench session duration by session type
chronicle_data("workbench/session_duration", sp) |>
  group_by(session_type) |>
  summarise(median_minutes = median(duration_seconds) / 60) |>
  collect()
```

When counting users, deduplicate carefully — `*_list` datasets have one row per user
**per day**, so a distinct count of `id` across a range is not the same as summing a
daily count.

## Notes for AI Assistants

- When modifying R code, preserve the existing style (pipe operators, spacing, roxygen docs)
- Test changes with `devtools::load_all()` before committing
- Keep visualizations consistent with existing color schemes
- Consider responsive design when modifying UI layouts
- Validate data filtering logic carefully - user counts must be calculated correctly
- S3 and local filesystem paths should both be supported
