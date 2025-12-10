# Chronicle Reports - Context for Claude Code

Keep your notes in the .claude-notes directory. Create (or use) a directory with today's date and create files within that directory as needed (for example .claude-notes/2025-10-25/some-notes.md). Refer to these notes when starting new tasks or when you need context about the codebase.

## Project Overview

This is an R package (`chronicle.reports`) that provides Shiny dashboard applications for analyzing usage data from Posit Chronicle. Chronicle helps data science managers understand their organization's use of Posit Connect and Posit Workbench.

**Repository**: https://github.com/posit-dev/chronicle-reports

## Key Components

### Available Reports

1. **Connect Users Dashboard** ([R/connect_users_app.R](R/connect_users_app.R))
   - Analyzes Posit Connect user activity
   - Tracks: licensed users, daily active users, publishers
   - Visualizes trends over time and activity patterns by day of week
   - Entry point: `connect_users_app()`

2. **Workbench Users Dashboard** ([R/workbench_users_app.R](R/workbench_users_app.R))
   - Analyzes Posit Workbench user activity
   - Tracks: licensed users, daily active users
   - Entry point: `workbench_users_app()`

3. **Connect User Totals Dashboard** ([R/connect_user_totals_app.R](R/connect_user_totals_app.R))
   - Entry point: `connect_user_totals_app()`

### Key Files

- [R/utils.R](R/utils.R) - Shared utility functions
- [R/chronicle_constants.R](R/chronicle_constants.R) - Constants and configuration
- [DESCRIPTION](DESCRIPTION) - Package metadata and dependencies
- [tests/](tests/) - Test suite using testthat

## Technology Stack

- **Language**: R (>= 4.1.0)
- **UI Framework**: Shiny with bslib for modern Bootstrap UI
- **Data Processing**: arrow, duckdb, dplyr, dbplyr
- **Visualization**: ggplot2, plotly
- **Testing**: testthat, shinytest2

## Development Workflow

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
# Local filesystem
connect_users_app("/path/to/chronicle/data")

# S3
connect_users_app("s3://chronicle-bucket/optional-prefix")
```

### Testing

The project uses testthat for unit tests:

```r
library(testthat)
test()
```

### Deployment to Posit Connect

Create an `app.R` file:

```r
chronicle.reports::connect_users_app(base_path="/path/to/chronicle/data")
```

Then deploy:

```r
rsconnect::deployApp(appDir="your-report-dir", appFiles=c("app.R"))
```

## Architecture Patterns

### Data Flow

1. **Data Loading**: Reports use `chr_data()` (for curated data) or `chr_raw_data()` (for raw data) from utils.R to load Chronicle metrics
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

## Important Conventions

1. **Function Documentation**: Use roxygen2 comments with `@param`, `@return`, `@export`
2. **Internal Functions**: Mark with `@noRd` to exclude from package docs
3. **Pipe Operator**: Use `|>` (base R pipe) not `%>%` (magrittr)
4. **Dependencies**: Import functions explicitly with `@importFrom` or use `package::function()`
5. **Code Style**: Follow tidyverse style guide (enforced by `.lintr` config)

## Common Tasks

### Adding a New Report

1. Create a new file in `R/` (e.g., `R/new_report_app.R`)
2. Define UI function using `bslib` components
3. Define server function with reactive data processing
4. Create exported wrapper function that calls `shiny::shinyApp()`
5. Document with roxygen2
6. Add entry to README.md

### Modifying Data Calculations

Look for functions named `calculate_*_daily_*_counts()` in the app files. These contain the business logic for metric calculations.

### Updating UI Components

- Value boxes: `bslib::value_box()`
- Cards: `bslib::card()` with `bslib::card_header()`
- Layout: `bslib::layout_columns()` with `col_widths`
- Spinners: `shinycssloaders::withSpinner()` for loading states

## Data Sources

Reports read Chronicle data files using arrow/duckdb from:
- Local filesystem: `/var/lib/posit-chronicle/data` (default)
- S3: `s3://bucket-name/prefix`
- Custom path: specified via `base_path` parameter or `CHRONICLE_BASE_PATH` env var

The data structure includes:
- Timestamp-based metrics with daily granularity
- User-level data with fields like `id`, `created_at`, `last_active_at`, `locked`, `user_role`

## Pre-commit Hooks

The project uses pre-commit hooks (`.pre-commit-config.yaml`) for code quality checks. Make sure changes pass linting before committing.

## Related Resources

- [Chronicle Announcement](https://posit.co/blog/chronicle-product-announcement-aug-2025/)
- [Chronicle Cookbook](https://docs.posit.co/chronicle/reports/)
- [Example Cookbook Report](https://pub.current.posit.team/public/example-chronicle-cookbook/)
- [Package Manager Configuration](https://docs.posit.co/rspm/user/get-repo-url.html)

## Current Branch

Working on branch: `2461-curation`
Main branch: `main`

## Notes for AI Assistants

- When modifying R code, preserve the existing style (pipe operators, spacing, roxygen docs)
- Test changes with `devtools::load_all()` before committing
- Keep visualizations consistent with existing color schemes
- Consider responsive design when modifying UI layouts
- Validate data filtering logic carefully - user counts must be calculated correctly
- S3 and local filesystem paths should both be supported
