# Chronicle Reports

Chronicle Reports is an R package that provides pre-built Shiny dashboards and data access tools for analyzing usage data from Posit Chronicle. Chronicle helps organizations understand their use of Posit Connect and Posit Workbench through comprehensive metrics on users, content, and activity patterns. See the [Chronicle announcement](https://posit.co/blog/chronicle-product-announcement-aug-2025/) for more details.

This package serves two primary use cases:
- **For IT admins**: Deploy pre-built dashboards to Posit Connect with minimal configuration
- **For power users**: Access Chronicle data programmatically to build custom analyses and reports

## Quick Start

Install the package and explore using sample data:

```r
# Install from GitHub
pak::pak("posit-dev/chronicle-reports")

# Get sample data path
sample_path <- chronicle.reports::chronicle_sample_data()

# Run the Connect dashboard
chronicle.reports::chronicle_run_app("connect", base_path = sample_path)
```

## Installation

### From GitHub

Install the stable release from the main branch:

```r
# install.packages("pak")
pak::pak("posit-dev/chronicle-reports")
```

For contributors, the development version is available:

> [!IMPORTANT]
> The `dev` branch may contain unstable or experimental features. Use at your own risk.

```r
pak::pak("posit-dev/chronicle-reports@dev")
```

### Prerequisites

**R Version**: Requires R >= 4.1.0. The package supports the latest release version of R, as well as the previous four minor release versions. For example, if the latest release R version is 4.5, then versions 4.5, 4.4, 4.3, 4.2, and 4.1 are supported.

**Package Manager (optional)**: If using Posit Package Manager, [obtain an appropriate URL](https://docs.posit.co/rspm/user/get-repo-url.html) and [configure your R environment](https://docs.posit.co/rspm/user/configure-r.html) for optimized package installation:

```r
# Example configuration (adjust 'noble' to match your Linux distribution)
options(repos = c(
  CRAN = sprintf(
    "https://packagemanager.posit.co/cran/latest/bin/linux/noble-%s/%s",
    R.version["arch"],
    substr(getRversion(), 1, 3)
  )
))
```

## Running Reports

### Basic Usage

Run any report using `chronicle_run_app()`:

```r
# With default data path (/var/lib/posit-chronicle/data)
chronicle.reports::chronicle_run_app("connect")
```

### Data Source Options

#### Custom Filesystem Path

If your Chronicle data is in a non-default location:

```r
chronicle.reports::chronicle_run_app(
  "connect",
  base_path = "/path/to/chronicle/data"
)
```

#### S3 Storage

For Chronicle data stored in S3:

```r
chronicle.reports::chronicle_run_app(
  "connect",
  base_path = "s3://chronicle-bucket/optional-prefix"
)
```

#### Environment Variable

Set `CHRONICLE_BASE_PATH` to avoid hard coding the path:

```r
Sys.setenv(CHRONICLE_BASE_PATH = "/path/to/chronicle/data")
chronicle.reports::chronicle_run_app("connect")
```

## Available Reports

Chronicle Reports includes two comprehensive dashboards. List available reports at any time:

```r
chronicle.reports::chronicle_list_apps()
# Returns: c("connect", "workbench")
```

### Connect Dashboard

**Run it**: `chronicle_run_app("connect")`

<img width="2509" height="1296" alt="Connect Dashboard showing user overview, content metrics, and usage statistics" src="https://github.com/user-attachments/assets/9a9726ea-22e0-44a5-ae1c-c5a0ea1f1ffa" />

A multi-page dashboard for Posit Connect usage analysis with three main sections:

- **Users**:
  - Overview: Total licensed users, daily active users, publishers with trend visualizations
  - User List: Searchable, filterable table of all users with role and activity details
- **Content**:
  - Overview: Content metrics and trends
  - Content List: Searchable inventory of all published content
- **Usage**:
  - Overview: Visit trends and patterns
  - Shiny Apps: Overall usage trends and a breakdown of usage by app
  - Content Visits by User: Detailed view of content visits per user
  - Shiny Sessions by User: Detailed view of Shiny app sessions per user

### Workbench Dashboard

**Run it**: `chronicle_run_app("workbench")`

<img width="2005" height="1236" alt="Workbench Dashboard displaying user analytics and activity trends" src="https://github.com/user-attachments/assets/072e6dd7-f2a4-444f-ac2b-87f9382a3709" />

A multi-page dashboard for Posit Workbench user analytics:

- **Users**:
  - Overview: Licensed users, daily active users, administrators, and super administrators with trend analysis
  - User List: Searchable, Filterable table showing all users with role and activity information

## Publishing to Posit Connect

Deploy Chronicle Reports dashboards to Posit Connect for organization-wide access.

### Step 1: Create Project Directory

Create a directory with an `app.R` file:

```bash
mkdir chronicle-connect-report
cd chronicle-connect-report
```

Create `app.R`:

```r
# app.R
chronicle.reports::chronicle_run_app(
  "connect",
  base_path = "/path/to/chronicle/data"
)
```

### Step 2: Deploy to Connect

```r
# install.packages("rsconnect")
rsconnect::deployApp(
  appDir = "chronicle-connect-report",
  appFiles = c("app.R")
)
```

### Step 3: Configure Environment Variable (Recommended)

Instead of hard coding the path, configure it in the Posit Connect UI:

1. Navigate to your deployed application in Connect
2. Go to the "Vars" panel
3. Add environment variable: `CHRONICLE_BASE_PATH = /path/to/chronicle/data`

Then simplify your `app.R`:

```r
# app.R
chronicle.reports::chronicle_run_app("connect")
```

## Sample Data

Use `chronicle_sample_data()` to explore Chronicle Reports without access to real Chronicle data. This function creates a temporary directory with minimal sample metrics for both Connect and Workbench.

### What's Included

- **Connect metrics**: User totals and user list with 26 sample users across 3 days
- **Workbench metrics**: User totals and user list with 21 sample users across 3 days
- **Raw data examples**: Sample raw Connect user data

Sample data includes realistic user roles (viewers, publishers, administrators), environments (Production, Development, Staging), and activity patterns.

### Using Sample Data

```r
# Get path to sample data
sample_path <- chronicle.reports::chronicle_sample_data()

# List available metrics
chronicle.reports::chronicle_list_data(sample_path)

# Run an app with sample data
chronicle.reports::chronicle_run_app("connect", base_path = sample_path)

# Load and explore sample data
data <- chronicle.reports::chronicle_data("connect/user_totals", sample_path)
data |> dplyr::collect()
```

The sample data is cached per R session and automatically cleaned up. Use `chronicle_sample_data(refresh = TRUE)` to regenerate.

## Data Access for Custom Analysis

Beyond pre-built dashboards, Chronicle Reports provides functions for accessing Chronicle data programmatically to build custom analyses and reports.

### Curated Metrics (Recommended)

Use `chronicle_data()` to access curated metrics. These are pre-processed datasets optimized for common analyses:

```r
# Load a curated metric
data <- chronicle.reports::chronicle_data(
  "connect/user_totals",
  base_path = "/path/to/chronicle/data"
)

# Works seamlessly with dplyr
library(dplyr)
data |>
  filter(date >= as.Date("2024-01-01")) |>
  collect()
```

**Available curated metrics**:
- `connect/user_totals` - Daily user totals and activity metrics
- `connect/user_list` - Current user list with roles and activity
- `workbench/user_totals` - Daily Workbench user totals
- `workbench/user_list` - Current Workbench user list

List all available curated metrics:

```r
chronicle.reports::chronicle_list_data(base_path = "/path/to/chronicle/data")
```

### Raw Metrics (Advanced)

Use `chronicle_raw_data()` for granular control over raw Chronicle metrics:

```r
# Load raw daily metrics
data <- chronicle.reports::chronicle_raw_data(
  "connect_users",
  base_path = "/path/to/chronicle/data",
  frequency = "daily"
)

# Load specific date range
data <- chronicle.reports::chronicle_raw_data(
  "connect_users",
  base_path = "/path/to/chronicle/data",
  frequency = "daily",
  ymd = list(year = 2024, month = 1, day = 15)
)
```

List all available raw metrics:

```r
chronicle.reports::chronicle_list_raw_data(
  base_path = "/path/to/chronicle/data",
  frequency = "daily"
)
```

### Additional Documentation

For detailed information about Chronicle data structure, metrics, and analysis patterns, see the [Chronicle Cookbook](https://docs.posit.co/chronicle/reports/).

## Building Custom Reports

Power users can build custom reports by copying and modifying existing dashboards.

### Copy Existing App as Template

```r
# Copy an existing app as a starting point
file.copy(
  system.file("apps/connect", package = "chronicle.reports"),
  "my-custom-report",
  recursive = TRUE
)

# Modify the code in my-custom-report/app.R

# Test your custom app
shiny::runApp("my-custom-report")
```

### Key Patterns

Existing apps in [`inst/apps/`](inst/apps/) demonstrate:
- Shiny app structure using `bslib` for modern UI
- Reactive data loading with error handling
- Value boxes, cards, and responsive layouts
- Interactive plots with plotly
- Filterable data tables with DT

Refer to the [Connect](inst/apps/connect/app.R) and [Workbench](inst/apps/workbench/app.R) dashboards as reference implementations.

### Extending the Package

Contributors can add apps directly to the package:

```r
library(devtools)

# Clone repository and add custom app to inst/apps/my_custom_app/app.R
load_all()
chronicle.reports::chronicle_run_app("my_custom_app", base_path = "/path/to/chronicle/data")
```

## Resources

- **GitHub Repository**: [posit-dev/chronicle-reports](https://github.com/posit-dev/chronicle-reports)
- **Chronicle Cookbook**: [docs.posit.co/chronicle/reports](https://docs.posit.co/chronicle/reports/) - Detailed data documentation and report examples
- **Example Report**: [Example Chronicle Cookbook Report](https://pub.current.posit.team/public/example-chronicle-cookbook/) with mock data
- **Bug Reports**: [GitHub Issues](https://github.com/posit-dev/chronicle-reports/issues)
- **Contact**: [Posit Support](https://posit.co/support/) for assistance with Chronicle

## License

MIT License. See LICENSE file for details.
