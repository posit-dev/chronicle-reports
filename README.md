# Chronicle Example Reports

Chronicle helps data science managers and other stakeholders understand their organization’s use of supported professional products (Posit Connect and Posit Workbench). See [this announcement](https://posit.co/blog/chronicle-product-announcement-aug-2025/) for more details.

This repository contains a few example Shiny apps to generate reports using data from Posit Chronicle. The reports are designed to provide insights into user activity, engagement, and trends over time. Beyond these reports, Chronicle also ships with a [Cookbook](https://docs.posit.co/chronicle/reports/) of additional examples. You can browse an [example cookbook report with mock data](https://pub.current.posit.team/public/example-chronicle-cookbook/).

Coming soon - live examples of these reports.

## Prerequisites

### R version

These reports use Shiny, which is supported on the latest release version of R, as well as the previous four minor release versions of R. For example, if the latest release R version is 4.5, then that version is supported, as well as 4.4, 4.3, 4.2, and 4.1.

### Package Manager Configuration (optional but recommended)

If you are using Posit Package Manager, refer to the documentation to [obtain an appropriate URL](https://docs.posit.co/rspm/user/get-repo-url.html) and then [configure your R environment](https://docs.posit.co/rspm/user/configure-r.html).


Following these steps should give you something similar to this:

```R
# NOTE: Replace 'noble' with your Linux distribution (e.g., 'jammy', 'focal', etc.)
options(repos = c(CRAN = sprintf("https://packagemanager.posit.co/cran/latest/bin/linux/noble-%s/%s", R.version["arch"], substr(getRversion(), 1, 3))))
```

## Install the Chronicle Reports package

You can install the `chronicle.reports` package from GitHub with:

```R
# install.packages("pak")
pak::pak("posit-dev/chronicle-reports")
```

Alternatively, you can install the development version of the package with:

> [!IMPORTANT]
> The `dev` branch may contain unstable or experimental features. Use it at your own risk.

```R
# install.packages("pak")
pak::pak("posit-dev/chronicle-reports@dev")
```


You can then list available reports and run them:

```R
# List all available reports
chronicle.reports::chronicle_list_apps()

# Run a report with default data path
chronicle.reports::chronicle_run_app("connect_users")
```

If your Chronicle data is in a non-default directory (i.e., not `/var/lib/posit-chronicle/data`), then you need to pass in the base path.

```R
chronicle.reports::chronicle_run_app("connect_users", "/path/to/chronicle/data")
```

If your Chronicle data is in S3, then you need to pass in the bucket path.

```R
chronicle.reports::chronicle_run_app("connect_users", "s3://chronicle-bucket/optional-prefix")
```


## Publishing to Posit Connect

The reports can be published to Posit Connect using the `rsconnect` package. For example, for the Connect Users report, create a new `app.R` file:

```bash
mkdir chronicle-connect-users-report
cd chronicle-connect-users-report
vi app.R
```

```R
# The Connect Users report:
chronicle.reports::chronicle_run_app("connect_users", base_path="/path/to/chronicle/data")
```

You can then deploy that file to Connect:

```R
# install.packages("rsconnect")

# appDir should match whatever directory you are using
rsconnect::deployApp(appDir="chronicle-connect-users-report", appFiles=c("app.R"))
```

## Building your own report

You can use the reports in `inst/apps/` as a starting point for building your own report. Each app is a standalone Shiny application that you can copy and customize.

To develop and test your modifications:

```R
library(devtools)

# Copy an existing app as a template
file.copy(
  system.file("apps/connect_users", package = "chronicle.reports"),
  "my-custom-report",
  recursive = TRUE
)

# Modify the code in my-custom-report/app.R to fit your requirements

# Test your modified app
shiny::runApp("my-custom-report")
```

Alternatively, if you want to extend the package itself:

```R
library(devtools)

# Clone this repository and open it in your IDE
# Add your custom app to inst/apps/my_custom_app/app.R

# Re-load the code and run your app
load_all()
chronicle_run_app("my_custom_app", base_path="/path/to/chronicle/data")
```

## Reports

This section describes each of the available reports in this repository. You can list all available reports with `chronicle_list_apps()` and run any report with `chronicle_run_app("app_name")`.

### [Connect Users](./inst/apps/connect_users/) (`connect_users`)

This report analyzes user activity data from Posit Connect using raw Chronicle data, focusing on licensed users, daily active users, and publishers over time. It provides insights into user engagement patterns and trends.

### [Workbench Users](./inst/apps/workbench_users/) (`workbench_users`)

This report analyzes user activity data from Posit Workbench using raw Chronicle data, focusing on licensed users and daily active users over time. It provides insights into user engagement patterns and trends.

### [Connect User Totals](./inst/apps/connect_user_totals/) (`connect_user_totals`)

This report is similar to Connect Users but uses curated Chronicle data from the `connect/user_totals` dataset. It provides the same user activity insights with pre-processed data.

### [Connect Dashboard](./inst/apps/connect/) (`connect`)

A comprehensive multi-page dashboard for Posit Connect with three main sections:
- **Users**: User totals, trends, and detailed user list with filtering
- **Content**: Content overview and list (placeholder data)
- **Usage**: Visit trends and Shiny app usage statistics (placeholder data)

### [Workbench Dashboard](./inst/apps/workbench/) (`workbench`)

A comprehensive multi-page dashboard for Posit Workbench focused on user analytics:
- **Users**: User totals, trends, admin counts, and detailed user list with filtering
