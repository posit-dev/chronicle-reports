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

You can install the development version of `chronicle.reports` from GitHub with:

```R
# install.packages("pak")
pak::pak("posit-dev/chronicle-reports")
```

You can then run the report using:

```R
chronicle.reports::connect_users_app()
```

If your Chronicle data is in a non-default directory (i.e., not `/var/lib/posit-chronicle/data`), then you need to pass in the base path.

```R
chronicle.reports::connect_users_app("/path/to/chronicle/data")
```

If your Chronicle data is in S3, then you need to pass in the bucket path.

```R
chronicle.reports::connect_users_app("s3://chronicle-bucket/optional-prefix")
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
chronicle.reports::connect_users_app(base_path="/path/to/chronicle/data")
```

You can then deploy that file to Connect:

```R
# install.packages("rsconnect")

# appDir should match whatever directory you are using
rsconnect::deployApp(appDir="chronicle-connect-users-report", appFiles=c("app.R"))
```

## Building your own report

You can also use the source code in this repo as a starting point for building your own report. This is easiest to do using the [`devtools` package](https://devtools.r-lib.org/). You can open this repository in Positron or RStudio. Then, in the R console:

```R
library(devtools)

# Modify the code to fit your requirements

# Re-load the code and run your app
load_all()
connect_users_app(base_path="/path/to/chronicle/data")

# It may make sense for you to define your own report function and call
# your_new_report_app(base_path="/path/to/chronicle/data")
```

## Reports

This section describes each of the available reports in this repository.

### [Connect Users](./R/connect_users_app.R)

This report analyzes user activity data from Posit Connect, focusing on licensed users, daily active users, and publishers over time. It provides insights into user engagement patterns and trends.


### [Workbench Users](./R/workbench_users_app.R)

This report analyzes user activity data from Posit Workbench, focusing on licensed users and daily active users over time. It provides insights into user engagement patterns and trends.
