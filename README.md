# chronicle-reports

This repository contains Shiny apps to generate reports using data from Posit Chronicle. The reports are designed to provide insights into user activity, engagement, and trends over time.

## Prerequisites

If you are using Posit Package Manager, refer to the documentation to [obtain an appropriate URL](https://packagemanager.posit.co/__docs__/user/get-repo-url.html) and then [configure your R environment](https://packagemanager.posit.co/__docs__/user/configure-r.html).


Following these steps should give you something similar to this:

```R
# NOTE: Replace 'noble' with your Linux distribution (e.g., 'jammy', 'focal', etc.)
options(repos = c(CRAN = sprintf("https://packagemanager.posit.co/cran/latest/bin/linux/noble-%s/%s", R.version["arch"], substr(getRversion(), 1, 3))))
```

## Install the Chronicle Reports package

You can install the development version of `chronicle.reports` from GitHub with:

```R
install.packages("pak")
pak::pak("posit-dev/chronicle-reports")
```

You can then run the report using:

```R
chronicle.reports::connect_users_app()
```

## Reports

This section describes each of the available reports in this repository.

### [Connect Users](./R/connect_users_app.R)

This report analyzes user activity data from Posit Connect, focusing on licensed users, daily active users, and publishers over time. It provides insights into user engagement patterns and trends.


### [Workbench Users](./R/workbench_users_app.R)

This report analyzes user activity data from Posit Workbench, focusing on licensed users and daily active users over time. It provides insights into user engagement patterns and trends.
