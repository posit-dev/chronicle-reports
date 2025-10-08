# chronicle-reports

TBD - overview...

## Getting Started

You can install the development version of `chronicle.reports` from GitHub with:

```R
# install.packages("pak")
pak::pak("posit-dev/chronicle-reports")
```

You can then run the reports using:

```R
library(chronicle.reports)
connect_users_app()
```

## Reports

### [Connect Users](./R/connect_users_app.R)

This report analyzes user activity data from Chronicle Connect, focusing on licensed users, daily active users, and publishers over time. It provides insights into user engagement patterns and trends.


