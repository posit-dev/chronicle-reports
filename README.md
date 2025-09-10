# Chronicle Reports

A collection of open source Shiny reports for Posit Chronicle.

## Overview

Chronicle Reports provides example Shiny applications designed to help Posit Team administrators analyze and understand usage data. These reports serve two key purposes:

1. **Ready-to-use deployable reports**: Deploy these reports directly to your Posit Connect instance for immediate user data insights
2. **Example applications**: Use these reports as templates and examples for building your own custom user data analysis tools

## Available Reports


### Posit Connect Users

A Shiny application for understanding named users in Posit Connect. This report counts named users, daily users, and visualizes trends and usage patterns.

TBD - add a screenshot

### Posit Connect Duplicate Users Finder

A Shiny application to identify and manage duplicate user accounts in Posit Connect. This tool helps administrators maintain clean user databases by detecting potential duplicates based on username or email address.

## Installation and Deployment

### Prerequisites

- R (version 4.5 or higher recommended)
- Posit Connect instance
- Required R packages (managed via the manifest.json files)

### Deploying to Posit Connect

1. In Posit Connect, navigate to the "Content" section and click "Publish" --> "Import from Git".
   - TBD - add a screenshot
2. Enter the Git repository URL: `https://github.com/posit-dev/chronicle-reports`
3. Confirm to use the `main` branch and click "Next".
4. From the drop-down list, select the desired report directory (e.g., `pct_dup_users_finder/` or `pct_users_poc/`).
5. Enter a title for the report and click "Deploy Content".

#### Setting the base path

The reports use the default base path for Chronicle data (`/var/lib/posit-chronicle/data`). If your Chronicle data is stored in a different location, you can set the `CHRONICLE_BASE_PATH` environment variable in Posit Connect:

1. Open the report you just deployed in Posit Connect.
2. Navigate to "Settings" --> "Vars".
3. Add a new environment variable:
   - Name: `CHRONICLE_BASE_PATH`
   - Value: The path to your Chronicle data directory (e.g., `/custom/path/to/chronicle/data` or `s3://chronicle-bucket-name`)

See the [Posit Connect documentation](https://docs.posit.co/connect/user/content-settings/#content-vars) for more details on setting environment variables.

## Customizing the Reports

Each report can be used as a starting point for building your own custom applications. You can fork the repository and modify the code to suit your needs. Common customizations include:

- Modify the UI components in the app.R files
- Adjust the data processing logic for your specific Connect instance
- Extend the reports with additional visualizations or features

See the [Posit Chronicle documentation](https://docs.posit.co/chronicle/appendix/library/chronicle-data.html) for more details on the data available from Chronicle.