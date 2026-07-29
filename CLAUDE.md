# Chronicle Reports — Context for Claude Code

Project context for AI coding assistants lives in [AGENTS.md](AGENTS.md), which contains information on:
what the project is, how to access and analyze Chronicle data
(including the curated metric data dictionary), and how to build or alter reports. Read that file first.

## Claude Code-specific notes

Keep your working notes in the `.claude-notes` directory. Create (or use) a directory with
today's date and add files within it as needed (for example
`.claude-notes/2025-10-25/some-notes.md`). Refer to these notes when starting new tasks or
when you need context about the codebase.

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
# List available apps
chronicle_list_apps()
# Returns: c("connect", "workbench")

# Run an app with local filesystem data
chronicle_run_app("connect", base_path = "/path/to/chronicle/data")

# Run an app with S3 data
chronicle_run_app("workbench", base_path = "s3://chronicle-bucket/optional-prefix")
```

### Testing

The project uses testthat for unit tests:

```r
library(testthat)
test()
```

### Pre-commit Hooks

The project uses pre-commit hooks (`.pre-commit-config.yaml`) for code quality checks.
Make sure changes pass linting before committing.
