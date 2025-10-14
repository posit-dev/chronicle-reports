# Justfile for managing R package tasks

R_CMD := "Rscript"

setup:
  @echo "Setting up R environment..."
  brew install pre-commit
  {{R_CMD}} -e 'if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools"); if (!requireNamespace("roxygen2", quietly = TRUE)) install.packages("roxygen2"); if (!requireNamespace("testthat", quietly = TRUE)) install.packages("testthat"); if (!requireNamespace("lintr", quietly = TRUE)) install.packages("lintr")'
  {{R_CMD}} -e 'if (!requireNamespace("precommit", quietly = TRUE)) install.packages("precommit");'
  {{R_CMD}} -e 'precommit::use_precommit()'

run:
  @echo "Starting Shiny app..."
  {{R_CMD}} -e 'devtools::load_all(); connect_users_app()'

install:
  @echo "Installing R package..."
  {{R_CMD}} -e 'devtools::install()'

# Returns non-zero exit code if linting errors are found
lint: install
  @echo "Linting R code..."
  {{R_CMD}} -e 'if (!requireNamespace("lintr", quietly = TRUE)) install.packages("lintr"); errors <- lintr::lint_package(); print(errors); quit(save = "no", status = length(errors))'

test:
  @echo "Testing R code..."
  {{R_CMD}} -e 'devtools::test()'

check:
  @echo "Checking R code..."
  {{R_CMD}} -e 'devtools::check(args = c("--no-manual", "--as-cran", "--no-examples"))'

docs:
  @echo "Building documentation..."
  {{R_CMD}} -e 'devtools::document()'
