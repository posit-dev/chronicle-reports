# Justfile for managing R package tasks

R_CMD := "Rscript"

run:
  @echo "Starting Shiny app..."
  {{R_CMD}} -e 'devtools::load_all(); connect_users_app()'

install:
  @echo "Installing R package..."
  {{R_CMD}} -e 'devtools::install()'

lint:
  @echo "Linting R code..."
  {{R_CMD}} -e 'if (!requireNamespace("lintr", quietly = TRUE)) install.packages("lintr"); lintr::lint_package()'

test:
  @echo "Testing R code..."
  {{R_CMD}} -e 'devtools::test()'

check:
  @echo "Checking R code..."
  {{R_CMD}} -e 'devtools::check(args = c("--no-manual", "--as-cran", "--no-examples"))'

docs:
  @echo "Building documentation..."
  {{R_CMD}} -e 'devtools::document()'