# Record the same repository packages were installed from, so renv.lock's
# repositories and per-package "Repository" fields match the install source
# (avoids CRAN<->RSPM changes).
options(repos = c(CRAN = Sys.getenv("CRAN")))

# Derive R version requirement from the running R version (set by the
# Dockerfile FROM image), so changing the base image is the only edit needed.
r_minor <- strsplit(R.version$minor, "\\.")[[1]][1]
r_requires <- paste0(">=", R.version$major, ".", r_minor, ".0")

app_dirs <- c("inst/apps/connect", "inst/apps/workbench")

for (app_dir in app_dirs) {
  message("Processing: ", app_dir)
  manifest_path <- file.path(app_dir, "manifest.json")
  ext_meta <- NULL
  if (file.exists(manifest_path)) {
    old <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
    ext_meta <- old$extension
  }
  renv::snapshot(project = app_dir, prompt = FALSE, force = TRUE)
  rsconnect::writeManifest(appDir = app_dir)

  # Post-process the manifest
  manifest <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
  if (!is.null(ext_meta)) {
    manifest <- c(list(extension = ext_meta), manifest)
    message("Restored extension metadata for: ", app_dir)
  }
  manifest$environment$r$requires <- r_requires
  message("Set R version requirement to ", r_requires, " for: ", app_dir)
  jsonlite::write_json(
    manifest,
    manifest_path,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )
}
