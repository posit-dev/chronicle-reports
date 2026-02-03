# chronicle.reports (development version)

# chronicle.reports 0.2.1

* Improved Connect dashboard load time performance.
* Added optional `base_path` parameter to `chronicle_sample_data()` to allow writing sample data to a custom location.
* Aligned sample data structure with actual Chronicle data format.
* Simplified `chronicle_sample_data()` to generate only curated datasets (raw data generation removed).
* Fixed lintr warnings.

# chronicle.reports 0.2.0

* Significant refactor of the package structure and functions for improved maintainability.
* Add support for Curated Datasets (available in Chronicle 2026.1.0 and later).
* Introduced `chronicle_list_apps()` and `chronicle_run_app()` functions to manage and launch reporting apps.
* Introduced functions `chronicle_list_data()` and `chronicle_data()` to facilitate data retrieval from curated datasets.
* Introduced `chronicle_sample_data()` function to provide users with sample datasets for testing and exploration.
* Introduced `chronicle_list_raw_data()` and `chronicle_raw_data()` functions to facilitate data retrieval from raw datasets.
* Updated documentation to reflect changes in function names and usage.
* Miscellaneous dependency updates and bug fixes.

## Breaking Changes

* Removed deprecated `chr_get_metric_data()` and `chr_get_curated_metric_data()` functions. Use `chronicle_raw_data()` and `chronicle_data()` instead.
* Removed legacy apps: `connect_users`, `connect_user_totals`, and `workbench_users`. Use the newer `connect` and `workbench` apps instead.

# chronicle.reports 0.1.0

* Initial release of the `chronicle.reports` package.
* Provides two example reports that count named users in Connect and Workbench.
