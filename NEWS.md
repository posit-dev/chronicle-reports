# chronicle.reports (development version)

## Breaking Changes

* Removed deprecated `chr_get_metric_data()` and `chr_get_curated_metric_data()` functions. Use `chronicle_raw_data()` and `chronicle_data()` instead.
* Removed legacy apps: `connect_users`, `connect_user_totals`, and `workbench_users`. The package now provides only two comprehensive dashboards: `connect` and `workbench`.
* Users should use `chronicle_run_app("connect")` or `chronicle_run_app("workbench")` to launch apps.

# chronicle.reports 0.1.0

* Initial release of the `chronicle.reports` package.
* Provides two example reports that count named users in Connect and Workbench.
