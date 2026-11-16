# Servers running chart (usage report)

Daily chart per product (Connect, Workbench) and `product_id` of how many servers were running, from curated `connect/heartbeats` and `workbench/heartbeats`.

## Definition

- Time is cut into 5-minute UTC windows. A host is running in a window if it sent a heartbeat in that window or the one before it (a single late heartbeat is not an outage).
- Per day: `servers_min`, `servers_max`, `servers_median` over the 288 windows of the UTC day; `hosts_seen` is the distinct host names with a heartbeat that day (rises above `servers_max` when hosts are replaced).
- The as-of day is excluded (days run through today - 1).
- Gaps are zero-filled: each `product_id` is filled from its first heartbeat window onward, so an outage shows as 0. A product_id that stops reporting (e.g. an old Connect license) therefore shows zeros after it stops.
- Host names are used only to count. They never enter the usage JSON export (the `servers` rows carry counts only).

## Files

- `R/usage_servers.R`: `usage_server_counts()`, `usage_read_heartbeats()`
- `R/usage_data.R`: `servers` table in `chronicle_usage_data()` and `usage_empty_tables()`
- `R/usage_export.R`: servers table in the JSON export and `chronicle_usage_from_json()`
- `inst/apps/usage/app.R`: `usage_servers_chart()` and the two cards
- `R/sample-data.R`: `sample_heartbeats_internal()`; `validate_sample_data_structure()` also requires the heartbeats so older cached samples are regenerated
- `tests/testthat/test_sample_data.R`: sample heartbeats, stale cache, servers table checks
- `README.md`, `CLAUDE.md`

## Sample data

Connect 2 hosts, Workbench 3, 288 rows per host per day with a few seconds of jitter. Staggered per product: a 30-minute outage with every host silent (about day -60), one extra host for 6 hours (about day -30), one churn day (about day -15) on which hosts are replaced 4 times; replacement hosts keep running afterwards. Connect heartbeats follow the license switch via `add_sample_product_id()`.

Performance: `chronicle_usage_data()` takes about 2 s on 12 months of sample heartbeats, so no tuning was needed.
