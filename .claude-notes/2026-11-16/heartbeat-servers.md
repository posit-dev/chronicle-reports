# Servers running chart (usage report)

Daily chart per product (Connect, Workbench) and `product_id` of how many servers were running, from curated `connect/heartbeats` and `workbench/heartbeats`.

## Definition

- Time is cut into 5-minute UTC windows. A host is running in a window if it sent a heartbeat in that window or the one before it (a single late heartbeat is not an outage).
- Per day: `servers_min`, `servers_max`, `servers_median` over up to 288 windows of the UTC day (fewer on a product ID's first day and on the last day with heartbeats); `hosts_seen` is the distinct host names with a heartbeat that day (rises above `servers_max` when hosts are replaced).
- The as-of day is excluded: days run through today - 1, or through the latest heartbeat of any product_id if the data ends earlier (so lagging data does not read as an outage). The as-of day's heartbeats are read only to show the data reaches today - 1.
- Gaps are zero-filled: each `product_id` is filled from its first heartbeat window up to the latest heartbeat window of any product_id, so an outage shows as 0. A product_id that stops reporting (e.g. an old Connect license) therefore shows zeros after it stops, up to the last day with data; days after the latest heartbeat get no rows at all.
- Host names are used only to count. They never enter the usage JSON export (the `servers` rows carry counts only).

## Files

- `R/usage_servers.R`: `usage_server_counts()`, `usage_heartbeat_bounds()`, `usage_heartbeat_chunks()`, `usage_read_heartbeats()`
- `R/usage_data.R`: `servers` table in `chronicle_usage_data()` and `usage_empty_tables()`
- `R/usage_export.R`: servers table in the JSON export and `chronicle_usage_from_json()`
- `inst/apps/usage/app.R`: `usage_servers_chart()` and the two cards
- `R/sample-data.R`: `sample_heartbeats_internal()`; `validate_sample_data_structure()` also requires the heartbeats so older cached samples are regenerated
- `tests/testthat/test_sample_data.R`: sample heartbeats, stale cache, servers table checks
- `README.md`, `CLAUDE.md`

## Sample data

Connect 2 hosts, Workbench 3, 288 rows per host per day with a few seconds of jitter. Staggered per product: a 30-minute outage with every host silent (about day -60; the w-1 rule makes it 25 minutes of zero windows), one extra host for 6 hours (about day -30), one churn day (about day -15) on which hosts are replaced 4 times; replacement hosts keep running afterwards. Connect heartbeats follow the license switch via `add_sample_product_id()`.

Performance: `usage_read_heartbeats()` counts one calendar month at a time (`usage_heartbeat_chunks()`), reading the day before each month for the first window's predecessor. Each product_id's first and last heartbeat come from one arrow aggregate (`usage_heartbeat_bounds()`) passed to every chunk as `bounds`, so the zero-fill start and end do not change per chunk and the result is identical to counting everything at once (tested in `test_usage_servers.R`).

Measured 2026-11-16 on 40 hosts x 365 days, 5-minute beats, pods renamed weekly (about 4.2M rows), `usage_read_heartbeats()` in a fresh `Rscript` (103 MB of R heap in use before the call, 281 MB RSS for R plus the package):

| | Before (all at once) | After (monthly chunks) |
|---|---|---|
| R heap max used (`gc()`) | 815 MB | 315 MB |
| arrow pool max | 117 MB | 22 MB |
| process max RSS | 1.54 GB | 0.64 GB |
| elapsed | 3.1 s | 3.1 s |

Net of the 103 MB already in use, R heap is 712 MB -> 212 MB (3.4x); RSS net of the 281 MB baseline is 1.26 GB -> 0.36 GB (3.5x).
