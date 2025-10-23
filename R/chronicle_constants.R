# Brand colors used across all Chronicle apps
BRAND_COLORS <- list(
  BLUE = "#447099",
  GREEN = "#72994E",
  BURGUNDY = "#9A4665",
  GRAY = "#404041"
)

# Common application configuration
APP_CONFIG <- list(
  # Users inactive for more than this period are filtered out
  INACTIVE_USER_THRESHOLD = lubridate::dyears(1),

  # Default Chronicle data path
  DEFAULT_BASE_PATH = "/var/lib/posit-chronicle/data"
)
