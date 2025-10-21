# Brand colors used across all Chronicle apps
BRAND_COLORS <- list(
  BLUE = "#447099",
  GREEN = "#72994E",
  BURGUNDY = "#9A4665",
  GRAY = "#404041"
)

# Connect-specific color mappings
CONNECT_COLORS <- list(
  LICENSED_USERS = BRAND_COLORS$BLUE,
  DAILY_USERS = BRAND_COLORS$GREEN,
  PUBLISHERS = BRAND_COLORS$BURGUNDY
)

# Workbench-specific color mappings (for future use)
WORKBENCH_COLORS <- list(
  LICENSED_USERS = BRAND_COLORS$BLUE,
  DAILY_USERS = BRAND_COLORS$GREEN,
  SESSIONS = BRAND_COLORS$BURGUNDY
)

# Common application configuration
APP_CONFIG <- list(
  # Users inactive for more than this period are filtered out
  INACTIVE_USER_THRESHOLD = lubridate::dyears(1),

  # Default Chronicle data path
  DEFAULT_BASE_PATH = "/var/lib/posit-chronicle/data"
)
