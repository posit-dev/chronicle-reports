# Sample data generation for Chronicle Reports

# Package-level environment to cache sample data path
.chronicle_sample_env <- new.env(parent = emptyenv())

#' Get path to sample Chronicle data
#'
#' This function creates a sample Chronicle data directory for
#' exploring package functionality without needing access to real Chronicle
#' data. The sample data is created once per R session and cached.
#'
#' The sample data includes:
#' \itemize{
#'   \item Connect user totals (30 days of data ending yesterday)
#'   \item Connect user list (26 sample users)
#'   \item Connect content list (150 sample content items)
#'   \item Connect content totals (30 days by type and environment)
#'   \item Connect content visits (30 days of user visit data)
#'   \item Connect Shiny usage (30 days of Shiny session data)
#'   \item Workbench user totals (30 days of data ending yesterday)
#'   \item Workbench user list (21 sample users)
#'   \item Raw Connect users data (30 days, 15 users per snapshot)
#' }
#'
#' All sample data uses realistic weekday/weekend patterns and maintains
#' referential integrity across datasets. Content items are created gradually
#' over the 30-day period and persist once created.
#'
#' @param refresh Logical. If TRUE, regenerate sample data even if cached.
#'   Default is FALSE. When FALSE, the function reuses existing valid data for
#'   both default temp directories and custom paths. When TRUE, data is always
#'   regenerated to ensure freshness.
#' @param base_path Character path to directory where sample data should be
#'   created. If NULL (default), creates in a temporary directory that is
#'   cached and automatically cleaned up at session end. If specified, creates
#'   data at the given path with no automatic cleanup. When \code{refresh=FALSE}
#'   and the directory exists, the function validates the structure and reuses
#'   existing data if valid, otherwise regenerates.
#'
#' @return Character path to the sample Chronicle data directory
#'
#' @export
#'
#' @examples
#' # Get path to sample data (default temp directory)
#' sample_path <- chronicle_sample_data()
#'
#' # Reuse cached sample data (fast)
#' sample_path <- chronicle_sample_data()
#'
#' # Force refresh of sample data
#' sample_path <- chronicle_sample_data(refresh = TRUE)
#'
#' # Create sample data in a custom directory
#' custom_dir <- tempfile("chronicle-custom-")
#' sample_path <- chronicle_sample_data(base_path = custom_dir)
#'
#' # Reuse existing custom directory data
#' sample_path <- chronicle_sample_data(base_path = custom_dir)
#'
#' # List available metrics
#' chronicle_list_data(sample_path)
#'
#' # Load sample data
#' data <- chronicle_data("connect/user_totals", sample_path)
#' dplyr::collect(data)
#'
#' # Clean up (default temp dir cleaned automatically at session end)
#' # For custom directories, manual cleanup is needed
#' unlink(custom_dir, recursive = TRUE)
chronicle_sample_data <- function(refresh = FALSE, base_path = NULL) {
  # Check if we have a cached path and it still exists (default paths only)
  if (
    !refresh &&
      is.null(base_path) &&
      exists("sample_path", envir = .chronicle_sample_env)
  ) {
    cached_path <- get("sample_path", envir = .chronicle_sample_env)
    if (dir.exists(cached_path)) {
      return(cached_path)
    }
  }

  # Determine which directory to use
  if (is.null(base_path)) {
    sample_dir <- file.path(tempdir(), "chronicle-sample-data")
  } else {
    sample_dir <- base_path
  }

  # For custom paths with refresh=FALSE, check if we can reuse existing data
  if (!is.null(base_path) && !refresh) {
    if (validate_sample_data_structure(sample_dir)) {
      # Valid existing data, return path without regenerating
      message("Using existing sample data at: ", sample_dir)
      return(sample_dir)
    }
    # Invalid or missing structure, will regenerate below
    if (dir.exists(sample_dir)) {
      message("Invalid sample data structure detected, regenerating...")
    }
  }

  # Remove if it exists (in case of refresh)
  if (dir.exists(sample_dir)) {
    unlink(sample_dir, recursive = TRUE)
  }

  # Create the sample data
  create_sample_chronicle_data_internal(sample_dir)

  # Cache the path
  assign("sample_path", sample_dir, envir = .chronicle_sample_env)

  # Register cleanup on exit (only for default paths)
  if (is.null(base_path)) {
    reg.finalizer(
      .chronicle_sample_env,
      function(e) {
        if (exists("sample_path", envir = e)) {
          path <- get("sample_path", envir = e)
          if (dir.exists(path)) {
            unlink(path, recursive = TRUE)
          }
        }
      },
      onexit = TRUE
    )
  }

  sample_dir
}

#' Validate sample data directory structure
#' @noRd
validate_sample_data_structure <- function(path) {
  if (!dir.exists(path)) {
    return(FALSE)
  }

  # Check for expected curated/v2 structure
  curated_path <- file.path(path, "curated", "v2")
  if (!dir.exists(curated_path)) {
    return(FALSE)
  }

  # Check for at least one expected metric directory
  expected_metrics <- c("connect", "workbench")
  has_metric <- any(sapply(expected_metrics, function(m) {
    dir.exists(file.path(curated_path, m))
  }))

  has_metric
}

# Internal helper functions for creating sample data
# These are not exported but are used by chronicle_sample_data()

#' Generate a date sequence for sample data
#' @noRd
generate_sample_date_sequence <- function(n_days = 30) {
  end_date <- Sys.Date() - 1 # Yesterday
  start_date <- end_date - (n_days - 1)
  seq(from = start_date, to = end_date, by = "day")
}

#' Calculate weekday activity factor
#' @noRd
calculate_weekday_factor <- function(date, weekend_reduction = 0.35) {
  # Returns multiplier: 1.0 for weekdays, ~0.35 for weekends
  day_of_week <- as.POSIXlt(date)$wday # 0 = Sunday, 6 = Saturday
  ifelse(day_of_week %in% c(0, 6), weekend_reduction, 1.0)
}

#' Generate stable user pool
#' @noRd
generate_user_pool <- function(n_users = 26) {
  set.seed(42) # Reproducible users

  # Generate fake names using charlatan
  fake <- charlatan::PersonProvider_en_US$new()
  names_list <- lapply(1:n_users, function(i) {
    list(
      first = fake$first_name(),
      last = fake$last_name()
    )
  })

  # Create usernames from first.last format
  usernames <- tolower(sprintf(
    "%s.%s",
    sapply(names_list, function(x) x$first),
    sapply(names_list, function(x) x$last)
  ))

  # Create emails from username@example.com
  emails <- sprintf("%s@example.com", usernames)

  # Role distribution: 15 viewers, 8 publishers, 3 administrators
  roles <- c(
    rep("viewer", 15),
    rep("publisher", 8),
    rep("administrator", 3)
  )

  # Environment distribution
  envs <- c(
    rep("Production", 10),
    rep("Development", 8),
    rep("Staging", 3),
    rep(NA_character_, 5)
  )

  # Set activity baseline based on role (more realistic)
  # Viewers: 90% active, Publishers: 95% active, Administrators: 40% active
  is_active_baseline <- ifelse(
    roles == "viewer",
    0.90,
    ifelse(roles == "publisher", 0.95, 0.40)
  )

  data.frame(
    user_id = 1:n_users,
    user_guid = sprintf("user-guid-%03d", 1:n_users),
    username = usernames,
    email = emails,
    first_name = sapply(names_list, function(x) x$first),
    last_name = sapply(names_list, function(x) x$last),
    user_role = roles,
    environment = envs,
    created_day = sample(1:5, n_users, replace = TRUE), # Created within first 5 days
    is_active_baseline = is_active_baseline,
    stringsAsFactors = FALSE
  )
}

#' Generate stable content pool
#' @noRd
generate_content_pool <- function(n_content = 150, n_days = 30, user_pool) {
  set.seed(100) # Reproducible content

  # Type distribution: 60% shiny, 25% rmarkdown, 15% quarto
  types <- sample(
    c("shiny", "rmarkdown", "quarto"),
    size = n_content,
    replace = TRUE,
    prob = c(0.60, 0.25, 0.15)
  )

  # Environment distribution: 60% Production, 25% Development, 15% other
  envs <- sample(
    c("Production", "Development", "Staging", NA_character_),
    size = n_content,
    replace = TRUE,
    prob = c(0.60, 0.25, 0.10, 0.05)
  )

  # Staggered creation schedule
  # Day 1: 20 items, Days 2-30: add ~4-5 items per day
  creation_days <- c(
    rep(1, 20), # Start with 20 items
    sample(2:n_days, n_content - 20, replace = TRUE)
  )
  creation_days <- sort(creation_days)[1:n_content]

  # Popularity: 20% get most visits
  popularity_tier <- sample(
    c("high", "medium", "low"),
    n_content,
    replace = TRUE,
    prob = c(0.20, 0.30, 0.50)
  )

  # Long session flag: 10% of Shiny apps have notably longer sessions
  long_session <- rep(FALSE, n_content)
  shiny_indices <- which(types == "shiny")
  if (length(shiny_indices) > 0) {
    n_long <- max(1, round(length(shiny_indices) * 0.10))
    long_session[sample(shiny_indices, n_long)] <- TRUE
  }

  # Generate realistic content names for dashboards/reports/apps
  # Use business/analytics terms instead of job titles
  subjects <- c(
    "Sales",
    "Revenue",
    "Customer",
    "Usage",
    "Performance",
    "Traffic",
    "Engagement",
    "Finance",
    "Budget",
    "Forecast",
    "Inventory",
    "Orders",
    "Marketing",
    "Campaign",
    "Analytics",
    "Metrics",
    "KPI",
    "Regional",
    "Product",
    "Service",
    "Support",
    "Quality",
    "Retention",
    "Growth"
  )

  types_display <- c(
    "Dashboard",
    "Report",
    "Analysis",
    "Tracker",
    "Monitor",
    "Summary",
    "Overview",
    "Explorer",
    "Viewer",
    "Calculator",
    "Tool",
    "Insights"
  )

  modifiers <- c(
    "Daily",
    "Weekly",
    "Monthly",
    "Quarterly",
    "Annual",
    "Real-time",
    "Executive",
    "Operational",
    "Strategic",
    "Detailed",
    "Quick",
    "Interactive"
  )

  # Generate titles by combining terms
  content_titles <- sapply(1:n_content, function(i) {
    set.seed(200 + i) # Reproducible per item
    # Randomly decide on 2-part or 3-part name
    if (runif(1) < 0.6) {
      # 2-part: Subject + Type (60%)
      sprintf("%s %s", sample(subjects, 1), sample(types_display, 1))
    } else {
      # 3-part: Modifier + Subject + Type (40%)
      sprintf(
        "%s %s %s",
        sample(modifiers, 1),
        sample(subjects, 1),
        sample(types_display, 1)
      )
    }
  })

  # Create URL-friendly names from titles
  content_names <- sapply(seq_along(content_titles), function(i) {
    # Clean and format the title (lowercase first, then clean)
    name <- tolower(content_titles[i])
    name <- gsub("[^a-z0-9]+", "-", name)
    name <- gsub("^-|-$", "", name) # Remove leading/trailing hyphens
    name <- gsub("-+", "-", name) # Collapse multiple hyphens
    # Add ID suffix for uniqueness
    sprintf("%s-%03d", name, i)
  })

  # Assign owners from publisher subset
  publishers <- user_pool[user_pool$user_role == "publisher", ]
  owner_guids <- sample(publishers$user_guid, n_content, replace = TRUE)

  # Access type distribution: 60% all (public), 30% logged_in, 10% acl
  set.seed(150)
  access_types <- sample(
    c("all", "logged_in", "acl"),
    size = n_content,
    replace = TRUE,
    prob = c(0.6, 0.3, 0.1)
  )

  data.frame(
    content_id = 1:n_content,
    content_guid = sprintf("content-guid-%03d", 1:n_content),
    name = content_names,
    title = content_titles,
    type = types,
    environment = envs,
    owner_guid = owner_guids,
    creation_day = creation_days,
    popularity_tier = popularity_tier,
    long_session = long_session,
    access_type = access_types,
    stringsAsFactors = FALSE
  )
}

#' Sample active users for a given day
#' @noRd
sample_active_users <- function(
  user_pool,
  day_num,
  weekday_factor,
  base_rate = 0.50
) {
  set.seed(1000 + day_num) # Reproducible per day

  # Adjust rate by weekday factor
  adjusted_rate <- base_rate * weekday_factor

  # Add some randomness (+/- 10%)
  jitter <- runif(1, 0.9, 1.1)
  final_rate <- min(adjusted_rate * jitter, 0.95)

  # Sample active users
  active <- runif(nrow(user_pool)) <
    (user_pool$is_active_baseline * final_rate * 2)
  user_pool$user_guid[active]
}

#' Sample active content for a given day
#' @noRd
sample_active_content <- function(content_pool, day_num, weekday_factor) {
  set.seed(2000 + day_num)

  # Only content created by this day
  available <- content_pool[content_pool$creation_day <= day_num, ]
  if (nrow(available) == 0) {
    return(character(0))
  }

  # Activity rate by popularity tier and weekday
  activity_prob <- ifelse(
    available$popularity_tier == "high",
    0.70,
    ifelse(available$popularity_tier == "medium", 0.40, 0.15)
  )
  activity_prob <- activity_prob * weekday_factor

  # Sample active content
  active <- runif(nrow(available)) < activity_prob
  available$content_guid[active]
}

#' @noRd
sample_connect_user_totals_internal <- function() {
  dates <- generate_sample_date_sequence(30)

  # Calculate daily metrics
  daily_data <- lapply(seq_along(dates), function(i) {
    date <- dates[i]
    weekday_factor <- calculate_weekday_factor(date)

    # Named users: gradual growth from 24 to 26
    named_users <- min(24 + floor(i / 15), 26)

    # Active users (1 day): 40-60% of named, affected by weekday
    base_active_rate <- 0.50
    active_users_1day <- round(
      named_users * base_active_rate * weekday_factor * runif(1, 0.9, 1.1)
    )
    active_users_1day <- max(1, min(active_users_1day, named_users))

    # Active users (30 days): 70-85% of named users (rolling window)
    active_users_30days <- round(named_users * runif(1, 0.70, 0.85))
    active_users_30days <- max(active_users_1day, active_users_30days)

    # Administrators: ~10-15% of named users
    administrators <- max(2, min(4, round(named_users * 0.12)))

    # Publishers: ~20-25% of named users
    publishers <- max(5, min(8, round(named_users * 0.25)))

    # Viewers: remaining users after administrators and publishers
    viewers <- named_users - administrators - publishers

    # Licensed user seats: typically >= named_users (some buffer)
    licensed_user_seats <- named_users + sample(0:5, 1)

    data.frame(
      named_users = as.integer(named_users),
      active_users_30days = as.integer(active_users_30days),
      active_users_1day = as.integer(active_users_1day),
      administrators = as.integer(administrators),
      publishers = as.integer(publishers),
      viewers = as.integer(viewers),
      licensed_user_seats = as.integer(licensed_user_seats),
      date = date,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, daily_data)
}

#' @noRd
sample_connect_user_list_internal <- function() {
  dates <- generate_sample_date_sequence(30)
  last_date <- max(dates)
  first_date <- min(dates)
  user_pool <- generate_user_pool(26)

  # Calculate last active times relative to last_date
  # Some active today, some in past few days, some weeks ago
  set.seed(500)
  days_since_active <- sample(
    c(0, 1, 2, 3, 7, 14, 30),
    nrow(user_pool),
    replace = TRUE,
    prob = c(0.35, 0.20, 0.15, 0.10, 0.10, 0.05, 0.05)
  )

  # Calculate created_at times (users created within first 5 days of the period)
  created_dates <- first_date + (user_pool$created_day - 1)

  # Calculate updated_at times (between created and last_date)
  set.seed(501)
  days_since_update <- sample(0:10, nrow(user_pool), replace = TRUE)
  updated_dates <- pmin(last_date, created_dates + days_since_update)

  data.frame(
    environment = user_pool$environment,
    id = user_pool$user_guid,
    username = user_pool$username,
    email = user_pool$email,
    first_name = user_pool$first_name,
    last_name = user_pool$last_name,
    user_role = user_pool$user_role,
    created_at = as.POSIXct(created_dates, tz = "UTC") +
      sample(3600 * 8:18, nrow(user_pool), replace = TRUE),
    updated_at = as.POSIXct(updated_dates, tz = "UTC") +
      sample(3600 * 8:18, nrow(user_pool), replace = TRUE),
    last_active_at = as.POSIXct(
      last_date - days_since_active,
      tz = "UTC"
    ) +
      sample(3600 * 8:18, nrow(user_pool), replace = TRUE), # Random time 8am-6pm
    active_today = days_since_active == 0,
    date = last_date,
    stringsAsFactors = FALSE
  )
}

#' @noRd
sample_connect_content_list_internal <- function() {
  dates <- generate_sample_date_sequence(30)
  last_date <- max(dates)
  first_date <- min(dates)
  user_pool <- generate_user_pool(26)
  content_pool <- generate_content_pool(150, 30, user_pool)

  # Convert creation_day to actual date
  created_dates <- first_date + (content_pool$creation_day - 1)

  # Last deployed is between creation and last_date
  set.seed(600)
  days_since_deploy <- sample(0:10, nrow(content_pool), replace = TRUE)
  last_deployed_dates <- pmin(last_date, created_dates + days_since_deploy)

  data.frame(
    environment = content_pool$environment,
    id = content_pool$content_guid,
    name = content_pool$name,
    title = content_pool$title,
    created_time = as.POSIXct(created_dates, tz = "UTC") +
      sample(3600 * 8:18, nrow(content_pool), replace = TRUE),
    last_deployed_time = as.POSIXct(last_deployed_dates, tz = "UTC") +
      sample(3600 * 8:18, nrow(content_pool), replace = TRUE),
    type = content_pool$type,
    description = paste("Sample", content_pool$type, "content"),
    access_type = content_pool$access_type,
    locked = sample(
      c(FALSE, TRUE),
      nrow(content_pool),
      replace = TRUE,
      prob = c(0.95, 0.05)
    ),
    locked_message = ifelse(
      sample(
        c(FALSE, TRUE),
        nrow(content_pool),
        replace = TRUE,
        prob = c(0.95, 0.05)
      ),
      "",
      "Locked"
    ),
    connection_timeout = as.integer(60),
    read_timeout = as.integer(3600),
    init_timeout = as.integer(300),
    idle_timeout = as.integer(900),
    max_processes = as.integer(sample(1:5, nrow(content_pool), replace = TRUE)),
    min_processes = as.integer(1),
    max_conns_per_process = as.integer(20),
    load_factor = 1.0,
    cpu_request = 0.5,
    cpu_limit = 1.0,
    memory_request = 512,
    memory_limit = 1024,
    amd_gpu_limit = as.integer(0),
    nvidia_gpu_limit = as.integer(0),
    bundle_id = sprintf("bundle-%03d", content_pool$content_id),
    content_category = sample(
      c("dashboard", "report", "application", "plot", "site"),
      nrow(content_pool),
      replace = TRUE
    ),
    parameterized = sample(
      c(TRUE, FALSE),
      nrow(content_pool),
      replace = TRUE,
      prob = c(0.3, 0.7)
    ),
    cluster_name = NA_character_,
    image_name = NA_character_,
    default_image_name = NA_character_,
    default_r_environment_management = TRUE,
    default_py_environment_management = content_pool$type == "quarto",
    service_account_name = NA_character_,
    r_version = ifelse(
      content_pool$type %in% c("shiny", "rmarkdown"),
      "4.3.0",
      NA_character_
    ),
    r_environment_management = content_pool$type %in% c("shiny", "rmarkdown"),
    py_version = ifelse(content_pool$type == "quarto", "3.11", NA_character_),
    py_environment_management = content_pool$type == "quarto",
    quarto_version = ifelse(
      content_pool$type == "quarto",
      "1.4.0",
      NA_character_
    ),
    run_as = "rstudio-connect",
    run_as_current_user = FALSE,
    owner_guid = content_pool$owner_guid,
    content_url = sprintf("/content/%d", content_pool$content_id),
    dashboard_url = sprintf("/dashboard/%d", content_pool$content_id),
    app_role = "viewer",
    vanity_url = ifelse(
      runif(nrow(content_pool)) < 0.1,
      sprintf("/app-%d", content_pool$content_id),
      NA_character_
    ),
    tags = I(lapply(seq_len(nrow(content_pool)), function(i) {
      c(content_pool$type[i], content_pool$environment[i])
    })),
    extension = FALSE,
    date = last_date,
    stringsAsFactors = FALSE
  )
}

#' @noRd
sample_connect_content_totals_internal <- function() {
  dates <- generate_sample_date_sequence(30)
  user_pool <- generate_user_pool(26)
  content_pool <- generate_content_pool(150, 30, user_pool)

  # Generate daily totals for each type/environment combo
  daily_totals <- lapply(seq_along(dates), function(i) {
    # Only count content created by this day
    available <- content_pool[content_pool$creation_day <= i, ]

    # Aggregate by type and environment
    if (nrow(available) == 0) {
      return(NULL)
    }

    agg <- aggregate(
      content_id ~ type + environment,
      data = available,
      FUN = length
    )
    names(agg)[3] <- "count"
    agg$date <- dates[i]

    agg[, c("count", "type", "environment", "date")]
  })

  do.call(rbind, Filter(Negate(is.null), daily_totals))
}

#' @noRd
sample_connect_visits_by_user_internal <- function() {
  dates <- generate_sample_date_sequence(30)
  user_pool <- generate_user_pool(26)
  content_pool <- generate_content_pool(150, 30, user_pool)

  # Generate visits for each day
  daily_visits <- lapply(seq_along(dates), function(i) {
    set.seed(5000 + i)
    date <- dates[i]
    weekday_factor <- calculate_weekday_factor(date)

    # Get active users and content for this day
    active_user_guids <- sample_active_users(
      user_pool,
      i,
      weekday_factor,
      base_rate = 0.45
    )
    active_content_guids <- sample_active_content(
      content_pool,
      i,
      weekday_factor
    )

    if (length(active_user_guids) == 0 || length(active_content_guids) == 0) {
      return(NULL)
    }

    # Create visit combinations (not all users visit all content)
    # Each user visits 1-5 content items
    visits_list <- lapply(active_user_guids, function(ug) {
      n_visits <- sample(1:5, 1)
      visited_content <- sample(
        active_content_guids,
        min(n_visits, length(active_content_guids))
      )

      data.frame(
        date = date,
        user_guid = ug,
        content_guid = visited_content,
        visits = sample(1:10, length(visited_content), replace = TRUE), # 1-10 visits per content
        stringsAsFactors = FALSE
      )
    })

    authenticated_visits <- do.call(rbind, visits_list)

    # Generate anonymous visits for public content
    # Filter for public content that has been created by this day
    public_content <- content_pool[
      content_pool$access_type == "all" &
        content_pool$creation_day <= i,
    ]

    anonymous_visits <- NULL
    if (nrow(public_content) > 0) {
      # 30-40% of public content gets anonymous visits
      set.seed(5500 + i)
      n_anon_content <- max(1, round(nrow(public_content) * runif(1, 0.3, 0.4)))

      # Favor high popularity content for anonymous visits
      anon_probs <- ifelse(
        public_content$popularity_tier == "high",
        0.5,
        ifelse(public_content$popularity_tier == "medium", 0.3, 0.2)
      )
      anon_probs <- anon_probs / sum(anon_probs)

      anon_content_guids <- sample(
        public_content$content_guid,
        size = min(n_anon_content, nrow(public_content)),
        prob = anon_probs
      )

      # Generate visits for anonymous users (lower volume than authenticated)
      anonymous_visits <- data.frame(
        date = date,
        user_guid = NA_character_,
        content_guid = anon_content_guids,
        visits = sample(1:8, length(anon_content_guids), replace = TRUE),
        stringsAsFactors = FALSE
      )
    }

    # Combine authenticated and anonymous visits
    if (!is.null(anonymous_visits)) {
      rbind(authenticated_visits, anonymous_visits)
    } else {
      authenticated_visits
    }
  })

  result <- do.call(rbind, Filter(Negate(is.null), daily_visits))

  # Add environment and path from content_pool
  result$environment <- content_pool$environment[match(
    result$content_guid,
    content_pool$content_guid
  )]
  set.seed(800)
  result$path <- sample(
    c("/", "/detail", "/api", NA_character_),
    nrow(result),
    replace = TRUE,
    prob = c(0.6, 0.2, 0.15, 0.05)
  )

  result[, c(
    "environment",
    "content_guid",
    "user_guid",
    "visits",
    "path",
    "date"
  )]
}

#' @noRd
sample_connect_shiny_by_user_internal <- function() {
  dates <- generate_sample_date_sequence(30)
  user_pool <- generate_user_pool(26)
  content_pool <- generate_content_pool(150, 30, user_pool)

  # Filter to only Shiny content
  shiny_content <- content_pool[content_pool$type == "shiny", ]

  # Generate usage for each day
  daily_usage <- lapply(seq_along(dates), function(i) {
    set.seed(6000 + i)
    date <- dates[i]
    weekday_factor <- calculate_weekday_factor(date)

    # Get active users
    active_user_guids <- sample_active_users(
      user_pool,
      i,
      weekday_factor,
      base_rate = 0.30
    ) # Lower rate

    # Get active Shiny content
    available_shiny <- shiny_content[shiny_content$creation_day <= i, ]
    if (nrow(available_shiny) == 0 || length(active_user_guids) == 0) {
      return(NULL)
    }

    set.seed(6100 + i)
    activity_prob <- ifelse(
      available_shiny$popularity_tier == "high",
      0.60,
      ifelse(available_shiny$popularity_tier == "medium", 0.30, 0.10)
    )
    activity_prob <- activity_prob * weekday_factor
    active_shiny <- available_shiny$content_guid[
      runif(nrow(available_shiny)) < activity_prob
    ]

    if (length(active_shiny) == 0) {
      return(NULL)
    }

    # Each user interacts with 1-3 Shiny apps
    usage_list <- lapply(active_user_guids, function(ug) {
      n_apps <- sample(1:3, 1)
      used_apps <- sample(active_shiny, min(n_apps, length(active_shiny)))

      # Base duration: 5-90 minutes
      base_durations <- sample(300:5400, length(used_apps), replace = TRUE)

      # Apply 50x multiplier for long_session apps
      long_session_flags <- available_shiny$long_session[match(
        used_apps,
        available_shiny$content_guid
      )]
      durations <- ifelse(
        long_session_flags,
        base_durations * 50,
        base_durations
      )

      data.frame(
        date = date,
        user_guid = ug,
        content_guid = used_apps,
        num_sessions = sample(1:5, length(used_apps), replace = TRUE),
        duration = as.integer(durations),
        stringsAsFactors = FALSE
      )
    })

    authenticated_usage <- do.call(rbind, usage_list)

    # Generate anonymous sessions for public Shiny apps
    public_shiny <- available_shiny[available_shiny$access_type == "all", ]

    anonymous_usage <- NULL
    if (nrow(public_shiny) > 0) {
      # 20-30% of public Shiny apps get anonymous sessions
      set.seed(6500 + i)
      n_anon_apps <- max(1, round(nrow(public_shiny) * runif(1, 0.2, 0.3)))

      # Favor high popularity apps for anonymous usage
      anon_probs <- ifelse(
        public_shiny$popularity_tier == "high",
        0.5,
        ifelse(public_shiny$popularity_tier == "medium", 0.3, 0.2)
      )
      anon_probs <- anon_probs / sum(anon_probs)

      anon_app_guids <- sample(
        public_shiny$content_guid,
        size = min(n_anon_apps, nrow(public_shiny)),
        prob = anon_probs
      )

      # Anonymous sessions: shorter duration (5-30 min) and fewer sessions
      anonymous_usage <- data.frame(
        date = date,
        user_guid = NA_character_,
        content_guid = anon_app_guids,
        num_sessions = sample(1:2, length(anon_app_guids), replace = TRUE),
        duration = as.integer(sample(
          300:1800,
          length(anon_app_guids),
          replace = TRUE
        )),
        stringsAsFactors = FALSE
      )
    }

    # Combine authenticated and anonymous usage
    if (!is.null(anonymous_usage)) {
      rbind(authenticated_usage, anonymous_usage)
    } else {
      authenticated_usage
    }
  })

  result <- do.call(rbind, Filter(Negate(is.null), daily_usage))

  # Add environment from content_pool
  result$environment <- shiny_content$environment[match(
    result$content_guid,
    shiny_content$content_guid
  )]

  result[, c(
    "environment",
    "content_guid",
    "user_guid",
    "num_sessions",
    "duration",
    "date"
  )]
}

#' @noRd
sample_workbench_user_totals_internal <- function() {
  dates <- generate_sample_date_sequence(30)

  daily_data <- lapply(seq_along(dates), function(i) {
    date <- dates[i]
    weekday_factor <- calculate_weekday_factor(date)

    # Named users: gradual growth from 19 to 21
    named_users <- min(19 + floor(i / 15), 21)

    # Active users (1 day): 35-45% of named, affected by weekday
    base_active_rate <- 0.40
    active_users_1day <- round(
      named_users * base_active_rate * weekday_factor * runif(1, 0.9, 1.1)
    )
    active_users_1day <- max(1, min(active_users_1day, named_users))

    # Active users (30 days): 70-85% of named users (rolling window)
    active_users_30days <- round(named_users * runif(1, 0.70, 0.85))
    active_users_30days <- max(active_users_1day, active_users_30days)

    # Administrators: ~20% of named
    administrators <- max(3, min(5, round(named_users * 0.20)))

    # Super admins: always 1
    super_administrators <- 1L

    # Users: remaining users after administrators and super_administrators
    users <- named_users - administrators - super_administrators

    # Licensed user seats: typically >= named_users (some buffer)
    licensed_user_seats <- named_users + sample(0:5, 1)

    data.frame(
      named_users = as.integer(named_users),
      active_users_30days = as.integer(active_users_30days),
      active_users_1day = as.integer(active_users_1day),
      administrators = as.integer(administrators),
      super_administrators = as.integer(super_administrators),
      users = as.integer(users),
      licensed_user_seats = as.integer(licensed_user_seats),
      date = date,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, daily_data)
}

#' @noRd
sample_workbench_user_list_internal <- function() {
  dates <- generate_sample_date_sequence(30)
  last_date <- max(dates)
  first_date <- min(dates)
  n_users <- 21

  set.seed(50)

  # Generate fake names using charlatan for usernames
  fake <- charlatan::PersonProvider_en_US$new()
  names_list <- lapply(1:n_users, function(i) {
    list(
      first = fake$first_name(),
      last = fake$last_name()
    )
  })

  # Create usernames from first.last format
  usernames <- tolower(sprintf(
    "%s.%s",
    sapply(names_list, function(x) x$first),
    sapply(names_list, function(x) x$last)
  ))

  # Create emails from username@example.com
  emails <- sprintf("%s@example.com", usernames)

  # Workbench role distribution
  roles <- c(
    rep("user", 16),
    rep("admin", 4),
    "super_admin"
  )

  envs <- c(
    rep("Production", 8),
    rep("Development", 6),
    rep("Staging", 4),
    rep(NA_character_, 3)
  )

  days_since_active <- sample(
    c(0, 1, 2, 3, 7, 14),
    n_users,
    replace = TRUE,
    prob = c(0.30, 0.20, 0.15, 0.15, 0.15, 0.05)
  )

  # Calculate created_at times (users created within first 5 days of the period)
  created_days <- sample(1:5, n_users, replace = TRUE)
  created_dates <- first_date + (created_days - 1)

  data.frame(
    environment = envs,
    id = sprintf("wb-user-guid-%03d", 1:n_users),
    username = usernames,
    email = emails,
    user_role = roles,
    created_at = as.POSIXct(created_dates, tz = "UTC") +
      sample(3600 * 8:18, n_users, replace = TRUE),
    last_active_at = as.POSIXct(
      last_date - days_since_active,
      tz = "UTC"
    ) +
      sample(3600 * 8:18, n_users, replace = TRUE),
    active_today = days_since_active == 0,
    date = last_date,
    stringsAsFactors = FALSE
  )
}

#' @noRd
sample_raw_connect_users_internal <- function() {
  dates <- generate_sample_date_sequence(30)
  user_pool <- generate_user_pool(26)

  # Create daily snapshots for subset of users (15 users)
  daily_snapshots <- lapply(seq_along(dates), function(i) {
    set.seed(7000 + i)
    date <- dates[i]

    # Sample 15 users for this snapshot
    sampled_users <- user_pool[sample(nrow(user_pool), 15), ]

    data.frame(
      date = date,
      timestamp = as.POSIXct(paste(date, "08:00:00"), tz = "UTC"),
      email = sampled_users$email,
      created_at = as.POSIXct("2023-06-01 10:00:00", tz = "UTC"),
      last_active_at = as.POSIXct(paste(date, "07:00:00"), tz = "UTC") -
        sample(0:7, 15, replace = TRUE) * 86400,
      locked = sample(c(FALSE, TRUE), 15, replace = TRUE, prob = c(0.95, 0.05)),
      user_role = sampled_users$user_role,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, daily_snapshots)
}

#' @noRd
write_sample_parquet_internal <- function(
  data,
  base_path,
  metric,
  partition_col = "date"
) {
  metric_path <- file.path(base_path, "curated", "v2", metric)

  arrow::write_dataset(
    data,
    path = metric_path,
    format = "parquet",
    partitioning = partition_col,
    hive_style = TRUE
  )
}

#' @noRd
write_raw_parquet_internal <- function(
  data,
  base_path,
  metric,
  frequency = "daily"
) {
  dates <- unique(data$date)

  for (d in dates) {
    date_obj <- as.Date(d, origin = "1970-01-01")
    year <- format(date_obj, "%Y")
    month <- format(date_obj, "%m")
    day <- format(date_obj, "%d")

    subset_data <- data[data$date == d, ]

    metric_path <- file.path(
      base_path,
      frequency,
      "v2",
      metric,
      year,
      month,
      day
    )

    dir.create(metric_path, recursive = TRUE, showWarnings = FALSE)

    arrow::write_parquet(
      subset_data,
      file.path(metric_path, "data.parquet")
    )
  }
}

#' @noRd
create_sample_chronicle_data_internal <- function(base_path) {
  dir.create(base_path, recursive = TRUE, showWarnings = FALSE)

  # Create curated data
  write_sample_parquet_internal(
    sample_connect_user_totals_internal(),
    base_path,
    "connect/user_totals"
  )

  write_sample_parquet_internal(
    sample_connect_user_list_internal(),
    base_path,
    "connect/user_list"
  )

  write_sample_parquet_internal(
    sample_connect_content_totals_internal(),
    base_path,
    "connect/content_totals"
  )

  write_sample_parquet_internal(
    sample_connect_content_list_internal(),
    base_path,
    "connect/content_list"
  )

  write_sample_parquet_internal(
    sample_connect_visits_by_user_internal(),
    base_path,
    "connect/content_visits_totals_by_user"
  )

  write_sample_parquet_internal(
    sample_connect_shiny_by_user_internal(),
    base_path,
    "connect/shiny_usage_totals_by_user"
  )

  write_sample_parquet_internal(
    sample_workbench_user_totals_internal(),
    base_path,
    "workbench/user_totals"
  )

  write_sample_parquet_internal(
    sample_workbench_user_list_internal(),
    base_path,
    "workbench/user_list"
  )

  base_path
}
