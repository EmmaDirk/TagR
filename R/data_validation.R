#' Validate a TeamTV tagged-shots data.frame
#'
#' Checks:
#' - column names: exact match (no missing/extra)
#' - column types: match expected types (with "integer-ish" tolerance)
#' - allowed values for: pressure, type, leg, result (case-insensitive; NA allowed)
#'
#' @param x A data.frame with TeamTV tagged shots
#' @return Invisibly returns TRUE if valid; otherwise errors.
#' @export

validate_teamtv_shots <- function(x) {
  # ---- expected schema ----
  expected <- list(
    X                           = "integer",
    sporting_event_id           = "character",
    sporting_event_name         = "character",
    sporting_event_scheduled_at = "character",
    observation_id              = "character",
    clock_id                    = "character",
    start_time                  = "integer",
    end_time                    = "integer",
    code                        = "character",
    description                 = "character",
    possession_id               = "character",
    team_id                     = "character",
    team_name                   = "character",
    team_ground                 = "character",
    position                    = "character",
    team_name_full              = "character",
    team_key                    = "character",
    person_id                   = "character",
    first_name                  = "character",
    last_name                   = "character",
    number                      = "character",
    full_name                   = "character",
    leg                         = "character",
    type                        = "character",
    angle                       = "numeric",
    result                      = "character",
    distance                    = "numeric",
    pressure                    = "character",
    x                           = "numeric",
    y                           = "numeric",
    participantsPersonIds       = "logical",
    opponent_person_id          = "character",
    opponent_first_name         = "character",
    opponent_last_name          = "character",
    opponent_number             = "character",
    opponent_full_name          = "character",
    shot_count                  = "integer"
  )

  # ---- helpers ----
  is_integerish <- function(v) {
    if (is.integer(v)) return(TRUE)
    if (!is.numeric(v)) return(FALSE)
    v2 <- v[!is.na(v)]
    all(abs(v2 - round(v2)) < .Machine$double.eps^0.5)
  }

  norm_token <- function(v) {
    # case-insensitive + normalize separators to hyphen
    # "running in" / "RUNNING_IN" / "RUNNING-IN" -> "RUNNING-IN"
    v <- trimws(v)
    v <- toupper(v)
    v <- gsub("[ _]+", "-", v)
    v
  }

  stopf <- function(...) stop(sprintf(...), call. = FALSE)

  # ---- basic checks ----
  if (!is.data.frame(x)) stopf("Expected a data.frame, got: %s", paste(class(x), collapse = "/"))

  exp_names <- names(expected)
  got_names <- names(x)

  missing <- setdiff(exp_names, got_names)
  extra   <- setdiff(got_names, exp_names)

  if (length(missing) > 0 || length(extra) > 0) {
    msg <- "Column-name mismatch.\n"
    if (length(missing) > 0) msg <- paste0(msg, "- Missing: ", paste(missing, collapse = ", "), "\n")
    if (length(extra)   > 0) msg <- paste0(msg, "- Extra:   ", paste(extra, collapse = ", "), "\n")
    msg <- paste0(msg, "If TeamTV changed its export format, TagR needs an update.")
    stop(msg, call. = FALSE)
  }

  # Enforce order too (optional but useful to catch subtle changes)
  if (!identical(got_names, exp_names)) {
    stop(
      "Column order differs from the expected TeamTV schema.\n",
      "This often indicates an export-format change. Reorder columns or update TagR.",
      call. = FALSE
    )
  }

  # ---- type checks ----
  type_errors <- character(0)
  for (nm in exp_names) {
    want <- expected[[nm]]
    v <- x[[nm]]

    ok <- switch(
      want,
      integer   = is_integerish(v),
      numeric   = is.numeric(v),
      character = is.character(v),
      logical   = is.logical(v),
      FALSE
    )

    if (!ok) {
      got <- paste(class(v), collapse = "/")
      type_errors <- c(type_errors, sprintf("- %s: expected %s, got %s", nm, want, got))
    }
  }

  if (length(type_errors) > 0) {
    stop(
      "Type mismatch in TeamTV shots data:\n",
      paste(type_errors, collapse = "\n"),
      "\nTip: if integers were read as numeric, convert with as.integer() after checking.",
      call. = FALSE
    )
  }

  # ---- value checks (case-insensitive; NA allowed) ----
  allowed_pressure <- c("LOW", "MEDIUM", "HIGH", "NONE", "ONBEKEND")
  allowed_type     <- c("LONG", "RUNNING-IN", "SHORT", "FREE-BALL", "PENALTY", "ONBEKEND")
  allowed_leg      <- c("LEFT", "BOTH", "RIGHT", "ONBEKEND")
  allowed_result   <- c("MISS", "GOAL", "ONBEKEND")

  check_allowed <- function(col, allowed, label) {
    v <- x[[col]]
    v0 <- v
    v <- v[!is.na(v)]
    if (!is.character(v)) stopf("Column '%s' must be character to validate values.", col)

    vn <- norm_token(v)
    bad <- sort(unique(v0[!is.na(v0)][!vn %in% allowed]))
    if (length(bad) > 0) {
      stop(
        sprintf(
          "Unknown %s value(s) in column '%s': %s\nAllowed: %s\nThis likely means TeamTV updated its coding.",
          label, col, paste(bad, collapse = ", "), paste(allowed, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  check_allowed("pressure", allowed_pressure, "pressure")
  check_allowed("type",     allowed_type,     "shot type")
  check_allowed("leg",      allowed_leg,      "leg")
  check_allowed("result",   allowed_result,   "result")

  invisible(TRUE)
}

