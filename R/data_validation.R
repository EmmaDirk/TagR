#' Validate a TeamTV tagged-shots data.frame
#'
#' Checks:
#' - column names: required columns must exist, other known columns are optional
#' - column order: not enforced because TeamTV may reorder or omit absent columns
#' - column types: match expected types with integerish tolerance for integer columns
#' - allowed values for pressure type leg result case insensitive na allowed
#'
#' Missing data handling:
#' - some columns may be completely absent in a TeamTV export (TeamTV may only export observed fields)
#' - required columns must exist or validation errors
#' - important optional columns trigger warnings because some functions may not work
#' - other missing known columns trigger a warning but are usually safe
#' - na values are allowed for pressure type leg result and are skipped in allowed value checks
#' - for integerish checks na values are ignored and only non missing values are validated
#' - if a present column is the wrong type validation errors even if many values are na
#'
#' @param x A data.frame with TeamTV tagged shots
#' @return Invisibly returns TRUE if valid otherwise errors
#' @export
validate_teamtv_shots <- function(x) {

  # expected schema as a named list
  # names are required known column names
  # values are the expected base type for each column when present
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

  # columns that must exist for core TagR functionality
  # these are the ones you described as crucial
  required_core <- c(
    "X", "result", "x", "y", "distance", "angle", "shot_count"
  )

  # columns that are not strictly required but often needed for some functions
  # missing these should warn because some plots and dashboards may be limited
  important_optional <- c(
    "type",
    "pressure",
    "leg",
    "person_id", "full_name", "number", "first_name", "last_name",
    "team_id", "team_name",
    "opponent_person_id", "opponent_first_name", "opponent_last_name",
    "opponent_number", "opponent_full_name"
  )

  # helper that accepts true integers or numeric values that are effectively integers
  # na values are ignored in this check
  is_integerish <- function(v) {
    if (is.integer(v)) return(TRUE)
    if (!is.numeric(v)) return(FALSE)

    v2 <- v[!is.na(v)]
    if (!length(v2)) return(TRUE)

    all(abs(v2 - round(v2)) < .Machine$double.eps^0.5)
  }

  # normalize tokens so coding checks are stable across case and separators
  # running in running_in running-in all become running-in in uppercase
  norm_token <- function(v) {
    v <- trimws(v)
    v <- toupper(v)
    v <- gsub("[ _]+", "-", v)
    v
  }

  # small helper for formatted errors without call trace noise
  stopf <- function(...) stop(sprintf(...), call. = FALSE)

  # basic input check to ensure we are working with a data.frame
  if (!is.data.frame(x)) {
    stopf("Expected a data.frame, got: %s", paste(class(x), collapse = "/"))
  }

  # expected and observed column names
  exp_names <- names(expected)
  got_names <- names(x)

  # check required core columns first
  missing_core <- setdiff(required_core, got_names)
  if (length(missing_core) > 0) {
    stop(
      "Column-name mismatch.\n",
      "- Missing required columns: ", paste(missing_core, collapse = ", "), "\n",
      "These are required for core TagR functionality.",
      call. = FALSE
    )
  }

  # warn about important optional columns that are missing
  missing_important <- setdiff(important_optional, got_names)
  if (length(missing_important) > 0) {
    warning(
      "Missing important columns: ",
      paste(missing_important, collapse = ", "),
      ". Some TagR functions may be limited.",
      call. = FALSE
    )
  }

  # warn about other known TeamTV columns that are missing
  # these are usually safe but we still notify
  missing_known <- setdiff(setdiff(exp_names, required_core), got_names)
  missing_known <- setdiff(missing_known, missing_important)
  if (length(missing_known) > 0) {
    warning(
      "Missing TeamTV columns: ",
      paste(missing_known, collapse = ", "),
      ". This is usually fine if those fields were not observed/exported.",
      call. = FALSE
    )
  }

  # warn about extra columns that TagR does not know about yet
  extra <- setdiff(got_names, exp_names)
  if (length(extra) > 0) {
    warning(
      "Extra columns not recognized by TagR: ",
      paste(extra, collapse = ", "),
      ". This is usually fine and may indicate a newer TeamTV export.",
      call. = FALSE
    )
  }

  # check each present known column type against the expected type
  # collect all mismatches and report them together
  type_errors <- character(0)

  for (nm in intersect(exp_names, got_names)) {
    want <- expected[[nm]]
    v <- x[[nm]]

    # character fields are often read as factor in older defaults
    # accept factor and treat it as character for validation
    is_char_like <- function(v) is.character(v) || is.factor(v)

    ok <- switch(
      want,
      integer   = is_integerish(v),
      numeric   = is.numeric(v),
      character = is_char_like(v),
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

  # allowed categorical codes in normalized uppercase hyphen form
  allowed_pressure <- c("LOW", "MEDIUM", "HIGH", "NONE", "ONBEKEND")
  allowed_type     <- c("LONG", "RUNNING-IN", "SHORT", "FREE-BALL", "PENALTY", "ONBEKEND")
  allowed_leg      <- c("LEFT", "BOTH", "RIGHT", "ONBEKEND")
  allowed_result   <- c("MISS", "GOAL", "ONBEKEND")

  # check that a given categorical column contains only allowed codes
  # na values are allowed and ignored by this check
  # if the column does not exist in this export, skip the check
  check_allowed <- function(col, allowed, label) {

    if (!col %in% names(x)) return(invisible(TRUE))

    v0 <- x[[col]]

    # type guard to avoid calling string functions on non character-like data
    if (!(is.character(v0) || is.factor(v0))) {
      stopf("Column '%s' must be character to validate values.", col)
    }

    v0 <- as.character(v0)

    keep <- !is.na(v0)
    if (!any(keep)) return(invisible(TRUE))

    vn <- norm_token(v0[keep])
    bad <- sort(unique(v0[keep][!vn %in% allowed]))

    if (length(bad) > 0) {
      stop(
        sprintf(
          "Unknown %s value(s) in column '%s': %s\nAllowed: %s\nThis likely means TeamTV updated its coding.",
          label, col, paste(bad, collapse = ", "), paste(allowed, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    invisible(TRUE)
  }

  # run allowed value checks only for columns that exist
  check_allowed("pressure", allowed_pressure, "pressure")
  check_allowed("type",     allowed_type,     "shot type")
  check_allowed("leg",      allowed_leg,      "leg")
  check_allowed("result",   allowed_result,   "result")

  invisible(TRUE)
}
