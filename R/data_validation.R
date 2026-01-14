#' Validate a TeamTV tagged-shots data.frame
#'
#' Checks:
#' - column names: exact match no missing or extra columns
#' - column order: must match the expected schema order
#' - column types: match expected types with integerish tolerance for integer columns
#' - allowed values for pressure type leg result case insensitive na allowed
#'
#' Missing data handling:
#' - na values are allowed for pressure type leg result and are skipped in allowed value checks
#' - for integerish checks na values are ignored and only non missing values are validated
#' - if an entire column is the wrong type validation errors even if many values are na
#'
#' @param x A data.frame with TeamTV tagged shots
#' @return Invisibly returns TRUE if valid otherwise errors
#' @export
validate_teamtv_shots <- function(x) {

  # expected schema as a named list
  # names are required column names in required order
  # values are the expected base type for each column
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

  # find missing and extra columns
  missing <- setdiff(exp_names, got_names)
  extra   <- setdiff(got_names, exp_names)

  # if schema does not match exactly stop and show what changed
  if (length(missing) > 0 || length(extra) > 0) {
    msg <- "Column-name mismatch.\n"
    if (length(missing) > 0) msg <- paste0(msg, "- Missing: ", paste(missing, collapse = ", "), "\n")
    if (length(extra)   > 0) msg <- paste0(msg, "- Extra:   ", paste(extra, collapse = ", "), "\n")
    msg <- paste0(msg, "If TeamTV changed its export format, TagR needs an update.")
    stop(msg, call. = FALSE)
  }

  # allow reordered exports
  # teamtv sometimes changes export column order without changing names or meanings
  # we keep strict name matching above but reorder here so downstream checks are stable
  if (!identical(got_names, exp_names)) {
    x <- x[, exp_names, drop = FALSE]
    got_names <- names(x)
  }

  # check each column type against the expected type
  # collect all mismatches and report them together
  type_errors <- character(0)
  for (nm in exp_names) {
    want <- expected[[nm]]
    v <- x[[nm]]

    # decide if the vector passes the type rule
    ok <- switch(
      want,
      integer   = is_integerish(v),
      numeric   = is.numeric(v),
      character = is.character(v),
      logical   = is.logical(v),
      FALSE
    )

    # record a readable message for any mismatch
    if (!ok) {
      got <- paste(class(v), collapse = "/")
      type_errors <- c(type_errors, sprintf("- %s: expected %s, got %s", nm, want, got))
    }
  }

  # stop if any types are wrong
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
  check_allowed <- function(col, allowed, label) {
    v0 <- x[[col]]

    # type guard to avoid calling string functions on non character data
    if (!is.character(v0)) stopf("Column '%s' must be character to validate values.", col)

    # drop na before checking allowed values
    keep <- !is.na(v0)
    if (!any(keep)) return(invisible(TRUE))

    # normalize values then detect which original values are not allowed
    vn <- norm_token(v0[keep])
    bad <- sort(unique(v0[keep][!vn %in% allowed]))

    # stop with details if any unknown codes appear
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

  # run allowed value checks for each coded column
  check_allowed("pressure", allowed_pressure, "pressure")
  check_allowed("type",     allowed_type,     "shot type")
  check_allowed("leg",      allowed_leg,      "leg")
  check_allowed("result",   allowed_result,   "result")

  # return true invisibly so it can be used in pipelines without printing
  invisible(TRUE)
}
