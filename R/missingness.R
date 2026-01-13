#' @importFrom dplyr mutate transmute across select count arrange slice_head distinct
#' @importFrom ggplot2 ggplot aes geom_tile scale_y_reverse scale_fill_manual labs
#'   theme_minimal theme geom_text coord_cartesian element_text element_blank margin
#' @importFrom stringr str_squish
#' @importFrom utils adist
NULL

utils::globalVariables(".data")

#' Plot missingness patterns for TeamTV shot data
#'
#' Pattern-based missingness heatmap (like md.pattern/VIM-style): complete patterns
#' on top, then patterns sorted by completeness. Adds percent observed above columns.
#'
#' Rules:
#' - Strings like "onbekend" are treated as missing (converted to NA).
#' - `leg` is only relevant for type in c("SHORT","LONG","FREEBALL").
#'   For other shot types, leg is treated as N/A (not missing).
#'
#' @param df A data.frame containing TeamTV export columns.
#' @param person Optional string. If provided, filters to the closest matching
#'   `full_name` (fuzzy match). Use `NULL` for all players.
#' @param max_patterns Maximum number of patterns to display (most frequent kept).
#' @return A ggplot object.
#' @export
tagr_plot_missingness <- function(df, person = NULL, max_patterns = 30) {

  # load required packages if available and stop with a clear message if not installed
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Install dplyr", call. = FALSE)
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Install ggplot2", call. = FALSE)
  if (!requireNamespace("stringr", quietly = TRUE)) stop("Install stringr", call. = FALSE)

  # validate the overall TeamTV shots schema and coded values before proceeding
  validate_teamtv_shots(df)

  # define the required column set used for cleaning, rule handling, and plotting
  vars <- c(
    "full_name", "number", "leg", "type",
    "result", "distance", "pressure", "x", "y",
    "opponent_full_name", "opponent_number", "shot_count"
  )

  # verify that all required columns are present before proceeding
  missing_cols <- setdiff(vars, names(df))
  if (length(missing_cols)) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  # define a helper that standardizes common unknown tokens to missing values for character vectors
  to_na_unknown <- function(x) {
    if (!is.character(x)) return(x)
    x_trim <- stringr::str_squish(x)
    bad <- tolower(x_trim) %in% c("onbekend", "unknown", "unk", "n/a", "na", "")
    x_trim[bad] <- NA_character_
    x_trim
  }

  # optionally restrict the dataset to a single person using approximate string matching on full_name
  if (!is.null(person)) {

    # extract unique candidate names after standardizing unknown tokens to missing values
    nm <- unique(to_na_unknown(df$full_name))
    nm <- nm[!is.na(nm)]
    if (!length(nm)) stop("No non-missing full_name values in df.", call. = FALSE)

    # compute edit distances between the target string and each candidate name
    target <- stringr::str_squish(person)
    dists <- adist(tolower(target), tolower(stringr::str_squish(nm)))

    # select the closest match and retain its distance for a sanity check
    best_i <- which.min(dists)
    best_name <- nm[best_i]
    best_dist <- dists[best_i]

    # prevent selecting a match that is likely unrelated by requiring a small edit distance
    if (best_dist > max(3, nchar(target) * 0.4)) {
      stop("No close match for '", person, "'. Closest was '", best_name, "'.", call. = FALSE)
    }

    # filter to rows matching the selected name after applying the same unknown standardization
    df <- df[to_na_unknown(df$full_name) == best_name, , drop = FALSE]
    if (!nrow(df)) stop("Matched name but found 0 rows (unexpected).", call. = FALSE)

    # report which name was selected for reproducibility of results
    message("Using player: ", best_name)
  }

  # define shot types for which the leg variable is considered required
  leg_relevant_types <- c("SHORT", "LONG", "FREEBALL")

  # clean relevant character columns, implement the leg rule, and build a working table for later missingness extraction
  work <- df |>
    dplyr::mutate(

      # standardize unknown tokens to missing values for the selected character columns
      dplyr::across(
        dplyr::all_of(c("full_name","number","leg","type","result","pressure",
                        "opponent_full_name","opponent_number")),
        to_na_unknown
      ),

      # indicate whether the leg variable is required for the given shot type
      leg_required = .data$type %in% leg_relevant_types,

      # compute rule based missingness for leg only when leg is required
      leg_missing  = is.na(.data$leg) & .data$leg_required,

      # create a display version of leg that is set to a constant label when not required
      leg_display  = dplyr::if_else(.data$leg_required, .data$leg, "N/A")
    ) |>
    dplyr::transmute(

      # retain the plotting variables in a consistent order and include the rule based leg_missing indicator
      full_name = .data$full_name,
      number = .data$number,
      leg = .data$leg_display,
      type = .data$type,
      result = .data$result,
      distance = .data$distance,
      pressure = .data$pressure,
      x = .data$x,
      y = .data$y,
      opponent_full_name = .data$opponent_full_name,
      opponent_number = .data$opponent_number,
      shot_count = .data$shot_count,
      leg_missing = .data$leg_missing
    )

  # create a logical missingness matrix where each cell indicates whether the corresponding value is missing
  miss_df <- work |>
    dplyr::mutate(dplyr::across(dplyr::all_of(vars), ~ is.na(.x))) |>
    dplyr::mutate(

      # replace the leg missingness indicator with the rule based missingness definition
      leg = .data$leg_missing
    ) |>
    dplyr::select(dplyr::all_of(vars))

  # compute the percentage of observed values per variable based on the missingness matrix
  pct_observed <- vapply(miss_df, function(m) mean(!m) * 100, numeric(1))

  # encode each row of the missingness matrix into a compact pattern string for counting distinct patterns
  pattern_key <- apply(miss_df, 1, function(r) paste(as.integer(r), collapse = ""))

  # count patterns, compute the number of observed fields per pattern, and keep the top patterns for plotting
  pat_tbl <- dplyr::tibble(key = pattern_key) |>
    dplyr::count(.data$key, name = "n") |>
    dplyr::mutate(

      # compute completeness as the number of zeroes in the pattern string
      observed = vapply(strsplit(.data$key, ""), function(bits) sum(bits == "0"), integer(1))
    ) |>
    dplyr::arrange(dplyr::desc(.data$observed), dplyr::desc(.data$n)) |>
    dplyr::slice_head(n = max_patterns) |>
    dplyr::mutate(pattern_id = dplyr::row_number())

  # define a decoder that converts a pattern string into an integer vector of missingness bits
  decode_pattern <- function(key) as.integer(strsplit(key, "")[[1]])

  # expand the pattern table into long format so each pattern becomes a row per variable for tile plotting
  tile_long <- dplyr::bind_rows(lapply(seq_len(nrow(pat_tbl)), function(i) {
    bits <- decode_pattern(pat_tbl$key[i])
    dplyr::tibble(
      pattern_id = pat_tbl$pattern_id[i],
      var = vars,
      missing = bits == 1L,
      n = pat_tbl$n[i],
      observed = pat_tbl$observed[i]
    )
  }))

  # define human readable labels for variables used in the x axis
  pretty_labels <- c(
    full_name = "Name",
    number = "Number",
    leg = "Leg",
    type = "Shot type",
    result = "Result",
    distance = "Distance",
    pressure = "Pressure",
    x = "X",
    y = "Y",
    opponent_full_name = "Opponent name",
    opponent_number = "Opponent #",
    shot_count = "Shot #"
  )

  # apply variable ordering and labels to the long table for consistent plotting
  tile_long$var <- factor(tile_long$var, levels = vars, labels = unname(pretty_labels[vars]))

  # construct a small table used to print percent observed above each variable column
  top_lab <- dplyr::tibble(
    var = factor(vars, levels = vars, labels = unname(pretty_labels[vars])),
    pct = pct_observed[vars]
  )

  # build a tile plot showing missingness across the most common patterns and annotate counts and column completeness
  ggplot2::ggplot(tile_long, ggplot2::aes(x = .data$var, y = .data$pattern_id, fill = .data$missing)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.3) +
    ggplot2::scale_y_reverse() +
    ggplot2::scale_fill_manual(
      values = c(`TRUE` = "tomato", `FALSE` = "seagreen3"),
      labels = c(`TRUE` = "Missing", `FALSE` = "Observed"),
      name = NULL
    ) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      title = "Missingness patterns",
      subtitle = "Most complete patterns at the top. Percent observed shown above each column."
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 35, hjust = 1, vjust = 1),
      panel.grid = ggplot2::element_blank(),
      legend.position = "top"
    ) +
    ggplot2::geom_text(
      data = dplyr::distinct(tile_long, .data$pattern_id, .data$n),
      mapping = ggplot2::aes(x = Inf, y = .data$pattern_id, label = paste0("n=", .data$n)),
      inherit.aes = FALSE,
      hjust = 1.1,
      size = 3
    ) +
    ggplot2::geom_text(
      data = top_lab,
      ggplot2::aes(x = .data$var, y = 0, label = paste0(round(.data$pct, 1), "%")),
      inherit.aes = FALSE,
      vjust = 1.2,
      size = 3
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme(plot.margin = ggplot2::margin(10, 40, 10, 10))
}


