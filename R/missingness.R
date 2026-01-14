#' @importFrom dplyr mutate transmute across select count arrange slice_head distinct
#' @importFrom ggplot2 ggplot aes geom_tile scale_y_reverse scale_fill_manual labs
#'   theme_minimal theme geom_text coord_cartesian element_text element_blank margin
#' @importFrom stringr str_squish
#' @importFrom utils adist
NULL

# keep data pronoun registered so r cmd check does not warn
utils::globalVariables(".data")

#' Plot missingness patterns for TeamTV shot data
#'
#' Pattern based missingness heatmap like md.pattern or vim style
#' complete patterns are placed at the top then patterns are sorted by completeness
#' percent observed is printed above each variable column
#'
#' Rules:
#' - strings like onbekend are treated as missing and converted to na
#' - leg is only relevant for type in c(short long freeball)
#'   for other shot types leg is treated as not applicable and not counted as missing
#'
#' Missing data handling:
#' - unknown tokens like onbekend unknown unk n a na and empty string are converted to na for character columns
#' - rows are not removed for missingness plotting patterns are computed from all rows after cleaning
#' - leg missingness is rule based and only counted when leg is required for the shot type
#'
#' @param df A data.frame containing TeamTV export columns.
#' @param person Optional string. If provided, filters to the closest matching
#'   full_name using fuzzy match. Use NULL for all players.
#' @param max_patterns Maximum number of patterns to display most frequent kept.
#' @return A ggplot object.
#' @export
tagr_plot_missingness <- function(df, person = NULL, max_patterns = 30) {

  # load required packages if available and stop with a clear message if not installed
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Install dplyr", call. = FALSE)
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Install ggplot2", call. = FALSE)
  if (!requireNamespace("stringr", quietly = TRUE)) stop("Install stringr", call. = FALSE)

  # validate the teamtv schema and coded values before doing any work
  validate_teamtv_shots(df)

  # define the columns used in the missingness plot
  # this is a subset of the full export that is most relevant for shots and opponents
  vars <- c(
    "full_name", "number", "leg", "type",
    "result", "distance", "pressure", "x", "y",
    "opponent_full_name", "opponent_number", "shot_count"
  )

  # stop early if required columns are not present
  missing_cols <- setdiff(vars, names(df))
  if (length(missing_cols)) {
    stop("Column-name mismatch", call. = FALSE)
  }

  # convert common unknown tokens to na for character vectors
  # this keeps the missingness plot focused on meaningful values
  to_na_unknown <- function(x) {
    if (!is.character(x)) return(x)

    # trim and collapse internal whitespace
    x_trim <- stringr::str_squish(x)

    # define tokens that should be treated as missing
    bad <- tolower(x_trim) %in% c("onbekend", "unknown", "unk", "n/a", "na", "")

    # replace those tokens with explicit na
    x_trim[bad] <- NA_character_
    x_trim
  }

  # optionally filter to one player using fuzzy matching on full_name
  if (!is.null(person)) {

    # get unique candidate names after converting unknown tokens to na
    nm <- unique(to_na_unknown(df$full_name))
    nm <- nm[!is.na(nm)]
    if (!length(nm)) stop("No non-missing full_name values in df.", call. = FALSE)

    # compute edit distance between the user input and each candidate name
    target <- stringr::str_squish(person)
    dists <- adist(tolower(target), tolower(stringr::str_squish(nm)))

    # pick the closest name and keep the distance as a quality check
    best_i <- which.min(dists)
    best_name <- nm[best_i]
    best_dist <- dists[best_i]

    # prevent accidental matches by requiring a reasonably small distance
    if (best_dist > max(3, nchar(target) * 0.4)) {
      stop("No close match for '", person, "'. Closest was '", best_name, "'.", call. = FALSE)
    }

    # filter to rows that match the selected name after applying the same cleaning
    df <- df[to_na_unknown(df$full_name) == best_name, , drop = FALSE]
    if (!nrow(df)) stop("Matched name but found 0 rows (unexpected).", call. = FALSE)

    # print the selected name so the user knows what was matched
    message("Using player: ", best_name)
  }

  # leg is considered required only for these shot types
  # for other types leg is treated as not applicable and should not count as missing
  leg_relevant_types <- c("SHORT", "LONG", "FREEBALL")

  # create a cleaned working dataset and apply the leg rule
  work <- df |>
    dplyr::mutate(

      # clean unknown tokens for the character columns used in the plot
      dplyr::across(
        dplyr::all_of(c(
          "full_name","number","leg","type","result","pressure",
          "opponent_full_name","opponent_number"
        )),
        to_na_unknown
      ),

      # mark whether leg is required based on shot type
      leg_required = .data$type %in% leg_relevant_types,

      # compute rule based missingness for leg only when it is required
      leg_missing  = is.na(.data$leg) & .data$leg_required,

      # create a display value for leg
      # if leg is not required set it to a constant label so it is not counted as missing later
      leg_display  = dplyr::if_else(.data$leg_required, .data$leg, "N/A")
    ) |>
    dplyr::transmute(

      # keep only the variables used for plotting in a consistent order
      # store the rule based leg_missing so we can override leg missingness later
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

  # build a missingness matrix as logicals
  # true means missing false means observed
  miss_df <- work |>
    dplyr::mutate(dplyr::across(dplyr::all_of(vars), ~ is.na(.x))) |>
    dplyr::mutate(

      # override leg missingness with the rule based definition
      # this ensures leg is only missing when it is required and absent
      leg = .data$leg_missing
    ) |>
    dplyr::select(dplyr::all_of(vars))

  # compute percent observed for each variable
  # this is used for the labels printed above each column
  pct_observed <- vapply(miss_df, function(m) mean(!m) * 100, numeric(1))

  # encode each row missingness as a compact string key
  # each character is 1 for missing and 0 for observed in the vars order
  pattern_key <- apply(miss_df, 1, function(r) paste(as.integer(r), collapse = ""))

  # count unique patterns and rank them by completeness and frequency
  # observed counts how many fields are observed which means how many zeros are in the pattern
  pat_tbl <- dplyr::tibble(key = pattern_key) |>
    dplyr::count(.data$key, name = "n") |>
    dplyr::mutate(
      observed = vapply(strsplit(.data$key, ""), function(bits) sum(bits == "0"), integer(1))
    ) |>
    dplyr::arrange(dplyr::desc(.data$observed), dplyr::desc(.data$n)) |>
    dplyr::slice_head(n = max_patterns) |>
    dplyr::mutate(pattern_id = dplyr::row_number())

  # convert a pattern string back to a vector of missingness bits
  decode_pattern <- function(key) as.integer(strsplit(key, "")[[1]])

  # expand patterns to long format for ggplot tiles
  # each pattern becomes one row per variable with missing true false
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

  # friendly labels for the x axis
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

  # apply ordering and labels for consistent plotting
  tile_long$var <- factor(tile_long$var, levels = vars, labels = unname(pretty_labels[vars]))

  # build a small table for percent observed labels above each column
  top_lab <- dplyr::tibble(
    var = factor(vars, levels = vars, labels = unname(pretty_labels[vars])),
    pct = pct_observed[vars]
  )

  # build the ggplot tile heatmap
  # y is reversed so the most complete patterns appear at the top
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
      # show pattern frequency on the right using one label per pattern row
      data = dplyr::distinct(tile_long, .data$pattern_id, .data$n),
      mapping = ggplot2::aes(x = Inf, y = .data$pattern_id, label = paste0("n=", .data$n)),
      inherit.aes = FALSE,
      hjust = 1.1,
      size = 3
    ) +
    ggplot2::geom_text(
      # show percent observed above columns using y set just above the first pattern row
      data = top_lab,
      ggplot2::aes(x = .data$var, y = 0, label = paste0(round(.data$pct, 1), "%")),
      inherit.aes = FALSE,
      vjust = 1.2,
      size = 3
    ) +
    # allow drawing labels outside the panel area
    ggplot2::coord_cartesian(clip = "off") +
    # add extra right margin so the n labels have space
    ggplot2::theme(plot.margin = ggplot2::margin(10, 40, 10, 10))
}
