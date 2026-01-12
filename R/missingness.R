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
#' #' @importFrom dplyr mutate transmute across select count arrange slice_head distinct
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_tile scale_y_reverse scale_fill_manual labs
#'   theme_minimal theme geom_text coord_cartesian element_text element_blank margin
#' @importFrom stringr str_squish
#' @param df A data.frame containing TeamTV export columns.
#' @param person Optional string. If provided, filters to the closest matching
#'   `full_name` (fuzzy match). Use `NULL` for all players.
#' @param max_patterns Maximum number of patterns to display (most frequent kept).
#' @return A ggplot object.
#' @export
tagr_plot_missingness <- function(df, person = NULL, max_patterns = 30) {
  # ---- packages ----
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Install dplyr", call. = FALSE)
  if (!requireNamespace("tidyr", quietly = TRUE)) stop("Install tidyr", call. = FALSE)
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Install ggplot2", call. = FALSE)
  if (!requireNamespace("stringr", quietly = TRUE)) stop("Install stringr", call. = FALSE)

  # ---- required columns for your spec ----
  vars <- c(
    "full_name", "number", "leg", "type",
    "result", "distance", "pressure", "x", "y",
    "opponent_full_name", "opponent_number", "shot_count"
  )
  missing_cols <- setdiff(vars, names(df))
  if (length(missing_cols)) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  # ---- helper: convert "onbekend"/"unknown" style strings to NA ----
  to_na_unknown <- function(x) {
    if (!is.character(x)) return(x)
    x_trim <- stringr::str_squish(x)
    bad <- tolower(x_trim) %in% c("onbekend", "unknown", "unk", "n/a", "na", "")
    x_trim[bad] <- NA_character_
    x_trim
  }

  # ---- optional: filter to one person using fuzzy match on full_name ----
  if (!is.null(person)) {
    nm <- unique(to_na_unknown(df$full_name))
    nm <- nm[!is.na(nm)]
    if (!length(nm)) stop("No non-missing full_name values in df.", call. = FALSE)

    target <- stringr::str_squish(person)
    dists <- adist(tolower(target), tolower(stringr::str_squish(nm)))
    best_i <- which.min(dists)
    best_name <- nm[best_i]
    best_dist <- dists[best_i]

    # guardrail: avoid nonsense matches
    if (best_dist > max(3, nchar(target) * 0.4)) {
      stop("No close match for '", person, "'. Closest was '", best_name, "'.", call. = FALSE)
    }

    df <- df[to_na_unknown(df$full_name) == best_name, , drop = FALSE]
    if (!nrow(df)) stop("Matched name but found 0 rows (unexpected).", call. = FALSE)

    message("Using player: ", best_name)
  }

  # ---- clean + apply rules ----
  leg_relevant_types <- c("SHORT", "LONG", "FREEBALL")

  work <- df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(c("full_name","number","leg","type","result","pressure",
                        "opponent_full_name","opponent_number")),
        to_na_unknown
      ),
      # leg rule: only relevant for certain types
      leg_required = .data$type %in% leg_relevant_types,
      leg_missing  = is.na(.data$leg) & .data$leg_required,
      # for display only: mark leg as "N/A" when not required
      leg_display  = dplyr::if_else(.data$leg_required, .data$leg, "N/A")
    ) |>
    dplyr::transmute(
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

  # ---- missingness matrix (logical): TRUE = missing ----
  miss_df <- work |>
    dplyr::mutate(dplyr::across(dplyr::all_of(vars), ~ is.na(.x))) |>
    dplyr::mutate(
      # overwrite leg missingness with rule-based missingness
      leg = .data$leg_missing
    ) |>
    dplyr::select(dplyr::all_of(vars))

  # ---- % observed per column (after cleaning + leg rule) ----
  pct_observed <- vapply(miss_df, function(m) mean(!m) * 100, numeric(1))

  # ---- collapse rows into patterns ----
  pattern_key <- apply(miss_df, 1, function(r) paste(as.integer(r), collapse = ""))

  pat_tbl <- dplyr::tibble(key = pattern_key) |>
    dplyr::count(.data$key, name = "n") |>
    dplyr::mutate(
      observed = vapply(strsplit(.data$key, ""), function(bits) sum(bits == "0"), integer(1))
    ) |>
    dplyr::arrange(dplyr::desc(.data$observed), dplyr::desc(.data$n)) |>
    dplyr::slice_head(n = max_patterns) |>
    dplyr::mutate(pattern_id = dplyr::row_number())

  decode_pattern <- function(key) as.integer(strsplit(key, "")[[1]])

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

  # ---- nicer labels ----
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

  tile_long$var <- factor(tile_long$var, levels = vars, labels = unname(pretty_labels[vars]))

  top_lab <- dplyr::tibble(
    var = factor(vars, levels = vars, labels = unname(pretty_labels[vars])),
    pct = pct_observed[vars]
  )

  # ---- plot ----
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
    # pattern counts on the right
    ggplot2::geom_text(
      data = dplyr::distinct(tile_long, .data$pattern_id, .data$n),
      ggplot2::aes(x = Inf, y = .data$pattern_id, label = paste0("n=", .data$n)),
      inherit.aes = FALSE,
      hjust = 1.1,
      size = 3
    ) +
    # % observed above columns
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



