#' @importFrom dplyr mutate group_by summarise arrange filter count slice_head
#' @importFrom ggplot2 ggplot aes geom_col labs theme_minimal theme element_text
#' @importFrom utils adist
NULL

utils::globalVariables(".data")

#' Team overview descriptives tables for TeamTV shot data
#'
#' Computes goal percentages by pressure, distance band, shot type, shot count band, and leg.
#' Returns a named list of plain data.frames so they can be used directly (and tested).
#'
#' Filtering can be applied before computing the descriptives. You can filter for a single
#' player (fuzzy match on full_name or exact match on number) or split results by player.
#'
#' @param df A TeamTV shots data.frame.
#' @param player Optional. Player name (fuzzy match) or shirt number (exact match). Default NULL for all players.
#' @param split_by_player Logical. If TRUE, returns a named list per player (each containing the five tables).
#' @param max_players Integer. When split_by_player is TRUE, limit to the top N players by number of shots.
#' @param filter_type Optional character vector. Keep only these shot types.
#' @param filter_pressure Optional character vector. Keep only these pressure values.
#' @param filter_leg Optional character vector. Keep only these leg values.
#' @param filter_result Optional character vector. Keep only these results.
#' @param filter_distance_band Optional character vector. Keep only these distance bands.
#'   Allowed: "<1 m", "1-3 m", "3-6 m", "6+ m".
#' @param filter_shot_count_band Optional character vector. Keep only these shot count bands.
#'   Allowed: "1", "2", "3", "4+".
#' @param exclude_types Optional character vector. Shot types to exclude.
#' @return If split_by_player is FALSE: a named list of data.frames:
#'   pressure, distance, type, shot_count, leg.
#'   If split_by_player is TRUE: a named list where each element is that same named list.
#' @export
tagr_team_overview_tables <- function(
  df,
  player = NULL,
  split_by_player = FALSE,
  max_players = 12,
  filter_type = NULL,
  filter_pressure = NULL,
  filter_leg = NULL,
  filter_result = NULL,
  filter_distance_band = NULL,
  filter_shot_count_band = NULL,
  exclude_types = NULL
) {

  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Install dplyr", call. = FALSE)

  validate_teamtv_shots(df)

  norm_code <- function(x) {
    x <- trimws(as.character(x))
    x <- toupper(x)
    x <- gsub("[ _]+", "-", x)
    x
  }

  filter_player_one <- function(data, player_value) {
    if (is.null(player_value)) return(data)

    player_chr <- trimws(as.character(player_value))

    if (grepl("^[0-9]+$", player_chr)) {
      out <- data[data$number == player_chr, , drop = FALSE]
      if (!nrow(out)) stop("No rows found for number '", player_chr, "'.", call. = FALSE)
      return(out)
    }

    nm <- unique(data$full_name)
    nm <- nm[!is.na(nm)]
    if (!length(nm)) stop("No non-missing full_name values in df.", call. = FALSE)

    target <- player_chr
    dists <- utils::adist(tolower(target), tolower(trimws(nm)))

    best_i <- which.min(dists)
    best_name <- nm[best_i]
    best_dist <- dists[best_i]

    if (best_dist > max(3, nchar(target) * 0.4)) {
      stop("No close match for '", player_chr, "'. Closest was '", best_name, "'.", call. = FALSE)
    }

    out <- data[trimws(data$full_name) == best_name, , drop = FALSE]
    if (!nrow(out)) stop("Matched name but found 0 rows (unexpected).", call. = FALSE)
    out
  }

  apply_filter <- function(data, col, values) {
    if (is.null(values)) return(data)
    values <- if (col %in% c("distance_band", "shot_count_band")) as.character(values) else norm_code(values)
    got <- if (col %in% c("distance_band", "shot_count_band")) data[[col]] else norm_code(data[[col]])
    data[!is.na(got) & got %in% values, , drop = FALSE]
  }

  summarize_rate <- function(data, group_var) {
    out <- data |>
      dplyr::filter(!is.na(.data[[group_var]])) |>
      dplyr::group_by(.data[[group_var]]) |>
      dplyr::summarise(
        shots = dplyr::n(),
        goals = sum(.data$is_goal, na.rm = TRUE),
        pct_goal = 100 * .data$goals / .data$shots,
        .groups = "drop"
      ) |>
      dplyr::arrange(dplyr::desc(.data$shots))

    names(out)[1] <- "level"
    out
  }

  df <- df |>
    dplyr::mutate(
      type = norm_code(.data$type),
      pressure = norm_code(.data$pressure),
      leg = norm_code(.data$leg),
      result = norm_code(.data$result),
      distance_band = dplyr::case_when(
        is.na(.data$distance) ~ NA_character_,
        .data$distance >= 1 & .data$distance < 3 ~ "1-3 m",
        .data$distance >= 3 & .data$distance < 6 ~ "3-6 m",
        .data$distance >= 6 ~ "6+ m",
        TRUE ~ "<1 m"
      ),
      shot_count_band = dplyr::case_when(
        is.na(.data$shot_count) ~ NA_character_,
        .data$shot_count == 1 ~ "1",
        .data$shot_count == 2 ~ "2",
        .data$shot_count == 3 ~ "3",
        .data$shot_count >= 4 ~ "4+",
        TRUE ~ NA_character_
      )
    )

  if (!is.null(exclude_types)) {
    ex <- norm_code(exclude_types)
    df <- df[is.na(df$type) | !df$type %in% ex, , drop = FALSE]
  }

  df <- apply_filter(df, "type", filter_type)
  df <- apply_filter(df, "pressure", filter_pressure)
  df <- apply_filter(df, "leg", filter_leg)
  df <- apply_filter(df, "result", filter_result)
  df <- apply_filter(df, "distance_band", filter_distance_band)
  df <- apply_filter(df, "shot_count_band", filter_shot_count_band)

  df <- df[!is.na(df$result), , drop = FALSE]
  if (!nrow(df)) stop("No rows left after filtering (or all results missing).", call. = FALSE)

  if (!split_by_player) {
    df <- filter_player_one(df, player)

    df <- df |>
      dplyr::mutate(is_goal = .data$result == "GOAL")

    res <- list(
      pressure = summarize_rate(df, "pressure"),
      distance = summarize_rate(df, "distance_band"),
      type = summarize_rate(df, "type"),
      shot_count = summarize_rate(df, "shot_count_band"),
      leg = summarize_rate(df, "leg")
    )

    res <- res[vapply(res, function(x) nrow(x) > 0, logical(1))]
    if (!length(res)) stop("No summaries available after filtering.", call. = FALSE)

    return(res)
  }

  if (!is.null(player)) {
    df <- filter_player_one(df, player)
  }

  top <- df |>
    dplyr::filter(!is.na(.data$full_name)) |>
    dplyr::count(.data$full_name, name = "shots") |>
    dplyr::arrange(dplyr::desc(.data$shots)) |>
    dplyr::slice_head(n = max_players)

  df <- df[df$full_name %in% top$full_name, , drop = FALSE]
  if (!nrow(df)) stop("No rows left after applying max_players.", call. = FALSE)

  out <- list()
  for (nm in top$full_name) {
    dfi <- df[df$full_name == nm, , drop = FALSE]
    if (!nrow(dfi)) next

    dfi <- dfi |>
      dplyr::mutate(is_goal = .data$result == "GOAL")

    res_i <- list(
      pressure = summarize_rate(dfi, "pressure"),
      distance = summarize_rate(dfi, "distance_band"),
      type = summarize_rate(dfi, "type"),
      shot_count = summarize_rate(dfi, "shot_count_band"),
      leg = summarize_rate(dfi, "leg")
    )

    res_i <- res_i[vapply(res_i, function(x) nrow(x) > 0, logical(1))]
    if (length(res_i)) out[[nm]] <- res_i
  }

  if (!length(out)) stop("No summaries available after splitting by player.", call. = FALSE)

  out
}

#' Team overview plot for one attribute
#'
#' Plots goal percentage by one attribute (pressure, distance, type, shot_count, leg).
#' If split_by_player is TRUE, each player is a different color with a legend.
#'
#' @param df A TeamTV shots data.frame.
#' @param attribute One of: "pressure", "distance", "type", "shot_count", "leg".
#' @param player Optional. Player name (fuzzy match) or shirt number (exact match). Default NULL for all players.
#' @param split_by_player Logical. If TRUE, show players as different colors.
#' @param max_players Integer. When split_by_player is TRUE, limit to the top N players by number of shots.
#' @param filter_type Optional character vector. Keep only these shot types.
#' @param filter_pressure Optional character vector. Keep only these pressure values.
#' @param filter_leg Optional character vector. Keep only these leg values.
#' @param filter_result Optional character vector. Keep only these results.
#' @param filter_distance_band Optional character vector. Keep only these distance bands.
#' @param filter_shot_count_band Optional character vector. Keep only these shot count bands.
#' @param exclude_types Optional character vector. Shot types to exclude.
#' @return A ggplot object.
#' @export
tagr_team_overview_plot <- function(
  df,
  attribute = c("pressure", "distance", "type", "shot_count", "leg"),
  player = NULL,
  split_by_player = FALSE,
  max_players = 10,
  filter_type = NULL,
  filter_pressure = NULL,
  filter_leg = NULL,
  filter_result = NULL,
  filter_distance_band = NULL,
  filter_shot_count_band = NULL,
  exclude_types = NULL
) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Install ggplot2", call. = FALSE)
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Install dplyr", call. = FALSE)

  attribute <- match.arg(attribute)

  validate_teamtv_shots(df)

  norm_code <- function(x) {
    x <- trimws(as.character(x))
    x <- toupper(x)
    x <- gsub("[ _]+", "-", x)
    x
  }

  filter_player_one <- function(data, player_value) {
    if (is.null(player_value)) return(data)

    player_chr <- trimws(as.character(player_value))

    if (grepl("^[0-9]+$", player_chr)) {
      out <- data[data$number == player_chr, , drop = FALSE]
      if (!nrow(out)) stop("No rows found for number '", player_chr, "'.", call. = FALSE)
      return(out)
    }

    nm <- unique(data$full_name)
    nm <- nm[!is.na(nm)]
    if (!length(nm)) stop("No non-missing full_name values in df.", call. = FALSE)

    target <- player_chr
    dists <- utils::adist(tolower(target), tolower(trimws(nm)))

    best_i <- which.min(dists)
    best_name <- nm[best_i]
    best_dist <- dists[best_i]

    if (best_dist > max(3, nchar(target) * 0.4)) {
      stop("No close match for '", player_chr, "'. Closest was '", best_name, "'.", call. = FALSE)
    }

    out <- data[trimws(data$full_name) == best_name, , drop = FALSE]
    if (!nrow(out)) stop("Matched name but found 0 rows (unexpected).", call. = FALSE)
    out
  }

  apply_filter <- function(data, col, values) {
    if (is.null(values)) return(data)
    values <- if (col %in% c("distance_band", "shot_count_band")) as.character(values) else norm_code(values)
    got <- if (col %in% c("distance_band", "shot_count_band")) data[[col]] else norm_code(data[[col]])
    data[!is.na(got) & got %in% values, , drop = FALSE]
  }

  df <- df |>
    dplyr::mutate(
      type = norm_code(.data$type),
      pressure = norm_code(.data$pressure),
      leg = norm_code(.data$leg),
      result = norm_code(.data$result),
      distance_band = dplyr::case_when(
        is.na(.data$distance) ~ NA_character_,
        .data$distance >= 1 & .data$distance < 3 ~ "1-3 m",
        .data$distance >= 3 & .data$distance < 6 ~ "3-6 m",
        .data$distance >= 6 ~ "6+ m",
        TRUE ~ "<1 m"
      ),
      shot_count_band = dplyr::case_when(
        is.na(.data$shot_count) ~ NA_character_,
        .data$shot_count == 1 ~ "1",
        .data$shot_count == 2 ~ "2",
        .data$shot_count == 3 ~ "3",
        .data$shot_count >= 4 ~ "4+",
        TRUE ~ NA_character_
      )
    )

  if (!is.null(exclude_types)) {
    ex <- norm_code(exclude_types)
    df <- df[is.na(df$type) | !df$type %in% ex, , drop = FALSE]
  }

  df <- apply_filter(df, "type", filter_type)
  df <- apply_filter(df, "pressure", filter_pressure)
  df <- apply_filter(df, "leg", filter_leg)
  df <- apply_filter(df, "result", filter_result)
  df <- apply_filter(df, "distance_band", filter_distance_band)
  df <- apply_filter(df, "shot_count_band", filter_shot_count_band)

  df <- df[!is.na(df$result), , drop = FALSE]
  if (!nrow(df)) stop("No rows left after filtering (or all results missing).", call. = FALSE)

  if (!is.null(player)) {
    df <- filter_player_one(df, player)
    split_by_player <- FALSE
  }

  if (split_by_player) {
    top <- df |>
      dplyr::filter(!is.na(.data$full_name)) |>
      dplyr::count(.data$full_name, name = "shots") |>
      dplyr::arrange(dplyr::desc(.data$shots)) |>
      dplyr::slice_head(n = max_players)

    df <- df[df$full_name %in% top$full_name, , drop = FALSE]
    if (!nrow(df)) stop("No rows left after applying max_players.", call. = FALSE)
  }

  df <- df |>
    dplyr::mutate(
      is_goal = .data$result == "GOAL",
      level = dplyr::case_when(
        attribute == "pressure" ~ .data$pressure,
        attribute == "distance" ~ .data$distance_band,
        attribute == "type" ~ .data$type,
        attribute == "shot_count" ~ .data$shot_count_band,
        attribute == "leg" ~ .data$leg,
        TRUE ~ NA_character_
      )
    )

  df <- df[!is.na(df$level), , drop = FALSE]
  if (!nrow(df)) stop("No non-missing values for the requested attribute after filtering.", call. = FALSE)

  if (split_by_player) {
    agg <- df |>
      dplyr::group_by(.data$level, .data$full_name) |>
      dplyr::summarise(
        shots = dplyr::n(),
        goals = sum(.data$is_goal, na.rm = TRUE),
        pct_goal = 100 * .data$goals / .data$shots,
        .groups = "drop"
      )

    ggplot2::ggplot(agg, ggplot2::aes(x = .data$level, y = .data$pct_goal, fill = .data$full_name)) +
      ggplot2::geom_col(position = "dodge") +
      ggplot2::labs(
        title = paste0("Goal percentage by ", attribute, " (split by player)"),
        x = NULL,
        y = "Goal percentage",
        fill = "Player"
      ) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 25, hjust = 1))
  } else {
    agg <- df |>
      dplyr::group_by(.data$level) |>
      dplyr::summarise(
        shots = dplyr::n(),
        goals = sum(.data$is_goal, na.rm = TRUE),
        pct_goal = 100 * .data$goals / .data$shots,
        .groups = "drop"
      )

    fill_col <- switch(
      attribute,
      pressure = "steelblue",
      distance = "darkorange",
      type = "seagreen3",
      shot_count = "mediumpurple3",
      leg = "goldenrod2",
      "grey70"
    )

    ggplot2::ggplot(agg, ggplot2::aes(x = .data$level, y = .data$pct_goal)) +
      ggplot2::geom_col(fill = fill_col) +
      ggplot2::labs(
        title = paste0("Goal percentage by ", attribute),
        x = NULL,
        y = "Goal percentage"
      ) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 25, hjust = 1))
  }
}
