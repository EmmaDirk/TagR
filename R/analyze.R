#' @importFrom dplyr mutate filter
#' @importFrom ggplot2 ggplot aes geom_point geom_line labs theme_minimal theme element_text position_jitter
#' @importFrom utils adist
#' @importFrom stats glm predict binomial
NULL

utils::globalVariables(".data")

# normalize categorical codes for comparisons
tagr_norm_code <- function(x) {
  x <- trimws(as.character(x))
  x <- toupper(x)
  x <- gsub("[ _]+", "-", x)
  x
}

# optionally restrict to only LONG and SHORT shots
tagr_filter_long_short <- function(df, long_short_only = FALSE) {
  if (!isTRUE(long_short_only)) return(df)

  tp <- tagr_norm_code(df$type)
  keep <- tp %in% c("LONG", "SHORT")
  out <- df[keep, , drop = FALSE]
  if (!nrow(out)) stop("No LONG/SHORT shots available after filtering.", call. = FALSE)
  out
}

# fuzzy match player by name or match by number
tagr_filter_player <- function(df, player) {
  if (is.null(player)) return(df)

  player_chr <- trimws(as.character(player))

  if (grepl("^[0-9]+$", player_chr)) {
    out <- df[df$number == player_chr, , drop = FALSE]
    if (!nrow(out)) stop("No rows found for number '", player_chr, "'.", call. = FALSE)
    return(out)
  }

  nm <- unique(df$full_name)
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

  out <- df[trimws(df$full_name) == best_name, , drop = FALSE]
  if (!nrow(out)) stop("Matched name but found 0 rows (unexpected).", call. = FALSE)
  out
}

# standardize result to 0/1 and drop unknowns
tagr_prepare_binary_result <- function(df) {
  df <- df |>
    dplyr::mutate(result = tagr_norm_code(.data$result))

  df$result[df$result == "ONBEKEND"] <- NA_character_

  df <- df[!is.na(df$result), , drop = FALSE]
  if (!nrow(df)) stop("No non-missing result values after filtering.", call. = FALSE)

  df <- df |>
    dplyr::mutate(y = .data$result == "GOAL")

  if (!any(df$y) || all(df$y)) {
    stop("Need both GOAL and MISS outcomes to fit a binomial model.", call. = FALSE)
  }

  df
}

# prediction on response scale for logistic regression
tagr_predict_prob <- function(fit, newdata) {
  stats::predict(fit, newdata = newdata, type = "response")
}

#' Plot scoring probability vs distance (binomial model)
#'
#' Fits a logistic regression of GOAL vs distance and plots points + fitted curve.
#' Distance is capped at 10 meters (values above 10 are set to 10).
#' If player is provided, the plot includes both the player line and the team line,
#' and colors points so it is clear which points are player vs team.
#' Optionally restricts to LONG and SHORT shots only.
#'
#' Input validation is performed using validate_teamtv_shots().
#'
#' @param df A TeamTV shots data.frame.
#' @param player Optional. Player name (fuzzy match) or shirt number (exact match). Default NULL for all players.
#' @param add_team_line Logical. If TRUE and player is not NULL, add a team fitted line for comparison.
#' @param long_short_only Logical. If TRUE, keep only type in c("LONG","SHORT") before fitting.
#' @return A ggplot object.
#' @export
tagr_plot_prob_by_distance <- function(df, player = NULL, add_team_line = TRUE, long_short_only = FALSE) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Install ggplot2", call. = FALSE)
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Install dplyr", call. = FALSE)

  validate_teamtv_shots(df)

  df <- tagr_filter_long_short(df, long_short_only)

  df_team <- tagr_prepare_binary_result(df)
  df_team <- df_team[!is.na(df_team$distance), , drop = FALSE]
  if (!nrow(df_team)) stop("No non-missing distance values after filtering.", call. = FALSE)

  df_team <- df_team |>
    dplyr::mutate(distance_cap = pmin(.data$distance, 10))

  if (!is.null(player)) {
    df_player <- tagr_filter_player(df_team, player)
  } else {
    df_player <- df_team
  }

  if (!nrow(df_player)) stop("No rows left after player filtering.", call. = FALSE)

  player_lab <- NULL
  if (!is.null(player)) {
    nm <- unique(df_player$full_name); nm <- nm[!is.na(nm)]
    nb <- unique(df_player$number);   nb <- nb[!is.na(nb)]
    nm <- if (length(nm)) nm[1] else NA_character_
    nb <- if (length(nb)) nb[1] else NA_character_
    if (!is.na(nm) && !is.na(nb)) player_lab <- paste0(nm, " (#", nb, ")")
    if (!is.na(nm) &&  is.na(nb)) player_lab <- nm
    if ( is.na(nm) && !is.na(nb)) player_lab <- paste0("#", nb)
  }

  fit_player <- stats::glm(y ~ distance_cap, data = df_player, family = stats::binomial())

  grid <- data.frame(distance_cap = seq(min(df_team$distance_cap, na.rm = TRUE), 10, length.out = 200))
  grid_player <- data.frame(
    distance_cap = grid$distance_cap,
    prob = tagr_predict_prob(fit_player, grid),
    model = if (is.null(player)) "Team" else "Player"
  )

  if (!is.null(player) && isTRUE(add_team_line)) {
    fit_team <- stats::glm(y ~ distance_cap, data = df_team, family = stats::binomial())
    grid_team <- data.frame(
      distance_cap = grid$distance_cap,
      prob = tagr_predict_prob(fit_team, grid),
      model = "Team"
    )
    grid_plot <- rbind(grid_team, grid_player)
  } else {
    grid_plot <- grid_player
  }

  if (!is.null(player)) {
    df_points <- rbind(
      df_team   |> dplyr::mutate(source = "Team",   y01 = as.numeric(.data$y)),
      df_player |> dplyr::mutate(source = "Player", y01 = as.numeric(.data$y))
    )
  } else {
    df_points <- df_team |> dplyr::mutate(source = "Team", y01 = as.numeric(.data$y))
  }

  ttl <- "Scoring probability vs distance"
  subttl <- "Logistic regression"
  if (!is.null(player)) {
    ttl <- paste0(ttl, " (", player_lab, " vs team)")
    if (isTRUE(add_team_line)) subttl <- "Player line + team line"
  }

  ggplot2::ggplot(df_points, ggplot2::aes(x = .data$distance_cap, y = .data$y01)) +
    ggplot2::geom_point(
      ggplot2::aes(color = .data$source),
      alpha = 0.35,
      position = ggplot2::position_jitter(height = 0.03, width = 0)
    ) +
    ggplot2::geom_line(
      data = grid_plot,
      ggplot2::aes(x = .data$distance_cap, y = .data$prob, color = .data$model),
      linewidth = 1
    ) +
    ggplot2::labs(
      title = ttl,
      subtitle = subttl,
      x = "Distance capped at 10 m",
      y = "Goal (0/1)",
      color = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11)
}

#' Plot scoring probability vs pressure (binomial model)
#'
#' Fits a logistic regression of GOAL vs ordered pressure and plots observed + fitted.
#' Pressure is treated as ordered: NONE < MEDIUM < HIGH.
#' If player is provided, adds a team fitted point series for comparison.
#' Optionally restricts to LONG and SHORT shots only.
#'
#' Input validation is performed using validate_teamtv_shots().
#'
#' @param df A TeamTV shots data.frame.
#' @param player Optional. Player name (fuzzy match) or shirt number (exact match). Default NULL for all players.
#' @param long_short_only Logical. If TRUE, keep only type in c("LONG","SHORT") before fitting.
#' @param add_team_points Logical. If TRUE and player is not NULL, add team fitted points for comparison.
#' @return A ggplot object.
#' @export
tagr_plot_prob_by_pressure <- function(df, player = NULL, long_short_only = FALSE, add_team_points = TRUE) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Install ggplot2", call. = FALSE)
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Install dplyr", call. = FALSE)

  validate_teamtv_shots(df)

  df <- tagr_filter_long_short(df, long_short_only)

  df_team <- tagr_prepare_binary_result(df) |>
    dplyr::mutate(pressure = tagr_norm_code(.data$pressure))

  df_team$pressure[df_team$pressure == "ONBEKEND"] <- NA_character_
  df_team <- df_team[!is.na(df_team$pressure), , drop = FALSE]
  if (!nrow(df_team)) stop("No non-missing pressure values after filtering.", call. = FALSE)

  lev <- c("NONE", "MEDIUM", "HIGH")
  df_team <- df_team[df_team$pressure %in% lev, , drop = FALSE]
  if (!nrow(df_team)) stop("Pressure must be one of NONE/MEDIUM/HIGH after filtering.", call. = FALSE)

  df_team$pressure <- factor(df_team$pressure, levels = lev, ordered = TRUE)

  if (!is.null(player)) {
    df_player <- tagr_filter_player(df_team, player)
  } else {
    df_player <- df_team
  }

  if (!nrow(df_player)) stop("No rows left after player filtering.", call. = FALSE)

  player_lab <- NULL
  if (!is.null(player)) {
    nm <- unique(df_player$full_name); nm <- nm[!is.na(nm)]
    nb <- unique(df_player$number);   nb <- nb[!is.na(nb)]
    nm <- if (length(nm)) nm[1] else NA_character_
    nb <- if (length(nb)) nb[1] else NA_character_
    if (!is.na(nm) && !is.na(nb)) player_lab <- paste0(nm, " (#", nb, ")")
    if (!is.na(nm) &&  is.na(nb)) player_lab <- nm
    if ( is.na(nm) && !is.na(nb)) player_lab <- paste0("#", nb)
  }

  fit_player <- stats::glm(y ~ pressure, data = df_player, family = stats::binomial())
  grid <- data.frame(pressure = factor(lev, levels = lev, ordered = TRUE))

  grid_player <- data.frame(
    pressure = grid$pressure,
    prob = tagr_predict_prob(fit_player, grid),
    series = if (is.null(player)) "Team" else "Player"
  )

  if (!is.null(player) && isTRUE(add_team_points)) {
    fit_team <- stats::glm(y ~ pressure, data = df_team, family = stats::binomial())
    grid_team <- data.frame(
      pressure = grid$pressure,
      prob = tagr_predict_prob(fit_team, grid),
      series = "Team"
    )
    grid_plot <- rbind(grid_team, grid_player)
  } else {
    grid_plot <- grid_player
  }

  if (!is.null(player)) {
    df_points <- rbind(
      df_team   |> dplyr::mutate(source = "Team",   y01 = as.numeric(.data$y)),
      df_player |> dplyr::mutate(source = "Player", y01 = as.numeric(.data$y))
    )
  } else {
    df_points <- df_team |> dplyr::mutate(source = "Team", y01 = as.numeric(.data$y))
  }

  ttl <- "Scoring probability vs pressure"
  subttl <- "Logistic regression"
  if (!is.null(player)) {
    ttl <- paste0(ttl, " (", player_lab, " vs team)")
    if (isTRUE(add_team_points)) subttl <- "Player points + team points"
  }

  ggplot2::ggplot() +
    ggplot2::geom_point(
      data = df_points,
      ggplot2::aes(x = .data$pressure, y = .data$y01, color = .data$source),
      alpha = 0.35,
      position = ggplot2::position_jitter(height = 0.03, width = 0.08)
    ) +
    ggplot2::geom_point(
      data = grid_plot,
      ggplot2::aes(x = .data$pressure, y = .data$prob, color = .data$series),
      size = 2.4
    ) +
    ggplot2::geom_line(
      data = grid_plot,
      ggplot2::aes(x = .data$pressure, y = .data$prob, group = .data$series, color = .data$series),
      linewidth = 0.9
    ) +
    ggplot2::labs(
      title = ttl,
      subtitle = subttl,
      x = "Pressure",
      y = "Goal probability",
      color = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11)
}

#' Plot scoring probability vs shot count band (binomial model)
#'
#' Fits a logistic regression of GOAL vs shot count band (1, 2, 3, 4+).
#' If player is provided, adds a team fitted point series for comparison.
#' Optionally restricts to LONG and SHORT shots only.
#'
#' Input validation is performed using validate_teamtv_shots().
#'
#' @param df A TeamTV shots data.frame.
#' @param player Optional. Player name (fuzzy match) or shirt number (exact match). Default NULL for all players.
#' @param long_short_only Logical. If TRUE, keep only type in c("LONG","SHORT") before fitting.
#' @param add_team_points Logical. If TRUE and player is not NULL, add team fitted points for comparison.
#' @return A ggplot object.
#' @export
tagr_plot_prob_by_shot_count <- function(df, player = NULL, long_short_only = FALSE, add_team_points = TRUE) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Install ggplot2", call. = FALSE)
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Install dplyr", call. = FALSE)

  validate_teamtv_shots(df)

  df <- tagr_filter_long_short(df, long_short_only)

  df_team <- tagr_prepare_binary_result(df)
  df_team <- df_team[!is.na(df_team$shot_count), , drop = FALSE]
  if (!nrow(df_team)) stop("No non-missing shot_count values after filtering.", call. = FALSE)

  df_team <- df_team |>
    dplyr::mutate(
      shot_count_band = dplyr::case_when(
        .data$shot_count == 1 ~ "1",
        .data$shot_count == 2 ~ "2",
        .data$shot_count == 3 ~ "3",
        .data$shot_count >= 4 ~ "4+",
        TRUE ~ NA_character_
      )
    )

  df_team <- df_team[!is.na(df_team$shot_count_band), , drop = FALSE]
  if (!nrow(df_team)) stop("No valid shot_count bands after filtering.", call. = FALSE)

  lev <- c("1", "2", "3", "4+")
  df_team$shot_count_band <- factor(df_team$shot_count_band, levels = lev, ordered = TRUE)

  if (!is.null(player)) {
    df_player <- tagr_filter_player(df_team, player)
  } else {
    df_player <- df_team
  }

  if (!nrow(df_player)) stop("No rows left after player filtering.", call. = FALSE)

  player_lab <- NULL
  if (!is.null(player)) {
    nm <- unique(df_player$full_name); nm <- nm[!is.na(nm)]
    nb <- unique(df_player$number);   nb <- nb[!is.na(nb)]
    nm <- if (length(nm)) nm[1] else NA_character_
    nb <- if (length(nb)) nb[1] else NA_character_
    if (!is.na(nm) && !is.na(nb)) player_lab <- paste0(nm, " (#", nb, ")")
    if (!is.na(nm) &&  is.na(nb)) player_lab <- nm
    if ( is.na(nm) && !is.na(nb)) player_lab <- paste0("#", nb)
  }

  fit_player <- stats::glm(y ~ shot_count_band, data = df_player, family = stats::binomial())
  grid <- data.frame(shot_count_band = factor(lev, levels = lev, ordered = TRUE))

  grid_player <- data.frame(
    shot_count_band = grid$shot_count_band,
    prob = tagr_predict_prob(fit_player, grid),
    series = if (is.null(player)) "Team" else "Player"
  )

  if (!is.null(player) && isTRUE(add_team_points)) {
    fit_team <- stats::glm(y ~ shot_count_band, data = df_team, family = stats::binomial())
    grid_team <- data.frame(
      shot_count_band = grid$shot_count_band,
      prob = tagr_predict_prob(fit_team, grid),
      series = "Team"
    )
    grid_plot <- rbind(grid_team, grid_player)
  } else {
    grid_plot <- grid_player
  }

  if (!is.null(player)) {
    df_points <- rbind(
      df_team   |> dplyr::mutate(source = "Team",   y01 = as.numeric(.data$y)),
      df_player |> dplyr::mutate(source = "Player", y01 = as.numeric(.data$y))
    )
  } else {
    df_points <- df_team |> dplyr::mutate(source = "Team", y01 = as.numeric(.data$y))
  }

  ttl <- "Scoring probability vs shot count"
  subttl <- "Logistic regression"
  if (!is.null(player)) {
    ttl <- paste0(ttl, " (", player_lab, " vs team)")
    if (isTRUE(add_team_points)) subttl <- "Player points + team points"
  }

  ggplot2::ggplot() +
    ggplot2::geom_point(
      data = df_points,
      ggplot2::aes(x = .data$shot_count_band, y = .data$y01, color = .data$source),
      alpha = 0.35,
      position = ggplot2::position_jitter(height = 0.03, width = 0.08)
    ) +
    ggplot2::geom_point(
      data = grid_plot,
      ggplot2::aes(x = .data$shot_count_band, y = .data$prob, color = .data$series),
      size = 2.4
    ) +
    ggplot2::geom_line(
      data = grid_plot,
      ggplot2::aes(x = .data$shot_count_band, y = .data$prob, group = .data$series, color = .data$series),
      linewidth = 0.9
    ) +
    ggplot2::labs(
      title = ttl,
      subtitle = subttl,
      x = "Shot count band",
      y = "Goal probability",
      color = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11)
}

