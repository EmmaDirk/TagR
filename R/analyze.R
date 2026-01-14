#' Shot analysis goal probability vs distance pressure shot count
#'
#' fits a binomial logistic regression of scoring goal vs miss against one predictor
#' then plots the observed shots plus the fitted probability curve or fitted points
#'
#' predictor choice is controlled by feature
#' - distance uses distance capped at 10 meters with distance_cap = pmin(distance, 10)
#' - pressure uses an ordered factor none then medium then high
#' - shot_count is binned as 1 2 3 4+
#'
#' player comparison
#' - players can be null for team only or a vector of 1 to 3 players by name or number
#' - if players is provided and add_team is true a team series is included for comparison
#' - legend shows team and player labels the title never includes player names
#'
#' missing and unknown handling
#' - unknown result onbekend is dropped then result must contain both goal and miss
#' - rows missing the chosen feature are dropped
#' - for pressure onbekend and values outside none medium high are dropped
#' - if a player has no shots in a given pressure or shot count level that level is omitted
#'   this avoids predict errors from new unused levels
#'
#' input validation
#' - calls validate_teamtv_shots(df) before any processing
#'
#' @param df A TeamTV shots data.frame.
#' @param feature One of distance pressure shot_count.
#' @param players Optional NULL for team only or a vector length 1 to 3 of player names or shirt numbers.
#' @param add_team Logical if true and players is not null include team series too.
#' @param long_short_only Logical if true keep only type in c(long short) before fitting.
#' @return A ggplot object.
#' @export
tagr_shot_analysis <- function(df,
                              feature = c("distance", "pressure", "shot_count"),
                              players = NULL,
                              add_team = TRUE,
                              long_short_only = FALSE) {

  # require packages used for data and plotting
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Install ggplot2", call. = FALSE)
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Install dplyr", call. = FALSE)

  # ensure feature is one of the supported values
  feature <- match.arg(feature)

  # helpers scoped inside this function so there are no external dependencies

  # normalize coded strings for consistent matching
  tagr_norm_code <- function(x) {
    x <- trimws(as.character(x))
    x <- toupper(x)
    x <- gsub("[ _]+", "-", x)
    x
  }

  # optionally restrict to long and short shot types only
  # this uses df$type and keeps only rows matching long or short after normalization
  tagr_filter_long_short <- function(df, long_short_only = FALSE) {
    if (!isTRUE(long_short_only)) return(df)

    tp <- tagr_norm_code(df$type)
    keep <- tp %in% c("LONG", "SHORT")
    out <- df[keep, , drop = FALSE]
    if (!nrow(out)) stop("No LONG/SHORT shots available after filtering.", call. = FALSE)
    out
  }

  # filter to one player using either shirt number exact match or fuzzy name match
  tagr_filter_player <- function(df, player) {
    if (is.null(player)) return(df)

    player_chr <- trimws(as.character(player))

    # digits means shirt number
    if (grepl("^[0-9]+$", player_chr)) {
      out <- df[df$number == player_chr, , drop = FALSE]
      if (!nrow(out)) stop("No rows found for number '", player_chr, "'.", call. = FALSE)
      return(out)
    }

    # otherwise use fuzzy matching on full_name
    nm <- unique(df$full_name)
    nm <- nm[!is.na(nm)]
    if (!length(nm)) stop("No non-missing full_name values in df.", call. = FALSE)

    target <- player_chr
    dists <- utils::adist(tolower(target), tolower(trimws(nm)))

    best_i <- which.min(dists)
    best_name <- nm[best_i]
    best_dist <- dists[best_i]

    # require a close enough match to avoid selecting the wrong player
    if (best_dist > max(3, nchar(target) * 0.4)) {
      stop("No close match for '", player_chr, "'. Closest was '", best_name, "'.", call. = FALSE)
    }

    out <- df[trimws(df$full_name) == best_name, , drop = FALSE]
    if (!nrow(out)) stop("Matched name but found 0 rows (unexpected).", call. = FALSE)
    out
  }

  # prepare y as a binary outcome and drop unknown results
  # this keeps only rows where result is goal or miss
  # it also checks there is variation so logistic regression can be fit
  tagr_prepare_binary_result <- function(df) {
    df <- df |>
      dplyr::mutate(result = tagr_norm_code(.data$result))

    # treat onbekend as missing so it is removed
    df$result[df$result == "ONBEKEND"] <- NA_character_

    # drop rows without usable result
    df <- df[!is.na(df$result), , drop = FALSE]
    if (!nrow(df)) stop("No non-missing result values after filtering.", call. = FALSE)

    # create the binary response for the glm
    df <- df |>
      dplyr::mutate(y = .data$result == "GOAL")

    # glm needs both outcomes present otherwise the fit is degenerate
    if (!any(df$y) || all(df$y)) {
      stop("Need both GOAL and MISS outcomes to fit a binomial model.", call. = FALSE)
    }

    df
  }

  # predict returns probabilities when type is response for a binomial glm
  tagr_predict_prob <- function(fit, newdata) {
    stats::predict(fit, newdata = newdata, type = "response")
  }

  # create a readable legend label from available name and number columns
  tagr_player_label <- function(df_player) {
    nm <- unique(df_player$full_name); nm <- nm[!is.na(nm)]
    nb <- unique(df_player$number);    nb <- nb[!is.na(nb)]
    nm <- if (length(nm)) nm[1] else NA_character_
    nb <- if (length(nb)) nb[1] else NA_character_

    if (!is.na(nm) && !is.na(nb)) return(paste0(nm, " (#", nb, ")"))
    if (!is.na(nm) &&  is.na(nb)) return(nm)
    if ( is.na(nm) && !is.na(nb)) return(paste0("#", nb))
    "Player"
  }

  # validate schema and coded fields before doing anything else
  validate_teamtv_shots(df)

  # optional type filtering
  df <- tagr_filter_long_short(df, long_short_only)

  # create the team dataset with binary outcome and cleaned results
  df_team <- tagr_prepare_binary_result(df)

  # normalize players input to a clean vector or null
  player_ids <- NULL
  if (!is.null(players)) {
    player_ids <- trimws(as.character(players))
    player_ids <- player_ids[nzchar(player_ids)]
    if (!length(player_ids)) player_ids <- NULL
  }

  # limit the number of compared players to keep plot readable
  if (!is.null(player_ids) && length(player_ids) > 3) {
    stop("You can specify up to 3 players.", call. = FALSE)
  }

  # feature specific preparation

  # distance feature uses distance_cap and drops rows without distance
  if (feature == "distance") {
    df_team <- df_team[!is.na(df_team$distance), , drop = FALSE]
    if (!nrow(df_team)) stop("No non-missing distance values after filtering.", call. = FALSE)

    df_team <- df_team |>
      dplyr::mutate(distance_cap = pmin(.data$distance, 10))
  }

  # pressure feature normalizes pressure and keeps only none medium high
  if (feature == "pressure") {
    df_team <- df_team |>
      dplyr::mutate(pressure = tagr_norm_code(.data$pressure))

    # treat onbekend as missing
    df_team$pressure[df_team$pressure == "ONBEKEND"] <- NA_character_

    # drop missing pressure
    df_team <- df_team[!is.na(df_team$pressure), , drop = FALSE]
    if (!nrow(df_team)) stop("No non-missing pressure values after filtering.", call. = FALSE)

    # keep only the allowed levels
    lev <- c("NONE", "MEDIUM", "HIGH")
    df_team <- df_team[df_team$pressure %in% lev, , drop = FALSE]
    if (!nrow(df_team)) stop("Pressure must be one of NONE/MEDIUM/HIGH after filtering.", call. = FALSE)

    # store as an ordered factor so models and axes have consistent ordering
    df_team$pressure <- factor(df_team$pressure, levels = lev, ordered = TRUE)
  }

  # shot_count feature creates banded categories and drops missing shot_count
  if (feature == "shot_count") {
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

    # drop rows that did not map to a band
    df_team <- df_team[!is.na(df_team$shot_count_band), , drop = FALSE]
    if (!nrow(df_team)) stop("No valid shot_count bands after filtering.", call. = FALSE)

    # set ordered levels for stable axis ordering
    lev_sc <- c("1", "2", "3", "4+")
    df_team$shot_count_band <- factor(df_team$shot_count_band, levels = lev_sc, ordered = TRUE)
  }

  # build series list containing team and optional players
  # each series stores a key label and filtered data used for fitting and plotting
  series <- list()

  # include team when no players are requested or when add_team is true
  include_team <- is.null(player_ids) || isTRUE(add_team)
  if (isTRUE(include_team)) {
    series[["Team"]] <- list(key = "Team", label = "Team", data = df_team)
  }

  # add up to 3 player series
  if (!is.null(player_ids)) {
    for (i in seq_along(player_ids)) {

      # filter to this player within the team filtered dataset
      df_pi <- tagr_filter_player(df_team, player_ids[i])
      if (!nrow(df_pi)) stop("No rows left after filtering player '", player_ids[i], "'.", call. = FALSE)

      # after subsetting reapply full factor levels so axes stay consistent
      if (feature == "pressure") {
        lev <- c("NONE", "MEDIUM", "HIGH")
        df_pi$pressure <- factor(df_pi$pressure, levels = lev, ordered = TRUE)
      }
      if (feature == "shot_count") {
        lev_sc <- c("1", "2", "3", "4+")
        df_pi$shot_count_band <- factor(df_pi$shot_count_band, levels = lev_sc, ordered = TRUE)
      }

      # store this series with a readable label
      series[[paste0("Player", i)]] <- list(
        key = paste0("Player", i),
        label = tagr_player_label(df_pi),
        data = df_pi
      )
    }
  }

  # build the plot depending on feature
  # each branch creates p which is later returned with stable manual colors

  if (feature == "distance") {

    # build a common x grid so series curves can be compared smoothly
    x_min <- min(df_team$distance_cap, na.rm = TRUE)
    grid_x <- data.frame(distance_cap = seq(x_min, 10, length.out = 200))

    # fit a model per series and predict on the grid
    grid_plot <- do.call(rbind, lapply(series, function(s) {
      fit <- stats::glm(y ~ distance_cap, data = s$data, family = stats::binomial())
      data.frame(
        distance_cap = grid_x$distance_cap,
        prob = tagr_predict_prob(fit, grid_x),
        series_key = s$key,
        series_label = s$label
      )
    }))

    # build point data for observed outcomes
    # y01 is numeric 0 or 1 used for plotting with jitter
    df_points <- do.call(rbind, lapply(series, function(s) {
      s$data |>
        dplyr::mutate(series_key = s$key, series_label = s$label, y01 = as.numeric(.data$y))
    }))

    # scatter of observations plus fitted probability curves
    p <- ggplot2::ggplot(df_points, ggplot2::aes(x = .data$distance_cap, y = .data$y01)) +
      ggplot2::geom_point(
        ggplot2::aes(color = .data$series_label),
        alpha = 0.35,
        position = ggplot2::position_jitter(height = 0.03, width = 0)
      ) +
      ggplot2::geom_line(
        data = grid_plot,
        ggplot2::aes(x = .data$distance_cap, y = .data$prob, color = .data$series_label),
        linewidth = 1
      ) +
      ggplot2::labs(
        title = "Scoring probability vs distance",
        x = "Distance capped at 10 m",
        y = "Goal probability",
        color = NULL
      ) +
      ggplot2::theme_minimal(base_size = 11)
  }

  if (feature == "pressure") {
    lev <- c("NONE", "MEDIUM", "HIGH")

    # fit a model per series and predict only on levels that exist in that series
    grid_plot <- do.call(rbind, lapply(series, function(s) {

      # present levels avoids predicting for a factor level not in the training data
      present <- unique(as.character(s$data$pressure))
      present <- lev[lev %in% present]

      grid_s <- data.frame(pressure = factor(present, levels = lev, ordered = TRUE))

      if (length(present) >= 2) {
        fit <- stats::glm(y ~ pressure, data = s$data, family = stats::binomial())
        pr <- tagr_predict_prob(fit, grid_s)
      } else {
        pr <- rep(mean(s$data$y), nrow(grid_s))
      }

      data.frame(
        pressure = grid_s$pressure,
        prob = pr,
        series_key = s$key,
        series_label = s$label
      )
    }))

    # build observed points for plotting
    df_points <- do.call(rbind, lapply(series, function(s) {
      s$data |>
        dplyr::mutate(series_key = s$key, series_label = s$label, y01 = as.numeric(.data$y))
    }))

    # jittered observations plus fitted points and connecting lines
    p <- ggplot2::ggplot() +
      ggplot2::geom_point(
        data = df_points,
        ggplot2::aes(x = .data$pressure, y = .data$y01, color = .data$series_label),
        alpha = 0.35,
        position = ggplot2::position_jitter(height = 0.03, width = 0.08)
      ) +
      ggplot2::geom_point(
        data = grid_plot,
        ggplot2::aes(x = .data$pressure, y = .data$prob, color = .data$series_label),
        size = 2.4
      ) +
      ggplot2::geom_line(
        data = grid_plot,
        ggplot2::aes(x = .data$pressure, y = .data$prob, group = .data$series_label, color = .data$series_label),
        linewidth = 0.9
      ) +
      ggplot2::labs(
        title = "Scoring probability vs pressure",
        x = "Pressure",
        y = "Goal probability",
        color = NULL
      ) +
      ggplot2::theme_minimal(base_size = 11)
  }

  if (feature == "shot_count") {
    lev_sc <- c("1", "2", "3", "4+")

    # fit a model per series and predict only for levels present in that series
    grid_plot <- do.call(rbind, lapply(series, function(s) {
      present <- unique(as.character(s$data$shot_count_band))
      present <- lev_sc[lev_sc %in% present]

      grid_s <- data.frame(shot_count_band = factor(present, levels = lev_sc, ordered = TRUE))

      if (length(present) >= 2) {
        fit <- stats::glm(y ~ shot_count_band, data = s$data, family = stats::binomial())
        pr <- tagr_predict_prob(fit, grid_s)
      } else {
        pr <- rep(mean(s$data$y), nrow(grid_s))
      }

      data.frame(
        shot_count_band = grid_s$shot_count_band,
        prob = pr,
        series_key = s$key,
        series_label = s$label
      )
    }))

    # observed points
    df_points <- do.call(rbind, lapply(series, function(s) {
      s$data |>
        dplyr::mutate(series_key = s$key, series_label = s$label, y01 = as.numeric(.data$y))
    }))

    # jittered observations plus fitted points and connecting lines
    p <- ggplot2::ggplot() +
      ggplot2::geom_point(
        data = df_points,
        ggplot2::aes(x = .data$shot_count_band, y = .data$y01, color = .data$series_label),
        alpha = 0.35,
        position = ggplot2::position_jitter(height = 0.03, width = 0.08)
      ) +
      ggplot2::geom_point(
        data = grid_plot,
        ggplot2::aes(x = .data$shot_count_band, y = .data$prob, color = .data$series_label),
        size = 2.4
      ) +
      ggplot2::geom_line(
        data = grid_plot,
        ggplot2::aes(x = .data$shot_count_band, y = .data$prob, group = .data$series_label, color = .data$series_label),
        linewidth = 0.9
      ) +
      ggplot2::labs(
        title = "Scoring probability vs shot count",
        x = "Shot count band",
        y = "Goal probability",
        color = NULL
      ) +
      ggplot2::theme_minimal(base_size = 11)
  }

  # map stable colors by series role
  # team always gets one color and player slots get fixed colors by slot order
  label_map <- character(0)
  if (!is.null(series[["Team"]])) label_map["Team"] <- "Team"
  for (i in 1:3) {
    k <- paste0("Player", i)
    if (!is.null(series[[k]])) label_map[k] <- series[[k]]$label
  }

  # define role colors and then rename them to the actual legend labels
  cols_by_role <- c(Team = "#1f77b4", Player1 = "#d62728", Player2 = "#2ca02c", Player3 = "#9467bd")
  cols <- stats::setNames(cols_by_role[names(label_map)], unname(label_map))

  # add manual color scale so comparisons are consistent across runs
  p + ggplot2::scale_color_manual(values = cols)
}
