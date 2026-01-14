# silence R CMD check NOTES about NSE (dplyr/ggplot2) and base function lookups
utils::globalVariables(c(
  ".data",
  "pressure",
  "distance_band",
  "type",
  "shot_count_band",
  "leg"
))

#' Team dashboard plots (coach overview)
#'
#' Creates a small dashboard as a collection of ggplot objects
#' the goal is a quick descriptive overview of a teams shooting data
#'
#' Dashboard contains 6 plots
#' 1 leg preference players placed on a left to right line no y axis
#'   left means left leg preference right means right leg preference
#'   point size scales with the number of shots used for the estimate
#' 2 goals by player bar chart counting total goal events per player
#'   uses the same player colors as the leg plot no legend no names on x axis
#' 3 scoring percent by pressure goal percentage by pressure category
#'   only types short long free ball
#' 4 scoring percent by distance goal percentage by distance band
#'   only types short long free ball distance bins are less than 1 1 to 3 3 to 6 6 plus
#' 5 scoring percent by type goal percentage by shot type no exclusions
#' 6 scoring percent by shot count goal percentage by shot_count band 1 2 3 4 plus
#'
#' Missing and onbekend handling
#' - character fields type pressure leg result are normalized to uppercase and separators become hyphen
#' - values equal to onbekend or empty string become na for those fields
#' - rows with missing result are excluded from goal percentage computations
#' - a goal is defined as result equals goal
#' - for leg preference only rows with type short long free ball and result goal miss and leg left right both are used
#'   players without sufficient leg data are not shown in the leg plot
#'
#' @param df A TeamTV shots data.frame.
#' @param max_players Integer maximum number of players to show based on total shots with non missing full_name.
#' @return If patchwork is available a patchwork object otherwise a named list of ggplot objects.
#' @export
tagr_team_dashboad <- function(df, max_players = 12) {

  # ensure required packages exist
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Install dplyr", call. = FALSE)
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Install ggplot2", call. = FALSE)

  # basic input type check
  if (!is.data.frame(df)) stop("Column-name mismatch: `df` must be a data.frame.", call. = FALSE)

  # validate the teamtv schema and coded values before plotting
  validate_teamtv_shots(df)

  # validate max_players as a single number at least 1
  if (!is.numeric(max_players) || length(max_players) != 1L || is.na(max_players) || max_players < 1) {
    stop("`max_players` must be a single number >= 1.", call. = FALSE)
  }
  max_players <- as.integer(max_players)

  # helpers used only inside this function

  # normalize codes to a consistent format for comparing and grouping
  norm_code <- function(x) {
    x <- trimws(as.character(x))
    x <- toupper(x)
    x <- gsub("[ _]+", "-", x)
    x
  }

  # treat unknown tokens as missing
  as_na_unknown <- function(x) {
    x <- as.character(x)
    x[x %in% c("ONBEKEND", "")] <- NA_character_
    x
  }

  # simple color palette generator so each player gets a stable distinct color
  hue_pal <- function(n) {
    if (n <= 0) return(character(0))
    hues <- seq(15, 375, length.out = n + 1)
    grDevices::hcl(h = hues[seq_len(n)], l = 65, c = 100)
  }

  # shared theme snippet to make axis titles bold without changing legend defaults
  base_theme_bold_titles <- ggplot2::theme(
    axis.title.x = ggplot2::element_text(face = "bold"),
    axis.title.y = ggplot2::element_text(face = "bold")
  )

  # required columns for this dashboard
  needed <- c("full_name", "number", "type", "pressure", "leg", "result", "distance", "shot_count")
  missing_cols <- setdiff(needed, names(df))
  if (length(missing_cols)) {
    stop("Column-name mismatch: `df` is missing required columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  # normalize key fields and create derived grouping bands
  df <- df |>
    dplyr::mutate(
      # ensure these are character so factors and matching are consistent
      full_name = as.character(.data$full_name),
      number    = as.character(.data$number),

      # normalize coded columns and convert unknowns to na
      type      = as_na_unknown(norm_code(.data$type)),
      pressure  = as_na_unknown(norm_code(.data$pressure)),
      leg       = as_na_unknown(norm_code(.data$leg)),
      result    = as_na_unknown(norm_code(.data$result)),

      # precompute a goal indicator used by multiple summaries
      is_goal   = !is.na(.data$result) & .data$result == "GOAL",

      # create distance bands used by the distance percent plot
      distance_band = dplyr::case_when(
        is.na(.data$distance) ~ NA_character_,
        .data$distance < 1 ~ "<1 m",
        .data$distance < 3 ~ "1-3 m",
        .data$distance < 6 ~ "3-6 m",
        TRUE ~ "6+ m"
      ),

      # create shot count bands used by the shot count percent plot
      shot_count_band = dplyr::case_when(
        is.na(.data$shot_count) ~ NA_character_,
        .data$shot_count == 1 ~ "1",
        .data$shot_count == 2 ~ "2",
        .data$shot_count == 3 ~ "3",
        .data$shot_count >= 4 ~ "4+",
        TRUE ~ NA_character_
      )
    )

  # pick the top players by number of shots with a usable name
  top_players <- df |>
    dplyr::filter(!is.na(.data$full_name), .data$full_name != "") |>
    dplyr::count(.data$full_name, name = "shots_total") |>
    dplyr::arrange(dplyr::desc(.data$shots_total)) |>
    dplyr::slice_head(n = max_players)

  # stop if there are no usable names at all
  if (!nrow(top_players)) stop("No non-missing `full_name` values found in `df`.", call. = FALSE)

  # restrict the dataset to only those players so all plots are consistent
  df <- df[df$full_name %in% top_players$full_name, , drop = FALSE]

  # keep a stable player order and stable colors across plots
  player_levels <- top_players$full_name
  player_cols <- stats::setNames(hue_pal(length(player_levels)), player_levels)

  # plot 1 leg preference

  # keep only rows where leg analysis makes sense
  # it limits to shot types where leg is meaningful and to rows with known result and leg
  leg_df <- df |>
    dplyr::filter(
      .data$type %in% c("SHORT", "LONG", "FREE-BALL"),
      !is.na(.data$result),
      .data$result %in% c("GOAL", "MISS"),
      !is.na(.data$leg),
      .data$leg %in% c("LEFT", "RIGHT", "BOTH"),
      !is.na(.data$full_name),
      .data$full_name != ""
    ) |>
    dplyr::mutate(is_goal = .data$result == "GOAL")

  # compute shots and goals per player per leg category
  per_player_leg <- leg_df |>
    dplyr::group_by(.data$full_name, .data$number, .data$leg) |>
    dplyr::summarise(
      shots = dplyr::n(),
      goals = sum(.data$is_goal, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      # goal rate within that leg category for the player
      goal_rate = dplyr::if_else(.data$shots > 0, .data$goals / .data$shots, 0),

      # weight combines volume and success so higher volume and higher accuracy matter more
      weight = .data$shots * (0.5 + .data$goal_rate)
    )

  # collapse to one preference index per player
  # negative means more left weighted positive means more right weighted
  leg_idx <- per_player_leg |>
    dplyr::group_by(.data$full_name, .data$number) |>
    dplyr::summarise(
      w_left  = sum(.data$weight[.data$leg == "LEFT"],  na.rm = TRUE),
      w_right = sum(.data$weight[.data$leg == "RIGHT"], na.rm = TRUE),
      w_both  = sum(.data$weight[.data$leg == "BOTH"],  na.rm = TRUE),
      shots_total = sum(.data$shots, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      # denom is total weight used to scale into a -1 to 1 index
      denom = .data$w_left + .data$w_right + .data$w_both,

      # leg_index is a normalized difference between right and left weights
      leg_index = dplyr::if_else(.data$denom > 0, (.data$w_right - .data$w_left) / .data$denom, NA_real_),

      # factor for stable color mapping and ordering
      full_name = factor(.data$full_name, levels = player_levels)
    ) |>
    # drop players that have no usable index
    dplyr::filter(!is.na(.data$leg_index))

  # build the leg preference plot
  p_leg <- ggplot2::ggplot(
    leg_idx,
    ggplot2::aes(x = .data$leg_index, y = 0, color = .data$full_name, size = .data$shots_total)
  ) +
    # draw the baseline from -1 to 1
    ggplot2::annotate("segment", x = -1, xend = 1, y = 0, yend = 0, linewidth = 1) +
    # draw a reference line at neutral
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
    # draw player points
    ggplot2::geom_point(alpha = 0.9) +
    # label the extremes as left and right
    ggplot2::scale_x_continuous(
      limits = c(-1, 1),
      breaks = c(-1, 0, 1),
      labels = c("LEFT", "", "RIGHT")
    ) +
    # apply stable player colors
    ggplot2::scale_color_manual(values = player_cols, drop = FALSE) +
    # scale point size by shot volume
    ggplot2::scale_size_continuous(range = c(2.5, 8)) +
    ggplot2::labs(
      title = "Leg preference",
      x = NULL,
      y = NULL,
      color = "Player",
      size = "Shots"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    base_theme_bold_titles +
    ggplot2::theme(
      # remove y axis decorations since all points sit on one line
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank()
    )

  # plot 2 goals by player

  # compute total goals per player
  goals_df <- df |>
    dplyr::filter(!is.na(.data$full_name), .data$full_name != "", !is.na(.data$result)) |>
    dplyr::group_by(.data$full_name) |>
    dplyr::summarise(goals = sum(.data$result == "GOAL", na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(full_name = factor(.data$full_name, levels = player_levels)) |>
    dplyr::arrange(dplyr::desc(.data$goals))

  # bar chart with the same player colors as the leg plot
  p_goals <- ggplot2::ggplot(goals_df, ggplot2::aes(x = .data$full_name, y = .data$goals, fill = .data$full_name)) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(values = player_cols, drop = FALSE, guide = "none") +
    ggplot2::labs(
      title = "Goals by player",
      x = NULL,
      y = "Goals"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    base_theme_bold_titles +
    ggplot2::theme(
      # hide x labels to reduce clutter since colors already encode player identity
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    )

  # helper used for the team level goal percentage plots
  # it groups by a chosen level column and returns pct_goal plus ordering
  summarise_pct <- function(data, level_col) {
    level_col <- rlang::ensym(level_col)

    out <- data |>
      # exclude rows without result because pct_goal needs goal or miss
      dplyr::filter(!is.na(.data$result), !is.na(!!level_col)) |>
      dplyr::group_by(level = !!level_col) |>
      dplyr::summarise(
        shots = dplyr::n(),
        goals = sum(.data$result == "GOAL", na.rm = TRUE),
        pct_goal = 100 * .data$goals / .data$shots,
        .groups = "drop"
      ) |>
      # order by best scoring first
      dplyr::arrange(dplyr::desc(.data$pct_goal))

    # lock factor levels to the sorted order so ggplot uses that order
    out$level <- factor(out$level, levels = out$level)
    out
  }

  # shared theme for the percent plots
  pct_theme_shared <- ggplot2::theme_minimal(base_size = 11) + base_theme_bold_titles +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 25, hjust = 1))

  # subset used for pressure and distance plots because those are restricted to these types
  df_pl <- df |>
    dplyr::filter(.data$type %in% c("SHORT", "LONG", "FREE-BALL"))

  # plot 3 scoring percent by pressure
  p_pressure <- {
    pressure_agg <- summarise_pct(df_pl, pressure)
    ggplot2::ggplot(pressure_agg, ggplot2::aes(x = .data$level, y = .data$pct_goal)) +
      ggplot2::geom_col(fill = "steelblue") +
      ggplot2::labs(title = "Scoring % by pressure", x = NULL, y = "Goal percentage") +
      pct_theme_shared
  }

  # plot 4 scoring percent by distance
  p_distance <- {
    distance_agg <- summarise_pct(df_pl, distance_band)
    ggplot2::ggplot(distance_agg, ggplot2::aes(x = .data$level, y = .data$pct_goal)) +
      ggplot2::geom_col(fill = "darkorange") +
      ggplot2::labs(title = "Scoring % by distance", x = NULL, y = NULL) +
      pct_theme_shared +
      # hide y axis to keep small multiples visually lighter
      ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank())
  }

  # plot 5 scoring percent by type
  p_type <- {
    type_agg <- summarise_pct(df, type)
    ggplot2::ggplot(type_agg, ggplot2::aes(x = .data$level, y = .data$pct_goal)) +
      ggplot2::geom_col(fill = "seagreen3") +
      ggplot2::labs(title = "Scoring % by type", x = NULL, y = NULL) +
      pct_theme_shared +
      ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank())
  }

  # plot 6 scoring percent by shot count
  p_shotcount <- {
    shotcount_agg <- summarise_pct(df, shot_count_band)
    ggplot2::ggplot(shotcount_agg, ggplot2::aes(x = .data$level, y = .data$pct_goal)) +
      ggplot2::geom_col(fill = "mediumpurple3") +
      ggplot2::labs(title = "Scoring % by shot count", x = NULL, y = NULL) +
      pct_theme_shared +
      ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank())
  }

  # collect plots in a named list so callers can use them even without patchwork
  plots <- list(
    goals = p_goals,
    leg = p_leg,
    pressure = p_pressure,
    distance = p_distance,
    type = p_type,
    shot_count = p_shotcount
  )

  # if patchwork is installed return a combined dashboard layout
  if (requireNamespace("patchwork", quietly = TRUE)) {

    # layout design using patchwork string notation
    # first row has two plots each spanning two columns
    # second row has four plots each taking one column
    design <- "
AABB
CDEF
"

    dash <- patchwork::wrap_plots(
      A = plots$goals,
      B = plots$leg,
      C = plots$pressure,
      D = plots$distance,
      E = plots$type,
      F = plots$shot_count,
      design = design
    ) +
      # collect guides so legends can be shared if needed
      patchwork::plot_layout(guides = "collect") +
      patchwork::plot_annotation(
        title = "TEAM DASBOARD",
        theme = ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", size = 22, hjust = 0)
        )
      )

    # attempt axis and axis title collection when supported by installed patchwork
    try({
      dash <- dash + patchwork::plot_layout(axes = "collect", axis_titles = "collect")
    }, silent = TRUE)

    return(dash)
  }

  # fallback return value when patchwork is not installed
  plots
}

#' Player dashboard plots (coach overview)
#'
#' Creates a player dashboard as a collection of ggplot objects
#' the goal is a quick descriptive overview of one players shooting data
#'
#' Dashboard contains 5 plots all bar charts
#' 1 scoring percent by pressure only types short long free ball
#' 2 scoring percent by distance only types short long free ball bins less than 1 1 to 3 3 to 6 6 plus
#' 3 scoring percent by type no exclusions
#' 4 scoring percent by shot count bands 1 2 3 4 plus no exclusions
#' 5 scoring percent by leg types short long free ball leg in left right both
#'
#' Missing and onbekend handling
#' - character fields type pressure leg result are normalized to uppercase and separators become hyphen
#' - values equal to onbekend or empty string become na for these fields
#' - rows with missing result are excluded from all goal percentage computations
#' - a goal is defined as result equals goal
#'
#' Player selection
#' - if player is all digits it matches shirt number exactly
#' - otherwise it fuzzy matches on full_name using utils adist and requires a close match
#'
#' @param df A TeamTV shots data.frame.
#' @param player Player name fuzzy match or shirt number exact match.
#' @return If patchwork is available a patchwork object otherwise a named list of ggplot objects.
#' @export
tagr_player_dashboard <- function(df, player) {

  # ensure required packages exist
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Install dplyr", call. = FALSE)
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Install ggplot2", call. = FALSE)

  # validate inputs
  if (!is.data.frame(df)) stop("Column-name mismatch: `df` must be a data.frame.", call. = FALSE)
  if (missing(player) || is.null(player) || trimws(as.character(player)) == "") {
    stop("`player` must be provided (name or shirt number).", call. = FALSE)
  }

  # validate schema and coded values
  validate_teamtv_shots(df)

  # helpers used only inside this function

  # normalize codes to a consistent format
  norm_code <- function(x) {
    x <- trimws(as.character(x))
    x <- toupper(x)
    x <- gsub("[ _]+", "-", x)
    x
  }

  # treat unknown tokens as missing
  as_na_unknown <- function(x) {
    x <- as.character(x)
    x[x %in% c("ONBEKEND", "")] <- NA_character_
    x
  }

  # filter data to one player either by number or fuzzy name match
  filter_player_one <- function(data, player_value) {
    player_chr <- trimws(as.character(player_value))

    # digits means a shirt number match
    if (grepl("^[0-9]+$", player_chr)) {
      out <- data[data$number == player_chr | as.character(data$number) == player_chr, , drop = FALSE]
      if (!nrow(out)) stop("No rows found for number '", player_chr, "'.", call. = FALSE)
      return(out)
    }

    # otherwise fuzzy match on full_name
    nm <- unique(data$full_name)
    nm <- nm[!is.na(nm)]
    if (!length(nm)) stop("No non-missing full_name values in df.", call. = FALSE)

    target <- player_chr
    dists <- utils::adist(tolower(target), tolower(trimws(nm)))

    best_i <- which.min(dists)
    best_name <- nm[best_i]
    best_dist <- dists[best_i]

    # require a close enough match to avoid accidental selection
    if (best_dist > max(3, nchar(target) * 0.4)) {
      stop("No close match for '", player_chr, "'. Closest was '", best_name, "'.", call. = FALSE)
    }

    out <- data[trimws(data$full_name) == best_name, , drop = FALSE]
    if (!nrow(out)) stop("Matched name but found 0 rows (unexpected).", call. = FALSE)
    out
  }

  # shared theme snippet
  base_theme_bold_titles <- ggplot2::theme(
    axis.title.x = ggplot2::element_text(face = "bold"),
    axis.title.y = ggplot2::element_text(face = "bold")
  )

  # required columns for this dashboard
  needed <- c("full_name", "number", "type", "pressure", "leg", "result", "distance", "shot_count")
  missing_cols <- setdiff(needed, names(df))
  if (length(missing_cols)) {
    stop("Column-name mismatch: `df` is missing required columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  # normalize key fields and create derived grouping bands
  df <- df |>
    dplyr::mutate(
      full_name = as.character(.data$full_name),
      number    = as.character(.data$number),
      type      = as_na_unknown(norm_code(.data$type)),
      pressure  = as_na_unknown(norm_code(.data$pressure)),
      leg       = as_na_unknown(norm_code(.data$leg)),
      result    = as_na_unknown(norm_code(.data$result)),
      is_goal   = !is.na(.data$result) & .data$result == "GOAL",
      distance_band = dplyr::case_when(
        is.na(.data$distance) ~ NA_character_,
        .data$distance < 1 ~ "<1 m",
        .data$distance < 3 ~ "1-3 m",
        .data$distance < 6 ~ "3-6 m",
        TRUE ~ "6+ m"
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

  # select player rows
  dfi <- filter_player_one(df, player)

  # pick a readable name for the dashboard title
  nm0 <- unique(dfi$full_name)
  nm0 <- nm0[!is.na(nm0) & trimws(nm0) != ""]
  player_name <- if (length(nm0)) nm0[1] else trimws(as.character(player))

  # pick a readable number for the dashboard title when available
  nr0 <- unique(dfi$number)
  nr0 <- nr0[!is.na(nr0) & trimws(nr0) != ""]
  player_number <- if (length(nr0)) nr0[1] else NA_character_

  # build the main title string
  title_txt <- if (!is.na(player_number)) {
    paste0(player_name, " (#", player_number, ") DASHBOARD")
  } else {
    paste0(player_name, " DASHBOARD")
  }

  # exclude rows with missing result for overall goal percent
  dfi_overall <- dfi[!is.na(dfi$result), , drop = FALSE]
  if (!nrow(dfi_overall)) stop("No rows left for this player after removing missing results.", call. = FALSE)

  # compute overall counts shown in subtitle
  total_shots <- nrow(dfi_overall)
  total_goals <- sum(dfi_overall$result == "GOAL", na.rm = TRUE)
  overall_pct <- if (total_shots > 0) 100 * total_goals / total_shots else NA_real_

  # subtitle shows summary stats for context
  subtitle_txt <- paste0(
    "Shots: ", total_shots,
    " | Goals: ", total_goals,
    " | Goal%: ", sprintf("%.1f", overall_pct)
  )

  # helper for percent plots
  # it groups by a chosen column and computes pct_goal
  summarise_pct <- function(data, level_col) {
    level_col <- rlang::ensym(level_col)

    out <- data |>
      dplyr::filter(!is.na(.data$result), !is.na(!!level_col)) |>
      dplyr::group_by(level = !!level_col) |>
      dplyr::summarise(
        shots = dplyr::n(),
        goals = sum(.data$result == "GOAL", na.rm = TRUE),
        pct_goal = 100 * .data$goals / .data$shots,
        .groups = "drop"
      ) |>
      dplyr::arrange(dplyr::desc(.data$pct_goal))

    if (nrow(out)) out$level <- factor(out$level, levels = out$level)
    out
  }

  # shared style for the percent plots
  pct_theme_shared <- ggplot2::theme_minimal(base_size = 11) +
    base_theme_bold_titles +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 25, hjust = 1))

  # subset for plots that only apply to these shot types
  dfi_pl <- dfi |>
    dplyr::filter(.data$type %in% c("SHORT", "LONG", "FREE-BALL"))

  # plot pressure
  pressure_agg <- summarise_pct(dfi_pl, pressure)
  p_pressure <- ggplot2::ggplot(pressure_agg, ggplot2::aes(x = .data$level, y = .data$pct_goal)) +
    ggplot2::geom_col(fill = "steelblue") +
    ggplot2::labs(title = "Scoring % by pressure", x = NULL, y = "Goal percentage") +
    pct_theme_shared

  # plot distance
  distance_agg <- summarise_pct(dfi_pl, distance_band)
  p_distance <- ggplot2::ggplot(distance_agg, ggplot2::aes(x = .data$level, y = .data$pct_goal)) +
    ggplot2::geom_col(fill = "darkorange") +
    ggplot2::labs(title = "Scoring % by distance", x = NULL, y = NULL) +
    pct_theme_shared +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank())

  # plot type
  type_agg <- summarise_pct(dfi, type)
  p_type <- ggplot2::ggplot(type_agg, ggplot2::aes(x = .data$level, y = .data$pct_goal)) +
    ggplot2::geom_col(fill = "seagreen3") +
    ggplot2::labs(title = "Scoring % by type", x = NULL, y = "Goal percentage") +
    pct_theme_shared

  # plot shot count
  shotcount_agg <- summarise_pct(dfi, shot_count_band)
  p_shotcount <- ggplot2::ggplot(shotcount_agg, ggplot2::aes(x = .data$level, y = .data$pct_goal)) +
    ggplot2::geom_col(fill = "mediumpurple3") +
    ggplot2::labs(title = "Scoring % by shot count", x = NULL, y = NULL) +
    pct_theme_shared +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank())

  # plot scoring percent by leg
  leg_agg <- summarise_pct(
    dfi_pl |>
      dplyr::filter(!is.na(.data$leg), .data$leg %in% c("LEFT", "RIGHT", "BOTH")),
    leg
  )
  p_leg <- ggplot2::ggplot(leg_agg, ggplot2::aes(x = .data$level, y = .data$pct_goal)) +
    ggplot2::geom_col(fill = "goldenrod2") +
    ggplot2::labs(title = "Scoring % by leg", x = NULL, y = NULL) +
    pct_theme_shared +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank())

  # collect plots in a list
  plots <- list(
    pressure = p_pressure,
    distance = p_distance,
    type = p_type,
    shot_count = p_shotcount,
    leg = p_leg
  )

  # if patchwork is installed return a combined layout with title and subtitle
  if (requireNamespace("patchwork", quietly = TRUE)) {

    # layout on a 6 column grid
    # top row two plots each spanning 3 columns
    # bottom row three plots each spanning 2 columns
    design <- "
AAABBB
CCDDEE
"

    dash <- patchwork::wrap_plots(
      A = plots$pressure,
      B = plots$distance,
      C = plots$type,
      D = plots$shot_count,
      E = plots$leg,
      design = design
    ) +
      patchwork::plot_annotation(
        title = title_txt,
        subtitle = subtitle_txt,
        theme = ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", size = 22, hjust = 0),
          plot.subtitle = ggplot2::element_text(size = 12, hjust = 0)
        )
      )

    # attempt axis and axis title collection when supported
    # keep axes so each row can have its own y axis (top row uses pressure, bottom row uses type)
    try({
      dash <- dash + patchwork::plot_layout(axes = "keep", axis_titles = "keep")
    }, silent = TRUE)

    return(dash)
  }

  # fallback return value when patchwork is not installed
  plots
}
