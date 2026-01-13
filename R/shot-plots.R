#' @importFrom ggplot2 ggplot aes geom_point coord_fixed theme_void
#'   geom_rect geom_polygon labs scale_color_manual theme margin
#' @importFrom dplyr mutate
#' @importFrom utils adist
NULL

utils::globalVariables(".data")

# define fixed half-pitch geometry and a fixed coordinate transform for TeamTV shots
tagr_pitch_settings <- function() {
  list(
    court_width = 20,
    court_length = 20,
    x_min = -10,
    x_max =  10,
    y_min = 0,
    y_max = 20,
    korf_from_backline = 6.7,
    area_width = 5,
    area_length = 7.5,
    area_to_backline = 2.5,
    area_to_midline = 5,
    shift_x = 0,
    shift_y = 10
  )
}

# normalize categorical codes for comparisons
tagr_norm_code <- function(x) {
  if (is.null(x)) return(NULL)
  x <- trimws(as.character(x))
  x <- toupper(x)
  x <- gsub("[ _]+", "-", x)
  x
}

# fuzzy match player by name or match by number
tagr_filter_player <- function(df, player, max_dist = 3) {
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

  if (best_dist > max_dist) {
    stop("No close match for '", player_chr, "'. Closest was '", best_name, "'.", call. = FALSE)
  }

  out <- df[trimws(df$full_name) == best_name, , drop = FALSE]
  if (!nrow(out)) stop("Matched name but found 0 rows (unexpected).", call. = FALSE)
  out
}

# apply optional filters, skipping the variable used for colouring
tagr_apply_filters <- function(df, player = NULL, type = NULL, pressure = NULL, leg = NULL, result = NULL, colour_by = NULL) {

  df <- tagr_filter_player(df, player)

  if (!is.null(type) && colour_by != "type") {
    want <- tagr_norm_code(type)
    got <- tagr_norm_code(df$type)
    df <- df[!is.na(got) & got %in% want, , drop = FALSE]
  }

  if (!is.null(pressure) && colour_by != "pressure") {
    want <- tagr_norm_code(pressure)
    got <- tagr_norm_code(df$pressure)
    df <- df[!is.na(got) & got %in% want, , drop = FALSE]
  }

  if (!is.null(leg) && colour_by != "leg") {
    want <- tagr_norm_code(leg)
    got <- tagr_norm_code(df$leg)
    df <- df[!is.na(got) & got %in% want, , drop = FALSE]
  }

  if (!is.null(result) && colour_by != "result") {
    want <- tagr_norm_code(result)
    got <- tagr_norm_code(df$result)
    df <- df[!is.na(got) & got %in% want, , drop = FALSE]
  }

  if (!nrow(df)) stop("No rows left after filtering.", call. = FALSE)
  df
}

# shift coordinates to the half-pitch frame and clamp outliers to the border
tagr_transform_xy <- function(df) {
  s <- tagr_pitch_settings()

  df <- df[!is.na(df$x) & !is.na(df$y), , drop = FALSE]
  if (!nrow(df)) stop("No non-missing x/y values to plot.", call. = FALSE)

  df$x <- df$x + s$shift_x
  df$y <- df$y + s$shift_y

  df$x <- pmin(pmax(df$x, s$x_min), s$x_max)
  df$y <- pmin(pmax(df$y, s$y_min), s$y_max)

  df
}

# draw the fixed half-pitch background
tagr_half_pitch_background <- function() {
  s <- tagr_pitch_settings()

  court <- data.frame(
    xmin = s$x_min,
    xmax = s$x_max,
    ymin = s$y_min,
    ymax = s$y_max
  )

  korf <- data.frame(
    x = 0,
    y = s$court_length - s$korf_from_backline
  )

  r <- s$area_width / 2
  straight <- s$area_length - 2 * r
  if (straight < 0) stop("Free-pass area length must be at least its width.", call. = FALSE)

  area_center_y <- korf$y - (s$area_to_midline - s$area_to_backline) / 2
  y_top_c <- area_center_y + straight / 2
  y_bot_c <- area_center_y - straight / 2

  theta_top <- seq(0, pi, length.out = 200)
  theta_bot <- seq(pi, 2 * pi, length.out = 200)

  top_arc <- data.frame(
    x = r * cos(theta_top),
    y = y_top_c + r * sin(theta_top)
  )

  left_side <- data.frame(
    x = rep(-r, 2),
    y = c(y_top_c, y_bot_c)
  )

  bottom_arc <- data.frame(
    x = r * cos(theta_bot),
    y = y_bot_c + r * sin(theta_bot)
  )

  area_poly <- rbind(top_arc, left_side, bottom_arc)

  ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = court,
      ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin, ymax = .data$ymax),
      fill = "lightblue",
      color = "grey50",
      linewidth = 0.7,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_polygon(
      data = area_poly,
      ggplot2::aes(x = .data$x, y = .data$y),
      fill = "yellow",
      color = "black",
      linewidth = 0.9,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_point(
      data = korf,
      ggplot2::aes(x = .data$x, y = .data$y),
      shape = 21,
      size = 3,
      stroke = 1,
      fill = "lightblue",
      color = "grey50",
      inherit.aes = FALSE
    ) +
    ggplot2::coord_fixed(xlim = c(s$x_min, s$x_max), ylim = c(s$y_min, s$y_max), clip = "off") +
    ggplot2::theme_void()
}

#' Plot shot locations coloured by result
#'
#' Plots shot x/y coordinates on a fixed half korfball pitch background.
#' Points are coloured by result (goal or miss).
#'
#' @param df A TeamTV shots data.frame.
#' @param player Optional. Player name (fuzzy match) or shirt number (exact match).
#' @param type Optional. Filter on shot type.
#' @param pressure Optional. Filter on pressure.
#' @param leg Optional. Filter on leg.
#' @return A ggplot object.
#' @export
tagr_plot_shots_result <- function(df, player = NULL, type = NULL, pressure = NULL, leg = NULL) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Install ggplot2", call. = FALSE)
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Install dplyr", call. = FALSE)

  validate_teamtv_shots(df)

  df <- tagr_apply_filters(df, player = player, type = type, pressure = pressure, leg = leg, colour_by = "result")
  df <- tagr_transform_xy(df)
  df <- dplyr::mutate(df, result = tolower(.data$result))

  tagr_half_pitch_background() +
    ggplot2::geom_point(
      data = df,
      ggplot2::aes(x = .data$x, y = .data$y, color = .data$result),
      alpha = 0.8,
      size = 2
    ) +
    ggplot2::scale_color_manual(
      values = c(goal = "green3", miss = "red3", onbekend = "grey60"),
      breaks = c("goal", "miss", "onbekend"),
      labels = c("Goal", "Miss", "Onbekend")
    ) +
    ggplot2::labs(
      title = "Shot locations",
      subtitle = "Colour shows result",
      color = NULL
    ) +
    ggplot2::theme(
      legend.position = "top",
      plot.margin = ggplot2::margin(10, 10, 10, 10)
    )
}

#' Plot shot locations coloured by shot type
#'
#' Plots shot x/y coordinates on a fixed half korfball pitch background.
#' Points are coloured by shot type.
#'
#' @param df A TeamTV shots data.frame.
#' @param player Optional. Player name (fuzzy match) or shirt number (exact match).
#' @param result Optional. Filter on result.
#' @param pressure Optional. Filter on pressure.
#' @param leg Optional. Filter on leg.
#' @return A ggplot object.
#' @export
tagr_plot_shots_type <- function(df, player = NULL, result = NULL, pressure = NULL, leg = NULL) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Install ggplot2", call. = FALSE)
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Install dplyr", call. = FALSE)

  validate_teamtv_shots(df)

  df <- tagr_apply_filters(df, player = player, result = result, pressure = pressure, leg = leg, colour_by = "type")
  df <- tagr_transform_xy(df)

  tagr_half_pitch_background() +
    ggplot2::geom_point(
      data = df,
      ggplot2::aes(x = .data$x, y = .data$y, color = .data$type),
      alpha = 0.8,
      size = 2
    ) +
    ggplot2::labs(
      title = "Shot locations",
      subtitle = "Colour shows shot type",
      color = NULL
    ) +
    ggplot2::theme(
      legend.position = "top",
      plot.margin = ggplot2::margin(10, 10, 10, 10)
    )
}

#' Plot shot locations coloured by pressure
#'
#' Plots shot x/y coordinates on a fixed half korfball pitch background.
#' Points are coloured by pressure.
#'
#' @param df A TeamTV shots data.frame.
#' @param player Optional. Player name (fuzzy match) or shirt number (exact match).
#' @param result Optional. Filter on result.
#' @param type Optional. Filter on shot type.
#' @param leg Optional. Filter on leg.
#' @return A ggplot object.
#' @export
tagr_plot_shots_pressure <- function(df, player = NULL, result = NULL, type = NULL, leg = NULL) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Install ggplot2", call. = FALSE)
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Install dplyr", call. = FALSE)

  validate_teamtv_shots(df)

  df <- tagr_apply_filters(df, player = player, result = result, type = type, leg = leg, colour_by = "pressure")
  df <- tagr_transform_xy(df)

  tagr_half_pitch_background() +
    ggplot2::geom_point(
      data = df,
      ggplot2::aes(x = .data$x, y = .data$y, color = .data$pressure),
      alpha = 0.8,
      size = 2
    ) +
    ggplot2::labs(
      title = "Shot locations",
      subtitle = "Colour shows pressure",
      color = NULL
    ) +
    ggplot2::theme(
      legend.position = "top",
      plot.margin = ggplot2::margin(10, 10, 10, 10)
    )
}

#' Plot shot locations coloured by leg
#'
#' Plots shot x/y coordinates on a fixed half korfball pitch background.
#' Points are coloured by leg.
#'
#' @param df A TeamTV shots data.frame.
#' @param player Optional. Player name (fuzzy match) or shirt number (exact match).
#' @param result Optional. Filter on result.
#' @param type Optional. Filter on shot type.
#' @param pressure Optional. Filter on pressure.
#' @return A ggplot object.
#' @export
tagr_plot_shots_leg <- function(df, player = NULL, result = NULL, type = NULL, pressure = NULL) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Install ggplot2", call. = FALSE)
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Install dplyr", call. = FALSE)

  validate_teamtv_shots(df)

  df <- tagr_apply_filters(df, player = player, result = result, type = type, pressure = pressure, colour_by = "leg")
  df <- tagr_transform_xy(df)

  tagr_half_pitch_background() +
    ggplot2::geom_point(
      data = df,
      ggplot2::aes(x = .data$x, y = .data$y, color = .data$leg),
      alpha = 0.8,
      size = 2
    ) +
    ggplot2::labs(
      title = "Shot locations",
      subtitle = "Colour shows leg",
      color = NULL
    ) +
    ggplot2::theme(
      legend.position = "top",
      plot.margin = ggplot2::margin(10, 10, 10, 10)
    )
}
