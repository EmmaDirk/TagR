#' @importFrom ggplot2 ggplot aes geom_point coord_fixed theme_void
#'   geom_rect geom_polygon labs scale_color_manual theme margin element_text
#' @importFrom dplyr mutate
#' @importFrom utils adist
NULL

# keep data pronoun registered so r cmd check does not warn
utils::globalVariables(".data")

# ------------------------------------------------------------------------------
# pitch geometry and helpers
# ------------------------------------------------------------------------------

# store all pitch constants in one place
# this defines the fixed half pitch coordinate system used for plotting
tagr_pitch_settings <- function() {
  list(
    # court dimensions
    court_width = 20,
    court_length = 20,

    # plot bounds in half pitch frame
    x_min = -10,
    x_max =  10,
    y_min = 0,
    y_max = 20,

    # korf placement in this frame
    korf_from_backline = 6.7,

    # free pass area geometry
    area_width = 5,
    area_length = 7.5,
    area_to_backline = 2.5,
    area_to_midline = 5,

    # constant shift to map teamtv coordinates into the half pitch frame
    shift_x = 0,
    shift_y = 10
  )
}

# normalize category codes for robust comparisons
# trims spaces drops empty values uppercases and normalizes separators
tagr_norm_code <- function(x) {
  # keep null as null so callers can skip filters cleanly
  if (is.null(x)) return(NULL)

  # convert to character and trim whitespace
  x <- trimws(as.character(x))

  # drop empty strings
  x <- x[nzchar(x)]
  if (!length(x)) return(NULL)

  # compare in uppercase to avoid case differences
  x <- toupper(x)

  # normalize spaces and underscores to dashes so values match consistently
  x <- gsub("[ _]+", "-", x)

  x
}

# filter by one or more players
# player can contain numbers names or a mix
# numbers match df number
# names are matched to df full_name using edit distance
tagr_filter_player <- function(df, player, max_dist = 3) {
  # if no player filter requested return df unchanged
  if (is.null(player)) return(df)

  # standardize player inputs and drop empties
  players <- trimws(as.character(player))
  players <- players[nzchar(players)]
  if (!length(players)) return(df)

  # match one player token and return the subset of rows
  match_one <- function(p) {
    # treat only digits as a shirt number
    if (grepl("^[0-9]+$", p)) {
      out <- df[df$number == p, , drop = FALSE]
      if (!nrow(out)) stop("No rows found for number '", p, "'.", call. = FALSE)
      return(out)
    }

    # otherwise treat as a name and fuzzy match to full_name
    nm <- unique(df$full_name)
    nm <- nm[!is.na(nm)]
    if (!length(nm)) stop("No non-missing full_name values in df.", call. = FALSE)

    # compute edit distances and select the closest name
    dists <- utils::adist(tolower(p), tolower(trimws(nm)))
    best_i <- which.min(dists)
    best_name <- nm[best_i]
    best_dist <- dists[best_i]

    # if the closest is still too far away stop with a helpful message
    if (best_dist > max_dist) {
      stop("No close match for '", p, "'. Closest was '", best_name, "'.", call. = FALSE)
    }

    # keep rows for the matched name
    out <- df[trimws(df$full_name) == best_name, , drop = FALSE]
    if (!nrow(out)) stop("Matched name but found 0 rows (unexpected).", call. = FALSE)
    out
  }

  # apply matching for each requested player and combine results
  out <- unique(do.call(rbind, lapply(players, match_one)))
  if (!nrow(out)) stop("No rows left after filtering players.", call. = FALSE)

  out
}

# apply optional filters while skipping the variable used for colouring
# this avoids filtering away levels that you want to display in the legend
tagr_apply_filters <- function(df,
                               player = NULL,
                               type = NULL,
                               pressure = NULL,
                               leg = NULL,
                               result = NULL,
                               colour_by = NULL) {

  # apply player filter first
  df <- tagr_filter_player(df, player)

  # filter type when requested and not used for colour mapping
  if (!is.null(type) && colour_by != "type") {
    want <- tagr_norm_code(type)
    got <- tagr_norm_code(df$type)
    # drop missing values before matching
    df <- df[!is.na(got) & got %in% want, , drop = FALSE]
  }

  # filter pressure when requested and not used for colour mapping
  if (!is.null(pressure) && colour_by != "pressure") {
    want <- tagr_norm_code(pressure)
    got <- tagr_norm_code(df$pressure)
    df <- df[!is.na(got) & got %in% want, , drop = FALSE]
  }

  # filter leg when requested and not used for colour mapping
  if (!is.null(leg) && colour_by != "leg") {
    want <- tagr_norm_code(leg)
    got <- tagr_norm_code(df$leg)
    df <- df[!is.na(got) & got %in% want, , drop = FALSE]
  }

  # filter result when requested and not used for colour mapping
  if (!is.null(result) && colour_by != "result") {
    want <- tagr_norm_code(result)
    got <- tagr_norm_code(df$result)
    df <- df[!is.na(got) & got %in% want, , drop = FALSE]
  }

  # fail early if nothing remains
  if (!nrow(df)) stop("No rows left after filtering.", call. = FALSE)
  df
}

# shift coordinates into the half pitch frame and clamp outliers to the border
# rows with missing x or y are removed before transforming
tagr_transform_xy <- function(df) {
  s <- tagr_pitch_settings()

  # keep only rows with usable coordinates
  df <- df[!is.na(df$x) & !is.na(df$y), , drop = FALSE]
  if (!nrow(df)) stop("No non-missing x/y values to plot.", call. = FALSE)

  # apply constant shift from teamtv coordinates to the half pitch frame
  df$x <- df$x + s$shift_x
  df$y <- df$y + s$shift_y

  # clamp values so points always fall within the drawn pitch area
  df$x <- pmin(pmax(df$x, s$x_min), s$x_max)
  df$y <- pmin(pmax(df$y, s$y_min), s$y_max)

  df
}

# draw the fixed half pitch background
# returns a ggplot object with court free pass area and korf marker
tagr_half_pitch_background <- function() {
  s <- tagr_pitch_settings()

  # rectangular court boundary
  court <- data.frame(
    xmin = s$x_min,
    xmax = s$x_max,
    ymin = s$y_min,
    ymax = s$y_max
  )

  # korf location in this half pitch frame
  korf <- data.frame(
    x = 0,
    y = s$court_length - s$korf_from_backline
  )

  # build a rounded free pass area polygon
  r <- s$area_width / 2
  straight <- s$area_length - 2 * r

  # guard against invalid geometry
  if (straight < 0) stop("Free-pass area length must be at least its width.", call. = FALSE)

  # position the area based on distances from midline and backline
  area_center_y <- korf$y - (s$area_to_midline - s$area_to_backline) / 2
  y_top_c <- area_center_y + straight / 2
  y_bot_c <- area_center_y - straight / 2

  # sample points along the arcs
  theta_top <- seq(0, pi, length.out = 200)
  theta_bot <- seq(pi, 2 * pi, length.out = 200)

  # top semicircle
  top_arc <- data.frame(
    x = r * cos(theta_top),
    y = y_top_c + r * sin(theta_top)
  )

  # left side line segment
  left_side <- data.frame(
    x = rep(-r, 2),
    y = c(y_top_c, y_bot_c)
  )

  # bottom semicircle
  bottom_arc <- data.frame(
    x = r * cos(theta_bot),
    y = y_bot_c + r * sin(theta_bot)
  )

  # combine into a single polygon path
  area_poly <- rbind(top_arc, left_side, bottom_arc)

  # build the background plot
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
    # fixed aspect ratio so geometry is not distorted
    ggplot2::coord_fixed(xlim = c(s$x_min, s$x_max), ylim = c(s$y_min, s$y_max), clip = "off") +
    ggplot2::theme_void()
}

# ------------------------------------------------------------------------------
# public function
# ------------------------------------------------------------------------------

#' Shooting heatmap
#'
#' Plots shot x/y coordinates on a fixed half korfball pitch background.
#' Points are coloured by one of: result, type, pressure, leg.
#'
#' Missing data handling:
#' - rows with missing x or y are removed before plotting
#' - for type pressure leg result filters missing values in that column are excluded
#' - if player is a name and df full_name is entirely missing an error is raised
#' - if player is a number and no rows match that number an error is raised
#' - after transforming coordinates out of bounds x y values are clamped to the pitch border
#'
#' @param df A TeamTV shots data.frame.
#' @param colour_by What to colour points by. One of "result", "type", "pressure", "leg".
#' @param filter Optional named list of filters. Example: list(result = "goal", pressure = "HIGH").
#'   Note: if you supply a filter for the same variable as `colour_by`, it is ignored.
#' @param player Optional. One or more player identifiers (numbers and/or names).
#'   Examples: "11", c("11","24"), "Jane Doe", c("Jane Doe","24").
#' @return A ggplot object.
#' @export
tagr_heatmap <- function(df,
                         colour_by = c("result", "type", "pressure", "leg"),
                         filter = list(),
                         player = NULL) {

  # ensure required packages are installed
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Install ggplot2", call. = FALSE)
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Install dplyr", call. = FALSE)

  # validate expected columns and basic types
  validate_teamtv_shots(df)

  # select the colouring variable
  colour_by <- match.arg(colour_by)
  colour_by <- tolower(colour_by)

  # validate filter input
  if (is.null(filter)) filter <- list()
  if (!is.list(filter)) {
    stop("`filter` must be a named list, e.g. filter = list(result = 'goal').", call. = FALSE)
  }

  # only allow these filter names
  allowed <- c("type", "pressure", "leg", "result")
  bad <- setdiff(names(filter), allowed)
  if (length(bad)) {
    stop(
      "Unknown filter name(s): ", paste(bad, collapse = ", "),
      ". Allowed: ", paste(allowed, collapse = ", "), ".",
      call. = FALSE
    )
  }

  # unpack filters into variables
  type     <- filter$type
  pressure <- filter$pressure
  leg      <- filter$leg
  result   <- filter$result

  # apply filters and player selection
  df <- tagr_apply_filters(
    df,
    player = player,
    type = type,
    pressure = pressure,
    leg = leg,
    result = result,
    colour_by = colour_by
  )

  # transform coordinates into plotting frame
  df <- tagr_transform_xy(df)

  # standardize result labels when colouring by result
  if (colour_by == "result") {
    df <- dplyr::mutate(df, result = tolower(.data$result))
  }

  # subtitle describing the colour mapping
  subtitle <- paste0("Colour by ", colour_by)

  # caption listing which players are included
  caption <- "Included player(s): All players"
  if (!is.null(player)) {
    nums <- unique(df$number)
    nums <- nums[!is.na(nums)]

    if (length(nums)) {
      nums_int <- suppressWarnings(as.integer(nums))
      if (all(!is.na(nums_int))) {
        nums <- as.character(sort(nums_int))
      } else {
        nums <- sort(as.character(nums))
      }
      caption <- paste0("Included player(s): ", paste0("#", nums, collapse = ", "))
    }
  }

  # build plot layers
  p <- tagr_half_pitch_background() +
    ggplot2::geom_point(
      data = df,
      ggplot2::aes(x = .data$x, y = .data$y, color = .data[[colour_by]]),
      alpha = 0.8,
      size = 2
    ) +
    ggplot2::labs(
      title = "SHOOTING HEATMAP",
      subtitle = subtitle,
      caption = caption,
      color = NULL
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      legend.position = "top",
      legend.justification = "left",
      legend.box.just = "left",
      plot.caption = ggplot2::element_text(hjust = 0),
      plot.margin = ggplot2::margin(10, 10, 10, 10)
    )

  # set consistent colours when colouring by result
  if (colour_by == "result") {
    p <- p + ggplot2::scale_color_manual(
      values = c(goal = "green3", miss = "red3", onbekend = "grey60"),
      breaks = c("goal", "miss", "onbekend"),
      labels = c("Goal", "Miss", "Onbekend")
    )
  }

  p
}
