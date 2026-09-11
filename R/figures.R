# Figures ----
#
# The risk matrix: incidence rate against average severity, with curves of
# equal burden. A point's position relative to those curves is the whole point
# of the figure, so the axes have to be the quantities the curves are drawn
# from.
#
# Severity here is the MEAN time loss per case, not the median. Burden is total
# time loss over exposure, which equals incidence rate times mean severity. With
# a median on the y axis the isobars would not be isobars. The tables report
# medians because they describe a typical case; this figure reports means
# because it decomposes burden. Say so in the caption.


## Matrix data ----

# Incidence rate, mean severity and burden for each category, with confidence
# intervals for both axes.
#
# Both axes are computed from the same set of cases. The previous version took
# the x axis from all cases and the y axis from cases with time loss above zero,
# so the two axes described different case sets.
create_matrix_data <- function(caselist,
                               exposure,
                               grouping_var,
                               replicates = get_setting("boot_replicates", 10000),
                               seed       = get_setting("random_seed", 1)) {
  
  check_exposure(exposure)
  
  summary <- caselist %>%
    group_by(across(all_of(grouping_var))) %>%
    summarise(
      n_cases        = n(),
      total_timeloss = sum(timeloss, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      x      = n_cases / exposure * 1000,
      y      = total_timeloss / n_cases,
      burden = total_timeloss / exposure * 1000
    )
  
  x_ci <- t(vapply(summary$n_cases, poisson_rate_ci, numeric(2), exposure = exposure))
  
  summary$xmin <- x_ci[, 1]
  summary$xmax <- x_ci[, 2]
  
  timeloss_by_group <- split(caselist$timeloss, caselist[[grouping_var]], drop = TRUE)
  
  set.seed(seed)
  
  y_ci <- t(vapply(
    as.character(summary[[grouping_var]]),
    function(group) mean_ci_bootstrap(timeloss_by_group[[group]], replicates),
    numeric(2)
  ))
  
  summary$ymin <- y_ci[, 1]
  summary$ymax <- y_ci[, 2]
  
  summary$label <- as.character(summary[[grouping_var]])
  
  arrange(summary, desc(burden))
}


# Bootstrap percentile interval for a mean. Time-loss days are heavily
# right-skewed, so a normal-theory interval puts the lower bound below zero.
# Returns NA below two cases - a single value cannot be resampled.
mean_ci_bootstrap <- function(values, replicates) {
  
  values <- values[!is.na(values)]
  
  if (length(values) < 2 || stats::var(values) == 0) {
    return(c(ymin = NA_real_, ymax = NA_real_))
  }
  
  draws <- boot::boot(values, function(x, i) mean(x[i]), R = replicates)
  
  bounds <- stats::quantile(draws$t, c(0.025, 0.975), na.rm = TRUE, names = FALSE)
  
  c(ymin = bounds[1], ymax = bounds[2])
}


## Disclosure ----

# Categories with too few cases to report a mean severity are removed.
# The figure plots mean time loss, and at one case that is one player's days
# out - recoverable exactly, since exposure is published alongside. It is also
# the only case where no confidence interval can be drawn, so the point would
# read as certain. Same threshold as the tables.
suppress_small_categories <- function(matrix_data,
                                      min_n = get_setting("min_n_severity", 2)) {
  
  filter(matrix_data, n_cases >= min_n)
}


## Axis limits ----

# Limits for the panels given.
#
# By default they follow the POINTS, not the error bars. Bars wider than the
# panel run off the edge, which reads as extending beyond it. Sizing the panel
# to the widest interval instead makes the whole plot taller than the data and
# crushes the isobar labels into the corner.
#
# The limit is padded past the last axis break, so that break's tick mark does
# not sit exactly on the panel border and appear to stick out of the corner.
#
# Set include_intervals = TRUE to fit every bar inside.
matrix_axis_limits <- function(..., expand = 1.15, include_intervals = FALSE) {
  
  data <- bind_rows(...)
  
  x_values <- if (include_intervals) c(data$x, data$xmax) else data$x
  y_values <- if (include_intervals) c(data$y, data$ymax) else data$y
  
  list(
    x = pad_past_last_break(max(x_values, na.rm = TRUE) * expand),
    y = pad_past_last_break(max(y_values, na.rm = TRUE) * expand)
  )
}


pad_past_last_break <- function(value) {
  
  breaks <- scales::breaks_pretty(6)(c(0, value))
  step   <- diff(breaks)[1]
  
  value + 0.08 * step
}


## Isobars ----

# Burden values to draw, chosen from the range in the data on a 1-2-5 scale.
# Replaces the hard-coded values the old script marked "ADAPT TO DATA".
choose_isobars <- function(..., n = 4) {
  
  burden <- bind_rows(...)$burden
  burden <- burden[is.finite(burden) & burden > 0]
  
  if (length(burden) == 0) return(numeric(0))
  
  candidates <- sort(as.vector(outer(c(1, 2, 5), 10^(-2:5))))
  
  keep <- candidates[candidates >= min(burden) * 0.5 &
                       candidates <= max(burden) * 2]
  
  if (length(keep) == 0) {
    return(candidates[which.min(abs(candidates - stats::median(burden)))])
  }
  
  utils::tail(keep, n)
}


# A curve of equal burden, labelled where it meets the top of the panel.
burden_isobar <- function(value, xlim, ylim) {
  
  list(
    ggplot2::geom_function(
      fun       = function(x) value / x,
      colour    = "black",
      linewidth = 0.2,
      alpha     = 0.3,
      n         = 1000,
      xlim      = c(value / ylim, xlim)
    ),
    ggplot2::annotate(
      "text",
      x     = value / ylim - 0.005 * xlim,
      y     = 0.97 * ylim,
      label = as.character(value),
      size  = 1.8,
      alpha = 0.5,
      hjust = 1
    )
  )
}


## Background ----

# A diagonal gradient, darkest where burden is highest. Drawn directly as a
# raster rather than rendered to a PNG and read back in.
matrix_background <- function(low = "#C6D9FF", high = "#3A92FF", n = 256) {
  
  ramp <- grDevices::colorRampPalette(c(low, high))(n)
  
  columns <- seq(0, 1, length.out = n)   # x, left to right
  rows    <- seq(1, 0, length.out = n)   # y, top row is the highest value
  
  value <- outer(rows, columns, `+`) / 2
  
  matrix(ramp[round(value * (n - 1)) + 1], nrow = n)
}


## Panel ----

# One panel of the risk matrix.
#
# `nudges` optionally hand-places labels: a data frame with columns label,
# nudge_x, nudge_y. Only the labels named need appear.
#
# `show_y_axis = FALSE` drops the y axis title, text and ticks, for the right
# panel of a pair that shares limits.
# One panel of the risk matrix.
#
# `nudges` optionally hand-places labels: a data frame with columns label,
# nudge_x, nudge_y. Only the labels named need appear.
#
# `show_y_title = FALSE` drops the axis title but keeps ticks and tick labels,
# for the right panel of a pair that shares limits - the scale still needs
# reading, the title does not need repeating.

# One panel of the risk matrix.
#
# `nudges` optionally hand-places labels: a data frame with columns label,
# nudge_x, nudge_y. Only the labels named need appear.
#
# `show_y_title = FALSE` drops the axis title but keeps ticks and tick labels,
# for the right panel of a pair that shares limits - the scale still needs
# reading, the title does not need repeating.


plot_risk_matrix <- function(matrix_data,
                             xlim,
                             ylim,
                             isobars        = NULL,
                             title          = NULL,
                             nudges         = NULL,
                             show_y_title   = TRUE,
                             show_y_labels  = TRUE,
                             font           = get_setting("figure_font", ""),
                             label_seed     = get_setting("random_seed", 1)) {
  
  if (is.null(isobars)) isobars <- choose_isobars(matrix_data)
  
  text_family <- if (nzchar(font)) font else ""
  
  if (!is.null(nudges)) {
    matrix_data <- left_join(matrix_data, nudges, by = "label")
  }
  
  if (!"nudge_x" %in% names(matrix_data)) matrix_data$nudge_x <- 0
  if (!"nudge_y" %in% names(matrix_data)) matrix_data$nudge_y <- 0
  
  matrix_data$nudge_x <- dplyr::coalesce(matrix_data$nudge_x, 0)
  matrix_data$nudge_y <- dplyr::coalesce(matrix_data$nudge_y, 0) + 0.03 * ylim
  
  plot <- ggplot2::ggplot(matrix_data, ggplot2::aes(x = x, y = y)) +
    
    ggplot2::annotation_raster(
      matrix_background(),
      xmin = 0, xmax = xlim, ymin = 0, ymax = ylim,
      interpolate = TRUE
    ) +
    
    do.call(c, lapply(isobars, burden_isobar, xlim = xlim, ylim = ylim)) +
    
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = ymin, ymax = ymax),
      width = 0, colour = "black", linewidth = 0.3, na.rm = TRUE
    ) +
    
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = xmin, xmax = xmax),
      orientation = "y",
      width = 0, colour = "black", linewidth = 0.3, na.rm = TRUE
    ) +
    
    ggplot2::geom_point(fill = "white", shape = 21, size = 2.1, colour = "black") +
    
    ggrepel::geom_text_repel(
      ggplot2::aes(label = label),
      size               = 2.3,
      nudge_x            = matrix_data$nudge_x,
      nudge_y            = matrix_data$nudge_y,
      segment.alpha      = 0.5,
      segment.size       = 0.2,
      point.padding      = 0.3,
      box.padding        = 0.4,
      min.segment.length = 0.2,
      max.overlaps       = Inf,
      seed               = label_seed,
      colour             = "black",
      family             = text_family
    ) +
    
    # Limits go on the coordinate system, not the scales, so an error bar wider
    # than the panel is drawn to the edge rather than dropped.
    ggplot2::coord_cartesian(xlim = c(0, xlim), ylim = c(0, ylim), expand = FALSE) +
    
    ggplot2::scale_x_continuous(
      breaks = scales::breaks_pretty(6),
      name   = "Incidence rate (per 1000 h)"
    ) +
    
    ggplot2::scale_y_continuous(
      breaks = scales::breaks_pretty(6),
      name   = "Average time loss (days)"
    ) +
    
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.margin       = ggplot2::unit(c(0.3, 0.05, 0.3, 0.3), "cm"),
      panel.border      = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 0.8),
      axis.text.x       = ggplot2::element_text(size = 8, colour = "black",
                                                margin = ggplot2::margin(t = 3)),
      axis.title.x      = ggplot2::element_text(size = 8, colour = "black",
                                                margin = ggplot2::margin(t = 7)),
      axis.text.y       = ggplot2::element_text(size = 8, colour = "black",
                                                margin = ggplot2::margin(r = 3)),
      axis.title.y      = ggplot2::element_text(size = 8, colour = "black", angle = 90,
                                                vjust = 1, margin = ggplot2::margin(r = 7)),
      axis.ticks.length = ggplot2::unit(0.1, "cm"),
      axis.ticks.x      = ggplot2::element_line(colour = "black"),
      axis.ticks.y      = ggplot2::element_line(colour = "black"),
      text              = ggplot2::element_text(family = text_family),
      legend.position   = "none",
      aspect.ratio      = 1
    )
  
  if (!show_y_labels) {
    plot <- plot + ggplot2::theme(
      axis.text.y  = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    )
  }
  
  if (!show_y_title) {
    plot <- plot + ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      plot.margin  = ggplot2::unit(c(0.3, 0.3, 0.3, 0.05), "cm")
    )
  }
  
  if (!is.null(title)) {
    plot <- plot +
      ggplot2::labs(title = title) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 9, colour = "black", hjust = 0,
                                           face = "bold",
                                           margin = ggplot2::margin(b = 8))
      )
  }
  
  plot
}