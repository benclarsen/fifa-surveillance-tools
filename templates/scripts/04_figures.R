# Figures ----
#
# The risk matrix, by body area and by tissue type.
#
# Reads the finished analysis caselist and exposure row written by run_all.R,
# rather than rebuilding them. Every exclusion, the taxonomy join and the body
# area recoding happen once, in run_all.R, so the figure cannot be drawn from a
# different set of cases than the tables.
#
# Run after run_all.R.

source("config.R")

start_run_notes()
note_settings(c("competition_code", "excluded_teams", "matrix_top_n",
                "min_n_severity", "figure_font"))


## Data ----

caselist <- read_deid("caselist_analysis.csv") %>%
  filter(problem_type == "Injury")

data_exposure <- readr::read_csv(
  file.path(output_dir, "exposure_summary.csv"),
  show_col_types = FALSE
)

note("Injuries plotted: ", nrow(caselist))
note("Exposure: ", round(data_exposure$total), " hours")


## Matrix data ----

by_area <- create_matrix_data(
  filter(caselist, !is.na(osiics_16_level_1)),
  data_exposure$total,
  "osiics_16_level_1"
)

by_tissue <- create_matrix_data(
  filter(caselist, !is.na(osiics_16_level_2)),
  data_exposure$total,
  "osiics_16_level_2"
)


# Categories with too few cases to report a mean severity are dropped, on the
# same disclosure rule as the tables.

min_n <- get_setting("min_n_severity", 2)

dropped <- bind_rows(
  mutate(filter(by_area,   n_cases < min_n), panel = "Body area"),
  mutate(filter(by_tissue, n_cases < min_n), panel = "Tissue type")
)

if (nrow(dropped) > 0) {
  note_table(
    select(dropped, panel, label, n_cases, y, burden),
    paste0("\nCategories with fewer than ", min_n,
           " cases, excluded from the figure:")
  )
}

by_area <- by_area %>%
  suppress_small_categories() %>%
  slice_head(n = matrix_top_n)

by_tissue <- by_tissue %>%
  suppress_small_categories() %>%
  slice_head(n = matrix_top_n)

note_table(select(by_area,   label, n_cases, x, y, burden), "\nBody areas plotted:")
note_table(select(by_tissue, label, n_cases, x, y, burden), "\nTissue types plotted:")

write_result(by_area,   "matrix_data_body_area")
write_result(by_tissue, "matrix_data_tissue_type")


## Plot ----
#
# Limits are shared across both panels and follow the points. Error bars wider
# than the panel run off the edge.
#
# Isobars are chosen from the burden range across both panels, so the same set
# is drawn on each. Override by passing isobars = c(...).
#
# To hand-place a label, add a row here:
#   label_nudges <- tibble::tribble(
#     ~label,       ~nudge_x, ~nudge_y,
#     "Lower leg",       0.4,      2.0,
#     "Thigh",          -0.3,     -2.0
#   )

label_nudges <- NULL

limits  <- matrix_axis_limits(by_area, by_tissue)
isobars <- choose_isobars(by_area, by_tissue)

note("Axis limits: x to ", round(limits$x, 1), ", y to ", round(limits$y, 1))
note("Isobars drawn at: ", paste(isobars, collapse = ", "), " days per 1000 h")

panel_area <- plot_risk_matrix(
  by_area, limits$x, limits$y,
  isobars = isobars, title = "Body area", nudges = label_nudges
)

panel_tissue <- plot_risk_matrix(
  by_tissue, limits$x, limits$y,
  isobars = isobars, title = "Tissue type", nudges = label_nudges,
  show_y_title = FALSE
)

aligned <- cowplot::align_plots(panel_area, panel_tissue, align = "hv", axis = "tb")

risk_matrix <- cowplot::plot_grid(aligned[[1]], aligned[[2]], ncol = 2, rel_widths = c(1, 1))


## Save ----

ggplot2::ggsave(
  filename = make_output_path("risk_matrix.png"),
  plot     = risk_matrix,
  width    = 16,
  height   = 8.5,
  units    = "cm",
  dpi      = 600
)

note("Wrote ", make_output_path("risk_matrix.png"))

write_run_notes("notes_figures")

message("\nFigures complete.")
