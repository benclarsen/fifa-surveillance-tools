# Render the reports ----
#
# Renders the shared template in the tools repo, with this competition's
# settings passed as parameters. The template is never copied - nothing in it
# is competition-specific.
#
# Run after run_all.R and 04_figures.R.

source("config.R")

dir.create("04_Publications/Drafts", recursive = TRUE, showWarnings = FALSE)


## Absolute paths ----
#
# Resolved here, not inside the render() call. Arguments to render() are
# evaluated lazily, and by the time they are forced render() has already changed
# the working directory to the template's own folder - so a relative path
# written inline resolves against the tools repo, not this competition.

results_path <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)
drafts_path  <- normalizePath("04_Publications/Drafts", winslash = "/", mustWork = TRUE)
template     <- file.path(tools_dir, "templates", "report_surveillance.Rmd")

suppression  <- suppression_footnote()
methods      <- methods_text()


## Render ----

rmarkdown::render(
  input       = template,
  output_file = paste0(competition_code, "_surveillance_results.docx"),
  output_dir  = drafts_path,
  params = list(
    competition_name = competition_name,
    results_dir      = results_path,
    suppression_note = suppression,
    methods_note     = methods,
    include_notes    = TRUE
  ),
  envir = new.env()
)

message("\nReport written to ", drafts_path)
