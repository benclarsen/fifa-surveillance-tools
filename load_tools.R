# Load the surveillance tools ----
#
# Entry point for the shared code. A competition project sources this once,
# from its config.R, having first set `tools_dir` to this folder:
#
#   tools_dir <- "~/.../fifa-surveillance-tools"
#   source(file.path(tools_dir, "load_tools.R"))


## Check the tools folder ----

if (!exists("tools_dir")) {
  stop("`tools_dir` is not set. Define it in config.R before sourcing load_tools.R.")
}

tools_dir <- path.expand(tools_dir)

if (!dir.exists(tools_dir)) {
  stop("`tools_dir` does not exist: ", tools_dir)
}

## Function definitions ----
#
# Everything in R/ is sourced, in alphabetical order. These files define
# functions and constants only - nothing runs on source - so order does not
# matter, and a new file needs no change here.

for (file in sort(list.files(file.path(tools_dir, "R"), pattern = "\\.R$", full.names = TRUE))) {
  source(file)
}

## Packages ----

load_or_install(c(
  "tidyverse",
  "digest",
  "openxlsx",
  "boot",
  "mice",
  "ggrepel",
  "cowplot",
  "png",
  "grid",
  "stringi",
  "flextable",
  "officer",
  "scales",
  "glue"
))


## Reference data ----

osiics_file  <- file.path(tools_dir, "data", "osiics_16_fifa_version.csv")
palette_file <- file.path(tools_dir, "data", "palette_nodes.csv")


message("Surveillance tools loaded from: ", tools_dir)