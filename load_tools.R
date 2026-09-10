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

source(file.path(tools_dir, "R", "setup.R"))
source(file.path(tools_dir, "R", "osiics.R"))
source(file.path(tools_dir, "R", "deidentify.R"))
source(file.path(tools_dir, "R", "analysis.R")) 
source(file.path(tools_dir, "R", "tables.R"))
source(file.path(tools_dir, "R", "notes.R"))


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