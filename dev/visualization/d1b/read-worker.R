args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) {
  stop("Usage: read-worker.R <installed-library> <output-rds> <fixture-helper>")
}
library_path <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
.libPaths(c(library_path, .libPaths()))
suppressPackageStartupMessages(library(oceancube, lib.loc = library_path))
source(args[[3L]], local = TRUE)

file <- make_netcdf_backend_fixture()
on.exit(unlink(file), add = TRUE)
new_storage <- getFromNamespace(".new_netcdf_storage", "oceancube")
new_cube <- getFromNamespace(".new_netcdf_cube", "oceancube")
cube <- new_cube(new_storage(file, c("temperature", "oxygen")))
path <- data.frame(
  longitude = c(-80, -79, -78), latitude = rep(-11, 3L)
)
namespace <- asNamespace("oceancube")

capture_return <- function(target, expression) {
  variable <- paste0(".d1b_capture_", target)
  trace(
    target, where = namespace,
    exit = substitute(
      assign(NAME, returnValue(), envir = .GlobalEnv),
      list(NAME = variable)
    ),
    print = FALSE
  )
  on.exit(untrace(target, where = namespace), add = TRUE)
  force(expression)
  get(variable, envir = .GlobalEnv, inherits = FALSE)
}

extract_read <- function(result) {
  qa <- attr(result, "oceancube_qa", exact = TRUE)
  read <- qa$extraction$netcdf_read
  if (is.null(read) && is.list(qa$transect$physical_reads)) {
    transect <- qa$transect$physical_reads
    read <- list(
      physical_start = NULL,
      physical_count = c(
        path_points = transect$n_points,
        unique_pairs = transect$n_unique_pairs,
        depth = transect$n_depth,
        time = 1L
      ),
      variables = transect$n_variables,
      values_requested = transect$n_values_requested,
      values_in_envelope = transect$n_values_read,
      amplification = transect$read_amplification
    )
  }
  if (is.null(read)) {
    stop("No NetCDF read evidence was captured.")
  }
  read$source_file <- NULL
  read
}

results <- list(
  viz.map = capture_return(
    "cube_extract",
    viz.map(cube, "temperature", cube$time[[1L]], cube$depth[[1L]])
  ),
  viz.profile = capture_return(
    "cube_extract",
    viz.profile(cube, "temperature", -79, -11, cube$time[[1L]])
  ),
  viz.section = capture_return(
    "cube_extract",
    viz.section(cube, "temperature", time = cube$time[[1L]], latitude = -11)
  ),
  viz.transect = capture_return(
    "cube_transect",
    viz.transect(cube, path, "temperature", time = cube$time[[1L]],
                 mode = "section")
  ),
  viz.timeseries = capture_return(
    "cube_extract",
    viz.timeseries(cube, "temperature", -79, -11, cube$depth[[1L]])
  )
)
reads <- lapply(results, extract_read)
saveRDS(reads, args[[2L]], version = 3L)
