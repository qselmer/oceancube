args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  stop("Usage: backend-parity.R <installed-library> <result-csv>")
}
library_path <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
.libPaths(c(library_path, .libPaths()))
suppressPackageStartupMessages(library(oceancube, lib.loc = library_path))

repo_root <- normalizePath(file.path(dirname(args[[2L]]), "..", "..", ".."),
                           winslash = "/", mustWork = TRUE)
benchmark <- file.path(repo_root, "dev", "data", "visualization", "benchmark",
                       "oceancube-viz-benchmark-v1.nc")
deferred <- cube_open(benchmark, vars = "pottmp",
                      dataset_id = "oceancube-viz-benchmark-v1")
memory <- cube_collect(deferred)
styles <- c("field", "contour", "field_contour")
result <- do.call(rbind, lapply(styles, function(style) {
  args <- list(
    variable = "pottmp", time = deferred$time[[1L]],
    depth = deferred$depth[[1L]], style = style, na.rm = FALSE
  )
  netcdf_plot <- do.call(viz.map, c(list(x = deferred), args))
  memory_plot <- do.call(viz.map, c(list(x = memory), args))
  data.frame(
    style = toupper(style),
    scientific_table_identical = identical(netcdf_plot$data, memory_plot$data),
    variable_identical = identical(attr(netcdf_plot, "oceancube_variable"),
                                   attr(memory_plot, "oceancube_variable")),
    time_identical = identical(attr(netcdf_plot, "oceancube_time"),
                               attr(memory_plot, "oceancube_time")),
    depth_identical = identical(attr(netcdf_plot, "oceancube_depth"),
                                attr(memory_plot, "oceancube_depth")),
    netcdf_backend = attr(netcdf_plot, "oceancube_backend"),
    memory_backend = attr(memory_plot, "oceancube_backend"),
    status = "PASS", stringsAsFactors = FALSE
  )
}))
result$status[!result$scientific_table_identical | !result$variable_identical |
                !result$time_identical | !result$depth_identical |
                result$netcdf_backend != "netcdf" |
                result$memory_backend != "memory"] <- "FAIL"
utils::write.csv(result, args[[2L]], row.names = FALSE)
if (any(result$status != "PASS")) stop("D2B backend scientific parity failed.")
cat("D2B_MEMORY_NETCDF_STYLE_PARITY=PASS\n")
