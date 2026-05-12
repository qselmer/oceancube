#' Download a subset from a Copernicus Marine dataset
#'
#' @param dataset_id Copernicus Marine dataset ID.
#' @param vars Character vector of variable short names.
#' @param lon Optional numeric range `c(min, max)`.
#' @param lat Optional numeric range `c(min, max)`.
#' @param time Optional date range `c(start, end)`.
#' @param depth Optional numeric depth range `c(min, max)`.
#' @param outdir Output directory.
#' @param fmt Output format: `netcdf`, `zarr`, `csv`, or `parquet`.
#' @param overwrite Logical. Overwrite existing files?
#' @param skip_existing Logical. Skip if the file already exists?
#' @param dry_run Logical. Ask Copernicus to show the request without downloading.
#' @param filename Optional output filename.
#' @param verbose Logical. Print progress messages?
#'
#' @return Output file path.
#' @export
download_nc <- function(dataset_id, vars, lon = NULL, lat = NULL, time = NULL,
                        depth = NULL, outdir = ".", fmt = c("netcdf", "zarr", "csv", "parquet"),
                        overwrite = FALSE, skip_existing = TRUE, dry_run = FALSE,
                        filename = NULL, verbose = TRUE) {
  fmt <- match.arg(fmt)

  if (!is.character(dataset_id) || length(dataset_id) != 1L) {
    .abort_badarg("dataset_id", "must be a single character string.")
  }
  if (!is.character(vars) || length(vars) < 1L) {
    .abort_badarg("vars", "must be a non-empty character vector.")
  }
  if (!is.null(lon)) .check_range(lon, "lon")
  if (!is.null(lat)) .check_range(lat, "lat")
  if (!is.null(depth)) .check_range(depth, "depth")
  if (!is.null(time) && length(time) != 2L) {
    .abort_badarg("time", "must have length 2.")
  }

  ext <- switch(fmt, netcdf = "nc", zarr = "zarr", csv = "csv", parquet = "parquet")

  outdir <- normalizePath(outdir, mustWork = FALSE)
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

  filename <- filename %||% .make_filename(
    dataset_id = dataset_id,
    vars = vars,
    lon = lon,
    lat = lat,
    time = time,
    depth = depth,
    ext = ext
  )

  outfile <- file.path(outdir, filename)

  if (file.exists(outfile) && isTRUE(skip_existing) && !isTRUE(overwrite)) {
    if (verbose) .message_done("File exists; skipping: ", outfile)
    return(outfile)
  }

  cmt <- reticulate::import("copernicusmarine", delay_load = TRUE)

  args <- list(
    dataset_id = dataset_id,
    variables = as.list(vars),
    output_directory = outdir,
    output_filename = basename(outfile),
    file_format = fmt,
    overwrite = overwrite,
    skip_existing = skip_existing,
    dry_run = dry_run,
    disable_progress_bar = !isTRUE(verbose)
  )

  if (!is.null(lon)) {
    args$minimum_longitude <- lon[1]
    args$maximum_longitude <- lon[2]
  }
  if (!is.null(lat)) {
    args$minimum_latitude <- lat[1]
    args$maximum_latitude <- lat[2]
  }
  if (!is.null(time)) {
    args$start_datetime <- as.character(time[1])
    args$end_datetime <- as.character(time[2])
  }
  if (!is.null(depth)) {
    args$minimum_depth <- depth[1]
    args$maximum_depth <- depth[2]
  }

  if (verbose) {
    .message_info("Downloading dataset: ", dataset_id)
    .message_info("Variables: ", paste(vars, collapse = ", "))
  }

  invisible(do.call(cmt$subset, args))

  if (verbose) .message_done("Download request completed: ", outfile)
  outfile
}
