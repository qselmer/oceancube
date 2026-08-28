#' Open a local NetCDF file as a deferred ocean cube
#'
#' `cube_open()` is an experimental source-opening API in the oceancube 0.3.0
#' development cycle. It opens one existing local NetCDF file as a read-only
#' deferred NetCDF backend. Construction reads structural metadata and the
#' coordinate values required by the canonical cube, but it does not read
#' scientific variable arrays or create `x$data`.
#'
#' The returned object has the usual `c("ocean_cube", "list")` class. Its
#' internal, versioned `x$storage` descriptor records how bounded I/O can be
#' performed. The descriptor schema is implementation metadata and is not a
#' stable public API. Independently, `x$metadata$cf` preserves the source CF
#' metadata in a backend-independent internal schema. No NetCDF connection is
#' retained in either object: each
#' scientific read validates the source, opens it, reads the required block,
#' and closes it.
#'
#' This is deferred I/O with bounded reads for supported operations, not a
#' lazy computation graph. Use [cube_collect()] when an independent in-memory
#' cube is required. A serialized deferred cube remains usable while its source
#' exists at the same path with the same size, modification time, and compatible
#' physical schema. Moving, deleting, or changing that source causes a
#' deterministic error when an operation next needs it.
#'
#' @param file A single non-empty path to an existing local NetCDF file. URLs,
#'   directories, and multiple files are not supported.
#' @param vars A character vector of unique data-variable names, or `NULL`.
#'   `NULL` discovers all non-coordinate data variables from NetCDF metadata in
#'   source order. The complete discovered set must share one compatible
#'   rectilinear cube; incompatible variables cause an error rather than being
#'   silently omitted.
#' @param lon_name,lat_name,depth_name,time_name Optional explicit physical
#'   dimension names. Explicit mappings take precedence over the backend's CF
#'   attribute evidence and known-name fallback.
#' @param source A non-empty source label stored with ingestion metadata.
#' @param dataset_id An optional non-empty dataset identifier.
#'
#' @return An `ocean_cube` with a read-only NetCDF storage backend and no
#'   materialized `data` component.
#'
#' @examples
#' \dontrun{
#' x <- cube_open("ocean.nc", vars = "sst")
#' cube_inspect(x)
#'
#' small <- cube_crop(
#'   x,
#'   longitude = c(-80, -78),
#'   latitude = c(-12, -10)
#' )
#'
#' values <- cube_extract(
#'   x,
#'   longitude = -79,
#'   latitude = -11,
#'   match = "nearest",
#'   mode = "series"
#' )
#'
#' mem <- cube_collect(x)
#' }
#'
#' @seealso [read_nc()], [cube_collect()], [cube_inspect()], [cube_crop()],
#'   [cube_extract()]
#' @export
cube_open <- function(file, vars = NULL, lon_name = NULL, lat_name = NULL,
                      depth_name = NULL, time_name = NULL, source = "netcdf",
                      dataset_id = NULL) {
  cf <- .cf_scan_netcdf(file)
  storage <- .new_netcdf_storage(
    file = file,
    variables = vars,
    lon_name = lon_name,
    lat_name = lat_name,
    depth_name = depth_name,
    time_name = time_name,
    source = source,
    dataset_id = dataset_id,
    cf_metadata = cf
  )
  metadata <- .cf_metadata_from_storage(cf, storage)
  .new_netcdf_cube(
    storage = storage,
    source = source,
    dataset_id = dataset_id,
    metadata = metadata
  )
}
