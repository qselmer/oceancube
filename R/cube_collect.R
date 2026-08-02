#' Materialize an ocean cube in memory
#'
#' `cube_collect()` converts a deferred NetCDF-backed cube into an independent
#' in-memory `<ocean_cube>`. An object that already uses the memory backend is
#' validated and returned unchanged.
#'
#' @param x An `<ocean_cube>` object.
#'
#' @return An `<ocean_cube>` using the memory backend.
#'
#' @details
#' Collecting a NetCDF cube reads its complete logical 5D array and preserves
#' coordinates, variables, units, scientific metadata, and prior provenance.
#' The result no longer depends on the source file and can be modified with
#' memory-backend operations; the NetCDF source remains read-only.
#'
#' Before reading, `cube_collect()` estimates the minimum memory occupied by the
#' materialized double array as `prod(.cube_shape(x)) * 8`. This estimate does
#' not include R object overhead, dimnames, coordinates, or metadata.
#'
#' @export
#'
#' @examples
#' values <- array(1, dim = c(1, 1, 1, 1, 1))
#' cube <- ocean_cube(
#'   lon = -80,
#'   lat = -12,
#'   depth = 0,
#'   time = as.Date("2020-01-01"),
#'   vars = "temperature",
#'   data = values
#' )
#' collected <- cube_collect(cube)
#' identical(collected, cube)
cube_collect <- function(x) {
  .check_cube(x)
  backend <- .cube_backend(x)
  estimated_bytes <- .cube_estimated_bytes(x)

  if (identical(backend, "memory")) {
    return(x)
  }
  if (!identical(backend, "netcdf")) {
    rlang::abort(
      paste0(
        "Cannot collect unsupported ocean_cube backend `",
        backend,
        "`."
      ),
      class = "oceancube_unsupported_backend"
    )
  }

  data <- tryCatch(
    .cube_read(x),
    error = function(e) {
      rlang::abort(
        paste0(
          "Failed to collect NetCDF cube: ",
          conditionMessage(e)
        ),
        class = "oceancube_collect_error",
        parent = e
      )
    }
  )
  record <- list(
    operation = "cube_collect",
    source_backend = "netcdf",
    target_backend = "memory",
    source_file = x$storage$file$normalized_path,
    variables = x$vars,
    shape = .cube_shape(x),
    estimated_bytes = estimated_bytes,
    collected_utc = .netcdf_as_utc(Sys.time())
  )
  provenance <- if (is.null(x$provenance)) {
    list(cube_collect = record)
  } else {
    list(
      parent = x$provenance,
      cube_collect = record
    )
  }

  .new_collected_memory_cube(x, data, provenance)
}

.cube_estimated_bytes <- function(x) {
  shape <- .cube_shape(x)
  elements <- .cube_product_as_double(
    shape,
    "logical cube elements"
  )
  if (elements > .Machine$double.xmax / 8) {
    rlang::abort(
      "Cannot estimate materialized cube memory: numeric overflow.",
      class = "oceancube_size_overflow"
    )
  }
  bytes <- elements * 8
  if (!is.finite(bytes)) {
    rlang::abort(
      "Cannot estimate materialized cube memory: numeric overflow.",
      class = "oceancube_size_overflow"
    )
  }
  bytes
}
