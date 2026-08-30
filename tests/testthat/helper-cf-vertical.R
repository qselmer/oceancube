make_cf_vertical_fixture <- function(
    values = c(0, 10, 20), units = "m", standard_name = "depth",
    positive = "down", axis = "Z", bounds = NULL, bounds_units = units,
    bounds_shape = "valid", formula_terms = NULL,
    boundary_formula_terms = NULL, vertical_name = "z") {
  file <- tempfile("oceancube-cf-vertical-", fileext = ".nc")
  lon <- ncdf4::ncdim_def("lon", "degrees_east", c(-80, -79))
  lat <- ncdf4::ncdim_def("lat", "degrees_north", c(-12, -11))
  z <- ncdf4::ncdim_def(vertical_name, units %||% "", values)
  time <- ncdf4::ncdim_def("time", "days since 2000-01-01", 0)
  definitions <- list(
    ncdf4::ncvar_def("temperature", "K", list(lon, lat, z, time))
  )
  if (!is.null(bounds)) {
    vertices <- if (identical(bounds_shape, "valid")) 2L else 3L
    nv <- ncdf4::ncdim_def("nv", "", seq_len(vertices), create_dimvar = FALSE)
    definitions <- c(
      definitions,
      list(ncdf4::ncvar_def("z_bnds", bounds_units %||% "", list(nv, z)))
    )
  }
  if (!is.null(formula_terms)) {
    definitions <- c(
      definitions,
      list(
        ncdf4::ncvar_def("eta", "m", list(lon, lat)),
        ncdf4::ncvar_def("bathymetry", "m", list(lon, lat))
      )
    )
  }
  nc <- ncdf4::nc_create(file, definitions, force_v4 = TRUE)
  ncdf4::ncvar_put(nc, "temperature", array(
    seq_len(2L * 2L * length(values)), dim = c(2L, 2L, length(values), 1L)
  ))
  if (!is.null(standard_name)) ncdf4::ncatt_put(nc, vertical_name, "standard_name", standard_name)
  if (!is.null(positive)) ncdf4::ncatt_put(nc, vertical_name, "positive", positive)
  if (!is.null(axis)) ncdf4::ncatt_put(nc, vertical_name, "axis", axis)
  if (!is.null(bounds)) {
    ncdf4::ncatt_put(nc, vertical_name, "bounds", "z_bnds")
    ncdf4::ncvar_put(nc, "z_bnds", t(bounds))
    if (!is.null(boundary_formula_terms)) {
      ncdf4::ncatt_put(nc, "z_bnds", "formula_terms", boundary_formula_terms)
    }
  }
  if (!is.null(formula_terms)) {
    ncdf4::ncatt_put(nc, vertical_name, "formula_terms", formula_terms)
    ncdf4::ncvar_put(nc, "eta", matrix(0, 2, 2))
    ncdf4::ncvar_put(nc, "bathymetry", matrix(100, 2, 2))
  }
  ncdf4::ncatt_put(nc, 0, "Conventions", "CF-1.13")
  ncdf4::nc_close(nc)
  file
}
