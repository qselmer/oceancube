make_cf_b3_fixture <- function(
    case = "valid",
    file = tempfile(pattern = "oceancube-cf-b3-", tmpdir = tempdir(),
                    fileext = ".nc")) {
  stopifnot(requireNamespace("ncdf4", quietly = TRUE))
  cases <- c(
    "valid", "coordinates_missing", "coordinates_dimensions",
    "ancillary_dimensions", "bounds_missing", "bounds_dimensions",
    "climatology_dimensions", "cell_measure_keyword",
    "cell_measure_missing", "grid_mapping_missing",
    "grid_mapping_name_missing", "formula_missing", "axis_conflict",
    "valid_range_conflict"
  )
  stopifnot(case %in% cases)

  lon <- ncdf4::ncdim_def("lon", "degrees_east", c(-80.5, -79.5))
  lat <- ncdf4::ncdim_def("lat", "degrees_north", c(-12.5, -11.5))
  sigma <- ncdf4::ncdim_def("sigma", "1", c(0.1, 0.9))
  time <- ncdf4::ncdim_def(
    "time", "days since 2000-01-01 00:00:00", c(15, 45)
  )
  nv <- ncdf4::ncdim_def("nv", "", 1:2, create_dimvar = FALSE)
  tri <- ncdf4::ncdim_def("tri", "", 1:3, create_dimvar = FALSE)
  extra <- ncdf4::ncdim_def("extra", "", 1:2, create_dimvar = FALSE)

  definitions <- list(
    ncdf4::ncvar_def("lon_bnds", "degrees_east", list(nv, lon)),
    ncdf4::ncvar_def("lat_bnds", "degrees_north", list(nv, lat)),
    ncdf4::ncvar_def("time_clim", "days since 2000-01-01 00:00:00", list(nv, time)),
    ncdf4::ncvar_def("bad_bounds", "degrees_east", list(extra)),
    ncdf4::ncvar_def("bad_clim", "days since 2000-01-01 00:00:00", list(tri, time)),
    ncdf4::ncvar_def("auxlat", "degrees_north", list(lon, lat)),
    ncdf4::ncvar_def("bad_aux", "1", list(extra)),
    ncdf4::ncvar_def("qc", "1", list(lon, lat, time), prec = "short"),
    ncdf4::ncvar_def("bad_anc", "1", list(extra), prec = "short"),
    ncdf4::ncvar_def("areacello", "m2", list(lon, lat)),
    ncdf4::ncvar_def("eta", "m", list(lon, lat)),
    ncdf4::ncvar_def("depth_ref", "m", list()),
    ncdf4::ncvar_def("crs", "", list(), prec = "integer"),
    ncdf4::ncvar_def("bad_crs", "", list(), prec = "integer"),
    ncdf4::ncvar_def(
      "temperature", "K", list(lon, lat, sigma, time),
      missval = -9999, prec = "float"
    )
  )
  nc <- ncdf4::nc_create(file, definitions, force_v4 = TRUE)
  tryCatch(
    {
      ncdf4::ncatt_put(nc, 0, "Conventions", "CF-1.13, ACDD-1.3")
      ncdf4::ncatt_put(nc, 0, "custom_provider_attribute", "preserve exact")
      ncdf4::ncatt_put(
        nc, "lon", "standard_name",
        if (identical(case, "axis_conflict")) "latitude" else "longitude"
      )
      ncdf4::ncatt_put(nc, "lon", "axis", "X")
      ncdf4::ncatt_put(
        nc, "lon", "bounds",
        switch(case, bounds_missing = "missing_bounds",
               bounds_dimensions = "bad_bounds", "lon_bnds")
      )
      ncdf4::ncatt_put(nc, "lat", "standard_name", "latitude")
      ncdf4::ncatt_put(nc, "lat", "axis", "Y")
      ncdf4::ncatt_put(nc, "lat", "bounds", "lat_bnds")
      ncdf4::ncatt_put(nc, "sigma", "standard_name", "ocean_sigma_coordinate")
      ncdf4::ncatt_put(nc, "sigma", "axis", "Z")
      ncdf4::ncatt_put(nc, "sigma", "positive", "down")
      ncdf4::ncatt_put(
        nc, "sigma", "formula_terms",
        if (identical(case, "formula_missing")) {
          "sigma: sigma eta: eta depth: missing_depth"
        } else {
          "sigma: sigma eta: eta depth: depth_ref"
        }
      )
      ncdf4::ncatt_put(nc, "time", "standard_name", "time")
      ncdf4::ncatt_put(nc, "time", "axis", "T")
      ncdf4::ncatt_put(nc, "time", "calendar", "standard")
      ncdf4::ncatt_put(
        nc, "time", "climatology",
        if (identical(case, "climatology_dimensions")) "bad_clim" else "time_clim"
      )
      ncdf4::ncatt_put(nc, "crs", "grid_mapping_name", "latitude_longitude")
      ncdf4::ncatt_put(nc, "auxlat", "standard_name", "latitude")
      ncdf4::ncatt_put(nc, "qc", "flag_values", c(0L, 1L), prec = "short")
      ncdf4::ncatt_put(nc, "qc", "flag_masks", c(1L, 2L), prec = "short")
      ncdf4::ncatt_put(nc, "qc", "flag_meanings", "good suspect")
      ncdf4::ncatt_put(nc, "temperature", "standard_name", "sea_water_temperature")
      ncdf4::ncatt_put(
        nc, "temperature", "coordinates",
        switch(case,
          coordinates_missing = "missing_coordinate",
          coordinates_dimensions = "bad_aux",
          "auxlat"
        )
      )
      ncdf4::ncatt_put(
        nc, "temperature", "ancillary_variables",
        if (identical(case, "ancillary_dimensions")) "bad_anc" else "qc"
      )
      ncdf4::ncatt_put(
        nc, "temperature", "cell_measures",
        switch(case,
          cell_measure_keyword = "length: areacello",
          cell_measure_missing = "area: missing_area",
          "area: areacello"
        )
      )
      ncdf4::ncatt_put(
        nc, "temperature", "grid_mapping",
        switch(case,
          grid_mapping_missing = "missing_crs",
          grid_mapping_name_missing = "bad_crs",
          "crs"
        )
      )
      ncdf4::ncatt_put(nc, "temperature", "cell_methods", "time: mean")
      ncdf4::ncatt_put(nc, "temperature", "actual_range", c(270, 310))
      if (identical(case, "valid_range_conflict")) {
        ncdf4::ncatt_put(nc, "temperature", "valid_range", c(260, 320))
        ncdf4::ncatt_put(nc, "temperature", "valid_min", 265)
      }
    },
    finally = ncdf4::nc_close(nc)
  )
  file
}

cf_b3_failures <- function(cf) {
  cf$diagnostics[vapply(cf$diagnostics, function(x) {
    identical(x$status, "FAIL") && identical(x$severity, "ERROR")
  }, logical(1L))]
}

cf_b3_codes <- function(cf, status = NULL, scope = NULL) {
  diagnostics <- cf$diagnostics
  if (!is.null(status)) diagnostics <- diagnostics[vapply(
    diagnostics, `[[`, character(1L), "status"
  ) == status]
  if (!is.null(scope)) diagnostics <- diagnostics[vapply(
    diagnostics, `[[`, character(1L), "scope"
  ) == scope]
  vapply(diagnostics, `[[`, character(1L), "code")
}
