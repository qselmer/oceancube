make_cf_b2_fixture <- function(
    file = tempfile(pattern = "oceancube-cf-b2-", tmpdir = tempdir(),
                    fileext = ".nc")) {
  stopifnot(requireNamespace("ncdf4", quietly = TRUE))

  lon <- ncdf4::ncdim_def("lon", "degrees_east", c(-80.5, -79.5))
  lat <- ncdf4::ncdim_def("lat", "degrees_north", c(-12.5, -11.5))
  sigma <- ncdf4::ncdim_def("sigma", "1", c(0.1, 0.9))
  time <- ncdf4::ncdim_def(
    "time", "days since 2000-01-01 00:00:00", c(15, 45)
  )
  nv <- ncdf4::ncdim_def("nv", "", 1:2, create_dimvar = FALSE)
  g1_x <- ncdf4::ncdim_def("g1/x", "1", 1:2)
  g2_x <- ncdf4::ncdim_def("g2/x", "1", 1:3)

  definitions <- list(
    ncdf4::ncvar_def("lon_bnds", "degrees_east", list(nv, lon)),
    ncdf4::ncvar_def("lat_bnds", "degrees_north", list(nv, lat)),
    ncdf4::ncvar_def(
      "time_clim", "days since 2000-01-01 00:00:00", list(nv, time)
    ),
    ncdf4::ncvar_def("eta", "m", list(lon, lat)),
    ncdf4::ncvar_def("depth_ref", "m", list()),
    ncdf4::ncvar_def("crs", "", list(), prec = "integer"),
    ncdf4::ncvar_def("areacello", "m2", list(lon, lat)),
    ncdf4::ncvar_def("qc", "1", list(lon, lat, time), prec = "short"),
    ncdf4::ncvar_def("auxlat", "degrees_north", list(lon, lat)),
    ncdf4::ncvar_def(
      "temperature", "K", list(lon, lat, sigma, time),
      missval = -9999, prec = "float"
    ),
    ncdf4::ncvar_def(
      "extended_temperature", "K", list(lon, lat, sigma, time),
      missval = -9999, prec = "float"
    ),
    ncdf4::ncvar_def("self", "1", list(lon)),
    ncdf4::ncvar_def("g1/dup", "1", list(g1_x)),
    ncdf4::ncvar_def("g2/dup", "1", list(g2_x))
  )
  nc <- ncdf4::nc_create(file, definitions, force_v4 = TRUE)
  tryCatch(
    {
      ncdf4::ncatt_put(nc, 0, "Conventions", "CF-1.13, ACDD-1.3")
      ncdf4::ncatt_put(nc, 0, "title", "Deterministic oceancube B2 fixture")
      ncdf4::ncatt_put(nc, 0, "institution", "oceancube tests")
      ncdf4::ncatt_put(nc, 0, "source", "synthetic")
      ncdf4::ncatt_put(
        nc, 0, "history", "provider history; not Provenance V1"
      )
      ncdf4::ncatt_put(nc, 0, "references", "offline fixture")
      ncdf4::ncatt_put(nc, 0, "comment", "compact")
      ncdf4::ncatt_put(nc, 0, "featureType", "grid")
      ncdf4::ncatt_put(
        nc, 0, "custom_provider_attribute", "keep this exact value"
      )

      ncdf4::ncatt_put(nc, "lon", "standard_name", "longitude")
      ncdf4::ncatt_put(nc, "lon", "axis", "X")
      ncdf4::ncatt_put(nc, "lon", "bounds", "lon_bnds")
      ncdf4::ncatt_put(nc, "lat", "standard_name", "latitude")
      ncdf4::ncatt_put(nc, "lat", "axis", "Y")
      ncdf4::ncatt_put(nc, "lat", "bounds", "lat_bnds")
      ncdf4::ncatt_put(nc, "sigma", "standard_name", "ocean_sigma_coordinate")
      ncdf4::ncatt_put(nc, "sigma", "axis", "Z")
      ncdf4::ncatt_put(nc, "sigma", "positive", "down")
      ncdf4::ncatt_put(
        nc, "sigma", "formula_terms", "sigma: sigma eta: eta depth: depth_ref"
      )
      ncdf4::ncatt_put(nc, "time", "standard_name", "time")
      ncdf4::ncatt_put(nc, "time", "axis", "T")
      ncdf4::ncatt_put(nc, "time", "calendar", "standard")
      ncdf4::ncatt_put(nc, "time", "climatology", "time_clim")
      ncdf4::ncatt_put(nc, "crs", "grid_mapping_name", "latitude_longitude")
      ncdf4::ncatt_put(nc, "crs", "longitude_of_prime_meridian", 0)
      ncdf4::ncatt_put(nc, "auxlat", "standard_name", "latitude")
      ncdf4::ncatt_put(nc, "qc", "flag_values", c(0L, 1L), prec = "short")
      ncdf4::ncatt_put(nc, "qc", "flag_masks", c(1L, 1L), prec = "short")
      ncdf4::ncatt_put(nc, "qc", "flag_meanings", "good suspect")
      ncdf4::ncatt_put(
        nc, "temperature", "standard_name", "sea_water_temperature"
      )
      ncdf4::ncatt_put(
        nc, "temperature", "coordinates", "auxlat auxlat missing dup"
      )
      ncdf4::ncatt_put(
        nc, "temperature", "cell_methods",
        "time: mean within years time: mean over years"
      )
      ncdf4::ncatt_put(
        nc, "temperature", "cell_measures", "area: areacello"
      )
      ncdf4::ncatt_put(nc, "temperature", "ancillary_variables", "qc")
      ncdf4::ncatt_put(nc, "temperature", "grid_mapping", "crs")
      ncdf4::ncatt_put(
        nc, "temperature", "custom_variable_attribute", "preserve me"
      )
      ncdf4::ncatt_put(
        nc, "extended_temperature", "grid_mapping", "crs: lon lat"
      )
      ncdf4::ncatt_put(nc, "self", "bounds", "self")
    },
    finally = ncdf4::nc_close(nc)
  )
  file
}

cf_attribute_record_by_name <- function(records, name) {
  names <- vapply(records, `[[`, character(1L), "name")
  records[[which(names == name)[[1L]]]]
}

cf_link_records <- function(cf, attribute = NULL) {
  links <- cf$source$links
  if (!is.null(attribute)) {
    links <- links[vapply(links, `[[`, character(1L), "attribute") == attribute]
  }
  links
}
