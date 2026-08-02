make_netcdf_backend_fixture <- function(
    file = tempfile(
      pattern = "oceancube-netcdf-descriptor-",
      tmpdir = tempdir(),
      fileext = ".nc"
    ),
    calendar = "gregorian",
    add_calendar = TRUE,
    time_units = "days since 2000-01-01 00:00:00",
    time_values = c(0, 1, 2, 3),
    add_cf_attributes = TRUE,
    neutral_spatial_units = FALSE,
    ambiguous_longitude = FALSE,
    coordinate_names = c(
      longitude = "longitude",
      latitude = "latitude",
      depth = "depth",
      time = "time"
    )) {
  stopifnot(requireNamespace("ncdf4", quietly = TRUE))
  fixture_parent <- normalizePath(dirname(file), winslash = "/", mustWork = TRUE)
  fixture_temp <- normalizePath(tempdir(), winslash = "/", mustWork = TRUE)
  if (!identical(fixture_parent, fixture_temp)) {
    stop("The NetCDF test fixture must be created directly in tempdir().")
  }

  longitude <- c(-80, -79, -78)
  latitude <- c(-12, -11)
  depth <- c(0, 50)
  if (!is.numeric(time_values) ||
      length(time_values) != 4L ||
      anyNA(time_values) ||
      any(!is.finite(time_values))) {
    stop("`time_values` must contain four finite numeric values.")
  }
  time <- as.numeric(time_values)
  latitude_chlorophyll <- c(-12.5, -11.5, -10.5)

  lon_units <- if (neutral_spatial_units) "1" else "degrees_east"
  lat_units <- if (neutral_spatial_units) "1" else "degrees_north"
  lon_dim <- ncdf4::ncdim_def(
    coordinate_names[["longitude"]],
    lon_units,
    longitude
  )
  lat_dim <- ncdf4::ncdim_def(
    coordinate_names[["latitude"]],
    lat_units,
    latitude
  )
  depth_dim <- ncdf4::ncdim_def(
    coordinate_names[["depth"]],
    "m",
    depth
  )
  time_dim <- ncdf4::ncdim_def(
    coordinate_names[["time"]],
    time_units,
    time
  )
  chlorophyll_lat_dim <- ncdf4::ncdim_def(
    "latitude_chlorophyll",
    "degrees_north",
    latitude_chlorophyll
  )

  temperature_def <- ncdf4::ncvar_def(
    "temperature",
    "degree_Celsius",
    list(lon_dim, lat_dim, depth_dim, time_dim),
    missval = -32767L,
    longname = "Sea water potential temperature",
    prec = "short"
  )
  oxygen_def <- ncdf4::ncvar_def(
    "oxygen",
    "mmol m-3",
    list(time_dim, depth_dim, lat_dim, lon_dim),
    missval = -9999,
    longname = "Dissolved molecular oxygen",
    prec = "double"
  )
  surface_def <- ncdf4::ncvar_def(
    "sst",
    "degree_Celsius",
    list(lon_dim, lat_dim, time_dim),
    missval = -9999,
    longname = "Sea surface temperature",
    prec = "float"
  )
  chlorophyll_def <- ncdf4::ncvar_def(
    "chlorophyll",
    "mg m-3",
    list(lon_dim, chlorophyll_lat_dim, time_dim),
    missval = -9999,
    longname = "Mass concentration of chlorophyll",
    prec = "float"
  )

  definitions <- list(
    temperature_def,
    oxygen_def,
    surface_def,
    chlorophyll_def
  )
  if (ambiguous_longitude) {
    auxiliary_lon_dim <- ncdf4::ncdim_def(
      "longitude_aux",
      "degrees_east",
      c(-80, -79, -78)
    )
    definitions <- c(
      definitions,
      list(ncdf4::ncvar_def(
        "longitude_probe",
        "1",
        list(auxiliary_lon_dim, lat_dim, time_dim),
        missval = -9999,
        prec = "float"
      ))
    )
  }

  nc <- ncdf4::nc_create(file, definitions, force_v4 = TRUE)
  tryCatch(
    {
      canonical_values <- function(variable_index, include_depth = TRUE) {
        dimensions <- if (isTRUE(include_depth)) {
          c(3L, 2L, 2L, 4L)
        } else {
          c(3L, 2L, 4L)
        }
        values <- array(NA_real_, dim = dimensions)
        if (isTRUE(include_depth)) {
          for (time_index in seq_len(dimensions[[4L]])) {
            for (depth_index in seq_len(dimensions[[3L]])) {
              for (latitude_index in seq_len(dimensions[[2L]])) {
                for (longitude_index in seq_len(dimensions[[1L]])) {
                  values[
                    longitude_index,
                    latitude_index,
                    depth_index,
                    time_index
                  ] <- 10000 * variable_index +
                    1000 * time_index +
                    100 * depth_index +
                    10 * latitude_index +
                    longitude_index
                }
              }
            }
          }
        } else {
          for (time_index in seq_len(dimensions[[3L]])) {
            for (latitude_index in seq_len(dimensions[[2L]])) {
              for (longitude_index in seq_len(dimensions[[1L]])) {
                values[
                  longitude_index,
                  latitude_index,
                  time_index
                ] <- 10000 * variable_index +
                  1000 * time_index +
                  100 +
                  10 * latitude_index +
                  longitude_index
              }
            }
          }
        }
        values
      }

      temperature <- canonical_values(1L)
      temperature_packed <- (temperature - 11000) / 0.5
      temperature_packed[1, 2, 2, 2] <- -32767
      temperature_packed[3, 1, 2, 3] <- -32766

      oxygen <- canonical_values(2L)
      oxygen[1, 2, 1, 3] <- NA_real_
      oxygen_physical <- aperm(oxygen, c(4L, 3L, 2L, 1L))

      ncdf4::ncvar_put(
        nc,
        "temperature",
        temperature_packed
      )
      ncdf4::ncvar_put(
        nc,
        "oxygen",
        oxygen_physical
      )
      ncdf4::ncvar_put(
        nc,
        "sst",
        canonical_values(1L, include_depth = FALSE)
      )
      ncdf4::ncvar_put(
        nc,
        "chlorophyll",
        array(as.double(301:336), dim = c(3, 3, 4))
      )
      if (ambiguous_longitude) {
        ncdf4::ncvar_put(
          nc,
          "longitude_probe",
          array(as.double(401:424), dim = c(3, 2, 4))
        )
      }

      ncdf4::ncatt_put(
        nc,
        "temperature",
        "standard_name",
        "sea_water_potential_temperature"
      )
      ncdf4::ncatt_put(
        nc,
        "temperature",
        "missing_value",
        -32766L,
        prec = "short"
      )
      ncdf4::ncatt_put(
        nc,
        "temperature",
        "scale_factor",
        0.5,
        prec = "double"
      )
      ncdf4::ncatt_put(
        nc,
        "temperature",
        "add_offset",
        11000,
        prec = "double"
      )
      ncdf4::ncatt_put(
        nc,
        "oxygen",
        "standard_name",
        "mole_concentration_of_dissolved_molecular_oxygen_in_sea_water"
      )
      ncdf4::ncatt_put(
        nc,
        "sst",
        "standard_name",
        "sea_surface_temperature"
      )

      if (add_cf_attributes) {
        ncdf4::ncatt_put(
          nc,
          coordinate_names[["longitude"]],
          "standard_name",
          "longitude"
        )
        ncdf4::ncatt_put(
          nc,
          coordinate_names[["longitude"]],
          "axis",
          "X"
        )
        ncdf4::ncatt_put(
          nc,
          coordinate_names[["latitude"]],
          "standard_name",
          "latitude"
        )
        ncdf4::ncatt_put(
          nc,
          coordinate_names[["latitude"]],
          "axis",
          "Y"
        )
        ncdf4::ncatt_put(
          nc,
          coordinate_names[["depth"]],
          "standard_name",
          "depth"
        )
        ncdf4::ncatt_put(
          nc,
          coordinate_names[["depth"]],
          "axis",
          "Z"
        )
        ncdf4::ncatt_put(
          nc,
          coordinate_names[["depth"]],
          "positive",
          "down"
        )
        ncdf4::ncatt_put(
          nc,
          coordinate_names[["time"]],
          "standard_name",
          "time"
        )
        ncdf4::ncatt_put(
          nc,
          coordinate_names[["time"]],
          "axis",
          "T"
        )
      }
      if (add_calendar) {
        ncdf4::ncatt_put(
          nc,
          coordinate_names[["time"]],
          "calendar",
          calendar
        )
      }
      if (ambiguous_longitude) {
        ncdf4::ncatt_put(
          nc,
          "longitude_aux",
          "standard_name",
          "longitude"
        )
        ncdf4::ncatt_put(nc, "longitude_aux", "axis", "X")
      }
    },
    finally = ncdf4::nc_close(nc)
  )

  file
}
