# Diagnostico reproducible para el contrato del futuro backend NetCDF.
# Este script no implementa el backend, no usa internet y solo escribe en
# tempdir().

if (!requireNamespace("ncdf4", quietly = TRUE)) {
  stop("Se necesita el paquete 'ncdf4'.", call. = FALSE)
}

check_contract <- function(condition, message) {
  if (!isTRUE(condition)) {
    stop("FALLO: ", message, call. = FALSE)
  }
  message("OK: ", message)
}

fixture_path <- tempfile(
  pattern = "oceancube-netcdf-contract-",
  tmpdir = tempdir(),
  fileext = ".nc"
)
on.exit({
  if (file.exists(fixture_path)) {
    unlink(fixture_path)
  }
}, add = TRUE)

longitude <- c(-80, -79, -78)
latitude <- c(-12, -11)
depth <- c(0, 50)
time_raw <- c(0, 1, 2, 3)
latitude_chlorophyll <- c(-12.5, -11.5, -10.5)

lon_dim <- ncdf4::ncdim_def("lon", "degrees_east", longitude)
lat_dim <- ncdf4::ncdim_def("lat", "degrees_north", latitude)
depth_dim <- ncdf4::ncdim_def("depth", "m", depth)
time_dim <- ncdf4::ncdim_def(
  "time",
  "days since 2000-01-01 00:00:00",
  time_raw,
  unlim = FALSE
)
lat_chl_dim <- ncdf4::ncdim_def(
  "lat_chlorophyll",
  "degrees_north",
  latitude_chlorophyll
)

temperature_def <- ncdf4::ncvar_def(
  name = "temperature",
  units = "degree_Celsius",
  dim = list(lon_dim, lat_dim, depth_dim, time_dim),
  missval = -32767L,
  longname = "Sea water potential temperature",
  prec = "short"
)
oxygen_def <- ncdf4::ncvar_def(
  name = "oxygen",
  units = "mmol m-3",
  dim = list(time_dim, depth_dim, lat_dim, lon_dim),
  missval = -9999,
  longname = "Dissolved molecular oxygen",
  prec = "double"
)
sst_def <- ncdf4::ncvar_def(
  name = "sst",
  units = "degree_Celsius",
  dim = list(lon_dim, lat_dim, time_dim),
  missval = -9999,
  longname = "Sea surface temperature",
  prec = "float"
)
chlorophyll_def <- ncdf4::ncvar_def(
  name = "chlorophyll",
  units = "mg m-3",
  dim = list(lon_dim, lat_chl_dim, time_dim),
  missval = -9999,
  longname = "Mass concentration of chlorophyll",
  prec = "float"
)

temperature_raw <- array(
  0:47,
  dim = c(lon = 3, lat = 2, depth = 2, time = 4)
)
temperature_raw[2, 1, 1, 2] <- -32767L
temperature_raw[3, 2, 2, 3] <- -32766L

oxygen_source <- array(
  NA_real_,
  dim = c(time = 4, depth = 2, lat = 2, lon = 3)
)
for (time_i in seq_len(4)) {
  for (depth_i in seq_len(2)) {
    for (lat_i in seq_len(2)) {
      for (lon_i in seq_len(3)) {
        oxygen_source[time_i, depth_i, lat_i, lon_i] <-
          1000 * time_i + 100 * depth_i + 10 * lat_i + lon_i
      }
    }
  }
}
oxygen_source[4, 2, 2, 3] <- NA_real_

sst_source <- array(
  as.double(201:224),
  dim = c(lon = 3, lat = 2, time = 4)
)
sst_source[1, 2, 4] <- NA_real_

chlorophyll_source <- array(
  as.double(301:336),
  dim = c(lon = 3, lat_chlorophyll = 3, time = 4)
)

nc_out <- ncdf4::nc_create(
  fixture_path,
  vars = list(
    temperature_def,
    oxygen_def,
    sst_def,
    chlorophyll_def
  ),
  force_v4 = TRUE
)
tryCatch(
  {
    ncdf4::ncvar_put(nc_out, "temperature", temperature_raw)
    ncdf4::ncvar_put(nc_out, "oxygen", oxygen_source)
    ncdf4::ncvar_put(nc_out, "sst", sst_source)
    ncdf4::ncvar_put(nc_out, "chlorophyll", chlorophyll_source)

    ncdf4::ncatt_put(
      nc_out,
      "temperature",
      "standard_name",
      "sea_water_potential_temperature"
    )
    ncdf4::ncatt_put(nc_out, "temperature", "missing_value", -32766L, prec = "short")
    ncdf4::ncatt_put(nc_out, "temperature", "scale_factor", 0.1, prec = "double")
    ncdf4::ncatt_put(nc_out, "temperature", "add_offset", 10, prec = "double")
    ncdf4::ncatt_put(
      nc_out,
      "oxygen",
      "standard_name",
      "mole_concentration_of_dissolved_molecular_oxygen_in_sea_water"
    )
    ncdf4::ncatt_put(
      nc_out,
      "sst",
      "standard_name",
      "sea_surface_temperature"
    )
    ncdf4::ncatt_put(
      nc_out,
      "chlorophyll",
      "standard_name",
      "mass_concentration_of_chlorophyll_in_sea_water"
    )
    ncdf4::ncatt_put(nc_out, "lon", "standard_name", "longitude")
    ncdf4::ncatt_put(nc_out, "lon", "axis", "X")
    ncdf4::ncatt_put(nc_out, "lat", "standard_name", "latitude")
    ncdf4::ncatt_put(nc_out, "lat", "axis", "Y")
    ncdf4::ncatt_put(nc_out, "depth", "standard_name", "depth")
    ncdf4::ncatt_put(nc_out, "depth", "axis", "Z")
    ncdf4::ncatt_put(nc_out, "depth", "positive", "down")
    ncdf4::ncatt_put(nc_out, "time", "standard_name", "time")
    ncdf4::ncatt_put(nc_out, "time", "axis", "T")
    ncdf4::ncatt_put(nc_out, "time", "calendar", "gregorian")
  },
  finally = ncdf4::nc_close(nc_out)
)

message("Fixture temporal: ", fixture_path)
check_contract(file.exists(fixture_path), "el fixture existe dentro de tempdir()")
check_contract(
  identical(
    normalizePath(dirname(fixture_path), winslash = "/", mustWork = TRUE),
    normalizePath(tempdir(), winslash = "/", mustWork = TRUE)
  ),
  "el fixture se creo exclusivamente en tempdir()"
)

nc <- ncdf4::nc_open(fixture_path)
diagnostic <- tryCatch(
  {
    dimension_names <- names(nc$dim)
    variable_names <- names(nc$var)
    message("Dimensiones: ", paste(dimension_names, collapse = ", "))
    for (dimension_name in dimension_names) {
      dimension <- nc$dim[[dimension_name]]
      message(
        "  ",
        dimension_name,
        ": longitud=",
        dimension$len,
        "; unidades=",
        dimension$units
      )
    }

    message("Variables: ", paste(variable_names, collapse = ", "))
    source_orders <- lapply(variable_names, function(variable_name) {
      variable <- nc$var[[variable_name]]
      order <- vapply(variable$dim, function(x) x$name, character(1))
      message(
        "  ",
        variable_name,
        ": ",
        paste(order, collapse = " x "),
        "; unidades=",
        variable$units,
        "; precision=",
        variable$prec
      )
      order
    })
    names(source_orders) <- variable_names

    temperature_decoded <- ncdf4::ncvar_get(
      nc,
      "temperature",
      collapse_degen = FALSE
    )
    temperature_packed <- ncdf4::ncvar_get(
      nc,
      "temperature",
      collapse_degen = FALSE,
      raw_datavals = TRUE
    )
    oxygen_read <- ncdf4::ncvar_get(
      nc,
      "oxygen",
      collapse_degen = FALSE
    )
    sst_read <- ncdf4::ncvar_get(
      nc,
      "sst",
      collapse_degen = FALSE
    )

    temperature_block <- ncdf4::ncvar_get(
      nc,
      "temperature",
      start = c(1, 1, 1, 1),
      count = c(2, 1, 1, 2),
      collapse_degen = FALSE
    )
    oxygen_block <- ncdf4::ncvar_get(
      nc,
      "oxygen",
      start = c(1, 1, 1, 1),
      count = c(2, 1, 1, 2),
      collapse_degen = FALSE
    )

    temperature_attributes <- list(
      units = ncdf4::ncatt_get(nc, "temperature", "units")$value,
      long_name = ncdf4::ncatt_get(nc, "temperature", "long_name")$value,
      standard_name = ncdf4::ncatt_get(
        nc,
        "temperature",
        "standard_name"
      )$value,
      fill_value = ncdf4::ncatt_get(nc, "temperature", "_FillValue")$value,
      missing_value = ncdf4::ncatt_get(
        nc,
        "temperature",
        "missing_value"
      )$value,
      scale_factor = ncdf4::ncatt_get(
        nc,
        "temperature",
        "scale_factor"
      )$value,
      add_offset = ncdf4::ncatt_get(
        nc,
        "temperature",
        "add_offset"
      )$value
    )
    time_attributes <- list(
      units = ncdf4::ncatt_get(nc, "time", "units")$value,
      calendar = ncdf4::ncatt_get(nc, "time", "calendar")$value
    )
    depth_attributes <- list(
      units = ncdf4::ncatt_get(nc, "depth", "units")$value,
      positive = ncdf4::ncatt_get(nc, "depth", "positive")$value,
      standard_name = ncdf4::ncatt_get(nc, "depth", "standard_name")$value,
      axis = ncdf4::ncatt_get(nc, "depth", "axis")$value
    )

    message(
      "Bloque temperature [2 x 1 x 1 x 2]: ",
      paste(as.vector(temperature_block), collapse = ", ")
    )
    message(
      "Bloque oxygen [2 x 1 x 1 x 2] en orden fuente: ",
      paste(as.vector(oxygen_block), collapse = ", ")
    )
    message(
      "temperature _FillValue distinto de missing_value, decodificado: ",
      temperature_decoded[2, 1, 1, 2]
    )
    message(
      "temperature missing_value decodificado: ",
      temperature_decoded[3, 2, 2, 3]
    )
    message(
      "temperature packed[1,1,1,1]=",
      temperature_packed[1, 1, 1, 1],
      "; decoded=",
      temperature_decoded[1, 1, 1, 1]
    )
    message(
      "temperature raw_datavals en _FillValue=",
      temperature_packed[2, 1, 1, 2],
      "; en missing_value=",
      temperature_packed[3, 2, 2, 3]
    )
    message(
      "Atributos temperature: ",
      paste(
        paste(names(temperature_attributes), unlist(temperature_attributes), sep = "="),
        collapse = "; "
      )
    )
    message(
      "Atributos time: ",
      paste(
        paste(names(time_attributes), unlist(time_attributes), sep = "="),
        collapse = "; "
      )
    )
    message(
      "Atributos depth: ",
      paste(
        paste(names(depth_attributes), unlist(depth_attributes), sep = "="),
        collapse = "; "
      )
    )

    check_contract(
      identical(source_orders$temperature, c("lon", "lat", "depth", "time")),
      "temperature conserva el orden fisico lon x lat x depth x time"
    )
    check_contract(
      identical(source_orders$oxygen, c("time", "depth", "lat", "lon")),
      "oxygen conserva el orden fisico time x depth x lat x lon"
    )
    check_contract(
      identical(source_orders$sst, c("lon", "lat", "time")),
      "sst no tiene dimension fisica de profundidad"
    )
    check_contract(
      identical(
        source_orders$chlorophyll,
        c("lon", "lat_chlorophyll", "time")
      ),
      "chlorophyll usa una grilla deliberadamente incompatible"
    )
    check_contract(
      identical(unname(dim(temperature_decoded)), c(3L, 2L, 2L, 4L)),
      "ncvar_get conserva el orden fuente de temperature"
    )
    check_contract(
      identical(unname(dim(oxygen_read)), c(4L, 2L, 2L, 3L)),
      "ncvar_get conserva el orden fuente distinto de oxygen"
    )
    check_contract(
      identical(unname(dim(sst_read)), c(3L, 2L, 4L)),
      "ncvar_get conserva los tres ejes fisicos de sst"
    )
    check_contract(
      isTRUE(all.equal(temperature_packed[2, 1, 1, 2], -32767)),
      "raw_datavals conserva _FillValue como valor empaquetado"
    )
    check_contract(
      isTRUE(all.equal(temperature_decoded[2, 1, 1, 2], -3266.7)),
      "ncdf4 escala _FillValue cuando missing_value define otro centinela"
    )
    check_contract(
      is.na(temperature_decoded[3, 2, 2, 3]),
      "ncdf4 convierte missing_value en NA cuando ambos atributos difieren"
    )
    check_contract(
      isTRUE(all.equal(temperature_packed[1, 1, 1, 1], 0)),
      "raw_datavals conserva el valor empaquetado"
    )
    check_contract(
      isTRUE(all.equal(temperature_decoded[1, 1, 1, 1], 10)),
      "ncdf4 aplica scale_factor y add_offset una sola vez"
    )
    check_contract(
      identical(unname(dim(temperature_block)), c(2L, 1L, 1L, 2L)),
      "temperature admite una lectura rectangular pequena"
    )
    check_contract(
      identical(unname(dim(oxygen_block)), c(2L, 1L, 1L, 2L)),
      "oxygen interpreta start/count en su orden fisico"
    )

    list(
      dimensions = dimension_names,
      variables = variable_names,
      source_orders = source_orders,
      temperature_attributes = temperature_attributes,
      time_attributes = time_attributes,
      depth_attributes = depth_attributes,
      fill_decoded = temperature_decoded[2, 1, 1, 2],
      missing_decoded = temperature_decoded[3, 2, 2, 3],
      packed_first = temperature_packed[1, 1, 1, 1],
      decoded_first = temperature_decoded[1, 1, 1, 1]
    )
  },
  finally = ncdf4::nc_close(nc)
)

for (attempt in seq_len(3L)) {
  reopened <- ncdf4::nc_open(fixture_path)
  check_contract(
    all(c("temperature", "oxygen", "sst", "chlorophyll") %in%
      names(reopened$var)),
    paste0("apertura repetida ", attempt, " reconoce las variables")
  )
  ncdf4::nc_close(reopened)
}

removed <- unlink(fixture_path)
check_contract(identical(removed, 0L), "el fixture temporal fue eliminado")
check_contract(!file.exists(fixture_path), "no queda un NetCDF temporal abierto")

message("Resumen de diagnostico:")
message("  Variables: ", paste(diagnostic$variables, collapse = ", "))
message(
  "  missing_value decodificado: ",
  diagnostic$missing_decoded
)
message("DIAGNÓSTICO NETCDF COMPLETADO")
