#' Construct a certified TEOS-10 thermodynamic state
#'
#' @param x A direct source-profile `<ocean_cube>` containing one exact
#'   CF-identified salinity variable and one exact CF-identified temperature
#'   variable with point-valued metric-depth semantics.
#' @param salinity Optional exact current variable name. When `NULL`, exactly
#'   one eligible Practical or Absolute Salinity variable must exist.
#' @param temperature Optional exact current variable name. When `NULL`,
#'   exactly one eligible in-situ, potential, or Conservative Temperature
#'   variable must exist.
#' @param pressure `NULL` derives sea pressure from metric depth and latitude.
#'   Otherwise, an exact current variable name declaring the CF standard name
#'   `sea_water_pressure` must be supplied.
#' @param reference_pressure_dbar Finite non-negative sea pressure in dbar at
#'   which full potential density is evaluated.
#'
#' @return A memory-backed `<ocean_cube>` with variables, in order,
#'   `absolute_salinity`, `conservative_temperature`, `sea_water_pressure`,
#'   `sea_water_density`, and `sea_water_potential_density`.
#'
#' @details
#' C9 uses the optional `gsw` package and the TEOS-10 75-term equation. It
#' accepts only preserved CF semantic identities; variable names and
#' `long_name` values never establish thermodynamic meaning. Practical
#' Salinity is converted with position and pressure, temperature bases remain
#' distinct, and complete finite states outside the GSW funnel abort the whole
#' operation. No equation-of-state fallback or runtime installation is used.
#'
#' The nonlinear state is evaluated from the supplied representative point
#' values. Cell-mean temperature or salinity, density-threshold mixed layers,
#' pycnoclines, and buoyancy frequency are outside this function's contract.
#'
#' @references
#' McDougall, T. J. and Barker, P. M. (2011). Getting started with TEOS-10 and
#' the Gibbs Seawater Oceanographic Toolbox. SCOR/IAPSO WG127.
#' \url{https://www.teos-10.org/pubs/Getting_Started.pdf}
#'
#' @examples
#' \dontrun{
#' state <- thermodynamic_state(cube)
#' state_1000 <- thermodynamic_state(cube, reference_pressure_dbar = 1000)
#' }
#' @export
thermodynamic_state <- function(
    x,
    salinity = NULL,
    temperature = NULL,
    pressure = NULL,
    reference_pressure_dbar = 0) {
  .teos_validate_reference_pressure(reference_pressure_dbar)
  gsw_version <- .teos_require_dependency()
  plan <- .teos_plan(x, salinity, temperature, pressure)
  calculated <- .teos_calculate(
    plan, reference_pressure_dbar = as.numeric(reference_pressure_dbar)
  )
  .teos_build_output(
    plan, calculated,
    reference_pressure_dbar = as.numeric(reference_pressure_dbar),
    gsw_version = gsw_version
  )
}

.TEOS_GSW_MIN_VERSION <- "1.2-0"
.TEOS_NUMERICAL_TOLERANCE <- 1.5e-8

.teos_gsw_available <- function() {
  requireNamespace("gsw", quietly = TRUE)
}

.teos_gsw_version <- function() {
  as.character(utils::packageVersion("gsw"))
}

.teos_require_dependency <- function() {
  if (!.teos_gsw_available()) {
    rlang::abort(
      paste0(
        "`thermodynamic_state()` requires the optional package `gsw` (>= ",
        .TEOS_GSW_MIN_VERSION, "). Install it explicitly with ",
        "`install.packages(\"gsw\")`."
      ),
      class = "oceancube_teos10_dependency_missing"
    )
  }
  version <- .teos_gsw_version()
  if (utils::compareVersion(version, .TEOS_GSW_MIN_VERSION) < 0L) {
    rlang::abort(
      paste0(
        "Installed `gsw` version ", version, " is below the certified minimum ",
        .TEOS_GSW_MIN_VERSION, "."
      ),
      class = "oceancube_teos10_dependency_version"
    )
  }
  version
}

.teos_validate_reference_pressure <- function(x) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x < 0) {
    rlang::abort(
      "`reference_pressure_dbar` must be one finite non-negative numeric value.",
      class = "oceancube_teos10_reference_pressure"
    )
  }
  invisible(TRUE)
}

.teos_salinity_basis <- function() {
  c(
    sea_water_practical_salinity = "PRACTICAL_SALINITY_PSS78",
    sea_water_absolute_salinity = "ABSOLUTE_SALINITY"
  )
}

.teos_temperature_basis <- function() {
  c(
    sea_water_temperature = "IN_SITU_TEMPERATURE",
    sea_water_potential_temperature = "POTENTIAL_TEMPERATURE_REF_0_DBAR",
    sea_water_conservative_temperature = "CONSERVATIVE_TEMPERATURE"
  )
}

.teos_source_item <- function(x, variable) {
  source <- x$metadata$cf$source %||% NULL
  map <- source$variables$map %||% NULL
  if (!is.list(map) || !length(map)) {
    rlang::abort(
      "C9 thermodynamics require immutable preserved source-variable metadata.",
      class = "oceancube_teos10_metadata"
    )
  }
  paths <- names(map)
  exact <- paths == variable
  keep <- if (sum(exact) == 1L) exact else {
    vapply(paths, .cf_basename, character(1L)) == .cf_basename(variable)
  }
  if (sum(keep) != 1L) {
    rlang::abort(
      paste0("Current variable `", variable,
             "` does not resolve uniquely to preserved source metadata."),
      class = "oceancube_teos10_metadata"
    )
  }
  record <- map[[which(keep)]]
  scalar_attribute <- function(name) {
    value <- .cf_attribute_value(record$attributes, name, default = NULL)
    if (is.character(value) && length(value) == 1L && !is.na(value) &&
        nzchar(trimws(value))) trimws(value) else NA_character_
  }
  list(
    current_variable = variable,
    source_path = record$source_path,
    standard_name = scalar_attribute("standard_name"),
    unit = scalar_attribute("units"),
    cell_methods = scalar_attribute("cell_methods")
  )
}

.teos_validate_requested_name <- function(value, argument) {
  if (!is.null(value) && (
    !is.character(value) || length(value) != 1L || is.na(value) ||
      !nzchar(value))) {
    rlang::abort(
      paste0("`", argument,
             "` must be NULL or one exact non-empty current variable name."),
      class = paste0("oceancube_teos10_", argument, "_variable")
    )
  }
}

.teos_resolve_variable <- function(x, requested, family) {
  .teos_validate_requested_name(requested, family)
  basis <- if (identical(family, "salinity")) {
    .teos_salinity_basis()
  } else {
    .teos_temperature_basis()
  }
  metadata <- lapply(x$vars, function(variable) .teos_source_item(x, variable))
  names(metadata) <- x$vars
  eligible <- vapply(metadata, function(item) {
    !is.na(item$standard_name) && item$standard_name %in% names(basis)
  }, logical(1L))
  if (is.null(requested)) {
    candidates <- x$vars[eligible]
    if (!length(candidates)) {
      rlang::abort(
        paste0("No variable has an eligible preserved source CF ", family,
               " standard_name."),
        class = paste0("oceancube_teos10_", family, "_variable")
      )
    }
    if (length(candidates) > 1L) {
      rlang::abort(
        paste0("Multiple ", family, " variables are eligible (",
               paste(candidates, collapse = ", "), "); supply one exact `",
               family, "=` name."),
        class = paste0("oceancube_teos10_", family, "_ambiguous")
      )
    }
    requested <- candidates[[1L]]
  } else if (!requested %in% x$vars) {
    rlang::abort(
      paste0("Variable `", requested, "` is not present in the cube."),
      class = paste0("oceancube_teos10_", family, "_variable")
    )
  } else if (!eligible[[requested]]) {
    standard_name <- metadata[[requested]]$standard_name
    detail <- if (is.na(standard_name)) "has no usable standard_name" else {
      paste0("declares incompatible standard_name `", standard_name, "`")
    }
    rlang::abort(
      paste0("Variable `", requested, "` ", detail, "."),
      class = paste0("oceancube_teos10_", family, "_variable")
    )
  }
  out <- metadata[[requested]]
  out$basis <- unname(basis[[out$standard_name]])
  out
}

.teos_resolve_pressure <- function(x, pressure) {
  if (is.null(pressure)) {
    return(list(
      current_variable = NA_character_, source_path = NA_character_,
      standard_name = "sea_water_pressure", unit = "dbar",
      cell_methods = NA_character_, origin = "DERIVED_FROM_DEPTH_AND_LATITUDE"
    ))
  }
  .teos_validate_requested_name(pressure, "pressure")
  if (!pressure %in% x$vars) {
    rlang::abort(
      paste0("Pressure variable `", pressure, "` is not present in the cube."),
      class = "oceancube_teos10_pressure_variable"
    )
  }
  item <- .teos_source_item(x, pressure)
  if (!identical(item$standard_name, "sea_water_pressure")) {
    rlang::abort(
      paste0("Explicit pressure variable `", pressure,
             "` must declare standard_name `sea_water_pressure`."),
      class = "oceancube_teos10_pressure_variable"
    )
  }
  item$origin <- "EXPLICIT_SOURCE_SEA_WATER_PRESSURE"
  item
}

.teos_unit_key <- function(x) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
    return(NA_character_)
  }
  tolower(gsub("[[:space:]]+", " ", trimws(x)))
}

.teos_salinity_unit <- function(item) {
  key <- .teos_unit_key(item$unit)
  if (identical(item$basis, "PRACTICAL_SALINITY_PSS78")) {
    if (!identical(key, "1")) {
      rlang::abort(
        "Practical Salinity requires the exact dimensionless unit declaration `1`.",
        class = "oceancube_teos10_salinity_unit"
      )
    }
    return(list(scale = 1, conversion = "IDENTITY_PSS78_UNITLESS"))
  }
  identity_units <- c("g kg-1", "g kg^-1", "g kg**-1", "g/kg")
  mass_fraction_units <- c("kg kg-1", "kg kg^-1", "kg kg**-1", "kg/kg")
  if (key %in% identity_units) {
    return(list(scale = 1, conversion = "IDENTITY_G_PER_KG"))
  }
  if (key %in% mass_fraction_units) {
    return(list(scale = 1000, conversion = "KG_PER_KG_TO_G_PER_KG_X1000"))
  }
  rlang::abort(
    paste0("Unsupported Absolute Salinity unit `", item$unit, "`."),
    class = "oceancube_teos10_salinity_unit"
  )
}

.teos_temperature_unit <- function(item) {
  key <- .teos_unit_key(item$unit)
  kelvin <- c("k", "kelvin")
  celsius <- c(
    "degree_celsius", "degrees_celsius", "degree_c", "degrees_c",
    "degree celsius", "degrees celsius"
  )
  if (key %in% kelvin) {
    return(list(offset = -273.15, conversion = "K_TO_DEGREE_CELSIUS_MINUS_273_15"))
  }
  if (key %in% celsius) {
    return(list(offset = 0, conversion = "IDENTITY_DEGREE_CELSIUS"))
  }
  rlang::abort(
    paste0("Unsupported thermodynamic temperature unit `", item$unit, "`."),
    class = "oceancube_teos10_temperature_unit"
  )
}

.teos_pressure_unit <- function(item) {
  key <- .teos_unit_key(item$unit)
  if (key %in% c("dbar", "decibar", "decibars")) {
    return(list(scale = 1, conversion = "IDENTITY_DBAR"))
  }
  if (key %in% c("pa", "pascal", "pascals")) {
    return(list(scale = 1e-4, conversion = "PA_TO_DBAR_X1E_MINUS_4"))
  }
  rlang::abort(
    paste0("Unsupported source sea-pressure unit `", item$unit, "`."),
    class = "oceancube_teos10_pressure_unit"
  )
}

.teos_validate_source_profile <- function(x) {
  .check_cube(x)
  .transition_validate_source_profile(x)
  current <- x$metadata$cf$current
  derived <- c(
    "vertical_sampling", "vertical_reduction", "vertical_gradient",
    "thermodynamic_state"
  )
  present <- derived[vapply(derived, function(name) {
    !is.null(current[[name]])
  }, logical(1L))]
  if (length(present)) {
    rlang::abort(
      paste0("C9 requires a direct source-profile cube; found current ",
             paste(present, collapse = ", "), "."),
      class = "oceancube_teos10_derived_input"
    )
  }
  invisible(TRUE)
}

.teos_plan <- function(x, salinity, temperature, pressure) {
  .teos_validate_source_profile(x)
  salinity_item <- .teos_resolve_variable(x, salinity, "salinity")
  temperature_item <- .teos_resolve_variable(x, temperature, "temperature")
  pressure_item <- .teos_resolve_pressure(x, pressure)
  selected_variables <- c(
    salinity_item$current_variable,
    temperature_item$current_variable,
    if (!is.null(pressure)) pressure_item$current_variable
  )
  if (anyDuplicated(selected_variables)) {
    rlang::abort(
      "Salinity, temperature, and pressure must resolve to distinct source variables.",
      class = "oceancube_teos10_variable_overlap"
    )
  }
  selected <- cube_slice(x, variable = selected_variables)
  semantics <- .vertical_value_semantics(selected)
  failed <- names(semantics)[!vapply(semantics, function(item) {
    identical(item$status, "VERTICAL_POINT")
  }, logical(1L))]
  if (length(failed)) {
    details <- vapply(semantics[failed], function(item) {
      paste0(item$variable, "=", item$status)
    }, character(1L))
    rlang::abort(
      paste0("C9 supports only direct VERTICAL_POINT state variables; found ",
             paste(details, collapse = ", "), "."),
      class = "oceancube_teos10_value_semantics_unsupported"
    )
  }
  metric <- .vertical_metric_depth(selected)
  if (any(!is.finite(metric$canonical_m)) || any(metric$canonical_m < 0)) {
    rlang::abort(
      "C9 pressure conversion requires finite physical depth >= 0 m.",
      class = "oceancube_teos10_depth_domain"
    )
  }
  .teos_validate_geography(selected$lon, selected$lat)
  salinity_unit <- .teos_salinity_unit(salinity_item)
  temperature_unit <- .teos_temperature_unit(temperature_item)
  pressure_unit <- if (is.null(pressure)) {
    list(scale = 1, conversion = "GSW_P_FROM_Z_NEGATIVE_DEPTH_M")
  } else {
    .teos_pressure_unit(pressure_item)
  }
  read_diagnostics <- selected$qa$selection$netcdf_read %||% NULL
  shape <- unname(.cube_shape(selected))
  output_elements <- .cube_product_as_double(
    c(shape[seq_len(4L)], 5), "thermodynamic_state output values"
  )
  if (output_elements > .Machine$double.xmax / 8) {
    rlang::abort(
      "Cannot estimate thermodynamic-state allocation: numeric overflow.",
      class = "oceancube_size_overflow"
    )
  }
  list(
    source = x,
    selected = selected,
    salinity = salinity_item,
    temperature = temperature_item,
    pressure = pressure_item,
    salinity_unit = salinity_unit,
    temperature_unit = temperature_unit,
    pressure_unit = pressure_unit,
    metric = metric,
    shape = shape,
    output_elements = output_elements,
    output_bytes = output_elements * 8,
    payload_reads = 1L,
    netcdf_payload_reads = as.integer(!is.null(read_diagnostics)),
    variables_read = selected_variables
  )
}

.teos_validate_geography <- function(longitude, latitude) {
  if (!is.numeric(longitude) || any(!is.finite(longitude)) ||
      any(longitude < -180 | longitude > 360)) {
    rlang::abort(
      "C9 requires finite decimal-degree longitude in [-180, 180] or [0, 360].",
      class = "oceancube_teos10_longitude"
    )
  }
  if (!is.numeric(latitude) || any(!is.finite(latitude)) ||
      any(latitude < -90 | latitude > 90)) {
    rlang::abort(
      "C9 requires finite latitude in [-90, 90] degrees north.",
      class = "oceancube_teos10_latitude"
    )
  }
  invisible(TRUE)
}

.teos_coordinate_vectors <- function(plan) {
  d <- plan$shape
  list(
    longitude = rep(plan$selected$lon, times = d[[2L]] * d[[3L]] * d[[4L]]),
    latitude = rep(
      rep(plan$selected$lat, each = d[[1L]]),
      times = d[[3L]] * d[[4L]]
    ),
    depth_m = rep(
      plan$metric$canonical_m,
      each = d[[1L]] * d[[2L]], times = d[[4L]]
    )
  )
}

.teos_normalized_longitude <- function(x) {
  normalized <- x %% 360
  normalized[normalized < 0] <- normalized[normalized < 0] + 360
  normalized
}

.teos_validate_numeric_source <- function(x, label) {
  invalid <- !is.na(x) & !is.finite(x)
  if (any(invalid)) {
    rlang::abort(
      paste0(label, " contains non-missing non-finite values."),
      class = "oceancube_teos10_source_domain"
    )
  }
}

.teos_validate_salinity_domain <- function(values, basis) {
  .teos_validate_numeric_source(values, "Source salinity")
  finite <- is.finite(values)
  if (any(values[finite] < 0)) {
    rlang::abort(
      "Finite source salinity must not be negative.",
      class = "oceancube_teos10_negative_salinity"
    )
  }
  if (identical(basis, "PRACTICAL_SALINITY_PSS78")) {
    outside <- finite & (values < 2 | values > 42)
    message <- "Certified Practical Salinity must be within the reviewed GSW range [2, 42]."
  } else {
    outside <- finite & values > 42
    message <- "Certified Absolute Salinity must be within the reviewed GSW range [0, 42] g kg-1."
  }
  if (any(outside)) {
    rlang::abort(message, class = "oceancube_teos10_salinity_domain")
  }
}

.teos_calculate <- function(plan, reference_pressure_dbar) {
  d <- plan$shape
  n <- .cube_product_as_double(d[seq_len(4L)], "thermodynamic input cells")
  if (n > .Machine$integer.max) {
    rlang::abort(
      "C9 state calculation exceeds the addressable R vector length.",
      class = "oceancube_size_overflow"
    )
  }
  n <- as.integer(n)
  source_values <- plan$selected$data
  salinity_raw <- as.numeric(source_values[, , , , 1L, drop = FALSE])
  temperature_raw <- as.numeric(source_values[, , , , 2L, drop = FALSE])
  .teos_validate_numeric_source(temperature_raw, "Source temperature")
  salinity_normalized <- salinity_raw * plan$salinity_unit$scale
  temperature_celsius <- temperature_raw + plan$temperature_unit$offset
  .teos_validate_salinity_domain(salinity_normalized, plan$salinity$basis)
  coordinates <- .teos_coordinate_vectors(plan)
  longitude_gsw <- .teos_normalized_longitude(coordinates$longitude)

  if (identical(plan$pressure$origin, "DERIVED_FROM_DEPTH_AND_LATITUDE")) {
    pressure_dbar <- gsw::gsw_p_from_z(-coordinates$depth_m, coordinates$latitude)
  } else {
    pressure_raw <- as.numeric(source_values[, , , , 3L, drop = FALSE])
    .teos_validate_numeric_source(pressure_raw, "Source sea pressure")
    pressure_dbar <- pressure_raw * plan$pressure_unit$scale
    if (any(is.finite(pressure_dbar) & pressure_dbar < 0)) {
      rlang::abort(
        "Finite source sea pressure must be non-negative.",
        class = "oceancube_teos10_pressure_domain"
      )
    }
  }

  absolute_salinity <- rep(NA_real_, n)
  if (identical(plan$salinity$basis, "ABSOLUTE_SALINITY")) {
    absolute_salinity <- salinity_normalized
  } else {
    convert <- is.finite(salinity_normalized) & is.finite(pressure_dbar)
    if (any(convert)) {
      absolute_salinity[convert] <- gsw::gsw_SA_from_SP(
        salinity_normalized[convert], pressure_dbar[convert],
        longitude_gsw[convert], coordinates$latitude[convert]
      )
    }
  }

  conservative_temperature <- rep(NA_real_, n)
  if (identical(plan$temperature$basis, "CONSERVATIVE_TEMPERATURE")) {
    conservative_temperature <- temperature_celsius
  } else if (identical(
    plan$temperature$basis, "POTENTIAL_TEMPERATURE_REF_0_DBAR"
  )) {
    convert <- is.finite(absolute_salinity) & is.finite(temperature_celsius)
    if (any(convert)) {
      conservative_temperature[convert] <- gsw::gsw_CT_from_pt(
        absolute_salinity[convert], temperature_celsius[convert]
      )
    }
  } else {
    convert <- is.finite(absolute_salinity) & is.finite(temperature_celsius) &
      is.finite(pressure_dbar)
    if (any(convert)) {
      conservative_temperature[convert] <- gsw::gsw_CT_from_t(
        absolute_salinity[convert], temperature_celsius[convert],
        pressure_dbar[convert]
      )
    }
  }

  complete <- is.finite(absolute_salinity) &
    is.finite(conservative_temperature) & is.finite(pressure_dbar)
  inside <- rep(NA, n)
  if (any(complete)) {
    inside[complete] <- gsw::gsw_infunnel(
      absolute_salinity[complete], conservative_temperature[complete],
      pressure_dbar[complete]
    )
  }
  outside <- complete & !inside
  if (any(outside)) {
    rlang::abort(
      paste0(sum(outside),
             " complete thermodynamic cell(s) lie outside the certified GSW 75-term funnel."),
      class = "oceancube_teos10_outside_funnel",
      outside_count = as.integer(sum(outside))
    )
  }

  density <- rep(NA_real_, n)
  potential_density <- rep(NA_real_, n)
  if (any(complete)) {
    density[complete] <- gsw::gsw_rho(
      absolute_salinity[complete], conservative_temperature[complete],
      pressure_dbar[complete]
    )
    potential_density[complete] <- gsw::gsw_rho(
      absolute_salinity[complete], conservative_temperature[complete],
      rep(reference_pressure_dbar, sum(complete))
    )
  }
  sigma0_error <- NA_real_
  if (identical(reference_pressure_dbar, 0) && any(complete)) {
    sigma0 <- gsw::gsw_sigma0(
      absolute_salinity[complete], conservative_temperature[complete]
    )
    sigma0_error <- max(abs(potential_density[complete] - 1000 - sigma0))
    if (!is.finite(sigma0_error) || sigma0_error > .TEOS_NUMERICAL_TOLERANCE) {
      rlang::abort(
        "Potential density failed the governed TEOS-10 sigma0 parity check.",
        class = "oceancube_teos10_sigma0_parity"
      )
    }
  }
  list(
    values = cbind(
      absolute_salinity,
      conservative_temperature,
      pressure_dbar,
      density,
      potential_density
    ),
    complete = complete,
    inside = inside,
    sigma0_error = sigma0_error,
    longitude_convention = "NORMALIZED_TO_0_360_DEGREES_EAST",
    cells_total = n
  )
}

.teos_path <- function(plan) {
  salinity <- if (identical(plan$salinity$basis, "ABSOLUTE_SALINITY")) {
    "SA_DIRECT"
  } else {
    "SP_TO_SA"
  }
  temperature <- switch(
    plan$temperature$basis,
    CONSERVATIVE_TEMPERATURE = "CT_DIRECT",
    POTENTIAL_TEMPERATURE_REF_0_DBAR = "PT0_TO_CT",
    IN_SITU_TEMPERATURE = "IN_SITU_T_TO_CT"
  )
  paste(salinity, temperature, sep = "__")
}

.teos_functions_used <- function(plan, reference_pressure_dbar) {
  functions <- c("gsw_rho", "gsw_infunnel")
  if (identical(plan$pressure$origin, "DERIVED_FROM_DEPTH_AND_LATITUDE")) {
    functions <- c("gsw_p_from_z", functions)
  }
  if (identical(plan$salinity$basis, "PRACTICAL_SALINITY_PSS78")) {
    functions <- c("gsw_SA_from_SP", functions)
  }
  functions <- c(functions, switch(
    plan$temperature$basis,
    IN_SITU_TEMPERATURE = "gsw_CT_from_t",
    POTENTIAL_TEMPERATURE_REF_0_DBAR = "gsw_CT_from_pt",
    CONSERVATIVE_TEMPERATURE = character()
  ))
  if (identical(reference_pressure_dbar, 0)) functions <- c(functions, "gsw_sigma0")
  unique(functions)
}

.teos_output_definitions <- function(reference_pressure_dbar) {
  units <- c(
    absolute_salinity = "g kg-1",
    conservative_temperature = "degree_Celsius",
    sea_water_pressure = "dbar",
    sea_water_density = "kg m-3",
    sea_water_potential_density = "kg m-3"
  )
  standard_names <- c(
    absolute_salinity = "sea_water_absolute_salinity",
    conservative_temperature = "sea_water_conservative_temperature",
    sea_water_pressure = "sea_water_pressure",
    sea_water_density = "sea_water_density",
    sea_water_potential_density = "sea_water_potential_density"
  )
  lapply(names(units), function(variable) list(
    variable = variable,
    standard_name = unname(standard_names[[variable]]),
    unit = unname(units[[variable]]),
    reference_pressure_dbar = if (identical(
      variable, "sea_water_potential_density"
    )) reference_pressure_dbar else NA_real_,
    value_semantics = "TEOS10_POINT_STATE_FROM_REPRESENTATIVE_SOURCE_VALUES"
  ))
}

.teos_descriptor <- function(plan, reference_pressure_dbar, gsw_version) {
  list(
    schema_name = "oceancube_thermodynamic_state",
    schema_version = "1.0.0",
    method = "TEOS-10",
    gsw_package_version = gsw_version,
    gsw_c_release = "3.06-16-0",
    gsw_c_commit = "657216dd4f5ea079b5f0e021a4163e2d26893371",
    salinity = list(
      source_variable = plan$salinity$current_variable,
      source_standard_name = plan$salinity$standard_name,
      basis = plan$salinity$basis,
      source_unit = plan$salinity$unit,
      unit_conversion = plan$salinity_unit$conversion,
      source_cell_methods = plan$salinity$cell_methods
    ),
    temperature = list(
      source_variable = plan$temperature$current_variable,
      source_standard_name = plan$temperature$standard_name,
      basis = plan$temperature$basis,
      source_unit = plan$temperature$unit,
      unit_conversion = plan$temperature_unit$conversion,
      source_cell_methods = plan$temperature$cell_methods
    ),
    pressure = list(
      origin = plan$pressure$origin,
      source_variable = plan$pressure$current_variable,
      source_standard_name = plan$pressure$standard_name,
      source_unit = plan$pressure$unit,
      unit_conversion = plan$pressure_unit$conversion,
      depth_to_pressure_method = if (identical(
        plan$pressure$origin, "DERIVED_FROM_DEPTH_AND_LATITUDE"
      )) "gsw_p_from_z" else NA_character_,
      height_rule = if (identical(
        plan$pressure$origin, "DERIVED_FROM_DEPTH_AND_LATITUDE"
      )) "z=-canonical_depth_m" else NA_character_
    ),
    input_path = .teos_path(plan),
    reference_pressure_dbar = reference_pressure_dbar,
    gsw_functions = .teos_functions_used(plan, reference_pressure_dbar),
    funnel_policy = "ALL_COMPLETE_SOURCE_STATES_INSIDE_GSW_75_TERM_FUNNEL",
    input_value_semantics = "DIRECT_VERTICAL_POINT_REPRESENTATIVE_STATE",
    nonlinear_interpretation = paste(
      "TEOS-10 thermodynamic state is evaluated from the supplied",
      "representative T/S point values; density of mean T/S is not claimed",
      "to equal mean density."
    ),
    output_variables = .teos_output_definitions(reference_pressure_dbar),
    certification_status = "CERTIFIED_C9_TEOS10_STATE"
  )
}

.teos_metadata <- function(metadata, descriptor) {
  out <- metadata
  current <- out$cf$current
  current$variables <- vapply(
    descriptor$output_variables, `[[`, character(1L), "variable"
  )
  current$links <- integer()
  current$semantic_status <- "CURRENT_SUPPORTED_SUBSET"
  current$derivation <- NULL
  current$vertical_reduction <- NULL
  current$vertical_sampling <- NULL
  current$vertical_gradient <- NULL
  current$stratification <- NULL
  current$thermodynamic_state <- descriptor
  out$cf$current <- current
  out$cf$interpretation$current$status <- "CURRENT_SUPPORTED_SUBSET"
  out$cf$interpretation$current$variables <- current$variables
  .cf_metadata_validate(out)
  out
}

.teos_build_output <- function(
    plan, calculated, reference_pressure_dbar, gsw_version) {
  variables <- c(
    "absolute_salinity", "conservative_temperature", "sea_water_pressure",
    "sea_water_density", "sea_water_potential_density"
  )
  output_shape <- c(plan$shape[seq_len(4L)], length(variables))
  data <- array(calculated$values, dim = output_shape)
  dimnames(data) <- list(
    lon = as.character(plan$selected$lon),
    lat = as.character(plan$selected$lat),
    depth = as.character(plan$selected$depth),
    time = as.character(plan$selected$time),
    var = variables
  )
  functions <- .teos_functions_used(plan, reference_pressure_dbar)
  descriptor <- .teos_descriptor(plan, reference_pressure_dbar, gsw_version)
  context <- .provenance_cube_context(
    source = plan$selected$source,
    dataset_id = plan$selected$dataset_id,
    time = plan$selected$time,
    shape = stats::setNames(as.integer(output_shape), .cube_axis_names()),
    variables = variables,
    backend = "memory",
    provenance = plan$selected$provenance
  )
  provenance <- .provenance_append(
    plan$selected$provenance,
    operation = "thermodynamic_state",
    parameters = list(
      requested = list(
        salinity = plan$salinity$current_variable,
        temperature = plan$temperature$current_variable,
        pressure = if (identical(
          plan$pressure$origin, "DERIVED_FROM_DEPTH_AND_LATITUDE"
        )) NULL else plan$pressure$current_variable,
        reference_pressure_dbar = reference_pressure_dbar
      ),
      resolved = list(
        salinity_standard_name = plan$salinity$standard_name,
        temperature_standard_name = plan$temperature$standard_name,
        pressure_mode = plan$pressure$origin,
        source_units = list(
          salinity = plan$salinity$unit,
          temperature = plan$temperature$unit,
          pressure = plan$pressure$unit
        ),
        thermodynamic_path = .teos_path(plan),
        salinity_conversion = plan$salinity_unit$conversion,
        temperature_conversion = plan$temperature_unit$conversion,
        pressure_conversion = plan$pressure_unit$conversion,
        longitude_convention = calculated$longitude_convention,
        reference_pressure_dbar = reference_pressure_dbar,
        gsw_package_version = gsw_version,
        gsw_functions = functions,
        funnel_test = descriptor$funnel_policy,
        complete_state_count = as.integer(sum(calculated$complete)),
        missing_state_count = as.integer(sum(!calculated$complete)),
        output_variables = variables,
        payload_read_count = plan$payload_reads
      )
    ),
    output = list(
      backend = "memory",
      shape = stats::setNames(as.integer(output_shape), .cube_axis_names()),
      variables = variables,
      time_kind = context$time_kind
    ),
    scientific_method = .provenance_method("thermodynamic_state", list()),
    context = context
  )
  qa <- list(thermodynamic_state = list(
    input_path = .teos_path(plan),
    pressure_origin = plan$pressure$origin,
    cells_total = calculated$cells_total,
    salinity_finite = as.integer(sum(is.finite(calculated$values[, 1L]))),
    temperature_finite = as.integer(sum(is.finite(calculated$values[, 2L]))),
    pressure_finite = as.integer(sum(is.finite(calculated$values[, 3L]))),
    complete_states = as.integer(sum(calculated$complete)),
    missing_states = as.integer(sum(!calculated$complete)),
    funnel_checked = as.integer(sum(calculated$complete)),
    funnel_inside = as.integer(sum(calculated$inside %in% TRUE, na.rm = TRUE)),
    funnel_outside = 0L,
    payload_reads = plan$payload_reads,
    netcdf_payload_reads = plan$netcdf_payload_reads,
    source_variables_read = plan$variables_read,
    reference_pressure_dbar = reference_pressure_dbar,
    sigma0_max_absolute_error = calculated$sigma0_error,
    output_estimated_bytes = plan$output_bytes,
    gsw_package_version = gsw_version
  ))
  out <- ocean_cube(
    lon = plan$selected$lon,
    lat = plan$selected$lat,
    depth = plan$selected$depth,
    time = plan$selected$time,
    vars = variables,
    data = data,
    units = stats::setNames(as.list(c(
      "g kg-1", "degree_Celsius", "dbar", "kg m-3", "kg m-3"
    )), variables),
    source = plan$selected$source,
    dataset_id = plan$selected$dataset_id,
    spatial_extent = plan$selected$spatial_extent,
    temporal_extent = plan$selected$temporal_extent,
    depth_extent = plan$selected$depth_extent,
    mask = plan$selected$mask,
    dc = plan$selected$dc,
    climatology = plan$selected$climatology,
    anomaly = plan$selected$anomaly,
    provenance = provenance,
    qa = qa
  )
  attributes(out$lon) <- attributes(plan$selected$lon)
  attributes(out$lat) <- attributes(plan$selected$lat)
  attributes(out$depth) <- attributes(plan$selected$depth)
  attributes(out$time) <- attributes(plan$selected$time)
  out <- .attach_cube_metadata(
    out, .teos_metadata(plan$selected$metadata, descriptor)
  )
  .check_cube(out)
  out
}
