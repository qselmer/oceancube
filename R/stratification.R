#' Compute certified TEOS-10 buoyancy-frequency-squared stratification
#'
#' @param x A memory-backed certified C9 thermodynamic state cube.
#' @param metric Stratification metric. C10 supports exactly "N2".
#' @param support Vertical support policy. "local" does not calculate across
#'   explicit support gaps; "all" may calculate while preserving gap metadata.
#'
#' @return A memory-backed ocean_cube with signed TEOS-10 N-squared and the
#' corresponding GSW pressure midpoint at every adjacent source-depth pair.
#'
#' @export
stratification <- function(x, metric = "N2", support = c("local", "all")) {
  if (!is.character(metric) || length(metric) != 1L || is.na(metric) ||
      !identical(metric, "N2")) {
    rlang::abort(
      "C10 supports exactly metric = N2.",
      class = "oceancube_stratification_metric"
    )
  }
  support <- match.arg(support)
  gsw_version <- .teos_require_dependency()
  variables <- c(
    "absolute_salinity", "conservative_temperature", "sea_water_pressure"
  )
  state <- .c10_state_require(x, variables, reference_zero = FALSE)
  selected <- if (identical(x$vars, variables)) x else {
    cube_slice(x, variable = variables)
  }
  metric_depth <- .vertical_metric_depth(selected)
  support_plan <- .vertical_gradient_support(metric_depth, require_cell = FALSE)
  source <- .cube_read(selected)
  d <- unname(.cube_shape(selected))
  output_elements <- .cube_product_as_double(
    c(d[[1L]], d[[2L]], d[[3L]] - 1L, d[[4L]], 2L),
    "stratification output values"
  )
  if (output_elements > .Machine$double.xmax / 8) {
    rlang::abort(
      "Cannot estimate stratification allocation: numeric overflow.",
      class = "oceancube_size_overflow"
    )
  }
  output <- array(
    NA_real_, dim = c(d[[1L]], d[[2L]], d[[3L]] - 1L, d[[4L]], 2L)
  )
  physical <- order(metric_depth$canonical_m)
  pair_for_physical <- vapply(seq_len(length(physical) - 1L), function(i) {
    min(physical[c(i, i + 1L)])
  }, integer(1L))
  relation_physical <- support_plan$relation[pair_for_physical]
  pressure_failures <- 0L
  finite_runs <- 0L
  singleton_runs <- 0L
  profiles <- 0L
  for (time_index in seq_len(d[[4L]])) {
    for (lat_index in seq_len(d[[2L]])) {
      for (lon_index in seq_len(d[[1L]])) {
        profiles <- profiles + 1L
        sa <- source[lon_index, lat_index, physical, time_index, 1L]
        ct <- source[lon_index, lat_index, physical, time_index, 2L]
        pressure <- source[lon_index, lat_index, physical, time_index, 3L]
        finite <- is.finite(sa) & is.finite(ct) & is.finite(pressure)
        adjacent_finite <- finite[-length(finite)] & finite[-1L]
        invalid_pressure <- adjacent_finite & diff(pressure) <= 0
        pressure_failures <- pressure_failures + sum(invalid_pressure)
        if (any(invalid_pressure)) next
        connected <- adjacent_finite
        if (identical(support, "local")) {
          connected <- connected & relation_physical != "GAPPED_SUPPORT"
        }
        group <- integer(length(finite))
        next_group <- 0L
        for (i in seq_along(finite)) {
          if (!finite[[i]]) next
          if (i == 1L || !connected[[i - 1L]]) next_group <- next_group + 1L
          group[[i]] <- next_group
        }
        groups <- unique(group[group > 0L])
        for (group_id in groups) {
          run <- which(group == group_id)
          if (length(run) < 2L) {
            singleton_runs <- singleton_runs + 1L
            next
          }
          finite_runs <- finite_runs + 1L
          calculated <- gsw::gsw_Nsquared(
            SA = sa[run], CT = ct[run], p = pressure[run],
            latitude = rep(selected$lat[[lat_index]], length(run))
          )
          n2 <- calculated$N2 %||% calculated[[1L]]
          p_mid <- calculated$p_mid %||% calculated[[2L]]
          physical_pairs <- run[-length(run)]
          storage_pairs <- pair_for_physical[physical_pairs]
          output[lon_index, lat_index, storage_pairs, time_index, 1L] <- n2
          output[lon_index, lat_index, storage_pairs, time_index, 2L] <- p_mid
        }
      }
    }
  }
  if (pressure_failures > 0L) {
    rlang::abort(
      paste0(
        pressure_failures,
        " adjacent complete state pair(s) do not have pressure increasing with physical depth."
      ),
      class = "oceancube_stratification_pressure_geometry",
      offending_count = pressure_failures
    )
  }
  out_variables <- c("buoyancy_frequency_squared", "sea_water_pressure_midpoint")
  midpoints <- metric_depth$midpoints_source
  dimnames(output) <- list(
    lon = as.character(selected$lon), lat = as.character(selected$lat),
    depth = as.character(midpoints), time = as.character(selected$time),
    var = out_variables
  )
  descriptor <- .stratification_descriptor(
    state$descriptor, metric_depth, support_plan, support, gsw_version
  )
  output_shape <- stats::setNames(as.integer(dim(output)), .cube_axis_names())
  n2_values <- output[, , , , 1L, drop = FALSE]
  finite_n2 <- is.finite(n2_values)
  neutral_tolerance <- 8 * sqrt(.Machine$double.eps) *
    max(c(1, abs(n2_values[finite_n2])))
  context <- .provenance_cube_context(
    source = selected$source, dataset_id = selected$dataset_id,
    time = selected$time, shape = output_shape, variables = out_variables,
    backend = "memory", provenance = selected$provenance
  )
  pairs_total <- d[[1L]] * d[[2L]] * (d[[3L]] - 1L) * d[[4L]]
  gapped_positions <- which(support_plan$relation == "GAPPED_SUPPORT")
  gapped_pair_cells <- length(gapped_positions) * profiles
  gapped_computed <- if (length(gapped_positions)) {
    sum(is.finite(n2_values[, , gapped_positions, , , drop = FALSE]))
  } else 0L
  provenance <- .provenance_append(
    selected$provenance, operation = "stratification",
    parameters = list(
      requested = list(metric = metric, support = support),
      resolved = list(
        input_thermodynamic_schema = state$descriptor$schema_name,
        input_thermodynamic_schema_version = state$descriptor$schema_version,
        input_certification = state$descriptor$certification_status,
        gsw_package_version = gsw_version,
        gsw_function = "gsw_Nsquared",
        physical_ordering = "CANONICAL_METRIC_DEPTH_ASCENDING_POSITIVE_DOWN",
        pressure_monotonicity_rule = "DEEPER_FINITE_PRESSURE_STRICTLY_GREATER",
        missingness_policy = "CONTIGUOUS_FINITE_RUNS; NEVER_BRIDGE_MISSING_STATE",
        support_policy = support,
        profiles_total = profiles, pairs_total = pairs_total,
        finite_N2 = as.integer(sum(finite_n2)),
        missing_N2 = as.integer(sum(!finite_n2)),
        stable_pairs = as.integer(sum(n2_values > neutral_tolerance, na.rm = TRUE)),
        neutral_pairs = as.integer(sum(abs(n2_values) <= neutral_tolerance, na.rm = TRUE)),
        unstable_pairs = as.integer(sum(n2_values < -neutral_tolerance, na.rm = TRUE)),
        gapped_pairs = as.integer(gapped_pair_cells),
        gapped_pairs_computed = as.integer(gapped_computed),
        pressure_midpoint_retained = TRUE,
        netcdf_scientific_payload_reads = 0L
      )
    ),
    output = .provenance_summary(context),
    scientific_method = .provenance_method("stratification", list()),
    context = context
  )
  qa <- list(stratification = list(
    profiles_total = profiles, pairs_total = as.integer(pairs_total),
    finite_N2 = as.integer(sum(finite_n2)),
    missing_N2 = as.integer(sum(!finite_n2)),
    stable_pairs = as.integer(sum(n2_values > neutral_tolerance, na.rm = TRUE)),
    neutral_pairs = as.integer(sum(abs(n2_values) <= neutral_tolerance, na.rm = TRUE)),
    unstable_pairs = as.integer(sum(n2_values < -neutral_tolerance, na.rm = TRUE)),
    local_contiguous_pairs = as.integer(pairs_total - gapped_pair_cells),
    gapped_pairs = as.integer(gapped_pair_cells),
    gapped_pairs_computed = as.integer(gapped_computed),
    pressure_geometry_failures = pressure_failures,
    finite_runs = finite_runs, singleton_runs = singleton_runs,
    gsw_package_version = gsw_version,
    netcdf_scientific_payload_reads = 0L,
    memory_cube_reads = 1L,
    output_estimated_bytes = output_elements * 8
  ))
  out <- ocean_cube(
    lon = selected$lon, lat = selected$lat, depth = midpoints,
    time = selected$time, vars = out_variables, data = output,
    units = stats::setNames(as.list(c("s-2", "dbar")), out_variables),
    source = selected$source, dataset_id = selected$dataset_id,
    spatial_extent = selected$spatial_extent,
    temporal_extent = selected$temporal_extent,
    depth_extent = range(midpoints), mask = selected$mask, dc = selected$dc,
    climatology = selected$climatology, anomaly = selected$anomaly,
    provenance = provenance, qa = qa
  )
  attributes(out$lon) <- attributes(selected$lon)
  attributes(out$lat) <- attributes(selected$lat)
  attributes(out$time) <- attributes(selected$time)
  attr(out$depth, "units") <- metric_depth$source_unit
  attr(out$depth, "positive") <- metric_depth$positive
  out <- .attach_cube_metadata(
    out, .cf_metadata_for_stratification(selected$metadata, midpoints, descriptor)
  )
  .check_cube(out)
  out
}

.stratification_descriptor <- function(
    state, metric, support, policy, gsw_version) {
  list(
    schema_name = "oceancube_stratification",
    schema_version = "1.0.0",
    method = "TEOS10_GSW_NSQUARED",
    source_thermodynamic_schema = state$schema_name,
    source_thermodynamic_schema_version = state$schema_version,
    source_thermodynamic_certification = state$certification_status,
    gsw_package_version = gsw_version,
    gsw_function = "gsw_Nsquared",
    metric = "N2",
    n2_standard_name = "square_of_brunt_vaisala_frequency_in_sea_water",
    source_absolute_salinity_variable = "absolute_salinity",
    source_conservative_temperature_variable = "conservative_temperature",
    source_pressure_variable = "sea_water_pressure",
    latitude_source = "cube latitude coordinate",
    source_pair_indices = metric$source_pair_indices,
    source_depth_midpoints = metric$midpoints_source,
    canonical_depth_midpoints_m = vapply(
      metric$source_pair_indices,
      function(pair) mean(metric$canonical_m[pair]), numeric(1L)
    ),
    support_policy = policy,
    support_relation = support$relation,
    support_gap_m = support$gap_m,
    missingness_policy = "CONTIGUOUS_FINITE_RUNS; NEVER_BRIDGE_MISSING_STATE",
    pressure_monotonicity_rule = "DEEPER_FINITE_PRESSURE_STRICTLY_GREATER",
    output_geometry = "GEOMETRY_NO_BOUNDS",
    output_bounds_status = "BOUNDS_MISSING",
    output_variables = list(
      list(
        variable = "buoyancy_frequency_squared",
        standard_name = "square_of_brunt_vaisala_frequency_in_sea_water",
        unit = "s-2", value_semantics = "TEOS10_PAIR_N2"
      ),
      list(
        variable = "sea_water_pressure_midpoint",
        standard_name = "sea_water_pressure", unit = "dbar",
        value_semantics = "GSW_P_MID_FOR_N2_PAIR"
      )
    ),
    certification_status = "CERTIFIED_C10_TEOS10_N2"
  )
}
