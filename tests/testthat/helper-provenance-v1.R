provenance_test_context <- function(time = as.Date("2020-01-01") + 0:2,
                                    time_kind = "historical") {
  list(
    source = "provenance-test",
    dataset_id = "fixture-v1",
    time = time,
    time_kind = time_kind,
    calendar = "proleptic_gregorian",
    backend = "memory",
    shape = c(longitude = 1L, latitude = 1L, depth = 1L,
              time = length(time), variable = 1L),
    variables = "sst"
  )
}

provenance_test_v1 <- function(operation = "read_nc",
                               context = provenance_test_context()) {
  base <- oceancube:::.provenance_normalize(NULL, context)
  oceancube:::.provenance_append(
    base,
    operation,
    parameters = list(requested = list(variable = "sst"), resolved = list()),
    output = oceancube:::.provenance_summary(context),
    scientific_method = oceancube:::.provenance_method(operation, list()),
    context = context
  )
}

provenance_test_legacy_chain <- function() {
  list(
    parent = list(
      package = "oceancube",
      package_version = "0.2.0.9000",
      r_version = "private execution detail",
      platform = "private execution detail",
      system = list(user = "private", nodename = "private"),
      date = "2026-08-20 12:00:00 UTC",
      function_name = "read_nc",
      arguments = list(file = "C:/private/data/oisst.nc", vars = "sst"),
      extra = list(calendar = "gregorian"),
      time = list(
        source_class = "CF numeric time", source_timezone = "+00:00",
        calendar = "gregorian", calendar_defaulted = FALSE,
        decoder = "oceancube::.decode_cf_time", decode_status = "decoded",
        normalization = "CF numeric offsets decoded as UTC POSIXct"
      )
    ),
    cube_crop = list(
      operation = "cube_crop", ranges_requested = list(longitude = c(-80, -78)),
      ranges_applied = list(longitude = c(-80, -78)), outside = "clip",
      resolved_indices = list(longitude = 1:500),
      selected_coordinates = list(longitude = seq(-80, -78, length.out = 500)),
      output_shape = c(longitude = 2L, latitude = 2L, depth = 1L,
                       time = 3L, variable = 1L),
      selected_variables = "sst", cropped_utc = "2026-08-20T12:01:00Z"
    ),
    provider_note = "legacy note retained non-semantically"
  )
}
